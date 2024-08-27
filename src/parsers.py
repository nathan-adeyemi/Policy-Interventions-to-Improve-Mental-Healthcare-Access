import os
import numpy as np
import pandas as pd
import re

from trainables import trainable
from ray import tune
from pathlib import Path
from functools import partial
from importlib import import_module
from ConfigSpace import (
    CategoricalHyperparameter,
    UniformFloatHyperparameter,
    ConfigurationSpace,
)
from utils import read_yaml


def parse_param_spaces(
    cfg: dict, ray_space: bool = False, tune_job: str = "ed-arr", seed: int = 42
):
    # Load the appropriate dataframe based on the tune_job
    df = (
        pd.read_csv("Data/HCCIS/hccis_ed_ips_2020.csv")
        if re.search("ed-arr-sep", tune_job)
        else pd.read_csv("src/simulations/function_requirements/ip_facilities_info.csv")
    )

    # Extract parameters list
    param_list = [i for i in cfg.keys() if "param-" in i]
    params = {}
    params_0 = (
        [{}]
        if ray_space and any(cfg.get(key).get("init_params") for key in param_list)
        else None
    )

    for key in param_list:
        df = (
            pd.read_csv(Path(cfg.get(key).get("df")).resolve())
            if "df" in cfg.get(key).keys()
            else None
        )
        param_names = (
            df[cfg.get(key).get("id-col")].unique()
            if "id-col" in cfg.get(key)
            else [cfg.get(key).get("name")]
        )

        for param_name in param_names:
            if cfg.get(key).get("type") == "grid":
                space = (
                    tune.grid_search(cfg.get(key).get("options"))
                    if ray_space
                    else CategoricalHyperparameter(
                        name=param_name, choices=cfg.get(key).get("options")
                    )
                )
            elif "quantized_grid" in cfg.get(key).get("type"):
                space = parse_quantized_grid_space(**cfg.get(key), ray_space=ray_space)
            elif cfg.get(key).get("type") == "uniform":
                space = (
                    tune.uniform(lower=cfg.get(key).get("min"), upper=cfg.get(key).get("max"))
                    if ray_space
                    else UniformFloatHyperparameter(
                        name=param_name, lower=cfg.get(key).get("min"), upper=cfg.get(key).get("max")
                    )
                )
            elif cfg.get(key).get("type") == "float":
                # To-Do : add in functionality for float and categorical variables
                pass

            elif cfg.get(key).get("type") == "categorical":
                pass

            params[param_name] = space

        if params_0 is not None and cfg.get(key).get("init_params"):
            for sub_dict in df[[cfg.get(key).get("id_col"), cfg.get(key).get("param-col")]].to_dict(
                orient="records"
            ):
                params_0[param_list.index(key)].update(
                    {list(sub_dict.values())[0]: list(sub_dict.values())[1]}
                )

    if ray_space:
        return params, params_0
    else:
        return ConfigurationSpace(seed=seed, space=params)


def parse_tune_class(class_type: str, cfg: dict, initial_parameters: list = None):
    tune_class = cfg.get("class")
    tune_module, tune_class = tune_class.rsplit(".", maxsplit=1)
    searcher = getattr(import_module(tune_module), tune_class)

    del cfg["class"]
    if class_type == "searcher" and initial_parameters is not None:
        cfg.update({"points_to_evaluate": initial_parameters})

    searcher = searcher(**cfg)
    return searcher


def parse_quantized_grid_space(
    min,
    max,
    interval,
    sample: bool = False,
    ray_space: bool = True,
    name: str = None,
    *args,
    **kwargs,
):
    space = np.around(np.arange(
        float(min),
        float(max)
        + float(interval),  # Ensures the maximum value is included in the grid search
        float(interval),
    ),
                      decimals=2)
    if sample:
        space = tune.sample_from(lambda _: space)
    elif ray_space:
        space = tune.grid_search(space)
    else:
        space = CategoricalHyperparameter(name, choices=list(space))

    return space


def parse_cluster_args(args, scratch_path: str = None) -> dict:
    cluster_cfg = read_yaml(filename="Code/experiments/configs/cluster.yaml")

    if args.backend == "ray":
        
        # Create configuration dictionary for Dask Cluster
        cluster_cfg = cluster_cfg.get("ray")
        
        if cluster_cfg.get('single-node'):            
            cluster_cfg.update(
                {
                    "num_cpus": args.num_cpus,
                    "storage": os.path.join(scratch_path,args.tune_job),
                }
            )
            for key, value in cluster_cfg.get("single-node").items():
                cluster_cfg[key] = value
            del cluster_cfg["single-node"]
        else:
            # Connect to compute cluster launched by Ray CLI
            cluster_cfg.update({"address": f"ray://{os.environ['ip_head']}"})
            
    elif args.backend == "dask":
        
        # Create configuration dictionary for Dask Cluster
        cluster_cfg = cluster_cfg.get("dask")[args.cluster_cfg]
        for field in ["local_directory", "log_directory"]:
            cluster_cfg[field] = Path(os.path.join(cluster_cfg[field],args.tune_job)).resolve()

        cluster_cfg.update({"walltime": args.time, "queue": args.partition})

    elif args.backend == "single":
        cluster_cfg = None

    else:
        raise

    return cluster_cfg


def parse_cli_args(args, scratch_path: str, seed: int = 42):
    cluster_cfg = read_yaml(filename="Code/experiments/configs/cluster.yaml")
    configs = read_yaml(filename="Code/experiments/configs/experiments.yaml")

    cfg = (
        configs.get(os.path.basename(args.checkpoint_path))
        if args.checkpoint_path
        else configs.get(args.tune_job)
    )

    if args.backend == "ray":
        scratch_path += "/ray"
    else:
        scratch_path = os.path.join(scratch_path, args.tune_job)

    job_cfg = {
        "job-type": cfg.get("job-type"),
        "trainables_path": scratch_path,
        "simulator": lambda params,seed, trainable_path = None: trainable(
            params=params,
            fn_cfg_sel=args.sim_params,
            fn_cfg_path=Path(cfg.get("fn-cfg-path")).resolve(),
            output_metric=cfg.get("output-statistic"),
            optim_stat=cfg.get("obj-fn"),
            job_name=args.tune_job,
            scratch_path=Path(scratch_path).resolve(),
            ray=False,
            seed=seed,
            trainable_path=trainable_path
        ),
        "cfg": configs.get(args.tune_job),
        "num-workers": 1
        if args.backend == "single"
        else cluster_cfg.get(args.backend).get(args.cluster_cfg).get("num-workers"),
        "res_dir": Path(
            os.path.join(
                "Results/validation"
                if any(i in args.tune_job for i in ["acceptance-prob", "ed-arr"])
                else "Results",
                args.tune_job,
            )
        ).resolve(),
    }
    job_cfg.update({"tune_job_name":args.tune_job})

    if args.checkpoint_path is None:
        job_cfg.update(
            {
                "num_samples": cfg.get("num-samples"),
            }
        )
        if args.backend == "ray":
            job_cfg.update({"cpus_per_trial": args.num_cpus / job_cfg.get("num-workers"),})
            job_cfg["params"], job_cfg["params_0"] = parse_param_spaces(
                cfg=cfg, ray_space=True, tune_job=args.tune_job
            )
            for obj in ["searcher", "scheduler"]:
                if obj in cfg:
                    job_cfg[obj] = parse_tune_class(
                        class_type=obj,
                        cfg=cfg[obj],
                        initial_parameters=job_cfg.get("params_0"),
                    )

            job_cfg.update({"reporter": tune.CLIReporter(
                max_report_frequency=300,
                metric_columns={"MSE": "Mean Squared Error"},
                parameter_columns=[
                    {list(job_cfg.get("params").keys())[i]: f"param_{i+1}"}
                    for i in range(min(len(list(job_cfg.get("params").keys())), 5))
                ],
                print_intermediate_tables=True,
                sort_by_metric=True,
            )})

        else:
            job_cfg.update({"seed":seed})

        cluster_cfg = None
        if not args.backend == "single":
            cluster_cfg = parse_cluster_args(args=args)
    else:
        job_cfg.update({"checkpoint_path" : args.checkpoint_path})
        cluster_cfg = None

    return job_cfg, cluster_cfg
