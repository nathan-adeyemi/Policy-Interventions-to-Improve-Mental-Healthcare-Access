import sys
sys.path.append("src")

import ray
import pandas as pd
import multiprocessing
import os
import argparse
import utils
import trainables
import re

from pathlib import Path
from ray import train, tune, air
from functools import partial
from ray.tune.experimental.output import AirVerbosity, get_air_verbosity

os.environ["RAY_AIR_NEW_OUTPUT"] = "1"
run_config = air.RunConfig(verbose=get_air_verbosity(AirVerbosity.DEFAULT))


def execute_tuning(
    trainer,
    res_dir: str,
    trainables_path: str,
    tune_job_name: str = None,
    workers_per_trial: int = 1,
    num_samples: int = 10,
    scheduler=None,
    searcher=None,
    reporter=None,
    outLogger: utils.OutLog = None,
    errLogger: utils.ErrLog = None,
    checkpoint_path: str = None,
):
    if checkpoint_path is not None:
        if tune.Tuner.can_restore(checkpoint_path):
            tuner = tune.Tuner.restore(path=checkpoint_path, trainable=trainer)
    else:
        # Set logging configuration
        if outLogger is not None and errLogger is not None:
            logger_info = (outLogger.log_path, errLogger.log_path)
        else:
            logger_info = False

        # Set the trainable with resources/trial
        trainable_w_resources = tune.with_resources(
            trainable=trainer,
            resources=tune.PlacementGroupFactory(
                bundles=[{"CPU": workers_per_trial}], strategy="PACK"
            ),
        )

        # Set the baseline tune config
        tuner_config = tune.TuneConfig(
            search_alg=searcher,
            scheduler=scheduler,
            num_samples=num_samples,
            trial_dirname_creator=utils.trial_str_creator,
            trial_name_creator=utils.trial_str_creator,
        )

        # Set the baseline run config
        run_config = train.RunConfig(
            storage_path=os.path.dirname(trainables_path),
            name=tune_job_name,
            checkpoint_config=train.CheckpointConfig(num_to_keep=10),
            verbose=2,
            log_to_file=logger_info,
            progress_reporter=reporter,
        )

        if not os.path.exists(trainables_path):
            os.makedirs(trainables_path)

        tuner = tune.Tuner(
            trainable_w_resources,
            tune_config=tuner_config,
            param_space=params,
            run_config=run_config,
        )

    tuner.fit()
    results = tuner.get_results()
    results_df = results.get_dataframe().sort_values("RMSE")
    results_df.to_csv(os.path.join(res_dir, "tuning_results.csv"), index=False)
    print(
        "Best Probabilities Configuration: ",
        results.get_best_result(metric="MSE", mode="min"),
        "\n",
    )
    print(results.get_dataframe().sort_values("RMSE"))

    utils.move_results(
        res_dir=res_dir,
        tmp_dir=trainables_path,
        names= [f"trainable_{name}" for name in list(results_df.iloc[10, :]["logdir"])],
    )


if __name__ == "__main__":
    os.chdir(
        "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
    )

    hccis = pd.read_csv("Data/HCCIS/hccis_ed_ips_2020.csv")
    siteInfo = pd.read_csv(
        "src/simulations/function_requirements/ip_facilities_info.csv"
    )

    scratch_path = (
        "/scratch/adeyemi.n/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
    )

    configs = utils.read_yaml(filename="Code/experiments/configs/experiments.yaml")

    sim_configs = utils.read_yaml(filename="Code/experiments/configs/sim_params.yaml")

    parser = argparse.ArgumentParser(
        description="Execute the ED arrival scaling paramter tuning"
    )
    parser.add_argument(
        "--tune-job",
        "-j",
        choices=list(configs.keys()),
        default="ed-arr",
        help="Choose the paramter tuning job to execute",
    )
    parser.add_argument(
        "--test-sim-params",
        "-t",
        action="store_true",
        help="Uses the test simulation parameters",
    )

    parser.add_argument(
        "--checkpoint-path",
        "-c",
        default=None,
        help="Path to tune job checkpoint location",
    )

    args = parser.parse_args()

    if args.checkpoint_path is None:
        param_config = configs[args.tune_job]
    else:
        param_config = configs[os.path.basename(args.checkpoint_path)]

    if args.test_sim_params:
        # Testing run paramters
        sim_run_info = sim_configs["test"]
    else:
        sim_run_info = sim_configs["full"]
        sim_run_info.update({"nproc": multiprocessing.cpu_count()})

    simulator = partial(
        trainables.trainable,
        sim_run_info=sim_run_info,
        output_metric=param_config["output_metric"],
        job_name=args.tune_job,
    )

    job_cfg = {"trainer": simulator, "trainables_path": scratch_path}

    if any([i in args.tune_job for i in ["acceptance-prob", "ed-arr"]]):
        job_cfg.update({"res_dir": os.path.join("Results/validation", args.tune_job)})
    else:
        job_cfg.update({"res_dir": os.path.join("Results", args.tune_job)})

    if args.checkpoint_path is None:
        params, params_0 = utils.parse_tune_param_spaces(
            cfg=param_config,
            df=hccis if re.search("ed-arr-sep", args.tune_job) else siteInfo,
        )

        sim_run_info.update({"num_samples": param_config["num_samples"]})

        # Initially set all hyperparm tune options to None
        workers_per_trial = sim_run_info["nproc"] / sim_run_info["concurrent_trials"]

        job_cfg.update(
            {
                "workers_per_trial": workers_per_trial,
                "tune_job_name": args.tune_job,
                "num_samples": sim_run_info["num_samples"],
            }
        )
        for i in [
            obj for obj in ["searcher", "scheduler"] if obj in list(param_config.keys())
        ]:
            job_cfg.update(
                {
                    i: utils.parse_tune_class(
                        class_type=i,
                        cfg=param_config[i],
                        initial_parameters=params_0,
                    )
                }
            )

        job_cfg.update(
            {
                "reporter": tune.CLIReporter(
                    max_report_frequency=300,
                    metric_columns={"MSE": "Mean Squared Error"},
                    parameter_columns=[
                        {list(params.keys())[i]: f"param_{i+1}"}
                        for i in range(0, min(len(list(params.keys())), 5))
                    ],
                    print_intermediate_tables=True,
                    sort_by_metric=True,
                )
            }
        )
    else:
        job_cfg.update({"checkpoint_path": args.checkpoint_path})

    # ray.init(
    #     runtime_env={
    #         "working_dir": "src",
    #         "excludes": ["/Results/deprecated/", "/{res_dir}/"],
    #         "pip": ["pandas","numpy",]
    #     },
    #     num_cpus=sim_run_info["nproc"],
    #     storage= scratch_path
    # )
    ray.init(
        address=os.environ["ip_head"]
    )

    results = execute_tuning(**job_cfg)
