import sys

sys.path.append("src")

import ray
import os
import argparse
import trainables as trains

from smac import HyperparameterOptimizationFacade as HPOFacade, Scenario
from parsers import parse_cli_args
from tuners import tune_job, execute_ray_tuning
from utils import wait_for_cluster, read_yaml
from dask_jobqueue import SLURMCluster
from pathlib import Path
from dask.distributed import Client


if __name__ == "__main__":
    home_dir = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
    os.chdir(home_dir)

    parser = argparse.ArgumentParser(
        description="Execute the ED arrival scaling paramter tuning"
    )
    parser.add_argument(
        "--tune-job",
        "-j",
        choices=list(read_yaml(filename="Code/experiments/configs/experiments.yaml").keys()),
        default="ed-arr",
        help="Choose the paramter tuning job to execute",
    )
    parser.add_argument(
        "--sim-params",
        default="test",
        choices=list(read_yaml(filename="Code/experiments/configs/sim_params.yaml").keys()),
        action="store",
        help="Choose which simulation configuration to use"
    )

    parser.add_argument(
        "--checkpoint-path",
        "-c",
        default=None,
        help="Path to tune job checkpoint location",
    )

    parser.add_argument(
        "--num-workers",
        "-w",
        type=int,
        action="store",
        default=len(os.sched_getaffinity(0)),
    )
    
    parser.add_argument(
        '--time',
        action="store",
        help="How long to hold compute resources?",
        default="1:00:00"
    )
    
    parser.add_argument(
        "--cluster-cfg",
        choices=list(read_yaml(filename="Code/experiments/configs/cluster.yaml")['dask'].keys()),
        default="debug",
        help="What resource configuration should be used to execute jobs?",
    )
    
    parser.add_argument(
        '--partition',
        action='store',
        default='short',
        choices=["express",'short', 'long'],
        help='Which HPC partition to request'
    )

    parser.add_argument(
        "--backend",
        "-b",
        choices=['single','ray','dask'],
        default='single',
        action='store'
    )

    cli_args = parser.parse_args()
    scratch_path = (
        "/scratch/adeyemi.n/"
    )
    
    job_cfg, cluster_cfg = parse_cli_args(
        args=cli_args,
        scratch_path=scratch_path,
    )
    if (
        cli_args.backend == "ray"
    ):  # Ignore creating a Ray cluster if only obtaining results
        try:  # Try to connect to a created Ray cluster or create the cluster (if local)
            ray.init(**cluster_cfg)
            results = execute_ray_tuning(**job_cfg)
        except KeyError as e:
            # Print cluster creation error code
            print(e)
            
    else:
        if cli_args.backend == 'dask':
            scratch_path = os.path.join(scratch_path, cli_args.tune_job)
            if not os.path.exists(scratch_path):
                os.makedirs(scratch_path)
            # Set up Dask distr cluster and client
            cluster = SLURMCluster(**cluster_cfg)
            cluster.scale(cores=cli_args.num_workers)
            
            dsk_client = Client(cluster, timeout="600s")
            wait_for_cluster(
                client=dsk_client,
                expected_workers = cli_args.num_workers,
                timeout = 7200,
                check_interval = 15
            )
            for module in ["utils", "trainables", "parsers", "tuners"]:
                dsk_client.upload_file(f"src/{module}.py")
                
        else:
            dsk_client = None
        
        if job_cfg.get('job-type') == "tune" :
            # Set up SMAC3 optimization job    
            job = tune_job(**job_cfg)
            scenario = Scenario(
                configspace=job.configspace,
                deterministic=True,
                output_directory=Path(job.res_dir).resolve(),
                n_trials=job.n_samples,
                seed=job.seed,
            )
            smac = HPOFacade(scenario, job.train, dask_client=dsk_client, logging_level=20)
            incumbent = smac.optimize()
            cluster.close()
            incumbent_sim = trains.trainable(
                params=dict(incumbent),
                job_name=job_cfg.get("tune_job_name"), 
                trainable_name="Incumbent",
                fn_cfg_path= "Code/experiments/configs/sim_params.yaml",
                fn_cfg_sel = "debug" if cli_args.cluster_cfg == "debug" else "full",
                output_metric="admissions_by_faciltiy",
                optim_stat="RMSE",
                ray=False,
                scratch_path=Path(job.res_dir).resolve(),
            )
        elif job_cfg.get('job-type') == 'evaluation':
            job = tune_job(**job_cfg)
            results = job.grid_search(runner = dsk_client if cli_args.backend == 'dask' else None)
        
    print(results)
    print("Tuning Job Complete")
