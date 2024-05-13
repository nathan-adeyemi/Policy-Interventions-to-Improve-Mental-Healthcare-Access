import sys
sys.path.append('src')
import ray
import pickle as pkl
import pandas as pd
import multiprocessing
import os
import argparse
import shutil
import numpy as np
import utils
import trainables
import ray.tune.schedulers as sched

from pathlib import Path
from ray import train, tune
from ray.tune.search.bayesopt import BayesOptSearch
from functools import partial

def execute_tuning(
    trainer,
    tune_job_name: str,
    workers_per_trial: int = None,
    num_samples: int = 10,
    scheduler=None,
    searcher=None,
    reporter=None,
    outLogger: utils.OutLog = None,
    errLogger: utils.ErrLog = None,
    trainables_path: str = None
):
    # Set logging configuration
    if outLogger is not None and errLogger is not None:
        logger_info = (outLogger.log_path, errLogger.log_path)
    else:
        logger_info = False
        
    # Set the trainable with resources/trial
    trainable_w_resources = tune.with_resources(
            trainable=trainer,
            resources=tune.PlacementGroupFactory(bundles=[{"CPU": workers_per_trial}],
                                                 strategy="PACK"),
            )
    
    # print(f'There are {workers_per_trial} cpus required per trial')
    
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
        storage_path=trainables_path,
        name=tune_job_name,
        checkpoint_config=train.CheckpointConfig(num_to_keep=10),
        verbose=1,
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

    results = tuner.fit()
    with open(Path(f"Results/validation/{tune_job_name}.pkl").resolve(), "wb") as f:
        # pkl.dump(results, f)
        results = pkl.load(f)
        
    print("Best Probabilities Configuration: ", results.get_best_result(metric='MSE'))
    results_df = results.get_dataframe().sort_values('RMSE')
    results_df.to_csv(f"Results/validation/{tune_job_name}.csv",index=False)
    optim_trainable_name = results_df.iloc[1, :]["Trial name"]
    shutil.copytree(
        f"/scratch/adeyemi.n/validation/{tune_job_name}/{optim_trainable_name}",
        f"Results/validation/{tune_job_name}/"
    )

if __name__ == "__main__":
    os.chdir(
        "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
    )
    
    hccis = pd.read_csv("Data/HCCIS/hccis_ed_ips_2020.csv")
    siteInfo = pd.read_csv("src/simulations/function_requirements/ip_facilities_info.csv")
    
    scratch_path = (
        "/scratch/adeyemi.n/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
    )
    
    configs = utils.read_yaml(
        filename ="Code/experiments/configs/experiments.yaml"
    )
    
    sim_configs = utils.read_yaml(
        filename="Code/experiments/configs/sim_params.yaml"
    )

    parser = argparse.ArgumentParser(
        description="Execute the ED arrival scaling paramter tuning"
    )
    parser.add_argument(
        "--tune-job",
        choices=[
            "ed-arr",
            "ed-arr-sep",
            "acceptance-prob",
            "los_sensitivity_analysis",
            "lambda-sensitivity_analysis",
            "interventions"
        ],
        default=["ed-arr"],
        help="Choose the paramter tuning job to execute",
    )
    parser.add_argument(
        "--test-sim-params",
        action="store_true",
        help="Uses the test simulation parameters",
    )
    
    args = parser.parse_args()
    
    params = {}
    if args.tune_job == 'ed-arr':
        min_val=1
        max_val=2
        interval=1e-2
        
        num_samples = np.arange(min_val,max_val+interval,interval).shape[0]
        job_name = "ed_arrival_tuning"
        params = {"ed_scale_param": tune.qloguniform(min_val, max_val, interval)}

    elif args.tune_job == 'ed-arr-sep':
        job_name = "ed_arrival_tuning_sep"
        num_samples = 200
        params_0 = {}

        for unit_name, scale_param in zip(
            hccis.hccis_id.unique(), hccis.ed_scale_param.unique()
        ):
            params.update({unit_name: tune.uniform(0.5, 1.5)})
            params_0.update({unit_name: scale_param})

        params_0 = [params_0]
        
    elif args.tune_job == 'acceptance-prob':
        job_name = "acceptance_prob_tuning"
        num_samples = 300
        params_0 = {}
        for unit_name, prob in zip(
            siteInfo.Facility_name.unique(), siteInfo.Acceptance_Prob.unique()
        ):
            params.update({unit_name: tune.uniform(0, 1)})
            params_0.update({unit_name: prob})

        params_0 = [params_0]
        
    elif args.tune_job == "interventions" or "sensitivity_analysis" in args.tune_job:
        
        param_config = configs[args.tune_job]
        
        for key in list(param_config.keys()):
            
            if param_config[key]['type'] in ['logical','boolean']:
                space = tune.grid_search(['TRUE','FALSE'])
            elif 'quantized' in param_config[key]['type'] :
                space = tune.grid_search(
                    np.arange(param_config[key]["min"], param_config[key]["max"], param_config[key]['interval'])
                )
                
            
            params.update({
                param_config[key]['name']: space
            })

    if args.test_sim_params:
        # Testing run paramters
        sim_run_info = sim_configs['test']
        num_samples = sim_run_info['num_samples']
        cpu_count = sim_run_info['nproc']
    else:
        sim_run_info = sim_configs['full']
        cpu_count = multiprocessing.cpu_count()
        sim_run_info.update(
            {'nproc': cpu_count,
             'num_samples': num_samples}
        )        
        
    # Initially set all hyperparm tune options to None
    workers_per_trial = sim_run_info["nproc"] / sim_run_info["concurrent_trials"]
    scheduler = None
    searcher = None
    reporter = None
    
    if args.tune_job == 'ed-arr':
        reporter = tune.CLIReporter(
            max_report_frequency=300,
            metric_columns=["MSE"]
            )
        subfolder = "/validation/"
        simulator = partial(
            trainables.validation_trainable,
            sim_run_info=sim_run_info,
            job_name=args.tune_job,
        )

    elif args.tune_job in ['ed-arr-sep','acceptance-probs']:
        scheduler = sched.hb_bohb.HyperBandForBOHB(
            time_attr="training_iteration",
            metric="MSE",
            mode="min",
            max_t=sim_run_info["num_replications"] / workers_per_trial,
            reduction_factor=2,
            stop_last_trials=False,
        )
        searcher = BayesOptSearch(
                metric="MSE",
                mode="min",
                points_to_evaluate=params_0,
            )
        reporter = tune.CLIReporter(
            max_report_frequency=300,
            metric_columns=["MSE"],
            parameter_columns=list(params.keys())[:3], 
        )
        subfolder = "/validation/"
        simulator = partial(
            trainables.validation_trainable, 
            sim_run_info=sim_run_info, 
            job_name=args.tune_job
        )

    elif args.tune_job in ['sensitivity-analysis' ,'interventions']:
        job_name = args.tune_job
        subfolder = args.tune_job.replace('-','_')
        simulator = partial(
            trainables.sensitivity_trainable, 
            sim_run_info=sim_run_info, 
            job_name=args.tune_job
        )
        
    ray.init(
        runtime_env={
            "working_dir": "src",
            "excludes": ["/Results/deprecated/", "/Results/validation/"],
            "pip": ["pandas"],
        },
        num_cpus=cpu_count,
    )
    
    results = execute_tuning(
        trainer = simulator,
        workers_per_trial=workers_per_trial,
        tune_job_name = job_name,
        num_samples=sim_run_info['num_samples'],
        reporter = reporter,
        scheduler=scheduler,
        searcher=searcher,
        trainables_path=scratch_path+subfolder
    )
