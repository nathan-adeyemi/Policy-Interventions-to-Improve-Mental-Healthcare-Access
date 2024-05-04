import ray
import pickle as pkl
import pandas as pd
import multiprocessing
import os
import subprocess
import math
import socket
import sklearn.metrics as metrics
import argparse

from pathlib import Path
from ray import train, tune
from io import StringIO
from ray.tune.search.bayesopt import BayesOptSearch
from ray.tune.schedulers.hb_bohb import HyperBandForBOHB
from ray.tune.experiment.trial import Trial

class CustomReporter(tune.CLIReporter):
    def __init__(
        self,
        sort_by_metric=True,
        parameter_columns=None,
        metric_columns=["MSE"],
        max_report_frequency=300,
        metric="MSE",
        mode="min",
    ):
        super().__init__(
            sort_by_metric=sort_by_metric,
            parameter_columns=parameter_columns,
            metric_columns=metric_columns,
            max_report_frequency=max_report_frequency,
            metric=metric,
            mode=mode,
        )
        self.num_terminated = 0

    def should_report(self, trials, done=False):
        """Reports only on trial termination events."""
        old_num_terminated = self.num_terminated
        self.num_terminated = len([t for t in trials if t.status == Trial.TERMINATED])
        return self.num_terminated > old_num_terminated


class OutLog:
    def __init__(self, log_dir):
        self.log_dir = log_dir
        self.log_path = Path(self.log_dir + "/tuning.out").resolve()
        if not os.path.exists(Path(self.log_dir).resolve()):
            os.makedirs(self.log_dir)

    def write(self, message):
        with open(self.log_path, "a") as f:
            f.write(message)


class ErrLog:
    def __init__(self, log_dir):
        self.log_dir = log_dir
        self.log_path = Path(self.log_dir + "/tuning.err").resolve()
        if not os.path.exists(Path(self.log_dir).resolve()):
            os.makedirs(self.log_dir)

    def write(self, message):
        with open(self.log_path, "a") as f:
            f.write(message)


def trial_str_creator(trial):
    return "{}_{}".format(trial.trainable_name, trial.trial_id)


def sim_trainable(params: dict):
    if len(params) > 1:
        ed_arrival_factor = (
            pd.DataFrame.from_dict(params, orient="index")
            .reset_index()
            .to_json(orient="records")
        )
    else:
        ed_arrival_factor = hccis.loc[:, ["hccis_id", "ed_scale_param"]]
        ed_arrival_factor["ed_scale_param"] = (
            ed_arrival_factor["ed_scale_param"] * params["ed_arrival_factor"]
        )
        ed_arrival_factor = ed_arrival_factor.reset_index().to_json(orient="records")

    ed_arrival_factor = ed_arrival_factor.replace('"', '"')
    sh_path = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/validation/ed_arrival_rate_tuning/sim_setup.sh"
    # sh_path = "/Users/nadeyemi/Library/CloudStorage/OneDrive-NortheasternUniversity/Graduate/Research/Minn_MH_Sim_Projects/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/validation/ed_arrival_rate_tuning/sim_setup.sh"
    port = find_available_port()

    # Create a socket
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind the socket to a specific address and port
    server_address = ("localhost", port)
    server.bind(server_address)

    # Listen for incoming connections
    server.listen(1)

    # Push all the sim meta-information to the R program via the bash
    subprocess_env = os.environ.copy()
    subprocess_env["port"] = str(port)
    subprocess_env["num_replications"] = str(sim_run_info["num_replications"])
    subprocess_env["warm_period"] = str(sim_run_info["warmup"])
    subprocess_env["sim_period"] = str(sim_run_info["sim_length"])
    subprocess_env["nproc"] = str(sim_run_info["nproc"] / concurrent_trials)

    # Begin the R simulation subprocess
    try:
        subprocess.Popen(["bash", sh_path], env=subprocess_env)
    except Exception as e:
        print(f"Error starting the shell script: {e}")

    # Accept the client connection
    client, _ = server.accept()
    client.sendall(ed_arrival_factor.encode())

    # Receieve the simulation trial's loss information
    for i in range(
        0,
        math.ceil(
            sim_run_info["num_replications"]
            / (sim_run_info["nproc"] / concurrent_trials)
        ),
    ):
        info = client.recv(4096).decode("utf-8")
        info = pd.read_json(StringIO(info))

        res_dict = {
            "MSE": metrics.mean_squared_error(
                y_true=info.loc[:, "hccis_admissions"],
                y_pred=info.loc[:, "admissions"],
            ),
            "training_iteration": i + 1
        }

        train.report(res_dict)
    return res_dict
    # Function to find an available port
    # process.kill()


def execute_tuning(
    trainer,
    workers_per_trial: int = None,
    path: str = None,
    hospital_df: pd.DataFrame = None,
    separate_rates=False,
    outLogger: OutLog = None,
    errLogger: ErrLog = None,
):
    bundle_list = [
        {"CPU": workers_per_trial}
    ]  # Reserve a single CPU for the Tune job execution

    if separate_rates:
        params = {}
        current_rates = {}

        for unit_name, scale_param in zip(
            hospital_df.hccis_id.unique()[:3], hospital_df.ed_scale_param.unique()[:3]
        ):
            params.update({unit_name: tune.uniform(0.5, 1.5)})
            current_rates.update({unit_name: scale_param})

        current_rates = [current_rates]
        tuner_config = tune.TuneConfig(
            scheduler=HyperBandForBOHB(
                time_attr="training_iteration",
                metric="MSE",
                mode="min",
                max_t=sim_run_info["num_replications"] / workers_per_trial,
                reduction_factor=2,
                stop_last_trials=False,
            ),
            search_alg=BayesOptSearch(
                metric="MSE",
                mode="min",
                points_to_evaluate=current_rates,
            ),
            num_samples=200,
            trial_dirname_creator=trial_str_creator,
            trial_name_creator=trial_str_creator,
        )
        reporter = (
            CustomReporter(
                parameter_columns=list(params.keys())[:3], max_report_frequency=1000
            ),
        )
        path += "ed_arrival_tuning_sep_EDs/"
    else:
        params = {"ed_arrival_factor": tune.qloguniform(5e-1, 1.5, 5e-2)}
        tuner_config = tune.TuneConfig(
            num_samples=sim_run_info["num_samples"],
            trial_dirname_creator=trial_str_creator,
            trial_name_creator=trial_str_creator,
        )
        reporter = tune.CLIReporter(
            max_report_frequency=300,
            metric_columns=["MSE"],
        )
        path += "ed_arrival_tuning/"

    trainable_w_resources = tune.with_resources(
        trainable=trainer,
        resources=tune.PlacementGroupFactory(bundles=bundle_list, strategy="PACK"),
    )

    if not os.path.exists(path):
        os.makedirs(path)

    if outLogger is not None and errLogger is not None:
        logger_info = (outLogger.log_path, errLogger.log_path)
    else:
        logger_info = False

    tuner = tune.Tuner(
        trainable_w_resources,
        tune_config=tuner_config,
        param_space=params,
        run_config=train.RunConfig(
            storage_path=path,
            name="acceptance_prob_tuning",
            checkpoint_config=train.CheckpointConfig(
                num_to_keep=10, checkpoint_at_end=False
            ),
            progress_reporter=reporter,
            verbose=1,
            log_to_file=logger_info,
        ),
    )

    results = tuner.fit()
    print("Best Probabilities Configuration: ", results.get_best_result().config)

    with open(Path("Results/validation/ed_arrival_tuning.pkl").resolve(), "wb") as f:
        pkl.dump(results, f)


def find_available_port(print_port=False):
    # Create a socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind to a random port
    s.bind(("localhost", 0))

    # Get the actual port number
    _, port = s.getsockname()

    # Close the socket
    s.close()

    if print_port:
        print(port)

    return port


if __name__ == "__main__":
    os.chdir(
        "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
    )
    # custom_stdout = OutLog(
    #     log_dir="Code/experiments/validation/ed_arrival_rate_tuning/logs"
    # )
    # custom_stderr = ErrLog(
    #     log_dir="Code/experiments/validation/ed_arrival_rate_tuning/logs"
    # )
    hccis = pd.read_csv("Data/HCCIS/hccis_ed_ips_2020.csv")

    parser = argparse.ArgumentParser(
        description="Execute the ED arrival scaling paramter tuning"
    )
    parser.add_argument(
        "--separate-rates", action="store_true", help="Set separate rates to True"
    )
    parser.add_argument(
        "--test-sim-params",
        action="store_true",
        help="Uses the test simulation parameters",
    )
    args = parser.parse_args()
    separate_rates = args.separate_rates

    if args.test_sim_params:
        # Testing run paramters
        cpu_count = 10
        sim_run_info = {
            "num_replications": 10,
            "warmup": 0,
            "sim_length": 3,
            "nproc": cpu_count,
            "num_samples": 5,
        }
        concurrent_trials = 3
    else:
        # Actual trial run parameters
        cpu_count = multiprocessing.cpu_count()
        sim_run_info = {
            "num_replications": 30,
            "warmup": 50,
            "sim_length": 365,
            "nproc": cpu_count,
            "num_samples": 200,
        }
        concurrent_trials = 12

    ray.init(
        runtime_env={
            "working_dir": "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access",
            "excludes": ["/Results/deprecated/", "/Results/validation/"],
            "pip": ["pandas"],
        },
        num_cpus=cpu_count,
    )

    results = execute_tuning(
        trainer=sim_trainable,
        hospital_df=hccis,
        workers_per_trial=sim_run_info["nproc"] / concurrent_trials,
        path="/scratch/adeyemi.n/validation/",
        separate_rates=separate_rates,
        # outLogger=custom_stdout,
        # errLogger=custom_stderr,
    )
