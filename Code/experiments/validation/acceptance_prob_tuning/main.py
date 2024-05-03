import ray
import pickle as pkl
import pandas as pd
import multiprocessing
import os
import subprocess
import socket
import sklearn.metrics as metrics

from pathlib import Path
from ray import train, tune
from io import StringIO
from ray.tune.search.bayesopt import BayesOptSearch
from ray.tune.experiment.trial import Trial

class OutLog:
    def __init__(self, log_dir):
        self.log_dir = log_dir
        self.log_path = self.log_dir + "/tuning.out"
        if not os.path.exists(Path(self.log_dir).resolve()):
            os.makedirs(self.log_dir)

    def write(self, message):
        with open(self.log_path, "a") as f:
            f.write(message)


class ErrLog:
    def __init__(self, log_dir):
        self.log_dir = log_dir
        self.log_path = self.log_dir + "/tuning.err"
        if not os.path.exists(Path(self.log_dir).resolve()):
            os.makedirs(self.log_dir)

    def write(self, message):
        with open(self.log_path, "a") as f:
            f.write(message)


class CustomReporter(tune.CLIReporter):
    
    def __init__(self):
        super(CustomReporter, self).__init__(
            sort_by_metric=True,
            max_report_frequency=300,
            parameter_columns=['Mayo Clinic Hospital - Rochester'],
            metric = 'MSE',
            mode="min"
        )
        self.num_terminated = 0

    def should_report(self, trials, done=False):
        """Reports only on trial termination events."""
        old_num_terminated = self.num_terminated
        self.num_terminated = len([t for t in trials if t.status == Trial.TERMINATED])
        return self.num_terminated > old_num_terminated


def trial_str_creator(trial):
    return "{}_{}".format(trial.trainable_name, trial.trial_id)
    
def sim_trainable(acceptance_probs: dict):
    probs = (
        pd.DataFrame.from_dict(acceptance_probs, orient="index")
        .reset_index()
        .to_json(orient="records")
    )
    sh_path = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/validation/acceptance_prob_tuning/sim_setup.sh"
    
    port = find_available_port()

    # Create a socket
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind the socket to a specific address and port
    server_address = ("localhost", port)
    server.bind(server_address)

    # Listen for incoming connections
    server.listen(1)

    # Begin the R simulation subprocess
    subprocess_env = os.environ.copy()
    subprocess_env["port"] = str(port)
    subprocess_env["acceptance_probs"] = probs
    subprocess_env["num_replications"] = str(sim_run_info['num_replications'])
    subprocess_env["warm_period"] = str(sim_run_info['warmup'])
    subprocess_env["sim_period"] = str(sim_run_info['sim_length'])
    try:
        subprocess.Popen(["bash", sh_path], env=subprocess_env).wait()
    except Exception as e:
        print(f"Error starting the shell script: {e}")

    # Accept the client connection
    client, _ = server.accept()

    # Receieve the simulation trial's loss information
    info = client.recv(4096).decode("utf-8")
    info = pd.read_json(StringIO(info))

    train.report(
        {
            "MSE": metrics.mean_squared_error(
                y_true=info.loc[:, "hccis_admissions"],
                y_pred=info.loc[:, "admissions"],
            )
        }
    )
    # Function to find an available port
    # process.kill()


def execute_tuning(
    trainer,
    hospital_df: pd.DataFrame = None,
    workers_per_trial: int = None,
    path: str = None,
    outLogger: OutLog = None,
    errLogger: ErrLog = None
):
    bundle_list = [
        {"CPU": workers_per_trial}
    ]  # Reserve a single CPU for the Tune job execution

    if not os.path.exists(path):
        os.makedirs(path)

    probs = {}
    current_probs = {}

    for unit_name, prob in zip(
        hospital_df.Facility_name.unique(), hospital_df.Acceptance_Prob.unique()
    ):
        probs.update({unit_name: tune.uniform(0, 1)})
        current_probs.update({unit_name: prob})

    current_probs = [current_probs]

    trainable_w_resources = tune.with_resources(
        trainable=trainer,
        resources=tune.PlacementGroupFactory(bundles=bundle_list, strategy="PACK"),
    )
    
    reporter = CustomReporter()

    tuner = tune.Tuner(
        trainable_w_resources,
        tune_config=tune.TuneConfig(
            search_alg=BayesOptSearch(
                metric="MSE",
                mode="min",
                points_to_evaluate=current_probs
            ),
            num_samples=200,
            trial_dirname_creator=trial_str_creator,
            trial_name_creator=trial_str_creator,
        ),
        param_space=probs,
        run_config=train.RunConfig(
            storage_path=path,
            name="acceptance_prob_tuning",
            checkpoint_config=train.CheckpointConfig(
                num_to_keep=10, checkpoint_at_end=False
            ),
            progress_reporter=reporter,
            log_to_file=(outLogger.log_path, errLogger.log_path),
            # log_to_file="sbatch_out_files/acceptance_tuning.log",
        ),
    )

    results = tuner.fit()
    print("Best Probabilities Configuration: ", results.get_best_result().config)

    with open(
        Path("Results/validation/acceptance_prob_tuning.pkl").resolve(), "wb"
    ) as f:
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
    siteInfo = pd.read_csv("simulations/function_requirements/ip_facilities_info.csv")
    
    custom_stdout = OutLog(log_dir="Code/experiments/validation/acceptance_prob_tuning/logs")
    custom_stderr = ErrLog(log_dir="Code/experiments/validation/acceptance_prob_tuning/logs")

    ray.init(
        runtime_env={
            "working_dir": "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access",
            "excludes": ["/Results/deprecated/", "/Results/validation/"],
            "pip": ["pandas"],
        },
        num_cpus=multiprocessing.cpu_count(),
    )

    sim_run_info = {
        'num_replications' :30,
        'warmup' :50,
        'sim_length' : 365
    }

    results = execute_tuning(
        trainer=sim_trainable,
        hospital_df=siteInfo,
        workers_per_trial=sim_run_info['num_replications'],
        path=Path("/scratch/adeyemi.n/validation/").resolve(),
        outLogger=custom_stdout,
        errLogger=custom_stderr
    )
