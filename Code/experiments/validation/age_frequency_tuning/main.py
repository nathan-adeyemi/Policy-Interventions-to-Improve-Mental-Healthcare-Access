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
from ray.tune.search.hyperopt import HyperOptSearch
from ray.tune.experiment.trial import Trial

class CustomReporter(tune.CLIReporter):
    
    def __init__(self):
        super(CustomReporter, self).__init__(
            sort_by_metric=True,
            max_report_frequency=300,
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
    
def sim_trainable(frequencies: dict):

    sh_path = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/sim_setup.sh"
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
    subprocess_env["acceptance_probs"] = str(list(frequencies.values()))
    subprocess_env["num_replications"] = str(num_replications)

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
    workers_per_trial: int = None,
    path: str = None,
):
    bundle_list = [
        {"CPU": workers_per_trial}
    ]  # Reserve a single CPU for the Tune job execution

    if not os.path.exists(path):
        os.makedirs(path)
    
    
    freqs = {
        "Adolescent": tune.uniform(0.15, .25),
        "Adult": tune.uniform(0.45, 0.7),
        "Child": tune.uniform(0.05, 0.2),
        "Geriatric": tune.uniform(0.1, .25)
    }
    
    init_freq = [
        {"Adolescent": 0.2179, "Adult": 0.6038, "Children": 0.0727, "Geriatric": 0.1460}
    ]
    

    trainable_w_resources = tune.with_resources(
        trainable=trainer,
        resources=tune.PlacementGroupFactory(bundles=bundle_list, strategy="PACK"),
    )
    
    search_alg = HyperOptSearch(
        metric = 'MSE',
        mode='min',
        points_to_evaluate=init_freq
    )
    
    reporter = CustomReporter()

    tuner = tune.Tuner(
        trainable_w_resources,
        tune_config=tune.TuneConfig(
            search_alg=search_alg(),
            num_samples=200,
            metric="MSE",
            mode="min",
            trial_dirname_creator=trial_str_creator,
            trial_name_creator=trial_str_creator,
        ),
        param_space=freqs,
        run_config=train.RunConfig(
            storage_path=path,
            name="acceptance_prob_tuning",
            checkpoint_config=train.CheckpointConfig(
                num_to_keep=10, checkpoint_at_end=False
            ),
            progress_reporter=reporter,
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

    ray.init(
        runtime_env={
            "working_dir": "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access",
            "excludes": ["/Results/deprecated/", "/Results/validation/"],
            "pip": ["pandas"],
        },
        num_cpus=multiprocessing.cpu_count(),
    )

    num_replications = 5

    results = execute_tuning(
        trainer=sim_trainable,
        hospital_df=siteInfo,
        workers_per_trial=num_replications,
        path=Path("/scratch/adeyemi.n/validation/").resolve(),
    )
