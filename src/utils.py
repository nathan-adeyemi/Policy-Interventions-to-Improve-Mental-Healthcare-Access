import time
import yaml
import os
import subprocess
import string
import random

from pathlib import Path
from ray import tune
from ray.tune.experiment.trial import Trial



def read_yaml(filename):
    with open(filename, "r") as file:
        data = yaml.safe_load(file)
    return data


def read_json_con(socket, max_length: int = 256):
    json_string = ""

    while True:
        new_piece = socket.recv(max_length).decode("utf-8")
        json_string += new_piece
        if len(new_piece) < max_length:
            break

    return json_string

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
    return "{}-{}".format(trial.trainable_name, trial.trial_id)


def open_validation_metrics(
    experiment_path: str,
    metric: str = "MSE",
    mode: str = "min",
    print_path: bool = True,
    exe_command: bool = True,
):
    trainable_path = (
        tune.ExperimentAnalysis(experiment_checkpoint_path=experiment_path)
        .dataframe()
        .sort_values(metric, ascending=(mode == "min"))
        .iloc[1, :]
        .loc["logdir"]
    )
    trainable_path = f"{os.path.dirname(experiment_path)}/trainable_{trainable_path}/validation_frames.xslx"
    if print_path:
        print(trainable_path)
    elif exe_command:
        command = f"code {trainable_path}"
        print(command)
        subprocess.run(command, shell=True, capture_output=False, text=False)
    else:
        return trainable_path


def move_results(res_dir: str, tmp_dir: str, names: list, *args, **kwargs):
    try:
        subprocess.Popen(
            ["bash", Path("src/move_results.sh").resolve()],
            env={"res_dir": res_dir, "tmp_dir": tmp_dir, "names": ",".join(names)},
        )
    except Exception as e:
        print(f"Error starting the shell script: {e}")

def generate_random_string(n_char: int = 10):
    # Define the character set: you can include letters, digits, and punctuation
    char_set = string.ascii_letters + string.digits
    # Generate a random string of the specified length
    random_string = ''.join(random.choices(char_set, k=n_char))
    return random_string

def wait_for_cluster(client, expected_workers, timeout=600, check_interval=10):
    start_time = time.time()
    while True:
        # Get the number of currently connected workers
        n_workers = len(client.scheduler_info()["workers"])

        if n_workers >= expected_workers:
            print(f"Cluster is ready with {n_workers} workers.")
            break

        # Check for timeout
        elapsed_time = time.time() - start_time
        if elapsed_time > timeout:
            if n_workers > 0:
                print(f"Timeout waiting for Dask cluster to be ready.\n Running experiment with {n_workers} workers")
            else:
                raise TimeoutError("Timeout waiting for Dask cluster to be ready. No workers available")

        # Wait before checking again
        time.sleep(check_interval)
        print(
            f"Waiting for cluster to be ready... Currently {n_workers} workers connected."
        )
