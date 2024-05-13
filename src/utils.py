import yaml
import os
from pathlib import Path
from ray import tune
from ray.tune.experiment.trial import Trial

def read_yaml(filename):
    with open(filename, "r") as file:
        data = yaml.safe_load(file)
    return data

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
