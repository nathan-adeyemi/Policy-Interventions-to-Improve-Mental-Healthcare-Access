import yaml
import os
import numpy as np
import pandas as pd
import string

from pathlib import Path
from ray import tune
from ray.tune.experiment.trial import Trial
from functools import partial
from operator import is_not
from importlib import import_module


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


def parse_tune_param_spaces(cfg: dict, 
                            df: pd.DataFrame = None):
    
    params = {}
    params_0 = None
            
    param_list = list(
        filter(
            partial(is_not, None),
            [i if "param_" in i else None for i in list(cfg.keys())],
        )
    )

    # Parse the param_{int} args from the experiment config file
    for key in param_list:
        if cfg[key]["init_params"] and params_0 is None:
            params_0 = [{}]
        if 'id_col' in list(cfg[key]):
            params_names_list = df[cfg[key]['id_col']].unique()
        else:
            params_names_list = [cfg[key]['name']]
            
        for param_name in params_names_list:
            if cfg[key]["type"] == 'grid':
                space = tune.grid_search(cfg[key]['options'])
            elif "quantized_grid" in cfg[key]["type"]:
                space = parse_quantized_grid_space(**cfg[key])
            elif cfg[key]['type'] == 'uniform':
                space = tune.uniform(lower = cfg[key]['min'],
                                     upper = cfg[key]['max'])

            params.update({param_name: space})
        if cfg[key]['init_params']:    
            df = df.loc[:, [cfg[key]['id_col'], cfg[key]['param_col']]].to_dict(orient="records")
            for sub_dict in df:
                params_0[param_list.index(key)].update(
                    {list(sub_dict.values())[0]: list(sub_dict.values())[1]}
                )
                

    return params, params_0

def parse_tune_class(cfg, initial_parameters: list = None):
    
    tune_class = cfg['class']
    tune_module, tune_class = tune_class.rsplit('.',maxsplit=1)
    searcher = getattr(import_module(tune_module),tune_class)
    
    del cfg['class']
    if initial_parameters is not None:
        cfg["points_to_evaluate"] = initial_parameters

    searcher = searcher(**cfg)
    return searcher
    
    

def parse_quantized_grid_space(min, max, interval, sample, **kwargs):
    space = np.arange(
                        float(min),
                        float(max) + float(interval),  # Ensures the maximum value is included in the grid search
                        float(interval),
                    )
    if sample:
        space = tune.sample_from(lambda _: space)
    else:
        space = tune.grid_search(space)
        
    return space

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
