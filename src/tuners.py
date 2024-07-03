import utils
import pandas as pd
import os
import trainables
import itertools

from ConfigSpace import Configuration, ConfigurationSpace
from typing import Union
from ray import tune, train
from parsers import parse_param_spaces

def execute_ray_tuning(
    trainer,
    params,
    res_dir: str,
    trainables_path: str,
    tune_job_name: str = None,
    cpus_per_trial: int = 1,
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
                bundles=[{"CPU": cpus_per_trial}], strategy="PACK"
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
        names=[f"trainable_{name}" for name in list(results_df.iloc[10, :]["logdir"])],
    )


class tune_job:
    def __init__(
        self,
        tune_job_name: str ,
        cfg: dict ,
        res_dir: str,
        simulator: trainables.trainable,
        seed: int = 42,
        *args,
        **kwargs,
    ):
        self.tune_job = kwargs.get('tune_job_name', tune_job_name)
        self.param_config = kwargs.get('cfg',cfg)
        self.seed = kwargs.get('seed',seed)
        self.simulator = kwargs.get("simulator", simulator)
        self.n_samples = self.param_config["num-samples"]
        self.res_dir = kwargs.get("res_dir", res_dir)
        if not os.path.exists(self.res_dir):
            os.makedirs(self.res_dir)

        # Removing extraneoous arguments
        del args
        
    @property
    def configspace(self) -> ConfigurationSpace:
        cs = parse_param_spaces(self.param_config, ray_space=False, seed=self.seed)
        return cs

    def add_initial_parameters(self) -> dict:
        pass

    def get_dataframe(self) -> pd.DataFrame:
        # Method to convert job results to a dataframe for further processing
        pass

    def train(self, config: Union[Configuration,dict], seed: int = 0) -> float:
        
        return self.simulator(
            params=dict(config) if isinstance(config, Configuration) else config,
            seed=seed,
        )
    
    def generate_all_configs(self):
        # Extract all possible values for each hyperparameter
        all_values = [hp.choices for hp in self.configspace.get_hyperparameters()]
        
        # Generate the Cartesian product of all possible values
        all_combinations = itertools.product(*all_values)
        
        # Create configurations from the combinations
        self.grid_search_results = [
            dict(zip(self.configspace.get_hyperparameter_names(), values))
            for values in all_combinations
        ]
    
    def grid_search(self,runner = None):
        self.generate_all_configs()
        
        if runner:
            run_job = runner.map(self.train, self.grid_search_results)
            results = runner.gather(run_job)
        else:
            results = [self.train(i) for i in self.grid_search_results]
            
        self.grid_search_results = pd.concat([pd.DataFrame(res) for res in results], axis = 0)
        self.grid_search_results.to_csv(path_or_buf=os.path.join(self.res_dir,'grid-search-results.csv'))
        
        return self.grid_search_results
        