# Policy-Interventions-to-Improve-Mental-Healthcare-Access

This repo holds code for the paper [*Policy Intervention to Improve Mental Health Access: A Discrete Event Simulation Study*](https://arxiv.org/abs/2304.13849). This project comprises of a discrete event simulation model detailing when and where patients presenting to the Emergency Department receive psychaitric inpatient care. Additionally, we include methods for determining simulation input parameters given limited data availability for individual hospitals/healthcare providers, sensitivity analysis around these input parameters, and systemic modifications to reduce time spent in the healthcare system where patients do not receive psychiatric care.

# Usage
All intervention sensitivity analysis and model tuning experiments should be initiated with the `Code/experiments/__main__.py`. The main has the following arguments to configure the job being run:
- `tune-job`: The name of the job being run. Experiments are denoted by their respective key in the `Code/experiments/configs/experiments.yaml` (e.g. to run the interventions analysis, one would add `--tune-job=interventions`)
- `sim-params`: Argument controls which simulation configuration to use. Currently 3 options are supported for development/debugging, testing, and full simulation replications
- `checkpoint-path`: Path created by SMAC during model tuning. Use this azrgument when resuming a tuning job.
- `cluster-cfg`: Which cluster configuration to use,. Choose from the keys of the `Code/experiments/configs/cluster.yaml`
- `backend`: What method for running multiple sdimualtion replications. Default to single replication executiuon but parallelism is supported through the python multiprocessing library and dask.distributed
- `num-workers`: *Only used when backend=Dask* Number of Dask workers to call.

For those using the HPC/SLURM system, using the `slurm_launch.py` file, one can submit some or multiple jobs to the SLURM scheduler without creating a sheel script for every job. Running the file with following arguments will create a temporary shell script to submit the job to the scheduler

*SLURM-Specific Arguments*
- `partition`: Which HPC partition to call workers from
- `time`: Duration of SLURM worker node calls
- `mem`: Memory (in Gb) each worker node should have access to
- `load_env`: Any additional commands to add to the SLURM job submission script
- `command`: Command to be executed with the SLURM job submission 
- `backend`: see above

## Examples
Examples for running experiments both with the `Code/experiments/__main__.py` or the `slurm-launch.py` can be found in hte `Code/experiments/experiment-commands`

# Data Analysis Usage
Data cleaning, input parameter modelling, and validation metric calculation are executed using R rather than Python. Currently 2 data analysis workflows are supported.
- `Code/data_analysis/Simulation Input Parameters.R `: Generates the DES input parameters. Calls two scripts to clean and organize data from clinical partners and publicly available data. Upon completion, most model inputs are stored in `src/simulations/
- `Code/data_analysis/Validation Metric Calculation.R`: Calculates the metrics used to validate the simulation model. Validation metrics are stored at`Data/Validation Metric Dataframes.rds`

# Requirements

Refer to the `environment.yaml` for system requirements for python workflows. Refer to the `.Rprofile` for required R libraries.
