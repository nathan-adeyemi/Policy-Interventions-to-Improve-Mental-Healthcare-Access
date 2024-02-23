#!/bin/bash
#SBATCH --partition=short
#SBATCH --mem=64GB
# SBATCH --array=0-2
#SBATCH --cpus-per-task=120
#SBATCH --time=23:00:00
#SBATCH --ntasks=1
#SBATCH --output=sbatch_out_files/sensitivity_analysis/slurm-%A_%a.out 
#SBATCH --error=sbatch_out_files/sensitivity_analysis/slurm-%A_%a.err

# Testing SLURM Paramaters
#SBATCH --array=0-1
# SBATCH --cpus-per-task=10
# SBATCH --time=00:10:00

# Parameter lists to be passed to each node in the sbatch array
input_SA_options=("edToIpLambda" "ipAcceptance" "ipLoS")

cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
sensitivity_param=${input_SA_options[$SLURM_ARRAY_TASK_ID]} 

module load singularity/3.5.3

singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/experiments/Sensitivity_Analysis.R" "$sensitivity_param"
