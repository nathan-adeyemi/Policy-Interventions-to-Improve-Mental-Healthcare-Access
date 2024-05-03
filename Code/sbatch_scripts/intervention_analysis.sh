#!/bin/bash
#SBATCH --partition=long
#SBATCH --time=1-12:00:00
#SBATCH --array=0-3
#SBATCH --mem=128GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=120
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/baseline_and_interventions/slurm-%A_%a.out 
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/baseline_and_interventions/slurm-%A_%a.err

# Parameter lists to be passed to each node in the sbatch array
intervention_options=("run_baseline" "run_int_1" "run_int_2" "run_int_3")
echo ${intervention_options[$SLURM_ARRAY_TASK_ID]} 
cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
module load singularity/3.5.3
singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/experiments/Baseline_and_Intervention_Analysis.R" ${intervention_options[$SLURM_ARRAY_TASK_ID]} 
