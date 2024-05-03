#!/bin/bash
#SBATCH --partition=short
#SBATCH --time=08:00:00
#SBATCH --nodes=1
#SBATCH --mem=128GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=51
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/convergence_analysis/slurm-%A_%a.out 
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/convergence_analysis/slurm-%A_%a.err


cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
module load singularity/3.5.3
singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/experiments/Warmup_and_Replications.R" "normal_validation"