#!/bin/bash
#SBATCH --partition=short
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --mem=64GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=30
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/validation.out 
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/validation.err


cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
module load singularity/3.5.3

module load anaconda3/2022.05
conda activate ed_ip_smulation

singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/experiments/validation/ed_factor_ED_Admissions/auto_accept/main.R"