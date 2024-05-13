#!/bin/bash
#SBATCH --partition=short
#SBATCH --time=24:00:00
#SBATCH --nodes=1
#SBATCH --mem=64GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=120
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/validation/acceptance_prob_tuning/logs/output.out
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/validation/acceptance_prob_tuning/logs/output.err



cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
module load singularity/3.5.3

module load anaconda3/2022.05
conda activate ed_ip_smulation

python3.11 Code/experiments/validation/acceptance_prob_tuning/main.py