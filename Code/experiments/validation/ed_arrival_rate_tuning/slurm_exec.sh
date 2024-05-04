#!/bin/bash
#SBATCH --partition=long
#SBATCH --time=2-00:00:00
#SBATCH --nodes=1
#SBATCH --mem=64GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=120
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/validation/ed_arrival_rate_tuning/logs/output.out
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/validation/ed_arrival_rate_tuning/logs/output.err


cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
module load singularity/3.5.3

module load anaconda3/2022.05
conda activate ed_ip_smulation
if [ "$1" == "--separate-rates" ]; then
    python3.11 Code/experiments/validation/ed_arrival_rate_tuning/main.py --separate-rates
else 
    python3.11 Code/experiments/validation/ed_arrival_rate_tuning/main.py
fi