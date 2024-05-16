#!/bin/bash
#SBATCH --partition=express
#SBATCH --time=00:30:00
#SBATCH --nodes=1
#SBATCH --mem=16GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/logs/validation/output_%x.out
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/experiments/logs/validation/output_%x.err


cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
module load singularity/3.5.3
module load anaconda3/2022.05

eval "$(conda shell.bash hook)"
conda activate ed_ip_simulation

python3.11 Code/experiments/__main__.py --tune-job=$1 --test-sim-params