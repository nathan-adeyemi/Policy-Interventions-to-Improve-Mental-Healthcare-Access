cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
export PATH="$PATH:/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/Code/slurm_exe/kill-dask-workers"
module load vscode-server/4.16.1
module load singularity/3.5.3
module load anaconda3/2022.05
eval "$(conda shell.bash hook)"
conda init bash
conda activate SMAC
alias python='~/.conda/envs/SMAC/bin/python3.11'
