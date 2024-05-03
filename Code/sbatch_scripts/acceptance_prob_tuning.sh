#!/bin/bash
#SBATCH --partition=short
#SBATCH --time=22:00:00
#SBATCH --nodes=1
#SBATCH --mem=256G
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=120
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/acceptance_tuning.out 
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/sbatch_out_files/acceptance_tuning.err

if [ "$(uname)" == "Darwin" ]; then

    __conda_setup="$('/Users/nadeyemi/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/Users/nadeyemi/anaconda3/etc/profile.d/conda.sh" ]; then
            . "/Users/nadeyemi/anaconda3/etc/profile.d/conda.sh"
        else
            export PATH="/Users/nadeyemi/anaconda3/bin:$PATH"
        fi
    fi
    unset __conda_setup

    if [ -f "/Users/nadeyemi/anaconda3/etc/profile.d/mamba.sh" ]; then
        . "/Users/nadeyemi/anaconda3/etc/profile.d/mamba.sh"
    fi

    cd /Users/nadeyemi/Library/CloudStorage/OneDrive-NortheasternUniversity/Graduate/Research/Minn_MH_Sim_Projects/Policy_Interventions_to_Improve_Mental_Healthcare_Access
    
    conda activate ed_ip_simulation

    python "Code/experiments/acceptance_prob_tuning.py"
else
    __conda_setup="$('/shared/centos7/anaconda3/2022.01/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/shared/centos7/anaconda3/2022.01/etc/profile.d/conda.sh" ]; then
            . "/shared/centos7/anaconda3/2022.01/etc/profile.d/conda.sh"
        else
            export PATH="/shared/centos7/anaconda3/2022.01/bin:$PATH"
        fi
    fi
    cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
    unset __conda_setup

    module load singularity/3.5.3
    conda activate ed_ip_simulation

     /home/adeyemi.n/.conda/envs/ed_ip_simulation/bin/python3.11 "Code/experiments/acceptance_prob_tuning.py"
fi
