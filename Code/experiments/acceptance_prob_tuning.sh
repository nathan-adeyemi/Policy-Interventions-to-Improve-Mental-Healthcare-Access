#!/bin/bash
#SBATCH --partition=short
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --mem=64GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=30
#SBATCH --output=sbatch_out_files/validation.out 
#SBATCH --error=sbatch_out_files/validation.err

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
    
    conda activate bed_allocation_proj

    Rscript "Code/experiments/Validate_Simulation.R" "$port" "$acceptance_probs"
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
    unset __conda_setup

    cd /home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access
    module load singularity/3.5.3
    singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript "Code/experiments/Validate_Simulation.R"  "$port" "$acceptance_probs"
fi
