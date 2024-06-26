# Define the name of the conda environment
module load singularity/3.5.3
module load anaconda3/2022.05
conda activate ed_ip_simulation
alias python='~/.conda/envs/ed_ip_simulation/bin/python'
alias R='singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif R'