# slurm-launch.py
# Usage:
# python slurm-launch.py --exp-name test \
#     --command "rllib train --run PPO --env CartPole-v0"

import argparse
import subprocess
import sys
import time
import os
import random
import string
from pathlib import Path

def generate_random_string(n_char: int = 10):
    # Define the character set: you can include letters, digits, and punctuation
    char_set = string.ascii_letters + string.digits
    # Generate a random string of the specified length
    random_string = ''.join(random.choices(char_set, k=n_char))
    return random_string


JOB_NAME = "${JOB_NAME}"
NUM_NODES = "${NUM_NODES}"
NUM_GPUS_PER_NODE = "${NUM_GPUS_PER_NODE}"
MEMORY = "${MEMORY}"
TIME = "${TIME}"
NUM_CPUS_PER_NODE = "${NUM_CPUS_PER_NODE}"
PARTITION_OPTION = "${PARTITION_OPTION}"
COMMAND_PLACEHOLDER = "${COMMAND_PLACEHOLDER}"
GIVEN_NODE = "${GIVEN_NODE}"
LOAD_ENV = "${LOAD_ENV}"


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--exp-name",
        type=str,
        required=True,
        help="The job name and path to logging file (exp_name.log).",
    )
    parser.add_argument(
        "--num-nodes", "-n", type=int, default=1, help="Number of nodes to use."
    )
    parser.add_argument(
        "--node",
        "-w",
        type=str,
        help="The specified nodes to use. Same format as the "
        "return of 'sinfo'. Default: ''.",
    )
    parser.add_argument(
        "--num-gpus",
        "-ng",
        type=int,
        default=0,
        help="Number of GPUs to use in each node. (Default: 0)",
    )
    
    parser.add_argument(
        "--num-workers",
        "-nc",
        type=int,
        default=1,
        help="Number of SLURM workers to call for the job. (Default: 1)",
    )
    
    parser.add_argument(
        '--time',
        '-t',
        type=str,
        default="02:00:00",
        help=" How long should I reserve the cpus for?"
    )
    
    parser.add_argument(
        "--partition",
        "-p",
        type=str,
    )
    parser.add_argument(
        "--load-env",
        type=str,
        help="The script to load your environment ('module load cuda/10.1')",
        default="",
    )
    
    parser.add_argument(
        "--mem",
        "--memory",
        '-m',
        type=int,
        default=1,
        action='store'
    )
    
    parser.add_argument(
        "--command",
        type=str,
        required=True,
        help="The command you wish to execute. For example: "
        " --command 'python test.py'. "
        "Note that the command must be a string.",
    )
    
    parser.add_argument(
        "--backend",
        "-b",
        choices=['single','ray','dask'],
        default='single',
        action='store'
    )
    args = parser.parse_args()

    if args.node:
        # assert args.num_nodes == 1
        node_info = "#SBATCH -w {}".format(args.node)
    else:
        node_info = ""
    
    job_name = args.exp_name

    partition_option = (
        "#SBATCH --partition={}".format(args.partition) if args.partition else ""
    )
    args.command += f" --backend={args.backend}" if '--backend' not in args.command else ""
    if not args.backend == 'ray':
        template_file = 'Code/slurm_exe/slurm-template-single-node.sh'
        args.command += f" --num-workers={args.num_workers}"
        args.command += f" --time={args.time}"
        args.command += f" --partition={args.partition}"
        cpu_count = 30
    else:
        template_file = "Code/slurm_exe/slurm-template.sh"
        
    # ===== Modified the template script =====
    with open(template_file, "r") as f:
        text = f.read()
    text = text.replace(JOB_NAME, job_name)
    text = text.replace(NUM_NODES, str(args.num_nodes))
    text = text.replace(NUM_CPUS_PER_NODE, str(cpu_count))
    text = text.replace(NUM_GPUS_PER_NODE, str(args.num_gpus))
    text = text.replace(PARTITION_OPTION, partition_option)
    text = text.replace(COMMAND_PLACEHOLDER, str(args.command))
    text = text.replace(LOAD_ENV, str(args.load_env))
    text = text.replace(TIME, str(args.time))
    text = text.replace(GIVEN_NODE, node_info)
    text = text.replace(MEMORY,str(args.mem))
    text = text.replace(
        "# THIS FILE IS A TEMPLATE AND IT SHOULD NOT BE DEPLOYED TO " "PRODUCTION!",
        "# THIS FILE IS MODIFIED AUTOMATICALLY FROM TEMPLATE AND SHOULD BE "
        "RUNNABLE!",
    )

    # ===== Save the script =====
    script_file = f"{generate_random_string()}.sh"
    with open(script_file, "w") as f:
        f.write(text)

    # ===== Submit the job =====
    print("Starting to submit job!")
    subprocess.Popen(["sbatch", script_file])
    print(
        "Job submitted! Script file is at: <{}>. Log file is at: <{}>".format(
            script_file, "{}.log".format(job_name)
        )
    )
    time.sleep(3)
    subprocess.Popen(['rm','-f',script_file])
    sys.exit(0)
    
    # Sample terminal command to execute tuning rob

    # python slurm-launch.py --num-cpus=30 --partition=short --time=3:00:00 --exp-name=test-debug --command="python3.11 Code/experiments/__main__.py --tune-job=debug --test-sim-params"