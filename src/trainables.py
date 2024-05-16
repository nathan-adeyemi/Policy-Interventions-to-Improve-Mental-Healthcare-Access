import os
import socket
import subprocess
import math
import pandas as pd
import re
import json

from ray import train
from utils import read_json_con


def trainable(params: dict, job_name: str, output_metric: str, sim_run_info: dict, test = False):
    
    home_dir = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/"
    params = json.dumps(params)

    port = find_available_port()

    # Create a socket
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind the socket to a specific address and port
    server_address = ("localhost", port)
    server.bind(server_address)

    # Listen for incoming connections
    server.listen(1)

    # Push all the sim meta-information to the R program via the bash
    subprocess_env = os.environ.copy()
    subprocess_env["port"] = str(port)
    subprocess_env["num_replications"] = str(sim_run_info["num_replications"])
    subprocess_env["warm_period"] = str(sim_run_info["warmup"])
    subprocess_env["sim_period"] = str(sim_run_info["sim_length"])
    subprocess_env["nproc"] = str(sim_run_info["nproc"] / sim_run_info['concurrent_trials'])
    subprocess_env["trainable_path"] = str(train.get_context().get_trial_dir())
    subprocess_env['tune_job'] = str(job_name)
    subprocess_env["output_metric"] = str(output_metric)

    # Begin the R simulation subprocess
    try:
        subprocess.Popen(
            ["bash", home_dir + "src/sim_trigger.sh"],
            env=subprocess_env,
        )
    except Exception as e:
        print(f"Error starting the shell script: {e}")

    # Accept the client connection
    client, _ = server.accept()
    client.sendall(params.encode())

    # Receieve the simulation trial's loss information
    for i in range(
        0,
        math.ceil(
            sim_run_info["num_replications"]
            / (sim_run_info["nproc"] / sim_run_info["concurrent_trials"])
        ),
    ):
        res_dict = json.loads(read_json_con(socket=client))
        res_dict.update({"training_iteration": i + 1})
        
        if test:
            print(f'Training Iteration {i} is complete')
            print(res_dict)
        else: 
            train.report(res_dict)
        
    return res_dict
    # Function to find an available port
    # process.kill()

def find_available_port(print_port=False):
    # Create a socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind to a random port
    s.bind(("localhost", 0))

    # Get the actual port number
    _, port = s.getsockname()

    # Close the socket
    s.close()

    if print_port:
        print(port)

    return port