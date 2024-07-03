import os
import socket
import subprocess
import math
import json
import time

from ray import train
from pathlib import Path
from utils import read_yaml, read_json_con, generate_random_string


def trainable(
    job_name: str,
    output_metric: str,
    optim_stat: str,
    fn_cfg_sel: str,
    fn_cfg_path: str,
    params: dict = None,
    ray: bool = False,
    scratch_path: str = None,
    seed: int = 0,
    server_wait_tol: int = 1,
    trainable_name: str = None
):
    
    home_dir = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
    scratch_path = os.getcwd() if not scratch_path else scratch_path
    total_reps = read_yaml(fn_cfg_path).get(fn_cfg_sel).get('num-replications')

    if ray:
        trainable_path = str(train.get_context().get_trial_dir())
    else:
        if not params and not trainable_name:
            trainable_name = "Baseline"
        elif not trainable_name:
            trainable_name = generate_random_string()

        trainable_path = os.path.join(scratch_path, f"trainable-{trainable_name}")
        if not os.path.exists(trainable_path):
            os.makedirs(trainable_path)

    params = json.dumps(params, indent=4, sort_keys=True)

    sim_cpus = str(len(os.sched_getaffinity(0)))
    server, port = _find_and_connect(server_wait_tol=server_wait_tol)

    print(f"Configuration Results Location: {trainable_path}")
    if (trainable_name == 'Incumbent'):
        print(f"Tuning Input Params: \n {params}")
    # Push all the sim meta-information to the R program via the bash
    subprocess_env = os.environ.copy()
    subprocess_env.update(
        {
            "port": str(port),
            "seed": str(seed),
            "sim_config": fn_cfg_sel,
            "sim_config_path": fn_cfg_path,
            "nproc": sim_cpus,
            "trainable_name": trainable_name,
            "trainable_path": trainable_path,
            "tune_job": str(job_name),
            "output_metric": str(output_metric),
        }
    )

    # Begin the R simulation subprocess
    try:
        subprocess.Popen(
            ["bash", Path(os.path.join(home_dir,"src/sim_trigger.sh")).resolve()],
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
        math.ceil(total_reps / int(sim_cpus)),
    ):
        n_rep = min(total_reps, int((i + 1) * sim_cpus))
        res_dict = json.loads(read_json_con(socket=client))
        if ray:
            train.report(res_dict)

        else:
            print(
                f"Results of Trainable-{trainable_name} at round {i+1}: \n {res_dict}"
            ) 

        ret = res_dict if optim_stat is None else res_dict.get(optim_stat)

    return ret
    
def _find_available_port(print_port=False):
    # Function to find an available port
    
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


def _find_and_connect(server_wait_tol: int = 1) -> socket.socket:
    cont = True
    while cont:
        try:
            port = _find_available_port()

            # Create a socket
            server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

            # Bind the socket to a specific address and port
            server_address = ("localhost", port)
            server.bind(server_address)
            cont = False
        except OSError as e:
            if e.errno == 98:
                print(
                    f"Port {port} is already in use. Retrying in {server_wait_tol} seconds..."
                )
                time.sleep(server_wait_tol)
            else:
                cont = False  # Exit the loop and raise the exception
                raise

    # Listen for incoming connections
    server.listen(1)

    return server, port
