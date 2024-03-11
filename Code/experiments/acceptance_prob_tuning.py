import ray 
import pickle as pkl
import pandas as pd
import os
import subprocess
import socket 
import numpy as np

from pathlib import Path
from omegaconf import OmegaConf  # noqa: E402
from ray import train, tune

def sim_trainable(acceptance_probs: dict):
    sh_path = "Code/sbatch_scripts/acceptance_prob_tune.sh"
        
    port = find_available_port()
    print(port)
    
    # Create a socket
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind the socket to a specific address and port
    server_address = ('localhost', port)
    server.bind(server_address)
    
    # Listen for incoming connections
    server.listen(1)
    
    # Begin the R simulation subprocess 
    subprocess_env = os.environ.copy()
    subprocess_env['port'] = str(port)

    try:
        process = subprocess.Popen(['bash', sh_path], env=subprocess_env)
    except Exception as e:
        print(f'Error starting the shell script: {e}')        
    
    # Accept the client connection
    client, _ = server.accept()
    
    acceptance_probs = pd.DataFrame(acceptance_probs, orient="index").to_json().encode()
    client.sendall(acceptance_probs)

    # Receieve the simulation trial's loss information
    info = client.recv(1024).decode('utf-8')
    info = np.array(float(info)).mean()

    train.report({'mean_accuracy': info})
    # Function to find an available port
    process.terminate()


def execute_tuning(trainer, hospital_df: pd.DataFrame = None, workers_per_trial: int = None, path: str = None):

    bundle_list = [
        {"CPU": workers_per_trial},
    ]  # Reserve a single CPU for the Tune job execution

    probs = [{unit_name: tune.uniform(0, 1)} for unit_name in hospital_df.Facility_name.unique()]
    probs_cfg = {}
    _ = [probs_cfg.update(i) for i in probs]
    
    if path is None:

        trainable_w_resources = tune.with_resources(
            trainable=trainer,
            resources=tune.PlacementGroupFactory(bundles=bundle_list, strategy="PACK")
        )
                
        tuner = tune.Tuner(
            trainable_w_resources,
            tune_config=tune.TuneConfig(
                metric="mean_accuracy", mode="min", num_samples=1
            ),
            param_space=probs_cfg,
            run_config=train.RunConfig(storage_path=Path('Results/validation/acceptance_prob_tuning').resolve(),
                                    name = 'p_acceptance_tuning',
                                    checkpoint_config=train.CheckpointConfig(
                                        num_to_keep = 1,
                                        checkpoint_at_end = True))
        )

        results = tuner.fit()
        
        with open(Path('Results/validation/acceptance_prob_tuning').resolve(),'wb') as f:
            pkl.dump(results,f)
            
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

if __name__ == "__main__":
    siteInfo = pd.read_excel(io = 'simulations/function_requirements/ip_facilities_info.xlsx')

    # ray.init()
    # results = execute_tuning(trainer = sim_trainable, 
    #                          hospital_df=siteInfo,
    #                          workers_per_trial = 10)
    
    test = sim_trainable(acceptance_probs = siteInfo)