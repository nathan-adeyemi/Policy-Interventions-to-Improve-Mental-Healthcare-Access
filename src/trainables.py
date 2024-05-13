import os
import socket
import subprocess
import math
import sklearn.metrics as metrics
import pandas as pd
import json

from io import StringIO
from ray import train


def validation_trainable(params: dict, job_name: str, sim_run_info: dict):
    home_dir = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/"
    if job_name == 'ed-arr':
        params = str(params['ed_scale_param'])
        
    elif job_name == 'ed-arr-sep':
        hccis = pd.read_csv(
            home_dir + "Data/HCCIS/hccis_ed_ips_2020.csv"
        )
        df = hccis.loc[:, ["hccis_id", "ed_scale_param"]]
        df["ed_scale_param"] = (
            df["ed_scale_param"] * params["ed_scale_param"]
        )
        params = df.reset_index().to_json(orient="records")

        params = params.replace('"', '"')
        
    elif job_name == 'acceptance-prob':
        params = (
            pd.DataFrame.from_dict(params, orient="index")
            .reset_index()
            .to_json(orient="records")
        )

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
        info = client.recv(4096).decode("utf-8")
        info = pd.read_json(StringIO(info))

        mse = metrics.mean_squared_error(
            y_true=info.loc[:, "hccis_admissions"],
            y_pred=info.loc[:, "admissions"],
        )
        res_dict = {"MSE": mse, "RMSE": math.sqrt(mse), "training_iteration": i + 1}
        
        train.report(res_dict)
        
    # with tempfile.TemporaryDirectory() as temp_checkpoint_dir:
    #     train.report(res_dict, checkpoint=temp_checkpoint_dir)
        
    return res_dict
    # Function to find an available port
    # process.kill()
    
def sensitivity_trainable(params: dict, job_name: str, sim_run_info: dict):
    home_dir = "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access/"
    
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
    subprocess_env["nproc"] = str(
        sim_run_info["nproc"] / sim_run_info["concurrent_trials"]
    )
    subprocess_env["trainable_path"] = str(train.get_context().get_trial_dir())
    subprocess_env["tune_job"] = str(job_name)

    # Begin the R simulation subprocess
    try:
        subprocess.Popen(
            [
                "bash",
                home_dir
                + "src/sim_trigger.sh"
            ],
            env=subprocess_env,
        )
    except Exception as e:
        print(f"Error starting the shell script: {e}")

    # Accept the client connection
    client, _ = server.accept()
    client.sendall(json.dumps(params).encode())

    # Receieve the simulation trial's loss information
    for _ in range(
        0,
        math.ceil(
            sim_run_info["num_replications"]
            / (sim_run_info["nproc"] / sim_run_info["concurrent_trials"])
        ),
    ):
        # Make sure the sim_setup.R sends back to back results through the socket
        res_df = client.recv(4096).decode("utf-8")
        res_dict = pd.read_json(res_df)
        res_dict = res_dict.groupby(['Vulnerable Patient','type']).mean().reset_index()
        print(res_dict)
        train.report(
            res_dict[
                (res_dict["Vulnerable Patient"]) & (res_dict["type"] == "Transfer")
            ].loc[:, ["Coordination Time", "Treatment Delay"]].to_dict(orient='records')[0]
        )


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