import sys
sys.path.append('src')

import os
import pandas as pd
import numpy as np
import trainables as trains
from utils import read_yaml 

os.chdir(
    "/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access"
)

hccis = pd.read_csv("Data/HCCIS/hccis_ed_ips_2020.csv")
siteInfo = pd.read_csv(
    "src/simulations/function_requirements/ip_facilities_info.csv"
)
test_params = {}
for  site in siteInfo.Facility_name:
    test_params.update({site: np.random.uniform(5,1)})

test_trainable = trains.trainable(
    params=test_params,
    job_name="acceptance-probs",
    sim_run_info=read_yaml(filename="Code/experiments/configs/sim_params.yaml")['test'],
    test=True
)
