# Policy-Interventions-to-Improve-Mental-Healthcare-Access

This repo holds code for the paper [*Policy Intervention to Improve Mental Health Access: A Discrete Event Simulation Study*](https://arxiv.org/abs/2304.13849). This project comprises of a discrete event simulation model detailing when and where patients presenting to the Emergency Department psychiatric receive inpatient care. Additionally, we include methods for determining simulation input parameters given limited data availability for individual hospitals/healthcare providers, sensitivity analysis around these input parameters, and systemic modifications to reduce time spent in the healthcare system where patients do not receive psychiatric care.

# Directories and Files
`Simulations/`Directory with all simulation models
- `/Minnesota MH Network Simulation.R`Simmer based Discrete-Event Simulation 
- `/Minnesota MH Network Simulation.R`Simmer based DES used for input parameter sensitivity analysis
- `/Function Requirements/` Directory holding DES input parameters

`Data Analysis/` Directory holding code for data analysis
- `/Build Siteinfo.R` Builds the siteInfo data.frame used as input for the DES model
- `/Drive Time Matrix Builder (Google API).R` Calculates driving distance between all EDs and inpatient units consideered in the model
- `/HCCIS Analysis.R` Calculates input parameters for all other hospitals included in the Minn. HCCIS in which patient-level data was inaccesible
- `/Read and Format Mayo Data.R`Formats available patient-level data
- `/Simulation Input Parameters.R` Calculates input paramter distributions and values from patient level data
- `/Validation Metric Calculation.R` Validation calculation for the DES model

`Functions/` Directory holding custom function used throughout the project

`Code/` Directory holding related code
- `Intervention Results.R` Implements and analyzes 3 interventions proposed in the original article
    - Intervention 1: Request transfer to facilities with the highest likelihood to accept patients rather than the nearest inpatient unit
    - Intervention 2: Request transfer to multiple facilities simultaneously. We evaluate the effect on average patient treatment delays while sending up to 8 transfer referrals concurrently. 
    - Intervention 3: Combine interventions 1 & 2. 
- `Multiple Facility Check and Widened Check Radius.R` Deprecated version of the above `Intervention Results.R`
- `Sensitivity Analysis.R` Performs sensitivity analysis on simulation inputs including
    - Emergency Department -> Inpatient arrival rates 
    - Inpatient Length of Stay
    - Probability a hospital rejects a patients
- `Warmup_and_Replications.R` Identifies how long the before the model reaches steady state

