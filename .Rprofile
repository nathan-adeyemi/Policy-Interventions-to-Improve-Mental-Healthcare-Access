rm(list = ls())
setwd('/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access')
source(file.path(Sys.getenv(
   if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
 ), ".vscode-R", "init.R"))

options(scipen = 999,
        digits = 6)

# Utilities ----------------------------
if(!interactive()){
  library(jsonlite) 
  library(tidyverse)
}
library(data.table)
library(openxlsx)
library(writexl)
library(tictoc)
library(gtools)
library(lubridate)
library(stringdist)
library(tidytext)
library(MLmetrics)

# Packages for Statistics/Bootstrapping/etc. ------------------------------
library(fitdistrplus)
library(boot)
library(simpleboot)
library(EnvStats)

# Packages for Discrete Event Simulation ----------------------------------
library(simmer)
library(simtimer)

# Packages for Parallel Processing ----------------------------------------
library(doParallel)
library(parallelly)


invisible(lapply(X = file.path('src','r_functions',list.files(path = file.path('src','r_functions'))), FUN = source))
source(file.path('src/simulations','Minnesota MH Network Simulation.R'))
source(file.path('src/simulations','post_processing.R'))
list2env(readRDS(file.path(
  ".",
  "src/simulations",
  "function_requirements",
  "MH_Network_sim_input_list.rds"
)), .GlobalEnv)

