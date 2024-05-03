rm(list = ls())
source(file.path(Sys.getenv(
   if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
 ), ".vscode-R", "init.R"))

options(scipen = 999,
        digits = 6)

# Utilities ----------------------------
if(!interactive()){
  library(jsonlite) 
}
library(data.table)
library(openxlsx)
library(writexl)
library(tictoc)
library(gtools)
library(lubridate)
library(tidyverse)
library(stringdist)
library(tidytext)

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


invisible(lapply(X = file.path('.','Functions',list.files(path = file.path('.','Functions'))), FUN = source))
source(file.path('simulations','Minnesota MH Network Simulation.R'))
source(file.path('simulations','post_processing.R'))
list2env(readRDS(file.path(
  ".",
  "simulations",
  "function_requirements",
  "MH_Network_sim_input_list.rds"
)), .GlobalEnv)

