rm(list = ls())
setwd('/home/adeyemi.n/MH_Simulation/Policy_Interventions_to_Improve_Mental_Healthcare_Access')
source(file.path(Sys.getenv(
   if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
 ), ".vscode-R", "init.R"))

options(scipen = 999,
        digits = 6)

# Utilities ----------------------------
if(!interactive()){
  suppressMessages(library(jsonlite))
  suppressMessages(library(tidyverse))
}
suppressMessages(library(data.table))
suppressMessages(library(openxlsx))
suppressMessages(library(writexl))
suppressMessages(library(tictoc))
suppressMessages(library(gtools))
suppressMessages(library(lubridate))
suppressMessages(library(stringdist))
suppressMessages(library(tidytext))
suppressMessages(library(MLmetrics))

# Packages for Statistics/Bootstrapping/etc. ------------------------------
suppressMessages(library(fitdistrplus))
suppressMessages(library(boot))
suppressMessages(library(simpleboot))
suppressMessages(library(EnvStats))

# Packages for Discrete Event Simulation ----------------------------------
suppressMessages(library(simmer))
suppressMessages(library(simtimer))

# Packages for Parallel Processing ----------------------------------------
suppressMessages(library(doParallel))
suppressMessages(library(parallelly))


invisible(lapply(X = file.path('src','r_functions',list.files(path = file.path('src','r_functions'))), FUN = source))
invisible(source(file.path('src/simulations','Minnesota MH Network Simulation.R')))
invisible(source(file.path('src/simulations','post_processing.R')))
invisible(list2env(readRDS(file.path(
  ".",
  "src/simulations",
  "function_requirements",
  "MH_Network_sim_input_list.rds"
)), .GlobalEnv))

