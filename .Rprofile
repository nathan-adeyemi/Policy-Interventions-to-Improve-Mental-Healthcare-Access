source(file.path(Sys.getenv(
   if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
 ), ".vscode-R", "init.R"))

options(scipen = 999,
        digits = 6)

# Utilities ----------------------------
library(data.table)
library(openxlsx)
library(writexl)
library(tictoc)
library(gtools)
library(lubridate)
library(tidyverse)
library(jsonlite)
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
source(file.path('Simulations','Minnesota MH Network Simulation.R'))
siteInfo <-
  data.table(readRDS(file.path(
    ".",
    "Simulations",
    "Function Requirements",
    "Rates5.rds"
  )))
