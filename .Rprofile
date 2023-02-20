source("renv/activate.R")

options(scipen = 999,
        digits = 6,
        echo = F)

# Utilities ----------------------------
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(writexl)
library(tictoc)
library(gtools)
library(ps)
library(lubridate)

# Packages for Statistics/Bootstrapping/etc. ------------------------------
library(fitdistrplus)
library(boot)
library(simpleboot)
library(EnvStats)

# Packages for Discrete Event Simulation ----------------------------------
library(simmer)
library(simmer.plot)
library(simtimer)

# Packages for Parallel Processing ----------------------------------------
library(doParallel)
library(pbmcapply)
library(parallelly)

# library(PMCMRplus)
setwd('..')
source("functions.R")
source(file.path('Simulations','Minnesota MH Network Simulation.R'))
setwd('Policy Interventions to Improve Mental Healthcare Access')
source('Api_Keys.R')
