# ---------------------------------------------------------------------------- #
# Prepare Data
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

mdib_dat <- read.csv("./data/bot_cleaned/Prelim Data for Jeremy 2-3-22_MDIB.csv")
mthd_dat <- read.csv("./data/raw/Prelim Data for Jeremy 2-3-22_MindTrails-HD pilot.csv")

# ---------------------------------------------------------------------------- #
# Score selected scales ----
# ---------------------------------------------------------------------------- #

# TODO: Define items for negative interpretation bias (MDIB, BBSIQ) and anxiety 
# symptoms (NeuroQoL) based on mean of available items





# TODO: Compute total scores based on mean of available items




