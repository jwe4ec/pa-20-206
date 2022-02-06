# ---------------------------------------------------------------------------- #
# Compute Scores
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
# Define scale items ----
# ---------------------------------------------------------------------------- #

# Confirm variable names for negative interpretation bias (MDIB) and anxiety
# symptoms are same across datasets

all(names(mdib_dat)[grepl("md_bbsiq", names(mdib_dat)) & grepl("neg", names(mdib_dat))] == 
      names(mthd_dat)[grepl("md_bbsiq", names(mthd_dat)) & grepl("neg", names(mthd_dat))])

all(names(mdib_dat)[grepl("neuroqol", names(mdib_dat)) & grepl("anx", names(mdib_dat)) &
                      !(grepl("complete", names(mdib_dat)))] ==
      names(mthd_dat)[grepl("neuroqol", names(mthd_dat)) & grepl("anx", names(mthd_dat)) &
                        !(grepl("complete", names(mthd_dat)))])

# Define items for negative interpretation bias (MDIB, BBSIQ) and anxiety 
# symptoms (NeuroQoL)

mdib_neg_items <- 
  names(mdib_dat)[grepl("md_bbsiq", names(mdib_dat)) & grepl("neg", names(mdib_dat))]
bbsiq_neg_items <- 
  names(mdib_dat)[grepl("bbsiq", names(mdib_dat)) & !grepl("md_bbsiq", names(mdib_dat)) &
                    grepl("neg", names(mdib_dat))]
neuroqol_anx_items <-
  names(mdib_dat)[grepl("neuroqol", names(mdib_dat)) & grepl("anx", names(mdib_dat)) &
                    !(grepl("complete", names(mdib_dat)))]

all(mdib_neg_items == c("md_bbsiq_1b_neg", "md_bbsiq_2a_neg", "md_bbsiq_3c_neg", 
                        "md_bbsiq_4c_neg", "md_bbsiq_5a_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_8b_neg", "md_bbsiq_9c_neg", 
                        "md_bbsiq_10a_neg", "md_bbsiq_11c_neg", "md_bbsiq_12b_neg"))
all(bbsiq_neg_items == c("bbsiq_1c_neg", "bbsiq_2b_neg", "bbsiq_3c_neg", "bbsiq_4c_neg", 
                         "bbsiq_5a_neg", "bbsiq_6a_neg", "bbsiq_7b_neg", "bbsiq_8c_neg", 
                         "bbsiq_9b_neg", "bbsiq_10b_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                         "bbsiq_13c_neg", "bbsiq_14c_neg"))
all(neuroqol_anx_items == c("neuroqol_edanx53", "neuroqol_edanx46", "neuroqol_edanx48", 
                            "neuroqol_edanx41", "neuroqol_edanx54", "neuroqol_edanx55", 
                            "neuroqol_edanx18", "neuroqol_nqanx07"))

length(mdib_neg_items) == 12
length(bbsiq_neg_items) == 14
length(neuroqol_anx_items) == 8

# Define items for purported MDIB scales (internal threats = catastrophizing 
# about disease progression, external threats = negative social evaluation) and 
# BBSIQ scales (internal threats, external threats)

mdib_neg_int_items <- c("md_bbsiq_1b_neg", "md_bbsiq_4c_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_8b_neg", "md_bbsiq_12b_neg")
mdib_neg_ext_items <- c("md_bbsiq_2a_neg", "md_bbsiq_3c_neg", "md_bbsiq_5a_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_9c_neg", "md_bbsiq_10a_neg", 
                        "md_bbsiq_11c_neg")

bbsiq_neg_int_items <- c("bbsiq_2b_neg", "bbsiq_3c_neg", "bbsiq_5a_neg", "bbsiq_8c_neg", 
                         "bbsiq_11b_neg", "bbsiq_12a_neg", "bbsiq_14c_neg")
bbsiq_neg_ext_items <- c("bbsiq_1c_neg", "bbsiq_4c_neg", "bbsiq_6a_neg", "bbsiq_7b_neg", 
                         "bbsiq_9b_neg", "bbsiq_10b_neg", "bbsiq_13c_neg")

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Recode "prefer not to answer" (coded as 99) in "mdib_dat". "mthd_dat" does not
# appear to contain any such values.

target_items <- c(mdib_neg_items, bbsiq_neg_items, neuroqol_anx_items)

mdib_dat[, target_items][mdib_dat[, target_items] == 99] <- NA

summary(mthd_dat[, c(mdib_neg_items, neuroqol_anx_items)])

# ---------------------------------------------------------------------------- #
# Score scales ----
# ---------------------------------------------------------------------------- #

# Compute mean of available items

mdib_dat$mdib_neg_int_m <-  rowMeans(mdib_dat[, mdib_neg_int_items], na.rm = TRUE)
mdib_dat$mdib_neg_ext_m <-  rowMeans(mdib_dat[, mdib_neg_ext_items], na.rm = TRUE)
mdib_dat$bbsiq_neg_int_m <- rowMeans(mdib_dat[, bbsiq_neg_int_items], na.rm = TRUE)
mdib_dat$bbsiq_neg_ext_m <- rowMeans(mdib_dat[, bbsiq_neg_ext_items], na.rm = TRUE)
mdib_dat$neuroqol_anx_m <-  rowMeans(mdib_dat[, neuroqol_anx_items], na.rm = TRUE)

mdib_dat$mdib_neg_int_m[is.nan(mdib_dat$mdib_neg_int_m)] <- NA
mdib_dat$mdib_neg_ext_m[is.nan(mdib_dat$mdib_neg_ext_m)] <- NA
mdib_dat$bbsiq_neg_int_m[is.nan(mdib_dat$bbsiq_neg_int_m)] <- NA
mdib_dat$bbsiq_neg_ext_m[is.nan(mdib_dat$bbsiq_neg_ext_m)] <- NA
mdib_dat$neuroqol_anx_m[is.nan(mdib_dat$neuroqol_anx_m)] <- NA

mthd_dat$mdib_neg_int_m <-  rowMeans(mthd_dat[, mdib_neg_int_items], na.rm = TRUE)
mthd_dat$mdib_neg_ext_m <-  rowMeans(mthd_dat[, mdib_neg_ext_items], na.rm = TRUE)
mthd_dat$neuroqol_anx_m <-  rowMeans(mthd_dat[, neuroqol_anx_items], na.rm = TRUE)

mthd_dat$mdib_neg_int_m[is.nan(mthd_dat$mdib_neg_int_m)] <- NA
mthd_dat$mdib_neg_ext_m[is.nan(mthd_dat$mdib_neg_ext_m)] <- NA
mthd_dat$neuroqol_anx_m[is.nan(mthd_dat$neuroqol_anx_m)] <- NA

# Compute total scores based on mean of available items

mdib_dat$mdib_neg_int_tot <-  mdib_dat$mdib_neg_int_m*length(mdib_neg_int_items)
mdib_dat$mdib_neg_ext_tot <-  mdib_dat$mdib_neg_ext_m*length(mdib_neg_ext_items)
mdib_dat$bbsiq_neg_int_tot <- mdib_dat$bbsiq_neg_int_m*length(bbsiq_neg_int_items)
mdib_dat$bbsiq_neg_ext_tot <- mdib_dat$bbsiq_neg_ext_m*length(bbsiq_neg_ext_items)
mdib_dat$neuroqol_anx_tot <-  mdib_dat$neuroqol_anx_m*length(neuroqol_anx_items)

mthd_dat$mdib_neg_int_tot <- mthd_dat$mdib_neg_int_m*length(mdib_neg_int_items)
mthd_dat$mdib_neg_ext_tot <- mthd_dat$mdib_neg_ext_m*length(mdib_neg_ext_items)
mthd_dat$neuroqol_anx_tot <- mthd_dat$neuroqol_anx_m*length(neuroqol_anx_items)

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/scored")

save(mdib_dat, file = "./data/scored/mdib_dat.RData")
save(mthd_dat, file = "./data/scored/mthd_dat.RData")