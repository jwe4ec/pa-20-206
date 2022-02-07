# ---------------------------------------------------------------------------- #
# Run MDIB Analyses
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages, set seed ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

groundhog.library(psych, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/scored/mdib_dat.RData")
load("./data/helper/items.RData")

# ---------------------------------------------------------------------------- #
# Restrict to baseline ----
# ---------------------------------------------------------------------------- #

mdib_dat_bl <- mdib_dat[mdib_dat$redcap_event_name == "baseline_arm_1", ]

# ---------------------------------------------------------------------------- #
# Compute internal consistency ----
# ---------------------------------------------------------------------------- #

# Compute unstandardized Cronbach's alpha given that items are not standardized 
# prior to computing scale scores (see Falk & Savalei, 2011). Compute both point 
# estimates and CIs (see Iacobucci & Duhachek, 2003).

# Compute bootstrapped CIs for negative MDIB scales at baseline using pairwise 
# deletion (default)

# psych::alpha(mdib_dat_bl[, items$mdib_neg_int], n.iter = 1000)$boot.ci
# psych::alpha(mdib_dat_bl[, items$mdib_neg_ext], n.iter = 1000)$boot.ci

# Given that pairwise deletion yields warning that matrix is not positive definite 
# for "mdib_neg_ext", try listwise deletion. Yields same warning.

# psych::alpha(mdib_dat_bl[, items$mdib_neg_ext], n.iter = 1000, 
#              use = "complete.obs")$boot.ci

# Applying listwise deletion prior to running function yields no warning, so use
# this approach for both scales

complete_dat <- na.omit(mdib_dat_bl[, items$mdib_neg_int])
nrow(complete_dat)
mdib_neg_int_bl_alpha <- psych::alpha(complete_dat, n.iter = 1000)$boot.ci

complete_dat <- na.omit(mdib_dat_bl[, items$mdib_neg_ext])
nrow(complete_dat)
mdib_neg_ext_bl_alpha <- psych::alpha(complete_dat, n.iter = 1000)$boot.ci

# ---------------------------------------------------------------------------- #
# Compute correlation between negative bias and anxiety symptoms ----
# ---------------------------------------------------------------------------- #

# Compute Pearson product-moment correlation between negative bias (MDIB) and 
# anxiety symptoms (NeuroQoL) at baseline using listwise deletion (default). Use 
# total scores (vs. mean item scores) for consistency with other grant materials.

mdib_neg_int_tot_bl_cor <- cor.test(mdib_dat_bl$mdib_neg_int_tot,
                                    mdib_dat_bl$neuroqol_anx_tot,
                                    method = "pearson")

mdib_neg_ext_tot_bl_cor <- cor.test(mdib_dat_bl$mdib_neg_ext_tot,
                                    mdib_dat_bl$neuroqol_anx_tot,
                                    method = "pearson")

# ---------------------------------------------------------------------------- #
# Save and write results ----
# ---------------------------------------------------------------------------- #

mdib_res <- list(mdib_neg_int_bl_alpha = mdib_neg_int_bl_alpha,
                 mdib_neg_ext_bl_alpha = mdib_neg_ext_bl_alpha,
                 mdib_neg_int_tot_bl_cor = mdib_neg_int_tot_bl_cor,
                 mdib_neg_ext_tot_bl_cor = mdib_neg_ext_tot_bl_cor)

dir.create("./results")

save(mdib_res, file = "./results/mdib_res.RData")

sink("./results/mdib_res.txt")
print(paste0("Unstandardized Cronbach's alpha and 95% bootstrapped CI for ",
             "'mdib_neg_int' items using complete baseline data (n = 42):"))
cat("\n")
round(mdib_neg_int_bl_alpha, 2)
cat("\n")
print(paste0("Unstandardized Cronbach's alpha and 95% bootstrapped CI for ",
             "'mdib_neg_ext' items using complete baseline data (n = 41):"))
cat("\n")
round(mdib_neg_ext_bl_alpha, 2)
cat("\n")
print(paste0("Pearson's correlation between 'mdib_neg_int_tot' and 'neuroqol_anx_tot' ",
             "total scores using complete baseline data (n = 44):"))
mdib_neg_int_tot_bl_cor
cat("\n")
print(paste0("Pearson's correlation between 'mdib_neg_ext_tot' and 'neuroqol_anx_tot' ",
             "total scores using complete baseline data (n = 44):"))
mdib_neg_ext_tot_bl_cor
sink()