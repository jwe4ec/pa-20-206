# ---------------------------------------------------------------------------- #
# Clean Data
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

# Import MDIB data from RedCap

mdib_dat <- read.csv("./data/bot_cleaned/Prelim Data for Jeremy 2-3-22_MDIB.csv")

# Import MT-HD data from RedCap

mthd_dat_rc <- read.csv("./data/raw/Prelim Data for Jeremy 2-3-22_MindTrails-HD pilot.csv")

# Import MT-HD data from Data Server

ds_data_dir <- paste0(wd_dir, "/data/raw/from_angel")
filenames <- list.files(ds_data_dir, pattern = "*.csv", full.names = FALSE)

mthd_dat_ds <- lapply(paste0(ds_data_dir, "/", filenames), read.csv)
names(mthd_dat_ds) <- sub("-data-2022-02-11.csv", "", filenames)

names(mthd_dat_ds) <- sub("HD ", "", names(mthd_dat_ds))
names(mthd_dat_ds) <- tolower(names(mthd_dat_ds))
names(mthd_dat_ds)[names(mthd_dat_ds) == "dass"] <- "dass21_as"

# Import MT-HD data to link RedCap and Data Server data

mthd_dat_lk <- read.csv("./data/2022.02.14-redcap_record_id-mt_participant_id.csv")

# ---------------------------------------------------------------------------- #
# Remove blank rows ----
# ---------------------------------------------------------------------------- #

# For rows where "record_id" is NA, all columns are NA or "". Remove such rows
# in "mdib_dat".

sum(is.na(mdib_dat$record_id)) == 205

sum(!(is.na(mdib_dat[is.na(mdib_dat$record_id), ]) |
        mdib_dat[is.na(mdib_dat$record_id), ] == "")) == 0

mdib_dat <- mdib_dat[!is.na(mdib_dat$record_id), ]

# No "record_id"s are NA in "mthd_dat_rc"

sum(is.na(mthd_dat_rc$record_id)) == 0

# No "participant_id"s are NA in "mthd_dat_ds"

all(unlist(lapply(mthd_dat_ds, function(x) sum(is.na(x$participant_id)) == 0)))

# ---------------------------------------------------------------------------- #
# Link MT-HD datasets ----
# ---------------------------------------------------------------------------- #

# Add "participant_id" to "mthd_dat_rc" and "record_id" to "mthd_dat_ds"

mthd_dat_rc <- merge(mthd_dat_rc, 
                     mthd_dat_lk[, c("record_id", "participant_id")],
                     by = "record_id",
                     all.x = TRUE)

mthd_dat_ds <- lapply(mthd_dat_ds, function(x) {
  merge(x,
        mthd_dat_lk[, c("record_id", "participant_id")],
        by = "participant_id",
        all.x = TRUE)
})

# ---------------------------------------------------------------------------- #
# Recode session-related columns in "mthd_dat_ds" ----
# ---------------------------------------------------------------------------- #

# "session" in "dass21_as" and "oa" tables conflates eligibility status with
# time point. Create "session_only" column that reflects only time point and
# rename "session" to "session_only" in other tables.

for (i in 1:length(mthd_dat_ds)) {
  if (names(mthd_dat_ds[i]) %in% c("dass21_as", "oa")) {
    names(mthd_dat_ds[[i]])[names(mthd_dat_ds[[i]]) == "session"] <- 
      "session_and_eligibility_status"
    
    mthd_dat_ds[[i]][, "session_only"] <-
      mthd_dat_ds[[i]][, "session_and_eligibility_status"]
    mthd_dat_ds[[i]][mthd_dat_ds[[i]][, "session_only"] %in% c("ELIGIBLE", ""),
                     "session_only"] <- "Eligibility"
  } else if ("session" %in% names(mthd_dat_ds[[i]])) {
    names(mthd_dat_ds[[i]])[names(mthd_dat_ds[[i]]) == "session"] <- "session_only"
  }
}

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# Confirm variable names for negative interpretation bias (MDIB) and anxiety
# symptoms (NeuroQoL) are same across MDIB and MT-HD RedCap datasets

all(names(mdib_dat)[grepl("md_bbsiq", names(mdib_dat)) & grepl("neg", names(mdib_dat))] == 
      names(mthd_dat_rc)[grepl("md_bbsiq", names(mthd_dat_rc)) & grepl("neg", names(mthd_dat_rc))])

all(names(mdib_dat)[grepl("neuroqol", names(mdib_dat)) & grepl("anx", names(mdib_dat)) &
                      !(grepl("complete", names(mdib_dat)))] ==
      names(mthd_dat_rc)[grepl("neuroqol", names(mthd_dat_rc)) & grepl("anx", names(mthd_dat_rc)) &
                           !(grepl("complete", names(mthd_dat_rc)))])

# Note: Variable names for negative interpretation bias (BBSIQ) differ between
# MDIB and MT-HD Data Server datasets

names(mdib_dat[grepl("bbsiq_", names(mdib_dat)) & !grepl("md_bbsiq", names(mdib_dat))])
names(mthd_dat_ds$bbsiq)

# Define items for negative interpretation bias (MDIB, BBSIQ) and anxiety 
# symptoms (NeuroQoL) in MDIB and MT-HD RedCap datasets

mdib_neg_items <- 
  names(mdib_dat)[grepl("md_bbsiq", names(mdib_dat)) & grepl("neg", names(mdib_dat))]
bbsiq_neg_items_mdib <- 
  names(mdib_dat)[grepl("bbsiq", names(mdib_dat)) & !grepl("md_bbsiq", names(mdib_dat)) &
                    grepl("neg", names(mdib_dat))]
neuroqol_anx_items <-
  names(mdib_dat)[grepl("neuroqol", names(mdib_dat)) & grepl("anx", names(mdib_dat)) &
                    !(grepl("complete", names(mdib_dat)))]

all(mdib_neg_items == c("md_bbsiq_1b_neg", "md_bbsiq_2a_neg", "md_bbsiq_3c_neg", 
                        "md_bbsiq_4c_neg", "md_bbsiq_5a_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_8b_neg", "md_bbsiq_9c_neg", 
                        "md_bbsiq_10a_neg", "md_bbsiq_11c_neg", "md_bbsiq_12b_neg"))
all(bbsiq_neg_items_mdib == c("bbsiq_1c_neg", "bbsiq_2b_neg", "bbsiq_3c_neg", 
                              "bbsiq_4c_neg", "bbsiq_5a_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_8c_neg", "bbsiq_9b_neg", 
                              "bbsiq_10b_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_13c_neg", "bbsiq_14c_neg"))
all(neuroqol_anx_items == c("neuroqol_edanx53", "neuroqol_edanx46", "neuroqol_edanx48", 
                            "neuroqol_edanx41", "neuroqol_edanx54", "neuroqol_edanx55", 
                            "neuroqol_edanx18", "neuroqol_nqanx07"))

length(mdib_neg_items) == 12
length(bbsiq_neg_items_mdib) == 14
length(neuroqol_anx_items) == 8

# Define items for purported MDIB scales (internal threats = catastrophizing 
# about disease progression, external threats = negative social evaluation) and 
# BBSIQ scales (internal threats, external threats) in MDIB and MT-HD RedCap datasets

mdib_neg_int_items <- c("md_bbsiq_1b_neg", "md_bbsiq_4c_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_8b_neg", "md_bbsiq_12b_neg")
mdib_neg_ext_items <- c("md_bbsiq_2a_neg", "md_bbsiq_3c_neg", "md_bbsiq_5a_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_9c_neg", "md_bbsiq_10a_neg", 
                        "md_bbsiq_11c_neg")

length(mdib_neg_int_items) == 5
length(mdib_neg_ext_items) == 7

bbsiq_neg_int_items_mdib <- c("bbsiq_2b_neg", "bbsiq_3c_neg", "bbsiq_5a_neg", 
                              "bbsiq_8c_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_14c_neg")
bbsiq_neg_ext_items_mdib <- c("bbsiq_1c_neg", "bbsiq_4c_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_9b_neg", "bbsiq_10b_neg", 
                              "bbsiq_13c_neg")

length(bbsiq_neg_int_items_mdib) == 7
length(bbsiq_neg_ext_items_mdib) == 7

# Define items for negative interpretation bias (RR, BBSIQ) and anxiety 
# symptoms (OASIS, DASS-21-AS) in MT-HD Data Server dataset

rr_neg_threat_items <- 
  c("blood_test_ns", "elevator_ns", "job_ns", "lunch_ns", "meeting_friend_ns", 
    "noise_ns", "scrape_ns", "shopping_ns", "wedding_ns")
rr_neg_nonthreat_items <- 
  c("blood_test_nf", "elevator_nf", "job_nf", "lunch_nf", "meeting_friend_nf", 
    "noise_nf", "scrape_nf", "shopping_nf", "wedding_nf")
rr_neg_items <- c(rr_neg_threat_items, rr_neg_nonthreat_items)

length(rr_neg_threat_items) == 9
length(rr_neg_nonthreat_items) == 9
length(rr_neg_items) == 18

bbsiq_neg_int_items_mthd_ds <- 
  c("breath_suffocate", "vision_illness", "lightheaded_faint", "chest_heart", 
    "heart_wrong", "confused_outofmind", "dizzy_ill")
bbsiq_neg_ext_items_mthd_ds <- 
  c("visitors_bored", "shop_irritating", "smoke_house", "friend_incompetent", 
    "jolt_burglar", "party_boring", "urgent_died")
bbsiq_neg_items_mthd_ds <- c(bbsiq_neg_int_items_mthd_ds, bbsiq_neg_ext_items_mthd_ds)

length(bbsiq_neg_int_items_mthd_ds) == 7
length(bbsiq_neg_ext_items_mthd_ds) == 7
length(bbsiq_neg_items_mthd_ds) == 14

oa_items <- c("axf", "axs", "avo", "wrk", "soc")

length(oa_items) == 5

dass21_as_items <- c("bre", "dry", "hea", "pan", "sca", "tre", "wor")

length(dass21_as_items) == 7

# Store items in list

mdib_dat_items <- list(mdib_neg = mdib_neg_items,
                       mdib_neg_int = mdib_neg_int_items,
                       mdib_neg_ext = mdib_neg_ext_items,
                       bbsiq_neg = bbsiq_neg_items_mdib,
                       bbsiq_neg_int = bbsiq_neg_int_items_mdib,
                       bbsiq_neg_ext = bbsiq_neg_ext_items_mdib,
                       neuroqol_anx = neuroqol_anx_items)

mthd_dat_ds_items <- list(rr_neg_threat = rr_neg_threat_items, 
                          rr_neg_nonthreat = rr_neg_nonthreat_items, 
                          rr_neg = rr_neg_items, 
                          bbsiq_neg_int = bbsiq_neg_int_items_mthd_ds, 
                          bbsiq_neg_ext = bbsiq_neg_ext_items_mthd_ds, 
                          bbsiq_neg = bbsiq_neg_items_mthd_ds, 
                          oa = oa_items, 
                          dass21_as = dass21_as_items)

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Recode "prefer not to answer" (coded as 99) in "mdib_dat"

target_items <- c(mdib_dat_items$mdib_neg, 
                  mdib_dat_items$bbsiq_neg, 
                  mdib_dat_items$neuroqol_anx)

mdib_dat[, target_items][mdib_dat[, target_items] == 99] <- NA

# "mthd_dat_rc" does not appear to contain any such values

summary(mthd_dat_rc[, c(mdib_dat_items$mdib_neg, mdib_dat_items$neuroqol_anx)])

# "mthd_dat_ds" does not appear to contain any such values (coded 555)

sum(mthd_dat_ds$bbsiq[, mthd_dat_ds_items$bbsiq_neg] == 555) == 0
sum(mthd_dat_ds$dass21_as[, mthd_dat_ds_items$dass21_as] == 555) == 0
sum(mthd_dat_ds$oa[, mthd_dat_ds_items$oa] == 555) == 0
sum(mthd_dat_ds$rr[, mthd_dat_ds_items$rr_neg] == 555) == 0

# ---------------------------------------------------------------------------- #
# Score scales ----
# ---------------------------------------------------------------------------- #

# Compute mean of available items

mdib_dat$mdib_neg_int_m <-  rowMeans(mdib_dat[, mdib_dat_items$mdib_neg_int], na.rm = TRUE)
mdib_dat$mdib_neg_ext_m <-  rowMeans(mdib_dat[, mdib_dat_items$mdib_neg_ext], na.rm = TRUE)
mdib_dat$bbsiq_neg_int_m <- rowMeans(mdib_dat[, mdib_dat_items$bbsiq_neg_int], na.rm = TRUE)
mdib_dat$bbsiq_neg_ext_m <- rowMeans(mdib_dat[, mdib_dat_items$bbsiq_neg_ext], na.rm = TRUE)
mdib_dat$neuroqol_anx_m <-  rowMeans(mdib_dat[, mdib_dat_items$neuroqol_anx], na.rm = TRUE)

mdib_dat$mdib_neg_int_m[is.nan(mdib_dat$mdib_neg_int_m)] <- NA
mdib_dat$mdib_neg_ext_m[is.nan(mdib_dat$mdib_neg_ext_m)] <- NA
mdib_dat$bbsiq_neg_int_m[is.nan(mdib_dat$bbsiq_neg_int_m)] <- NA
mdib_dat$bbsiq_neg_ext_m[is.nan(mdib_dat$bbsiq_neg_ext_m)] <- NA
mdib_dat$neuroqol_anx_m[is.nan(mdib_dat$neuroqol_anx_m)] <- NA

mthd_dat_rc$mdib_neg_int_m <-  rowMeans(mthd_dat_rc[, mdib_dat_items$mdib_neg_int], na.rm = TRUE)
mthd_dat_rc$mdib_neg_ext_m <-  rowMeans(mthd_dat_rc[, mdib_dat_items$mdib_neg_ext], na.rm = TRUE)
mthd_dat_rc$neuroqol_anx_m <-  rowMeans(mthd_dat_rc[, mdib_dat_items$neuroqol_anx], na.rm = TRUE)

mthd_dat_rc$mdib_neg_int_m[is.nan(mthd_dat_rc$mdib_neg_int_m)] <- NA
mthd_dat_rc$mdib_neg_ext_m[is.nan(mthd_dat_rc$mdib_neg_ext_m)] <- NA
mthd_dat_rc$neuroqol_anx_m[is.nan(mthd_dat_rc$neuroqol_anx_m)] <- NA

mthd_dat_ds$bbsiq$bbsiq_neg_int_m <- rowMeans(mthd_dat_ds$bbsiq[, mthd_dat_ds_items$bbsiq_neg_int], na.rm = TRUE)
mthd_dat_ds$bbsiq$bbsiq_neg_ext_m <- rowMeans(mthd_dat_ds$bbsiq[, mthd_dat_ds_items$bbsiq_neg_ext], na.rm = TRUE)
mthd_dat_ds$rr$rr_neg_threat_m <-    rowMeans(mthd_dat_ds$rr[, mthd_dat_ds_items$rr_neg_threat], na.rm = TRUE)
mthd_dat_ds$rr$rr_neg_nonthreat_m <- rowMeans(mthd_dat_ds$rr[, mthd_dat_ds_items$rr_neg_nonthreat], na.rm = TRUE)
mthd_dat_ds$oa$oa_m <-               rowMeans(mthd_dat_ds$oa[, mthd_dat_ds_items$oa], na.rm = TRUE)
mthd_dat_ds$dass21_as$dass21_as_m <- rowMeans(mthd_dat_ds$dass21_as[, mthd_dat_ds_items$dass21_as], na.rm = TRUE)

mthd_dat_ds$bbsiq$bbsiq_neg_int_m[is.nan(mthd_dat_ds$bbsiq$bbsiq_neg_int_m)] <- NA
mthd_dat_ds$bbsiq$bbsiq_neg_ext_m[is.nan(mthd_dat_ds$bbsiq$bbsiq_neg_ext_m)] <- NA
mthd_dat_ds$rr$rr_neg_threat_m[is.nan(mthd_dat_ds$rr$rr_neg_threat_m)] <- NA
mthd_dat_ds$rr$rr_neg_nonthreat_m[is.nan(mthd_dat_ds$rr$rr_neg_nonthreat_m)] <- NA
mthd_dat_ds$oa$oa_m[is.nan(mthd_dat_ds$oa$oa_m)] <- NA
mthd_dat_ds$dass21_as$dass21_as_m[is.nan(mthd_dat_ds$dass21_as$dass21_as_m)] <- NA

# Compute total scores based on mean of available items

mdib_dat$mdib_neg_int_tot <-  mdib_dat$mdib_neg_int_m*length(mdib_dat_items$mdib_neg_int)
mdib_dat$mdib_neg_ext_tot <-  mdib_dat$mdib_neg_ext_m*length(mdib_dat_items$mdib_neg_ext)
mdib_dat$bbsiq_neg_int_tot <- mdib_dat$bbsiq_neg_int_m*length(mdib_dat_items$bbsiq_neg_int)
mdib_dat$bbsiq_neg_ext_tot <- mdib_dat$bbsiq_neg_ext_m*length(mdib_dat_items$bbsiq_neg_ext)
mdib_dat$neuroqol_anx_tot <-  mdib_dat$neuroqol_anx_m*length(mdib_dat_items$neuroqol_anx)

mthd_dat_rc$mdib_neg_int_tot <- mthd_dat_rc$mdib_neg_int_m*length(mdib_dat_items$mdib_neg_int)
mthd_dat_rc$mdib_neg_ext_tot <- mthd_dat_rc$mdib_neg_ext_m*length(mdib_dat_items$mdib_neg_ext)
mthd_dat_rc$neuroqol_anx_tot <- mthd_dat_rc$neuroqol_anx_m*length(mdib_dat_items$neuroqol_anx)

mthd_dat_ds$bbsiq$bbsiq_neg_int_tot <- mthd_dat_ds$bbsiq$bbsiq_neg_int_m*length(mthd_dat_ds_items$bbsiq_neg_int)
mthd_dat_ds$bbsiq$bbsiq_neg_ext_tot <- mthd_dat_ds$bbsiq$bbsiq_neg_ext_m*length(mthd_dat_ds_items$bbsiq_neg_ext)
mthd_dat_ds$rr$rr_neg_threat_tot <-    mthd_dat_ds$rr$rr_neg_threat_m*length(mthd_dat_ds_items$rr_neg_threat)
mthd_dat_ds$rr$rr_neg_nonthreat_tot <- mthd_dat_ds$rr$rr_neg_nonthreat_m*length(mthd_dat_ds_items$rr_neg_nonthreat)
mthd_dat_ds$oa$oa_tot <-               mthd_dat_ds$oa$oa_m*length(mthd_dat_ds_items$oa)
mthd_dat_ds$dass21_as$dass21_as_tot <- mthd_dat_ds$dass21_as$dass21_as_m*length(mthd_dat_ds_items$dass21_as)

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/scored")

save(mdib_dat, file = "./data/scored/mdib_dat.RData")
save(mthd_dat_rc, file = "./data/scored/mthd_dat_rc.RData")
save(mthd_dat_ds, file = "./data/scored/mthd_dat_ds.RData")

dir.create("./data/helper")

save(mdib_dat_items, file = "./data/helper/mdib_dat_items.RData")
save(mthd_dat_ds_items, file = "./data/helper/mthd_dat_ds_items.RData")