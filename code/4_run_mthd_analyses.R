# ---------------------------------------------------------------------------- #
# Run MindTrails-HD Analyses
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

groundhog.library(ggplot2, groundhog_day)
groundhog.library(svglite, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/mthd/scored/mthd_dat_rc.RData")
load("./data/mthd/scored/mthd_dat_ds.RData")

# ---------------------------------------------------------------------------- #
# Prepare data for plot ----
# ---------------------------------------------------------------------------- #

# Restrict to session 5 training completers, restrict to data through session 5,
# and restrict to scale scores

s5_train_completers <- unique(mthd_dat_rc$record_id[mthd_dat_rc$redcap_event_name == 
                                                      "session_5_arm_1"])

tot_scores_rc <- mthd_dat_rc[mthd_dat_rc$record_id %in% s5_train_completers &
                               mthd_dat_rc$redcap_event_name != "interviewtfc_arm_1",
                             c("record_id", "participant_id", "redcap_event_name", 
                               "neuroqol_anx_m", "neuroqol_anx_tot")]

tot_scores_ds <- lapply(mthd_dat_ds, function(x) {
  x[x$record_id %in% s5_train_completers,
    c("record_id", "participant_id", "session_only",
      names(x)[endsWith(names(x), "_m") | endsWith(names(x), "_tot")])]
})

# Create data frame with time points for all sessions

assessments <- c("Baseline", paste0("Session ", 1:5))

plot_df <- data.frame(record_id = rep(s5_train_completers, each = 6),
                      assessment = rep(assessments, length(s5_train_completers)))

# Add "assessment" values to total scores

tot_scores_rc$assessment <- NA
tot_scores_rc$assessment[tot_scores_rc$redcap_event_name == "baseline_arm_1"] <- "Baseline"
tot_scores_rc$assessment[tot_scores_rc$redcap_event_name == "session_3_arm_1"] <- "Session 3"
tot_scores_rc$assessment[tot_scores_rc$redcap_event_name == "session_5_arm_1"] <- "Session 5"

for (i in 1:length(tot_scores_ds)) {
  tot_scores_ds[[i]][, "assessment"] <- NA
  tot_scores_ds[[i]][tot_scores_ds[[i]][, "session_only"] %in% c("Eligibility", "preTest"),
                     "assessment"] <- "Baseline"
  tot_scores_ds[[i]][tot_scores_ds[[i]][, "session_only"] == "firstSession",
                     "assessment"] <- "Session 1"
  tot_scores_ds[[i]][tot_scores_ds[[i]][, "session_only"] == "secondSession",
                     "assessment"] <- "Session 2"
  tot_scores_ds[[i]][tot_scores_ds[[i]][, "session_only"] == "thirdSession",
                     "assessment"] <- "Session 3"
  tot_scores_ds[[i]][tot_scores_ds[[i]][, "session_only"] == "fourthSession",
                     "assessment"] <- "Session 4"
  tot_scores_ds[[i]][tot_scores_ds[[i]][, "session_only"] == "fifthSession",
                     "assessment"] <- "Session 5"
}

# Remove time point columns other than "assessment"

tot_scores_rc$redcap_event_name <- NULL

for (i in 1:length(tot_scores_ds)) {
  tot_scores_ds[[i]][, c("session_only", "session_and_eligibility_status")] <- NULL
}

# Add scale scores to plotting data

plot_df <- merge(plot_df, 
                 tot_scores_rc[, names(tot_scores_rc)[names(tot_scores_rc) != 
                                                        "participant_id"]], 
                 by = c("record_id", "assessment"), 
                 all.x = TRUE)

for (i in 1:length(tot_scores_ds)) {
  plot_df <- 
    merge(plot_df, 
          tot_scores_ds[[i]][, names(tot_scores_ds[[i]])[names(tot_scores_ds[[i]]) != 
                                                                 "participant_id"]],
          by = c("record_id", "assessment"),
          all.x = TRUE)
}

# Define index of random consecutive integers for plotted participants

participant_rnd_ids <- sample(1:length(unique(plot_df$record_id)),
                              size = length(unique(plot_df$record_id)))

plot_df$participant_rnd_id <- rep(participant_rnd_ids,
                                  each = length(assessments))

# Recode participant and assessment variables as factors for ggplot2

plot_df$participant_rnd_id <- factor(plot_df$participant_rnd_id)
plot_df$assessment <- factor(plot_df$assessment, levels = assessments)

# Sort plotting data

plot_df <- plot_df[order(plot_df$record_id, plot_df$assessment), ]

# ---------------------------------------------------------------------------- #
# Plot scale scores over time ----
# ---------------------------------------------------------------------------- #

# Define function to plot scale scores over 5 weeks for completer sample. Use 
# total scores (vs. mean item scores) for consistency with other grant materials. 
# Use "loess" smoothing to connect points with "geom_smooth" given that "geom_line"
# does not connect points through NAs.

plot_tot_scores <- function(plot_df, y_var, 
                            title, 
                            y_label, 
                            y_breaks, scale_min, scale_max,
                            legend_position) {
  tot_plot <- 
    ggplot(plot_df,
           aes(x = assessment, y = {{y_var}},
               group = participant_rnd_id, 
               color = participant_rnd_id, 
               linetype = participant_rnd_id)) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_point(data = plot_df) +
    labs(title = title,
         x = "Assessment",
         y = y_label) +
    scale_linetype_manual(name = "Participant",
                          values = c("1" = "longdash",
                                     "2" = "solid",
                                     "3" = "twodash")) +
    scale_color_manual(name = "Participant",
                       values = c("1" = "#1b9e77", 
                                  "2" = "#7570b3",
                                  "3" = "#d95f02")) +
    scale_y_continuous(breaks = y_breaks,
                       limits = c(scale_min, scale_max)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.key.width = unit(2, "cm")) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(legend.position = legend_position)
  
  return(tot_plot)
}

# Run function for each outcome. All plots below were viewed with 2 treatment
# completers (RedCap data as of 2/3/22, Data Server data as of 2/11/22), but 
# only NeuroQoL and BBSIQ plots were viewed for 3 treatment completers (RedCap 
# and Data Server data as of 2/17/22).

neuroqol_anx_tot_plot <- plot_tot_scores(
  plot_df, neuroqol_anx_tot,
  "Fig. 2. Anxiety Over Time for Tx Compl.",
  "Total Score (NeuroQoL Anxiety)",
  c(1:5)*8, 1*8, 5*8, c(0.8, 0.8)
)

# oa_tot_plot <- plot_tot_scores(
#   plot_df, oa_tot,
#   "Fig. 3. Anxiety Over Time for Tx Compl.",
#   "Total Score (OASIS)",
#   c(0:4)*5, 0*5, 4*5, c(0.8, 0.8)
# )
# 
# dass21_as_tot_plot <- plot_tot_scores(
#   plot_df, dass21_as_tot,
#   "Fig. 4. Anxiety Over Time for Tx Compl.",
#   "Total Score (DASS-21-AS)",
#   c(0:3)*7, 0*7, 3*7, c(0.8, 0.8)
# )

bbsiq_neg_int_tot_plot <- plot_tot_scores(
  plot_df, bbsiq_neg_int_tot,
  "Fig. 3. Neg. Bias (Internal) Over Time for Tx Compl.",
  "Total Score (Neg. BBSIQ Internal)",
  c(0:4)*7, 0*7, 4*7, c(0.8, 0.8)
)

bbsiq_neg_ext_tot_plot <- plot_tot_scores(
  plot_df, bbsiq_neg_ext_tot,
  "Fig. 4. Neg. Bias (External) Over Time for Tx Compl.",
  "Total Score (Neg. BBSIQ External)",
  c(0:4)*7, 0*7, 4*7, c(0.8, 0.8)
)

# rr_neg_threat_tot_plot <- plot_tot_scores(
#   plot_df, rr_neg_threat_tot,
#   "Fig. 7. Neg. Bias (Threat) Over Time for Tx Compl.",
#   "Total Score (Neg. RR Threat)",
#   c(1:4)*9, 1*9, 4*9, c(0.8, 0.2)
# )
# 
# rr_neg_nonthreat_tot_plot <- plot_tot_scores(
#   plot_df, rr_neg_nonthreat_tot,
#   "Fig. 8. Neg. Bias (Nonthreat) Over Time for Tx Compl.",
#   "Total Score (Neg. RR Nonthreat)",
#   c(1:4)*9, 1*9, 4*9, c(0.8, 0.8)
# )

# Store plots in list

plots <- list(neuroqol_anx_tot_plot = neuroqol_anx_tot_plot,
              # oa_tot_plot = oa_tot_plot,
              # dass21_as_tot_plot = dass21_as_tot_plot,
              bbsiq_neg_int_tot_plot = bbsiq_neg_int_tot_plot,
              bbsiq_neg_ext_tot_plot = bbsiq_neg_ext_tot_plot)
              # rr_neg_threat_tot_plot = rr_neg_threat_tot_plot,
              # rr_neg_nonthreat_tot_plot = rr_neg_nonthreat_tot_plot

# Save plots

dir.create("./results/mthd/plots/svg", recursive = TRUE)
dir.create("./results/mthd/plots/png", recursive = TRUE)

for (i in 1:length(plots)) {
  ggsave(paste0("./results/mthd/plots/svg/", i, "_", names(plots[i]), ".svg"),
         plot = plots[[i]],
         device = "svg",
         width = 5, height = 5)
  ggsave(paste0("./results/mthd/plots/png/", i, "_", names(plots[i]), ".png"),
         plot = plots[[i]],
         width = 5, height = 5)
}