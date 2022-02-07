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

load("./data/scored/mthd_dat.RData")

# ---------------------------------------------------------------------------- #
# Prepare data for plot ----
# ---------------------------------------------------------------------------- #

# Restrict to session 5 training completers, restrict to data through session 5,
# and restrict to NeuroQoL scores

s5_train_completers <- unique(mthd_dat$record_id[mthd_dat$redcap_event_name == 
                                                   "session_5_arm_1"])

tot_scores <- mthd_dat[mthd_dat$record_id %in% s5_train_completers &
                         mthd_dat$redcap_event_name != "interviewtfc_arm_1",
                       c("record_id", "redcap_event_name", 
                         "neuroqol_anx_m", "neuroqol_anx_tot")]

# Create data frame with time points for all sessions

assessments <- c("Baseline", paste0("Session ", 1:5))

plot_df <- data.frame(record_id = rep(s5_train_completers, each = 6),
                      assessment = rep(assessments, length(s5_train_completers)),
                      redcap_event_name = rep(c("baseline_arm_1", NA, NA, 
                                                "session_3_arm_1", NA,
                                                "session_5_arm_1"),
                                              length(s5_train_completers)))

# Add total scores to plotting data

plot_df <- merge(plot_df, tot_scores, 
                 by = c("record_id", "redcap_event_name"), 
                 all.x = TRUE)

# Recode participant and assessment variables as factors for ggplot2

plot_df$record_id <- factor(plot_df$record_id)
plot_df$assessment <- factor(plot_df$assessment, levels = assessments)

# Sort plotting data

plot_df <- plot_df[order(plot_df$record_id, plot_df$assessment), ]

# ---------------------------------------------------------------------------- #
# Plot anxiety symptoms over time ----
# ---------------------------------------------------------------------------- #

# Plot anxiety symptoms (NeuroQoL) over 5 weeks for completer sample (n = 2). Use
# total scores (vs. mean item scores) for consistency with other grant materials.
# Use "loess" smoothing to connect points with "geom_smooth" given that "geom_line"
# does not connect points through NAs.

scale_min <- 1*8
scale_max <- 5*8

neuroqol_anx_tot_plot <- 
  ggplot(plot_df,
       aes(x = assessment, y = neuroqol_anx_tot,
           group = record_id, color = record_id, linetype = record_id)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_point(data = plot_df) +
  labs(title = "Figure 2. Anxiety Over Time for Tx Completers",
       x = "Assessment",
       y = "Total Score (NeuroQoL Anxiety)") +
  scale_linetype_manual(name = "Participant",
                        values = c("1" = "longdash",
                                   "2" = "solid")) +
  scale_color_manual(name = "Participant",
                     values = c("1" = "#1b9e77", 
                                "2" = "#7570b3")) +
  scale_y_continuous(breaks = c(1*8, 2*8, 3*8, 4*8, 5*8),
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
  theme(legend.position = c(0.8, 0.8))

# Save plot

dir.create("./results/mthd")

ggsave("./results/mthd/neuroqol_anx_tot_plot.svg",
       plot = neuroqol_anx_tot_plot,
       device = "svg",
       width = 5, height = 5)
ggsave("./results/mthd/neuroqol_anx_tot_plot.png",
       plot = neuroqol_anx_tot_plot,
       width = 5, height = 5)