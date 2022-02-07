# README

Author: Jeremy W. Eberle

# Background

The present scripts conduct preliminary analyses for [Professor Jessie Gibson](https://www.nursing.virginia.edu/people/js6zn/)'s NIH [K23](https://researchtraining.nih.gov/programs/career-development/k23) ([PA-20-206](https://grants.nih.gov/grants/guide/pa-files/PA-20-206.html)) submission titled "Modification of Threat Interpretation Bias to Reduce Anxiety in Neurodegenerative Movement Disorders."

# Data

The present scripts import deidentified preliminary datasets (stored privately on UVA Box) from two studies, for which recruitment is ongoing.

## MDIB Development Study

`Prelim Data for Jeremy 2-3-22_MDIB.csv` in `data/bot_cleaned` are preliminary long-format data from a RedCap survey of patients with Huntington's disease (HD) administered at two time points. The study's purpose is to develop the Movement Disorders Interpretation Bias Scale (MDIB). Bot responses have already been cleaned from the data by Dr. Gibson and a graduate research assistant.

Participants with above-threshold anxiety symptoms (on NeuroQoL) are eligible to participate in the MindTrails-HD pilot study (see below). However, the `record_id`s in the two datasets do not correspond to each other.

## MindTrails-HD Pilot Study

`Prelim Data for Jeremy 2-3-22_MindTrails-HD pilot.csv` in `data/raw` are preliminary long-format data from a RedCap survey for a pilot study of an adaptation of interpretation bias training (MindTrails for Huntington's Disease; MindTrails-HD) for anxious (on NeuroQoL) patients with HD. The data are raw, with no cleaning for bots required.

# Scripts

The following scripts in the `code` folder are to be run in order.

`1_define_functions.R` defines functions for use in later scripts.

`2_compute_scores.R` imports the MDIB and MindTrails-HD datasets from `data/bot_cleaned` and `data/raw`, respectively; computes selected scale scores; and outputs scored datasets to `data/scored`. It also outputs a list of items comprising selected scales to `data/helper`.

`3_run_mdib_analyses.R` imports the scored MDIB dataset, runs the analyses below, and outputs results to `results/mdib`.
- Compute Cronbach's alpha for each MDIB scale at baseline
- Compute correlations between each MDIB scale and NeuroQoL anxiety scale at baseline
- Compute correlations between each BBSIQ scale and NeuroQoL anxiety scale at baseline 

`4_run_mthd_analyses.R` imports the scored MindTrails-HD dataset, runs the analysis below, and outputs results to `results/mthd`.
- Plot raw NeuroQoL anxiety scale scores over time for treatment completers
