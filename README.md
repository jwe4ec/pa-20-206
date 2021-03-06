# pa-20-206

Author: Jeremy W. Eberle

## Background

The present scripts conduct preliminary analyses for [Professor Jessie Gibson](https://www.nursing.virginia.edu/people/js6zn/)'s NIH [K23](https://researchtraining.nih.gov/programs/career-development/k23) ([PA-20-206](https://grants.nih.gov/grants/guide/pa-files/PA-20-206.html)) submission titled "Modification of Threat Interpretation Bias to Reduce Anxiety in Neurodegenerative Movement Disorders."

## Data

The present scripts import deidentified preliminary datasets (stored privately on UVA Box) from two studies, for which recruitment is ongoing.

### MDIB Development Study

`Prelim Data for Jeremy 2-3-22_MDIB.csv` in `data/mdib/bot_cleaned` are preliminary long-format data from Dr. Gibson collected via a RedCap survey of patients with Huntington's disease (HD) administered at two time points. The study's purpose is to develop the Movement Disorders Interpretation Bias Scale (MDIB). Bot responses have already been cleaned from the data by Dr. Gibson and a graduate research assistant.

Participants with above-threshold anxiety symptoms (on NeuroQoL) are eligible to participate in the MindTrails-HD pilot study (see below). However, the `record_id`s in the two RedCap datasets do not correspond to each other.

### MindTrails-HD Pilot Study

`Prelim Data for Jeremy 2-17-22_MindTrails-HD pilot.csv` in `data/mthd/raw/from_rc` are preliminary long-format raw data from Dr. Gibson collected via a RedCap survey for a pilot study of an adaptation of interpretation bias training (MindTrails for Huntington's Disease; MindTrails-HD) for anxious (on NeuroQoL) patients with HD.

The following files in `data/mthd/raw/from_ds/2022.02.17` are preliminary long-format raw data collected via the MindTrails website and extracted from the TET study dataset by [Ángel Vela de la Garza Evia](https://github.com/avel22) from the `calm` database on the `teachmanlab` Data Server.

- `HD BBSIQ-data-2022-02-17.csv`
- `HD DASS-data-2022-02-17.csv`
- `HD OA-data-2022-02-17.csv`
- `HD RR-data-2022-02-17.csv`

`2022.02.14-redcap_record_id-mt_participant_id.csv` in `data/mthd/link` from Dr. Gibson links RedCap `record_id` to Data Server `participant_id`.

## Scripts

The following scripts in the `code` folder are to be run in order.

`1_define_functions.R` defines functions for use in later scripts.

`2_compute_scores.R` imports the MDIB, MindTrails-HD RedCap, and MindTrails-HD Data Server datasets from `data/mdib/bot_cleaned`, `data/mthd/raw/from_rc`, and `data/mthd/raw/from_ds` respectively. It cleans the data and, using the the MindTrails-HD linking file from `data/mthd/link`, links the RedCap and Data Server datasets. It then scores selected scales and outputs scored datasets to `data/mdib/scored` and `data/mthd/scored` and lists of each scale's items to `data/mdib/helper` and `data/mthd/helper`.

`3_run_mdib_analyses.R` imports the scored MDIB dataset, runs the analyses below, and outputs results to `results/mdib`.
- Compute Cronbach's alpha for each MDIB scale at baseline
- Compute correlations between each MDIB scale and NeuroQoL anxiety scale at baseline
- Compute correlations between each BBSIQ scale and NeuroQoL anxiety scale at baseline 

`4_run_mthd_analyses.R` imports the scored MindTrails-HD dataset, runs the analyses below, and outputs results to `results/mthd/plots`.
- Plot raw scale scores below over time for treatment completers
  - NeuroQoL anxiety
  - Negative BBSIQ (Internal), Negative BBSIQ (External)
- Note: Raw scale scores below were also plotted for treatment completers using an earlier dataset (code is retained but commented out)
  - OASIS, DASS-21-AS
  - Negative RR (Threat), Negative RR (Nonthreat)
