# SE_crab_assessments
Southeast Alaska crab assessments; red king crab and Tanner crab currently using CSA method

Both Southeast Alaska species have annual pot surveys that results in a weighted stratified CPUE estimate of 3 length bins.  These are used in a catch-survey-analysis (CSA) model to estimate biomass of mature and legal male crab for each survey area.

## CSA model
Ref: Collie and DeLong 1998, Clark et al. 2003, Kruse and Collie 1991, Collie and Kruse 1998, Zheng et al. 1997
See ROP for red king crab (reference below) for complete reference citations.

## Workflow
The model is implemented in R using the Template Model Builder (TMB) package (Kristensen et al., 2016). The TMB code is found in a file named `SE_csa_tmb.cpp`. Workflow for loading and running the model, then viewing output is found in `SE_csa_tmb_Rworkflow.R` and the supporting script `SE_csa_tmb_Rfunctions.R`.  

For now there is **not** separate models for RKC and Tanner crab.
