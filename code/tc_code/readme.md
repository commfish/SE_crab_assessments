# tanner crab assessment 

The southeast alaska tanner crab asssessment is performed on both the red king crab and the tanner crab survey areas. Analysis is different for each of these surveys, so instructions should be followed for each.
Use file: TCS_processingCODE.R
Need file in data portion of this file, also need to create results folder (results/tanner/tanner_tcs/cur_yr)


# red king crab survey areas
Tanner crab are assessed from the red king crab survey areas as a non-target or undesigned survey. Therefore the CPUE for crab calculated from these areas is not weighted or stratified (unlike the directed survey which is density stratified). Therefore the red crab survey areas are processed together as one group (so you need to group by area). 

see tanner_redkingcrab_areas_survey_results.R

DATA: data is pulled from OceanAK, currently setup to pull all rkc areas and all years, in 2020 row limit exceed so data was downloaded in 2 files. Can use older file and no need to download again unless historic data would change.
.\data\tanner\tanner_rkc

R code for CPUE and short/long term trends:
tanner_redkingcrab_areas_survey_results.R


Biomass file -
tanner_2020_biomassmodel.csv - udpated manually for each area from CSA model runs (this needs to be made more efficient)

## north juneau and stephens passage
Come from data from the Juneau leg of the red king crab survey. These pots need to be seperated into strata for Stephens Passage - using GIS (need to move instructions in here - currently in OneNote). 
see NJ_SP_processing.R