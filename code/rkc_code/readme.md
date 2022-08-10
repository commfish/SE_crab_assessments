# red king crab assessment 

The southeast alaska red king crab crab asssessment is performed on the red king crab survey areas. 
Analysis is done for each area seperate, partly due to survey data being available at different times and due to each area being a seperate stock.

Use file: survey_areas/"individual survey names"
Need file in data portion of this file, also need to create results folder (results/rkc/cur_yr)


## Juneau area 
Performed in mid-July. This area is done as it's own memo to inform a summer personal use fishery opening in 11-A. 


### Juneau personal use
Harvest is calculated in 'personal_use.R' - see output from OceanAK, at this time you still need 2 calls from data - one with harvest "no of crab" and one that gives "permit returned status designation"

Actualized harvest rate for the Juneau area, see tab in Excel file. All tables and calculations are in the main excel sheet for Juneau "Juneau XXXX new weighting.xlsx"


## Survey areas ----
Each survey area has code to summarize CPUE, weights, trends, etc. under 'code/rkc_code/survey_areas'
- run each of these
- code for updating CSA with values and .rmd for each survey area to summarize the results - see 'text/2022'
- update biomass.csv file after each CSA is run. This need to be updated BEFORE you run the "panel_figure" function to create figures

After these are complete: 
run 'figure2_tables.R'
- this file has code to create tables needed to summarize data in the "RIR draft XX.Rmd"


## RIR text ---
values in line 134 taken from change_in_modeled_regional_biomass_2022.csv
which is found in 'C:\Users\kjpalof\Documents\SE_crab_assessments\results\rkc\Region1|2022'

excel file for keeping track of estimates from each year - 'Figure 1 table 2022RKC.xlsx"
- update this file with values from "2022(23)_RKC scoring text_MATRIX_v2.xlsx" for surveyed area legal and mature that are NOT expanded from mark-recapture
