# red king crab assessment 

The southeast alaska red king crab crab asssessment is performed on the red king crab survey areas. 
Analysis is done for each area seperate, partly due to survey data being available at different times and due to each area being a seperate stock.

Use file: survey_areas/"individual survey names"
Need file in data portion of this file, also need to create results folder (results/rkc/cur_yr)


## Juneau area 
Performed in mid-July. This area is done as it's own memo to inform a summer personal use fishery opening in 11-A. 
Workflow:
  - download survey data from Ocean AK - see 'Shared Folders/ Commercial Fisheries/Region   1/Invertebrates/User Reports/kjpalof/se rkc areas' in Ocean AK. Analysis is 'RKC survey CSA_jnu' update to include most recent 2 years in filter options.
  - Open "Juneau.R" under code/rkc_code/survey_areas
    - Step through this file to summarize survey data etc. 
    - Copy "juneau.rmd" into current years /text/cur_yr folder and run it for Juneau. Change top info - current year, etc. in this markdown
  - 'biomass.csv' - same a copy of this for historic reference (usually saved as 'biomass_XXXX.csv, with XXXX being last year). After you run the CSA model in excel you need to move the biomass estimates here for the current year - see next step of running CSA
  - CSA model - currently still in Excel file, see "readme" sheet here for instructions
  - Once the CSA is completed and the 'biomass.csv' file is updated go back to "juneau.R" and run the code for the 'panel_figures'
  - Once the above is completed open the "text/cur_yr/Juneau_memo.Rmd" file and update it with current year, etc. file structure should autofind with current year updates. Go through text and update as needed with summarises, etc. 
  - Once this is completed and you "knit" the .doc, then I usually move it to the "2022 RKC" folder into the Juneau folder here. Then I can update the "TABLES and Figs_cur_yr 11-A....doc" and put those figures into one file with ones from markdown .doc. This then becomes the "cur_yr 11-A RKC_CONFIDENTIAL_draft.doc", once you present and give this to regional staff then we can determine the confidential nature of the data. All initial reports are determined to be confidential until this is done, plus current years survey data is confidential.
  

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
