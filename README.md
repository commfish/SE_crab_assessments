
# SE_crab_assessments
Southeast Alaska crab assessments; red king crab and Tanner crab currently using CSA method

Both Southeast Alaska species have annual pot surveys that results in a weighted stratified CPUE estimate of 3 length bins.  These are used in a catch-survey-analysis (CSA) model to estimate biomass of mature and legal male crab for each survey area.

## How to run the assessment - Juneau area

The Juneau area is run first and has its own deadline, and own memo.

1. Read in data - MANUALLY FROM ORACLE!!
Ideally using SE_crab_assessments/code/pull_data_for_csa and SE_crab_assessments/code/pull_personal_use_data. I do not know if the Oracle connection is active as of 2025. Alternatively, read in from OceanAK and put that OceanAK link HERE!!
- for the pull_data_for_csa.R, one will need the crab_survey_password text file. For obvious reasons, that password won't be stored on the GitHub. It will be on the S drive HERE. Save it into the code on your local project.
- the oracle connection that Caitlin had set up is broken, meaning the data needs to be pulled manually for now.
Manual data pulls: https://oceanak.adfg.alaska.gov/analytics/saw.dll?Answers#resultsTab1980f482e50
- update the filtered years to the current year and previous year. Re-save the downloaded file as a csv instead of csv-8
- the sablefish analysis did not like csv-8

For RKC_survey_CSA_Juneau_all_years.csv - had to import two csv's from OCeanAK, by changing the years on the aforementioned link.
Careful! OceakAK has a silent row max

2. Open SE_crab_assessments/code/rkc_code/survey_areas/Juneau.R
Work through the script, checking each output that is produced as you go. When you get to the part that says

#### STOP HERE AND run .Rmd file for this area for summary and to confirm things look ok
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

then...

3. Open SE_crab_assessments/text/2023/juneau.Rmd
Run the script and check the resulting html file against the one on the S: drive.

4. Open SE_crab_assessments/code/rkc_code/personal_use.R
Run through the script and check the outputs

5. Run the Juneau CSA model, using the outputs from the Juneau.R and personal_use.R scripts
SE_crab_assessments/CSA excel/Juneau 2023 new weighting
- note: most recent modle is Juneau 2025 CPUE fix exce; (and Juneau 2024 CPUE fix, accordingly)

5.1 ALT -  run the Juneau CSA RTMB model, using R and necessary imputs

6. Update the biomass file (SE_crab_assessments/data/rkc/biomass.csv) with the estimates from the Juneau CSA model

7. Return to SE_crab_assessments/code/rkc_code/suvey_areas/Juneau.R 



## How to run the assessment - all other areas

# Notes

## RTMB development
Juneau area model is complete, and tested for 2024. 2025 Juneau and other area models pending, as is finagling the output and input to run smoothlu

# OLD STUFF BELOW
## CSA model
Ref: Collie and DeLong 1998, Clark et al. 2003, Kruse and Collie 1991, Collie and Kruse 1998, Zheng et al. 1997
See ROP for red king crab (reference below) for complete reference citations.

see code/CSA_model for running CSA model

CSA model was constructed as a function for redcrab and tanner crab differently. Code for these model configurations will also be found under CSA_model folder

## red king crab 
Currently (2019) survey 7 areas: Juneau, Excursion, St.James Bay, Peril Strait, Gambier Bay, Pybus Bay, and Seymour Canal.
Historically other areas have been surveyed.
Data from these surveys is stored in OceanAK.
These data are also used to assess the Tanner crab populations in each of these bays. 


Code: see code/rkc_code/survey_areas for each area that is surveyed. 
code for regional figures, etc. is in 'code/rkc_code'

Area instructions:
- Juneau areas is usually done on its own, and there's a specific memo for this area to address Juneau (11-A) personal use openings - usually in August. 
- Each survey area is processed seperately due to the historic differences between them.


Stratman, J., A. Messmer, K. Wood, T. Bergmann, and K. Palof. 2019. Operational plan: Southeast Alaska red
king crab pot survey, 2018–2022. Alaska Department of Fish and Game, Regional Operational Plan
ROP.CF.1J.2019.02, Douglas.

## Tanner crab
Currently (2019) survey 4 areas in October: Icy Strait, Glacier Bay, Holkham Bay, and Thomas Bay. 
Data from the red king crab survey areas is also used to assess the Tanner crab populations in those bays.  
Tanner crab regionally consists of 11 areas, 4 Tanner survey areas and 7 rkc survey areas. 

data for tanner crab:
RKCS: data is pulled from red king crab surveys in southeast stored in /data/tanner/tanner_rkc/red crab survey for Tanner crab CSA_'cur_yr'.csv
SP or Juneau area: data is pulled for the current year and stored in /data/tanner/nj_stp
- data needs to be divided into pots from NJ and pots in SP. 

see readme.md in /code/tc_code/ for more details.

