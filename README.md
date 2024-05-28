# SE_crab_assessments
Southeast Alaska crab assessments; red king crab and Tanner crab currently using CSA method

Both Southeast Alaska species have annual pot surveys that results in a weighted stratified CPUE estimate of 3 length bins.  These are used in a catch-survey-analysis (CSA) model to estimate biomass of mature and legal male crab for each survey area.

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

