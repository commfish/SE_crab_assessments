# SE\_crab\_assessments

Southeast Alaska crab assessments; red king crab and Tanner crab currently using CSA method

Both Southeast Alaska species have annual pot surveys that results in a weighted stratified CPUE estimate of 3 length bins.  These are used in a catch-survey-analysis (CSA) model to estimate biomass of mature and legal male crab for each survey area.

## How to run the RKC assessment - Juneau area

The Juneau area is run first and has its own deadline, and own memo.

1. Read in data -  pull from Oracle with R code using SE\_crab\_assessments/code/pull\_data\_for\_csa and SE\_crab\_assessments/code/pull\_personal\_use\_data and pull_data_for_csa.R. Alternatively, read in from OceanAK and put that OceanAK link HERE -  should be saved in Alex Reich's public folders!!

* for the pull\_data\_for\_csa.R, one will need the crab\_survey\_password text file. For obvious reasons, that password won't be stored on the GitHub. It will be on the S drive HERE. Save it into the code on your local project.
* the oracle connection that Caitlin is currently functioning. If that breaks or needs to be independtly QCed, manually pull from the link below.
  Manual data pulls: https://oceanak.adfg.alaska.gov/analytics/saw.dll?Answers#resultsTab1980f482e50
* update the filtered years to the current year and previous year. 
* If the data is manually pulled, re-save the downloaded file as a csv instead of csv-8. Careful! OceakAK has a silent row max. Noted that when I manually pulled data in 2025 RKC_survey_CSA_Juneau_all_years.csv needed two OceanAK pulls due to the row max.

2. Open SE\_crab\_assessments/code/rkc\_code/survey\_areas/Juneau.R
   Work through the script, checking each output that is produced as you go. When you get to the part that says

#### STOP HERE AND run .Rmd file for this area for summary and to confirm things look ok

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

then...

3. Open SE\_crab\_assessments/text/(current year)/juneau.Rmd
   Run the script and check the resulting html file against the one on the S: drive.
4. Open SE\_crab\_assessments/code/rkc\_code/personal\_use.R
   Run through the script and check the outputs.
4.1 If commercial fishery the prior year, run code/RKC commercial catch summary.R
5. Run the Juneau CSA Excel model, using the outputs from the Juneau.R and personal\_use.R scripts
   SE\_crab\_assessments/CSA excel/Juneau (current year) new weighting

* note: most recent model is Juneau 2026 CPUE fix excel; (and Juneau 2024 CPUE fix, accordingly)

5.1 An improved model that uses RTMB and not Excel is in development, but is not ready yet.

6. Update the biomass file (SE\_crab\_assessments/data/rkc/biomass.csv) with the estimates from the Juneau CSA model (for all years)

7. Return to SE\_crab\_assessments/code/rkc\_code/suvey\_areas/Juneau.R

8. Run code/rkc_code/juneau_hindcast_figure.R to create other figures for the Juneau memo

9. Copy paste Juneau memo_pdf 1.rmd into the current year's text folder. Then update the text and run. This drafty document will be submitted and discussed with the SE crab biologists about ~2 weeks after the Juneau-area RKC survey.



## How to run the assessment - all other areas



## Tanner Assessment

1. Read in data, either using pull\_data\_for\_csa.R or OceanAK

* pick through this and just run the tanner-relevant code
* as of 10/2/25 - the pull\_data\_for\_csa.R code is functional. Insure the Juneau, Barlow areas pull everything that they need to and there is no weird alternation error.
* there are 3 main parts of data: "tanner\_rkc/rkc survey for Tanner crab CSA\_cur\_yr.csv", "tanner\_tcs/tanner crab survey for CSA\_13\_cur\_yr.csv", and "nj\_stp/Juneau\_red\_crab\_survey\_for\_Tanner\_crab\_CSA\_cur\_yr.csv"

-cur\_yr being "25" for 2025, etc.



2\. Get data for tanner\_logbook.R. Run tanner\_logbook.R



3\. And then get the data to run tanner\_harvest.R. Run tanner\_harvest.R. The fishery is in the spring so it is completed by March-ish

*  	this looks to be from OceanAK. I have it in my folder- agreich/SE Tanner crab/2025 Detailed Fish Tickets\_Tanner. Update the year (fishing season) and export as a csv.



NOTE: For steps 4 and 6, there is a QC aspect for the survey data. Pay attention to the wrangling and QC before the CPUE standardization. The data has already been QC'ed before entry, but field team handles a TON of crab and thus things slip through the cracks sometimes. So if you find data entry errors in step 4 and 6, ask the relevant contact (Zane, currently) for corrections, re-download and save the relevant data from OceanAK if necessary. Small things can be corrected by hand, but best data practices and all that.



4\. Run tanner\_redkingcrab\_areas\_survey\_results.R

* make a new biomass file for this year. NAME GOES HERE!!
* STOP at the stop point and run the R markdown with summary of RKC areas (line~335). Follow instructions
* Run the Excel CSAs- with the std CPUE data from the summary. Copy-paste the biomass output from this into the 2025 tanner biomass file
* Then continue running the rest of the R file



5\. Run NJ\_SP\_processing.R

* then run sp\_nj\_figures.R
* check to make sure no alternatey weird input issues with Juneau and Barlow (does barlow have BOTH odd and even pots??)



6\. Run TCS\_processingCODE.R

* this is for the Tanner survey areas of Holkham, Thomas, Glacier Bay, and Icy Strait
* Download this data from pull\_data\_for\_csa.R on OceanAK if you have not already
*  	- I tend to run the red king crab areas before the Tanner survey ends, because I can get a head start on the analysis that way
* \*FOR 2025 - Adjust GBay to delete the pots that are now outisde the survey area\*
* Stop at the STOP point, run the summary rmd, run CSA's, and manually update the biomass file.



\*NOTE\*- I plan to get rid of the excel stuff (Replace with RTMB) at some time in the future, but not this year.



7\. Run figures\_1\_and\_2\_Tanner.R

7a. Run curyr\_tanner\_draft.Rmd (in the text folder) as far as the line that creates tanner\_annual\_pt\_estimate\_historic\_curyr.csv

----you'll need this for figures\_1\_and\_2\_Tanner.R



8\. Update and run cur\_yr\_tanner\_draft.rmd



9\. update the Tanner crab presentation - for the meeting with biologists

## 

\_\_\_\_\_

'



Still old below, need to update::

2\.  Run the "processing code.R" files for both Tanner areas and RKC survey areas.
These should produce results from CPUE calcs from survey, long term and short term comparisons - see Excel sheet for summary of these.
**fix** these need to be summarized in R - mostly likely a markdown/ .csv file.
Excel file is 'Tanner Matrix 2020(21).xlsx'



3\. Results from 1) found in .rmd file need to be translated into the CSA files - these are still in Excel sheet and run with solver in Excel. Work has been made towards translating these to R (see separate git hub repo).
CSA models are run with solver and results - biomass are copied into '...biomass\_2020.csv' for graphing purposes.



4\.  See Excel file for summary of 2020 estimates of biomass for each area - taken from individual CSA excel file - Region GHL Summary tab.

Look at word file - tanner crab asessment for other figures/ tables needed. start with 'figures\_1\_and\_2\_Tanner.R'





# Notes

## RTMB development

1. make confidence intervals not go negative (bootsrap, perhaps)
2. Talk to RTMB advisory committee about the default nonlinearity in the excel, and if I want to code that into RTMB or drop that component

* it's likely just an old default

# 

# 

# 

# OLD STUFF BELOW

## CSA model

Ref: Collie and DeLong 1998, Clark et al. 2003, Kruse and Collie 1991, Collie and Kruse 1998, Zheng et al. 1997
See ROP for red king crab (reference below) for complete reference citations.

see code/CSA\_model for running CSA model

CSA model was constructed as a function for redcrab and tanner crab differently. Code for these model configurations will also be found under CSA\_model folder

## red king crab

Currently (2019) survey 7 areas: Juneau, Excursion, St.James Bay, Peril Strait, Gambier Bay, Pybus Bay, and Seymour Canal.
Historically other areas have been surveyed.
Data from these surveys is stored in OceanAK.
These data are also used to assess the Tanner crab populations in each of these bays.



Code: see code/rkc\_code/survey\_areas for each area that is surveyed.
code for regional figures, etc. is in 'code/rkc\_code'

Area instructions:

* Juneau areas is usually done on its own, and there's a specific memo for this area to address Juneau (11-A) personal use openings - usually in August.
* Each survey area is processed seperately due to the historic differences between them.



Stratman, J., A. Messmer, K. Wood, T. Bergmann, and K. Palof. 2019. Operational plan: Southeast Alaska red
king crab pot survey, 2018–2022. Alaska Department of Fish and Game, Regional Operational Plan
ROP.CF.1J.2019.02, Douglas.

## Tanner crab

Currently (2019) survey 4 areas in October: Icy Strait, Glacier Bay, Holkham Bay, and Thomas Bay.
Data from the red king crab survey areas is also used to assess the Tanner crab populations in those bays.  
Tanner crab regionally consists of 11 areas, 4 Tanner survey areas and 7 rkc survey areas.

data for tanner crab:
RKCS: data is pulled from red king crab surveys in southeast stored in /data/tanner/tanner\_rkc/red crab survey for Tanner crab CSA\_'cur\_yr'.csv
SP or Juneau area: data is pulled for the current year and stored in /data/tanner/nj\_stp

* data needs to be divided into pots from NJ and pots in SP.

see readme.md in /code/tc\_code/ for more details.





How to run the Tanner Assessment

