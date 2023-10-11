############################################################################################
# Script name: pull_personal_use_data.R
# Script goal: create data files to use in the RKC assessment using a direct database connection;
# these files were previously generated using an OceanAK query. 
# resulting file: King Crab Personal Use Permit Details-2022-2023.csv

# These files must have fields: Product Class Code (must be "KC"), Product Class (must be 
# "Southeast King Crab Personal Use Permit"), Year, Permit Number, First Name, Middle Initial, 
# Last Name, Email Address, Permit Returned Status, Harvest Reported, Did Not Fish, Is Done Fishing, 
# Permit Status ID, Permit Status Description, Location, Personal Use District, Location Code

# Date written: July 2023
# Author: Caitlin Stern
############################################################################################

# load packages
library(odbc)
library(DBI)
library(RODBC)
library(tidyverse)
library(lubridate)

# check that crab database is listed as a data source
odbcDataSources()

# set current year
cur_yr <- 2023
pr_yr <- cur_yr - 1
st_yr <- 2018

cur_yr2 <- 23
pr_yr2 <- 22

# load database password
password <- read_file("./code/crab_survey_password.txt")

# connect to crab personal use database
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "Oracle in OraClient19Home1",
                      DBQ = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com:1521/dfgr1p.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      UID = "i_cat_pu_reporter",
                      PWD = password)

# list of tables
tables <- dbListTables(con)

# find personal use permits for current and previous years (use PRODUCT_CLASS_CODE == "KC" for King Crab)
pu_year <- tbl(con, "CRAB_PERMIT") %>%
  filter(PRODUCT_CLASS_CODE == "KC" & PRODUCT_CLASS == "Southeast King Crab Personal Use Permit") %>%
  select(PERMIT_ID, PRODUCT_CLASS_CODE, PRODUCT_CLASS, YEAR, PERMIT_NO, FIRST_NAME, MIDDLE_INITIAL, LAST_NAME, EMAIL_ADDRESS, 
         PERMIT_RETURNED_STATUS, HARVEST_REPORTED, DID_NOT_FISH, IS_DONE_FISHING, PERMIT_STATUS_ID, PERMIT_STATUS_DESC, 
         PU_AREA_CODE) %>%
  as.data.frame()

# vector of permit IDs to use in filtering effort table
permit_sel <- pu_year %>%
  # get qualifying permit IDs as a vector
  select(PERMIT_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

# each list cannot contain more than 1000 permit IDs so have to split list
permit_sel01 <- permit_sel[1:1000]
permit_sel02 <- permit_sel[1001:2000]
permit_sel03 <- permit_sel[2001:3000]
permit_sel04 <- permit_sel[3001:4000]
permit_sel05 <- permit_sel[4001:5000]
permit_sel06 <- permit_sel[5001:6000]
permit_sel07 <- permit_sel[6001:7000]
permit_sel08 <- permit_sel[7001:8000]
permit_sel09 <- permit_sel[8001:9000]
permit_sel10 <- permit_sel[9001:10000]
permit_sel11 <- permit_sel[10001:11000]
permit_sel12 <- permit_sel[11001:length(permit_sel)]


# filter effort table to include only effort from the relevant trips
effort_tab01 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel01) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab02 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel02) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab03 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel03) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab04 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel04) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab05 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel05) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab06 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel06) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab07 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel07) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab08 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel08) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab09 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel09) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab10 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel10) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab11 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel11) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()
effort_tab12 <- tbl(con, "CRAB_EFFORT") %>%
  filter(PERMIT_ID %in% permit_sel12) %>%
  select(PERMIT_ID, EFFORT_ID, CATCH_DATE, LOCATION_CODE, CODED_LOCATION, DISTRICT, PERMITTEE_PROVIDED_LOCATION) %>%
  as.data.frame()

effort_tab <- rbind(effort_tab01, effort_tab02, effort_tab03, effort_tab04, effort_tab05, effort_tab06, effort_tab07, effort_tab08, effort_tab09, effort_tab10, effort_tab11, effort_tab12)

# vector of effort IDs to use in filtering catch table
effort_sel <- effort_tab %>%
  # get qualifying effort IDs as a vector
  select(EFFORT_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()
length(effort_sel)

# each list cannot contain more than 1000 effort IDs so have to split list
effort_sel01 <- effort_sel[1:1000]
effort_sel02 <- effort_sel[1001:2000]
effort_sel03 <- effort_sel[2001:3000]
effort_sel04 <- effort_sel[3001:4000]
effort_sel05 <- effort_sel[4001:length(effort_sel)]

# filter catch table to include only catch from the relevant trips
catch_tab1 <- tbl(con, "CRAB_CATCH") %>%
  filter(EFFORT_ID %in% effort_sel01) %>%
  select(EFFORT_ID, CATCH_ID, SPECIES_CODE, SPECIES, NUMBER_OF_CRAB) %>%
  as.data.frame()

catch_tab2 <- tbl(con, "CRAB_CATCH") %>%
  filter(EFFORT_ID %in% effort_sel02) %>%
  select(EFFORT_ID, CATCH_ID, SPECIES_CODE, SPECIES, NUMBER_OF_CRAB) %>%
  as.data.frame()

catch_tab3 <- tbl(con, "CRAB_CATCH") %>%
  filter(EFFORT_ID %in% effort_sel03) %>%
  select(EFFORT_ID, CATCH_ID, SPECIES_CODE, SPECIES, NUMBER_OF_CRAB) %>%
  as.data.frame()

catch_tab4 <- tbl(con, "CRAB_CATCH") %>%
  filter(EFFORT_ID %in% effort_sel04) %>%
  select(EFFORT_ID, CATCH_ID, SPECIES_CODE, SPECIES, NUMBER_OF_CRAB) %>%
  as.data.frame()

catch_tab5 <- tbl(con, "CRAB_CATCH") %>%
  filter(EFFORT_ID %in% effort_sel05) %>%
  select(EFFORT_ID, CATCH_ID, SPECIES_CODE, SPECIES, NUMBER_OF_CRAB) %>%
  as.data.frame()

catch_tab <- rbind(catch_tab1, catch_tab2, catch_tab3, catch_tab4, catch_tab5)

# join catch and effort information, such that each effort ID has information about how many crab were caught
effort_catch <- merge(effort_tab, catch_tab, by="EFFORT_ID", all=T) %>%
  # include only non-zero crab catches, and only RKC
  filter(NUMBER_OF_CRAB != 0 & SPECIES_CODE == 921) %>%
  # extract crab year. Crab year should be the catch date year for harvest in July-Dec, but the previous year for Jan-June
  mutate(crab_year = case_when(month(CATCH_DATE) %in% c(7, 8, 9, 10, 11, 12) ~ year(CATCH_DATE),
                               month(CATCH_DATE) %in% c(1, 2, 3, 4, 5, 6) ~ year(CATCH_DATE - years(1)))) %>%
  arrange(crab_year, DISTRICT)

# assign each catch record to a survey area or to "other" (not in a survey area)
# revise this logic as needed; run the following to check for needed revisions: 
# unique(effort_catch$CODED_LOCATION) unique(effort_catch$PERMITTEE_PROVIDED_LOCATION) maybe_jnu <- effort_catch %>% filter(DISTRICT == 111)
effort_catch_ar <- effort_catch %>%
  mutate(survey.area = case_when(
    CODED_LOCATION == "Pybus Bay" ~ "Pybus Bay",
    PERMITTEE_PROVIDED_LOCATION == "Gambier Bay" ~ "Gambier Bay",
    CODED_LOCATION == "Seymour Canal" ~ "Seymour Canal",
    PERMITTEE_PROVIDED_LOCATION == "Seymour Canal" ~ "Seymour Canal",
    CODED_LOCATION == "West Peril Strait" ~ "Peril Strait",
    CODED_LOCATION == "Portland Canal" ~ "Juneau",
    PERMITTEE_PROVIDED_LOCATION %in% c("11-A", "11A", "11-a", "111-50", "Amalga", "Lena", "Outer Point", "Portland island", "South Shelter", "Tee Harbor", "Coglan isle", "Auke Bay", "Portland Island        11A", "South Shelter  - Juneau") ~ "Juneau",
    CODED_LOCATION == "St. James Bay" ~ "Lynn Sisters",
    PERMITTEE_PROVIDED_LOCATION %in% c("St. James Bay", "ST. James Bay", "st. James Bay", "St James Bay", "St James", "115-10", "115.10", "Whidbey point", "East side of point whidbey", "Outside point whidbey", "SE of Pt. Whidbey", "St James bay", "Just outside of St James Bay Point ", "St. James area", "Off point Whidbey in Lynn Canal", "North of Pt. Whidbey", "St. James Line", "Off of Pt. Whidbey", "Just outside the Jt James Bay point", "Pt. Whidbey ", "outside of St. James Point ", "Off of Pt Whidbey", "Pt. Whidbey", "East side of point whidbey") ~ "Lynn Sisters",
    CODED_LOCATION == "Excursion Inlet" ~ "Excursion Inlet",
    PERMITTEE_PROVIDED_LOCATION == "Excursion Inlet" ~ "Excursion Inlet",
    .default = "Other"
  ))

# calculate catch mid-date for use in CSA model

# Lynn Sisters in 2022
LSdates1_22 <- effort_catch_ar %>%
  filter(survey.area == "Lynn Sisters" & crab_year == 2022)
LSdates2_22 <- unique(round_date(ymd(LSdates1_22$CATCH_DATE), unit="day"))
LSdate22.int <- interval(min(LSdates2_22, na.rm=TRUE), max(LSdates2_22, na.rm=TRUE))
# survey midpoint; see functions script for the int_midpoint function
LS.catch.midpoint.22 <- int_midpoint(LSdate22.int)

LSdates1_21 <- effort_catch_ar %>%
  filter(survey.area == "Lynn Sisters" & crab_year == 2021)
LSdates2_21 <- unique(round_date(ymd(LSdates1_21$CATCH_DATE), unit="day"))
LSdate21.int <- interval(min(LSdates2_21, na.rm=TRUE), max(LSdates2_21, na.rm=TRUE))
# survey midpoint; see functions script for the int_midpoint function
LS.catch.midpoint.21 <- int_midpoint(LSdate21.int)

LSdates1_20 <- effort_catch_ar %>%
  filter(survey.area == "Lynn Sisters" & crab_year == 2020)
LSdates2_20 <- unique(round_date(ymd(LSdates1_20$CATCH_DATE), unit="day"))
LSdate20.int <- interval(min(LSdates2_20, na.rm=TRUE), max(LSdates2_20, na.rm=TRUE))
# survey midpoint; see functions script for the int_midpoint function
LS.catch.midpoint.20 <- int_midpoint(LSdate20.int)

LSdates1_19 <- effort_catch_ar %>%
  filter(survey.area == "Lynn Sisters" & crab_year == 2019)
LSdates2_19 <- unique(round_date(ymd(LSdates1_19$CATCH_DATE), unit="day"))
LSdate19.int <- interval(min(LSdates2_19, na.rm=TRUE), max(LSdates2_19, na.rm=TRUE))
# survey midpoint; see functions script for the int_midpoint function
LS.catch.midpoint.19 <- int_midpoint(LSdate19.int)

LSdates1_18 <- effort_catch_ar %>%
  filter(survey.area == "Lynn Sisters" & crab_year == 2018)
LSdates2_18 <- unique(round_date(ymd(LSdates1_18$CATCH_DATE), unit="day"))
LSdate18.int <- interval(min(LSdates2_18, na.rm=TRUE), max(LSdates2_18, na.rm=TRUE))
# survey midpoint; see functions script for the int_midpoint function
LS.catch.midpoint.18 <- int_midpoint(LSdate18.int)

# calculate total number of RKC caught in location in each survey area in each year
pu_harvest_reg <- effort_catch_ar %>%
  group_by(crab_year, survey.area) %>%
  summarise(total_crab = sum(NUMBER_OF_CRAB)) %>%
  # keep only crab years > 2017
  filter(crab_year > 2017)

# export regional PU info
write.csv(pu_harvest_reg, paste0('./data/harvest/', cur_yr, '_regional_PU_harvest.csv'), row.names = FALSE) 

# *******************************************************************************************************************
# information about number of permits fished

# number of permits per year

pu_year_status <- pu_year %>%
  mutate(permit_status = case_when(
    IS_DONE_FISHING == 1 & HARVEST_REPORTED == 1 ~ "returned, harvest reported",
    IS_DONE_FISHING == 0 & HARVEST_REPORTED == 1 ~ "not returned, harvest reported",
    IS_DONE_FISHING == 1 & HARVEST_REPORTED == 0 ~ "returned, no harvest reported",
    IS_DONE_FISHING == 1 & DID_NOT_FISH == 1 ~ "returned, did not fish",
    IS_DONE_FISHING == 0 & HARVEST_REPORTED == 0 ~ "not returned, no harvest reported"
  ))

pu_year_sum <- pu_year_status %>%
  group_by(YEAR, permit_status) %>%
  summarise(count = n_distinct(PERMIT_ID))

# join permit and catch location information, such that permits with multiple effort IDs have multiple rows
pu_loc <- merge(pu_year, effort_tab, by="PERMIT_ID", all=T)  

# format data set for export

pu_data <- pu_loc %>%
  mutate(Year = YEAR, Permit.Number = PERMIT_NO, Permit.Returned.Status = PERMIT_RETURNED_STATUS, 
         Harvest.Reported = HARVEST_REPORTED, Did.Not.Fish = DID_NOT_FISH, Is.Done.Fishing = IS_DONE_FISHING, 
         Location = CODED_LOCATION, Location.Code = LOCATION_CODE)

