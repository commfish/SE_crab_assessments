# *****************************************************************************************************************************
# Script name: pull_data_for_csa.R
# Script goal: create data files to use in the CSA model using a direct database connection;
# these files were previously generated using an OceanAK query. 
# For example, "RKC survey CSA_survey.location_22_23.csv"

# RKC
# These files must have fields Year, Project Code (=7), Trip No, Location Code, Location, Pot No,
# Time Set, Time Hauled, Pot Condition Code, Pot Condition, Density Strata Code,
# Density Strata, Specimen No, Number of Specimens, Recruit Status, Species,
# Species Code (=921), Sex Code, Length Millimeters, Legal Size Code, Shell 
# Condition Code, Egg Percent, Egg Development Code, Egg Condition Code


# Tanner
# These files must have fields Year, Project Code (=7 for data collected on the summer crab survey), 
# Trip No, Location Code, Location, Pot No, Depth Fathoms, Time Set, Time Hauled, Pot Condition, 
# Density Strata Code, Density Strata, Specimen No, Number of Specimens, Recruit Status, Species Code (=931), 
# Sex Code, Width Millimeters, Shell Condition Code, Egg Percent, Egg Development Code, Egg Condition Code, Pot Comment

# Date written: July 2023
# Author: Caitlin Stern
# *****************************************************************************************************************************

# load packages
library(odbc)
library(DBI)
library(RODBC)
library(tidyverse)

# set current year
cur_yr <- 2023
pr_yr <- cur_yr - 1

cur_yr2 <- 23
pr_yr2 <- 22

# *****************************************************************************************************************************
# establish database connection
# *****************************************************************************************************************************

# check that crab survey database is listed as a data source
odbcDataSources()

# load database password
password <- read_file("./code/crab_survey_password.txt")

# connect to crab survey database
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "Oracle in OraClient19Home1",
                      DBQ = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com:1521/dfgr1p.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      UID = "i_sur_crab_reporter",
                      PWD = password)

# list of tables
tables <- dbListTables(con)

# this is how you import the data if you need the entire table from the database in R's memory. I recommend skipping this step and 
# filtering the data before bringing it into R's memory because loading these tables takes a long time. Using the 
# tbl(con, "EXAMPLE") method lets you run commands on the "EXAMPLE" table in the database without bringing it into R's memory.
# trip <- dbReadTable(con, "TRIP")
# effort <- dbReadTable(con, "EFFORT")
# catch <- dbReadTable(con, "CATCH") [this is information on bycatch; prob not necessary]
# specimen <- dbReadTable(con, "SPECIMEN") # this is very slow to add to R's memory because it is a large table


# *****************************************************************************************************************************
# For pulling red king crab data
# *****************************************************************************************************************************

# set location to filter data
# should be one of c("Barlow Cove", "Juneau"), "Excursion Inlet", "Lynn Sisters", "Gambier Bay", "Pybus Bay", "Peril Strait", 
# "Seymour Canal"
sur.location <- "Gambier Bay"

# set location to export files
# should be one of "Excursion", "Gambier", "Juneau", "LynnSisters", "Peril", "Pybus", "Seymour"
survey.location <- "Gambier"

# find trip ID for current year (use PROJECT_CODE = 007 which should correspond to PROJECT = "Red King Crab Survey")
trip_year <- tbl(con, "TRIP") %>%
  filter(PROJECT_CODE == "007" & YEAR %in% c(pr_yr, cur_yr)) %>%
  # fields needed from trip table: TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO) %>%
  as.data.frame()

# vector of trip IDs to use in filtering effort table
trip_sel <- trip_year %>%
  # get qualifying trip IDs as a vector
  select(TRIP_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

# filter effort table to include only effort from the relevant trips
effort_tab <- tbl(con, "EFFORT") %>%
  filter(TRIP_ID %in% trip_sel) %>%
  as.data.frame()

# join trip with effort table on TRIP_ID and filter for survey location
# fields needed from effort table: EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE,
# POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA
effort_year <- trip_year %>%
  inner_join(effort_tab, by = "TRIP_ID") %>%
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA) %>%
  filter(LOCATION == sur.location)

# vector of effort IDs to use in filtering specimen table
effort_sel <- effort_year %>%
  # get qualifying effort IDs as a vector
  select(EFFORT_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

# select only RKC specimens with wanted effort IDs
spec_effort <- tbl(con, "SPECIMEN") %>%
  filter(SPECIES_CODE == 921 & EFFORT_ID %in% effort_sel) %>%
  as.data.frame()

# join effort with specimens on EFFORT_ID
# fields needed from specimen table: SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES, SPECIES_CODE, SEX_CODE, 
# LENGTH_MILLIMETERS, LEGAL_SIZE_CODE, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE
spec_year <- merge(effort_year, spec_effort, by="EFFORT_ID", all=T) %>%
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES, SPECIES_CODE, SEX_CODE, LENGTH_MILLIMETERS, LEGAL_SIZE_CODE, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE)

# format data set (so it will work with existing data processing script)
data_for_csa <- spec_year %>%
  # rename columns and change column types
  mutate(Year = as.integer(YEAR), Project.Code = as.integer(PROJECT_CODE), Trip.No = as.integer(TRIP_NO), Location.Code = as.integer(LOCATION_CODE), Location = LOCATION, Pot.No = as.integer(POT_NO), Time.Set = TIME_SET, Time.Hauled = TIME_HAULED, Pot.Condition.Code = as.integer(POT_CONDITION_CODE), Pot.Condition = POT_CONDITION, Density.Strata.Code = as.integer(DENSITY_STRATA_CODE), Density.Strata = DENSITY_STRATA, Specimen.No = as.integer(SPECIMEN_NO), Number.Of.Specimens = as.integer(SUBSAMPLE_RATE), Recruit.Status = RECRUIT_STATUS, Species = SPECIES, Species.Code = as.integer(SPECIES_CODE), Sex.Code = as.integer(SEX_CODE), Length.Millimeters = as.integer(LENGTH_MILLIMETERS), Legal.Size.Code = as.integer(LEGAL_SIZE_CODE), Shell.Condition.Code = as.integer(SHELL_CONDITION_CODE), Egg.Percent = as.integer(EGG_PERCENT), Egg.Development.Code = as.integer(EGG_DEVELOPMENT_CODE), Egg.Condition.Code = as.integer(EGG_CONDITION_CODE)) %>%
  # get rid of old columns
  select(-c(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES, SPECIES_CODE, SEX_CODE, LENGTH_MILLIMETERS, LEGAL_SIZE_CODE, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE)) %>%
  arrange(Year, Pot.No, Specimen.No) %>%
  # replace NA in Number.Of.Specimens column with 0
  mutate_at(vars(Number.Of.Specimens), ~replace(., is.na(.), as.integer(0))) %>%
  # replace NA in Species.Code column with 921
  mutate_at(vars(Species.Code), ~replace(., is.na(.), as.integer(921))) %>%
  # replace NAs in Species column with "Red king crab"
  mutate_at(vars(Species), ~replace(., is.na(.), "Red king crab")) %>%
  # replace NAs in Recruit.Status column with ""
  mutate_at(vars(Recruit.Status), ~replace(., is.na(.), "")) %>%
  # replace 0 in Egg.Condition.Code with NA
  mutate_at(vars(Egg.Condition.Code), ~replace(., . == 0, NA))

# export csv to use in CSA model
write.csv(data_for_csa, paste0('./data/rkc/', survey.location, '/RKC_survey_CSA_', survey.location, '_', pr_yr2, '_', cur_yr2, '.csv'), row.names = FALSE) 


# *****************************************************************************************************************************
# For pulling Tanner crab data collected on the summer crab survey
# *****************************************************************************************************************************

# find trip ID for current year (use PROJECT_CODE = 007 which should correspond to PROJECT = "Red King Crab Survey")
trip_year <- tbl(con, "TRIP") %>%
  filter(PROJECT_CODE == "007" & YEAR > 2013) %>%
  # fields needed from trip table: TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO) %>%
  as.data.frame()

# vector of trip IDs to use in filtering effort table
trip_sel <- trip_year %>%
  # get qualifying trip IDs as a vector
  select(TRIP_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

# filter effort table to include only effort from the relevant trips
effort_tab <- tbl(con, "EFFORT") %>%
  filter(TRIP_ID %in% trip_sel) %>%
  as.data.frame()

# join trip with effort table on TRIP_ID
# fields needed from effort table: EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE,
# POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, COMMENTS
effort_year <- trip_year %>%
  inner_join(effort_tab, by = "TRIP_ID") %>%
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION_CODE, LOCATION, POT_NO, DEPTH_FATHOMS, TIME_SET, TIME_HAULED, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, COMMENTS)

# vector of effort IDs to use in filtering specimen table
effort_sel <- effort_year %>%
  # get qualifying effort IDs as a vector
  select(EFFORT_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

# each list cannot contain more than 1000 effort IDs so have to split list
effort_sel01 <- effort_sel[1:1000]
effort_sel02 <- effort_sel[1001:2000]
effort_sel03 <- effort_sel[2001:3000]
effort_sel04 <- effort_sel[3001:4000]
effort_sel05 <- effort_sel[4001:5000]
effort_sel06 <- effort_sel[5001:length(effort_sel)]

# select only Tanner specimens with wanted effort IDs
spec_effort01 <- tbl(con, "SPECIMEN") %>%
  filter(SPECIES_CODE == 931 & EFFORT_ID %in% effort_sel01) %>%
  as.data.frame()

spec_effort02 <- tbl(con, "SPECIMEN") %>%
  filter(SPECIES_CODE == 931 & EFFORT_ID %in% effort_sel02) %>%
  as.data.frame()

spec_effort03 <- tbl(con, "SPECIMEN") %>%
  filter(SPECIES_CODE == 931 & EFFORT_ID %in% effort_sel03) %>%
  as.data.frame()

spec_effort04 <- tbl(con, "SPECIMEN") %>%
  filter(SPECIES_CODE == 931 & EFFORT_ID %in% effort_sel04) %>%
  as.data.frame()

spec_effort05 <- tbl(con, "SPECIMEN") %>%
  filter(SPECIES_CODE == 931 & EFFORT_ID %in% effort_sel05) %>%
  as.data.frame()

spec_effort06 <- tbl(con, "SPECIMEN") %>%
  filter(SPECIES_CODE == 931 & EFFORT_ID %in% effort_sel06) %>%
  as.data.frame()

spec_effort <- rbind(spec_effort01, spec_effort02, spec_effort03, spec_effort04, spec_effort05, spec_effort06)

# join effort with specimens on EFFORT_ID
# fields needed from specimen table: SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES_CODE, SEX_CODE, 
# WIDTH_MILLIMETERS, LEGAL_SIZE_CODE, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE, COMMENTS
spec_year <- merge(effort_year, spec_effort, by="EFFORT_ID", all=T) %>%
  rename(COMMENTS = COMMENTS.x) %>%
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION_CODE, LOCATION, POT_NO, DEPTH_FATHOMS, TIME_SET, TIME_HAULED, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES_CODE, SEX_CODE, WIDTH_MILLIMETERS, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE, COMMENTS)

# Fields needed: Year, Project Code (=7 for data collected on the summer crab survey), 
# Trip No, Location Code, Location, Pot No, Depth Fathoms, Time Set, Time Hauled, Pot Condition, 
# Density Strata Code, Density Strata, Specimen No, Number of Specimens, Recruit Status, Species Code (=931), 
# Sex Code, Width Millimeters, Shell Condition Code, Egg Percent, Egg Development Code, Egg Condition Code, Pot Comment

# format data set (so it will work with existing data processing script)
data_for_csa <- spec_year %>%
  # rename columns and change column types
  mutate(Year = as.integer(YEAR), `Project Code` = as.integer(PROJECT_CODE), `Trip No` = as.integer(TRIP_NO), `Location Code` = as.integer(LOCATION_CODE), Location = LOCATION, `Pot No` = as.integer(POT_NO), `Depth Fathoms` = as.integer(DEPTH_FATHOMS), `Time Set` = TIME_SET, `Time Hauled` = TIME_HAULED, `Pot Condition` = POT_CONDITION, `Density Strata Code` = as.integer(DENSITY_STRATA_CODE), `Density Strata` = DENSITY_STRATA, `Specimen No` = as.integer(SPECIMEN_NO), `Number Of Specimens` = as.integer(SUBSAMPLE_RATE), `Recruit Status` = RECRUIT_STATUS, `Species Code` = as.integer(SPECIES_CODE), `Sex Code` = as.integer(SEX_CODE), `Width Millimeters` = as.integer(WIDTH_MILLIMETERS), `Shell Condition Code` = as.integer(SHELL_CONDITION_CODE), `Egg Percent` = as.integer(EGG_PERCENT), `Egg Development Code` = as.integer(EGG_DEVELOPMENT_CODE), `Egg Condition Code` = as.integer(EGG_CONDITION_CODE), `Pot Comment` = COMMENTS) %>%
  # get rid of old columns
  select(-c(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, DEPTH_FATHOMS, TIME_SET, TIME_HAULED, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES_CODE, SEX_CODE, WIDTH_MILLIMETERS, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE, COMMENTS)) %>%
  arrange(Year, `Pot No`, `Specimen No`) %>%
  # replace NA in Number.Of.Specimens column with 0
  mutate_at(vars(`Number Of Specimens`), ~replace(., is.na(.), as.integer(0))) %>%
  # replace NAs in Recruit.Status column with ""
  mutate_at(vars(`Recruit Status`), ~replace(., is.na(.), "")) %>%
  # replace 0 in Egg.Condition.Code with NA
  mutate_at(vars(`Egg Condition Code`), ~replace(., . == 0, NA))

error_check <- data_for_csa %>%
  filter(`Recruit Status` == "")

# export csv to use in CSA model
write.csv(data_for_csa, paste0('./data/tanner/tanner_rkc/red crab survey for Tanner crab CSA_14_', cur_yr, '.csv'), row.names = FALSE) 



# *****************************************************************************************************************************
# For pulling Tanner crab data collected on the fall crab survey
# *****************************************************************************************************************************
