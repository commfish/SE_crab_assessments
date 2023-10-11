library(odbc)
library(DBI)
library(RODBC)
library(tidyverse)
#library(RMySQL)


odbcDataSources()



#con <- DBI::dbConnect(odbc::odbc(),
                      #Driver = "SE_crab_data",
                      #Host   = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      #SVC    = "dfgr1p.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      #UID    = "i_sur_crab_reporter",
                      #PWD    = "KrabbyPatties",
                      #Port   = 1521)

#con <- DBI::dbConnect(odbc::odbc(),
                      #Driver = "Oracle in OraClient19Home1",
                      #Host   = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      #SVC    = "dfgr1p.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      #UID    = "i_sur_crab_reporter",
                      #PWD    = "KrabbyPatties",
                      #Port   = 1521)

#con <- DBI::dbConnect(odbc::odbc(),
                      #Driver = "Oracle in OraClient19Home1",
                      #Host   = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      #SVC    = "XE",
                      #UID    = "i_sur_crab_reporter",
                      #PWD    = "KrabbyPatties",
                      #Port   = 1521)


#con <- DBI::dbConnect(odbc::odbc(), 
                    #Driver= "Oracle in OraClient19Home1",
                    #DBQ = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com:1521/dfgr1p.us1.ocm.s7134325.oraclecloudatcustomer.com",
                    #UID="i_sur_crab_reporter",
                    #PWD="KrabbyPatties")

#con <- dbConnect(odbc::odbc(), .connection_string = "Driver={Oracle in OraClient19Home1};Dbq=XE;Uid=i_sur_crab_reporter;Pwd=KrabbyPatties;")


# Connect to the MySQL database: con
#con <- dbConnect(RMySQL::MySQL(), 
                 #dbname = "SE_crab_data", 
                 #host = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com", 
                 #port = 1521,
                 #user = "i_sur_crab_reporter",
                 #password = "KrabbyPatties")



# this one works!!!

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver= "Oracle in OraClient19Home1",
                      DBQ = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com:1521/dfgr1p.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      UID="i_sur_crab_reporter",
                      PWD="KrabbyPatties")

# list of tables
tables <- dbListTables(con)

# import data
trip <- dbReadTable(con, "TRIP")
effort <- dbReadTable(con, "EFFORT")
# catch <- dbReadTable(con, "CATCH") [this is information on bycatch; prob not necessary]
specimen <- dbReadTable(con, "SPECIMEN")
#specimen <- tbl(con, "SPECIMEN")

# import data via a query
my_table <- dbGetQuery(con, "SELECT country, count(*) as tims FROM accounts WHERE region = 'EU'")
my_table


# work with data directly using dbplyr (included in tidyverse)
head(tbl(con, "SPECIMEN"))

##########################################################################################
# find trip ID(s) for current year and location
##########################################################################################

cur_yr <- 2023
pr_yr <- cur_yr - 1

# specify location of interest
unique(effort$LOCATION)
survey.location <- c("Barlow Cove", "Juneau")

# find trip ID for current year
unique(trip$PROJECT)

trip_year <- filter(trip, YEAR == cur_yr & PROJECT == "Red King Crab Survey") %>%
  # get qualifying trip IDs as a vector
  select(TRIP_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

############################################################################################
# match effort IDs to current year's trip(s) 
############################################################################################

# pot conditions
unique(effort$POT_CONDITION)
pot_conditions <- c("Normal", "Not observed")

# qualifying effort IDs
effort_curyr <- filter(effort, TRIP_ID %in% trip_year) %>%
  # select only location(s) of interest
  filter(LOCATION %in% survey.location) %>%
  # select only pots with POT_CONDITION = normal or not observed
  filter(POT_CONDITION %in% pot_conditions) %>%
  # calculate soak time
  mutate(soak_time = TIME_HAULED - TIME_SET)

# data quality check: soak time must be between 18 and 24 hrs
qc_soak_time <- effort_curyr %>%
  filter(soak_time > 24.1 | soak_time < 17.9)

# vector of effort IDs to match to specimens
effort_trip <- effort_curyr %>%
  # get qualifying effort IDs as a vector
  select(EFFORT_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

############################################################################################
# match effort IDs to specimens
############################################################################################

spec_effort <- filter(specimen, EFFORT_ID %in% effort_trip)
  #filter(tbl(con, "SPECIMEN"), EFFORT_ID %in% effort_trip)

# only red king crab specimens
rkc <- spec_effort %>%
  filter(SPECIES_CODE == 921) %>%
  # number of specimens (= subsample rate)
  mutate(Number.of.specimens = SUBSAMPLE_RATE)



# calculate number of crab of each category (juvenile, pre-recruit, recruit) for each pot (effort ID)


############################################################################################
# create RKC survey CSA_survey.location_22_23.csv
# with fields Year, Project Code (=7), Trip No, Location Code, Location, Pot No,
# Time Set, Time Hauled, Pot Condition Code, Pot Condition, Density Strata Code,
# Density Strata, Specimen No, Number of Specimens, Recruit Status, Species,
# Species Code (=921), Sex Code, Length Millimeters, Legal Size Code, Shell 
# Condition Code, Egg Percent, Egg Development Code, Egg Condition Code
############################################################################################

# load packages
library(odbc)
library(DBI)
library(RODBC)
library(tidyverse)

# check that crab survey database is listed as a data source
odbcDataSources()

# set current year
cur_yr <- 2023
pr_yr <- cur_yr - 1

cur_yr2 <- 23
pr_yr2 <- 22

# set location to filter table
# should be one of c("Barlow Cove", "Juneau"), "Excursion Inlet", "Lynn Sisters", "Gambier Bay", "Pybus Bay", "Peril Strait", 
# "Seymour Canal"
unique(effort$LOCATION)
sur.location <- "Excursion Inlet"

# set location to export files
# should be one of "Excursion", "Gambier", "Juneau", "LynnSisters", "Peril", "Pybus", "Seymour"
survey.location <- "Excursion"

# connect to crab survey database
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver= "Oracle in OraClient19Home1",
                      DBQ = "soaora7-scan.us1.ocm.s7134325.oraclecloudatcustomer.com:1521/dfgr1p.us1.ocm.s7134325.oraclecloudatcustomer.com",
                      UID="i_sur_crab_reporter",
                      PWD="KrabbyPatties")

# list of tables
tables <- dbListTables(con)

# import data
trip <- dbReadTable(con, "TRIP")
effort <- dbReadTable(con, "EFFORT")
# catch <- dbReadTable(con, "CATCH") [this is information on bycatch; prob not necessary]
specimen <- dbReadTable(con, "SPECIMEN") # this is very slow

# find trip ID(s) for current year and location

# find trip ID for current year (use PROJECT_CODE = 007 which should correspond to PROJECT = "Red King Crab Survey")

trip_year <- filter(trip, PROJECT_CODE == "007" & YEAR %in% c(pr_yr, cur_yr)) %>%
  # fields needed from trip table: TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO)

# right join with effort table on TRIP_ID
# fields needed from effort table: EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE,
# POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA

effort_year <- trip_year %>%
  inner_join(effort, by = "TRIP_ID") %>%
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA) %>%
  filter(LOCATION == sur.location)

# vector of effort IDs to match to specimens
effort_sel <- effort_year %>%
  # get qualifying effort IDs as a vector
  select(EFFORT_ID) %>%
  as.vector() %>%
  unname() %>%
  unlist()

# select only RKC specimens with wanted effort IDs
spec_effort <- filter(specimen, SPECIES_CODE == 921 & EFFORT_ID %in% effort_sel)

# join with specimens on EFFORT_ID
# fields needed from specimen table: SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES, SPECIES_CODE, SEX_CODE, 
# LENGTH_MILLIMETERS, LEGAL_SIZE_CODE, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE

spec_year <- merge(effort_year, spec_effort, by="EFFORT_ID", all=T) %>%
  select(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES, SPECIES_CODE, SEX_CODE, LENGTH_MILLIMETERS, LEGAL_SIZE_CODE, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE)

# format data set (so it will work with existing data processing script)
data_for_csa <- spec_year %>%
  mutate(Year = YEAR, Project.Code = PROJECT_CODE, Trip.No = TRIP_NO, Location.Code = LOCATION_CODE, Location = LOCATION, Pot.No = POT_NO, Time.Set = TIME_SET, Time.Hauled = TIME_HAULED, Pot.Condition.Code = POT_CONDITION_CODE, Pot.Condition = POT_CONDITION, Density.Strata.Code = DENSITY_STRATA_CODE, Density.Strata = DENSITY_STRATA, Specimen.No = SPECIMEN_NO, Number.Of.Specimens = SUBSAMPLE_RATE, Recruit.Status = RECRUIT_STATUS, Species = SPECIES, Species.Code = SPECIES_CODE, Sex.Code = SEX_CODE, Length.Millimeters = LENGTH_MILLIMETERS, Legal.Size.Code = LEGAL_SIZE_CODE, Shell.Condition.Code = SHELL_CONDITION_CODE, Egg.Percent = EGG_PERCENT, Egg.Development.Code = EGG_DEVELOPMENT_CODE, Egg.Condition.Code = EGG_CONDITION_CODE) %>%
  select(-c(TRIP_ID, YEAR, PROJECT_CODE, TRIP_NO, EFFORT_ID, LOCATION, LOCATION_CODE, POT_NO, TIME_SET, TIME_HAULED, POT_CONDITION_CODE, POT_CONDITION, DENSITY_STRATA_CODE, DENSITY_STRATA, SPECIMEN_NO, SUBSAMPLE_RATE, RECRUIT_STATUS, SPECIES, SPECIES_CODE, SEX_CODE, LENGTH_MILLIMETERS, LEGAL_SIZE_CODE, SHELL_CONDITION_CODE, EGG_PERCENT, EGG_DEVELOPMENT_CODE, EGG_CONDITION_CODE)) 

# export csv to use in CSA model

write.csv(data_for_csa, paste0('./data/rkc/', survey.location, '/', 
                                 cur_yr, '/RKC_survey_CSA_', survey.location, '_', prv_yr2, '_', cur_yr2, '.csv'), row.names = FALSE) 
