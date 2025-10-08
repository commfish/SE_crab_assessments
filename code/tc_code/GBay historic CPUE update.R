###GBAY historic CPUE update
# Alex Reich alex.reich@alaska.gov
# this should be one once (in 2025) and ideally will never need to be run again

## in 2024 the survey crew reduced the size of Glacier bay
##and sent me info of the pots from past years set outside this survey area
## as a result, I re- ran the CPUE for 2013-2025
## BUT I still need to update the historical files(1999-2013) for GBAY.
## THIS IS THAT UPDATE - October 2025


#I manipulated the read-in code to get the data before 2013. Here it is
old_data <- read.csv("data/tanner/tanner_tcs/tanner crab survey for CSA_OLD.csv")

#just interested in gbay
Gbay_old_data <- old_data %>% filter(Location == "Glacier Bay")

#read in the matchy matchy pots I dont want in GBAY data
GB_pots_outside <- read.csv("data/glacier bay 2024 restrat/Glacier Bay pots in old strata.csv")

#get my new historical dataset
Old_pots_gone <- Gbay_old_data %>%
  anti_join(
    GB_pots_outside,
    by = c("Location", "Year", "Pot.No")
  )

#now I need to re-run the CPUE standardization for these old years:
