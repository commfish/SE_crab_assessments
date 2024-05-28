# ******************************************************************************
# Run the red king crab Juneau area GMACS model; produce figures and tables
# Date: May 2024
# Author: Caitlin Stern
# ******************************************************************************

# Instructions
# 1. To run the GMACS model, you need the following files in the same folder:
# gmacs.exe, gmacs.dat, jnuYR.ctl, jnuYR.dat, jnuYR.prj, where YR is the final two
# digits of the year, i.e. 24 for 2024.
# 2. These files must all be checked and updated before you run any models.
# gmacs.exe - use the GMACS version ADF&G is currently using. 
# gmacs.dat - update with the correct file names for .ctl, .dat, .prj files
# jnuYR.ctl - update the end years
# jnuYR.dat - update with the most recently available data
# jnuYR.prj - update years
# 3. Run the GMACS model using the script below. The script will step you through
# bringing in the model results and generating the figures and tables you need for
# the memo.
# 4. Open the RMarkdown file that creates the Juneau memo. Update the years. 

# load packages
library(tidyverse)
source(paste0(here::here(), "/gmacsr/gmacsr.R"))

# define variables
cur_yr <- 2023
pre_yr <- 2022


# ******************************************************************************
# run the model
# ******************************************************************************

# run model with iterative reweighting
gmacs_do_exe(gmacs.dat = "./GMACS models/2024/Juneau/jnu23/gmacs.dat", reweight = T, level = 0.1, max_iter = 5, reweight_only = T)

# run model without iterative reweighting
gmacs_do_exe(gmacs.dat = "./GMACS models/2024/Juneau/jnu23/gmacs.dat", reweight = F, wait = T)

# ******************************************************************************
# bring in the model output
# ******************************************************************************

# get gmacsall.out
out.23 <- gmacs_read_allout(file = "./GMACS models/2024/Juneau/jnu23/Gmacsall.out", model_name = "23.0")

# get index summary
indexsum.23 <- gmacs_get_index_summary(all_out = list(out.23))

# get reference points table
reftab.23 <- gmacs_get_ref_points(all_out = list(out.23))

# get likelihood table
liktab.23 <- gmacs_get_lik(all_out = list(out.23))

# get derived quantities summary
dquant.23 <- gmacs_get_derived_quantity_summary(all_out = list(out.23))

# get .std file
std.23 <- gmacs_read_std(file = "./GMACS models/2024/Juneau/jnu23/gmacs.std", model_name = "23.0")

# ******************************************************************************
# produce plots
# ******************************************************************************

# plot index


# plot catch


# plot MMB trajectory


# plot recruitment


# plot size comps


# ******************************************************************************
# run retrospectives
# ******************************************************************************




# ******************************************************************************
# run jitters
# ******************************************************************************