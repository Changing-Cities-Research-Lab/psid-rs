# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 7/11/2022             
# Last revised:             
# Project: PSID-Racial Stratification         
# Subproject: Identify MSA-level Income Quartile Cutoffs by-year 
# Re: Compare income quartile cutoffs using income category by household-counts to data generated from tract-level median household income data from the Census/ACS in Interpolate-ACS.R
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script cleans and processes tract-level ACS data downloaded from SE in order to generate the income-by-household distribution. 
# First, I adjust the values for inflation (2019 Jan 1 dollars) and find the midpoint of each income category. 
# Second, I aggregate up to the metdiv, to get the total # of households in each income category, but also the metdiv-level aggregate # of households. 
# Third, I get the metdiv-specific income cutoffs for each year 
# Fourth, make all of these into a wide-form longitudinal panel (like the LTDB)

# Inputs:
# Data files to clean: @@Andrew
# incomehh_1990
# incomehh_2000
# incomehh_0812 (use as 2010) 
# incomehh_0913 (use as 2011)
# incomehh_1014 (use as 2012)
# incomehh_1115 (use as 2013)
# incomehh_1216 (use as 2014)
# incomehh_1317 (use as 2015)
# incomehh_1418 (use as 2016)
# incomehh_1519 (use as 2017) 
# vector of cpi data to adjust for inflation
# cbsa_stcou_2013_cw.csv

# Outputs:
#  income_hh_metdiv.csv

# Updates log: 

# Setup -------------------------------------------------------------------

# Packages: 
library("tidyverse") 
library("readr") 

# I like using the %<>% from magrittr; `x %<>% mutate` is equivalent to `x <- x %>% mutate` 
library("magrittr") 

# Directories: 
homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
workdir <- paste0("/../misc_data/")
setwd(paste0(homedir, workdir))

# Import data: 
# when data are downloaded from the decennial Census, the inflation year is set at Census year - 1 (January)
# when they are downloaded from the ACS, the inflation year is set at the last year (January) 
# Hence, we do not need to adjust the 2015-2019 ACS data, since it's already adjusted for 2019$, even though we are using it as 2017.

# make a vector of all the cpi values, and label them so we can refer to them easily by year.
cpi <- c(2.08, 1.53, 1.11, 1.09, 1.08, 1.08, 1.06, 1.04, 1.02)
names(cpi) <- c("1990", "2000", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
names(cpi)

# the crosswalk data is needed to match county data to metropolitan divisions (kind of like a city/region)
# str_pad is a way to format our variables. Here I want to make sure it has 5 digits, so if it is not 5 digits, "pad" the string with a leading 0 starting on the left 
# ifelse function like ifelse(condition, value_if_true, value_if_false) 
# here I'm saying that if the variable msamd is missing (NA), assign it the cbsa value, otherwise keep it as is
cw <- read_csv("cbsa_stcou_2013_cw.csv") %>%
  mutate(stcou = state*1000 + county,
         stcou = str_pad(stcou, 5, "left", "0"), 
         metdiv = ifelse(is.na(msamd), cbsa, msamd)) %>%
  select(stcou, metdiv)

# Parameters:
# NA

# Main Script -------------------------------------------------------------
# get metdiv IDs for tract-level data @@Andrew you just have to copy and paste the below for each year 

# read in the data: x <- read_csv("data")
msa_years <- list()
years <- c("1990", "2000", "0812", "0913", "1014", "1115", "1216", "1317", "1418", "1519")

for (i in 1:length(years)) {
  msa_years[[i]] <- read_csv(paste0("cleaned/metdiv", years[[i]], ".csv"))
}

metdivs <- rbindlist(msa_years)
table(metdivs$Year)
rm(msa_years)

write_csv(metdivs, "cleaned/metdiv_9019.csv")
# expand the series to do interpolation ####
metdivs %<>%
  complete(., metdiv, nesting(Year = 1990:2017))

metdivs %<>%
  group_by(metdiv) %>%
  arrange(Year) %>%
  mutate_at(vars(inc_1, inc_2, inc_3, inc_4), ~na.approx(., na.rm = F)) %>% 
  ungroup()

write_csv(metdivs, "cleaned/metdiv_9019_filled.csv")  
