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
income1990 <- read_csv("incomehh/incomehh_1990.csv") 

# get the 5-digit unique county identifier and find their metdivs by merging with the crosswalk cw file
# %<>% means "to this data, do the following:" 
# mutate is how you create or modify a new variable. We're making a new variable called stcou 
# left_join is how we merge two datasets; by = the variable in common across two datasets. left_join retains all observations from the left. 
income1990 %<>%
  mutate(stcou = paste0(Geo_STATE, Geo_COUNTY)) %>%
  left_join(cw, by = "stcou")

# rename the variables. Normally, it's not allowed to name a variable starting with numbers (or to have a mathematical operation sign in the name, like -+/*), but here we do this for other reasons. We put those variable names in `` to tell R that this disallowed naming convention is intentional 
# rename follows logic new name = old name
# look at the codebook to find out which income category corresponds to which. These may not be the same across years, so you may have to add more. 
# We pick the mid-point for each category, so less than 10,000 (0 to 10,000) is assigned 5000, 10,000 to 15,000 is assigned 12,500. I top-code the more than 200,000 category to be 225,000 since the midpoints seem to increase in increments of multiples of 2250's 
# if ever you get lost for what the midpoint is, try this: value_1 + (value_2 rounded up - value_1)/2, for example for the 5000 to 9999 category 
5000 + (10000 - 5000)/2
# look at the codebook named the same as the dataset (.txt) to find out the categories. Notice that even though these are not named like this in the codebook, the variable names all have the SE_ prefix.
# I would literally copy and paste the codebook and manually edit them into this format:
income1990 %<>%
  rename(n_hh = SE_T041_001,
         `2500` = SE_T041_002,
         `7500` = SE_T041_003,
         `11250` = SE_T041_004,
         `13750` = SE_T041_005,
         `16250` = SE_T041_006,
         `18750` = SE_T041_007,
         `21250` = SE_T041_008,
         `23750` = SE_T041_009, 
         `26250` = SE_T041_010, 
         `28750` = SE_T041_011, 
         `31250` = SE_T041_012,
         `33750` = SE_T041_013,
         `36250` = SE_T041_014, 
         `38750` = SE_T041_015, 
         `41250` = SE_T041_016, 
         `43750` = SE_T041_017, 
         `46250` = SE_T041_018, 
         `48750` = SE_T041_019, 
         `52500` = SE_T041_020, 
         `57500` = SE_T041_021, 
         `67500` = SE_T041_022, 
         `87500` = SE_T041_023, 
         `112500` = SE_T041_024, 
         `137500` = SE_T041_025, 
         `175000` = SE_T041_026
         )

# pivot longer such that there's a income_cat variable, count variable, stcou, and metdiv variable 
# this part is a bit hard to understand. For following years, the only thing you need to make sure you edit is the starting variable and ending variable that bookend the colon :
# and make you sure change what's in the [[]] for cpi to match the current year
income1990 %<>%
  rename(trtid10 = Geo_FIPS) %>%
  select(trtid10, metdiv, `2500`:`175000`) %>%
  pivot_longer(cols = c(`2500`:`175000`),
               names_to = "income_cat", 
               values_to = "count") %>%
  mutate(income_cat_inf = as.numeric(income_cat)*cpi[["1990"]], 
         count = ifelse(is.na(count), 0, count))

# aggregate counts of the metdiv households so that we can get distribution
income_agg <- income1990 %>%
  group_by(metdiv) %>%
  dplyr::summarize(n_hh = sum(count, na.rm = T), 
         hh_1 = round(n_hh * 0.25, 0), 
         hh_2 = round(n_hh * 0.5, 0), 
         hh_3 = round(n_hh * 0.75, 0)) %>%
  ungroup() 

# get cumulative distribution (imagine lining every household up and assigning them a number; we're trying to find the number that corresponds to 25%, 50%, 75%)
income1990 <- income1990 %>%
  group_by(metdiv) %>%
  arrange(metdiv, income_cat_inf) %>%
  mutate(cum_hh_count = cumsum(count)) %>%
  left_join(income_agg, by = "metdiv") %>%
  mutate(income_1st = ifelse(cum_hh_count <= hh_1, 1, 0),
         income_2nd = ifelse(cum_hh_count > hh_1 & cum_hh_count <= hh_2, 1, 0),
         income_3rd = ifelse(cum_hh_count > hh_2 & cum_hh_count <= hh_3, 1, 0),
         income_4th = ifelse(cum_hh_count > hh_3, 1, 0)) %>%
  ungroup()

# have to do something different with metdivs where there are 0 households
na_metdiv <- income_agg %>% 
  filter(n_hh == 0)

# get the income value at the Xth spot of the distribution
income1990 <- income1990 %>%
  filter(!metdiv %in% na_metdiv$metdiv & !is.na(metdiv)) %>%
  group_by(metdiv) %>%
  dplyr::summarize(inc_1 = ifelse(income_1st==1, income_cat_inf, NA),
                   inc_2 = ifelse(income_2nd==1, income_cat_inf, NA),
                   inc_3 = ifelse(income_3rd==1, income_cat_inf, NA),
                   inc_4 = ifelse(income_4th==1, income_cat_inf, NA)) %>% 
  group_by(metdiv) %>%
  dplyr::summarize(inc_1 = max(inc_1, na.rm = T),
            inc_2 = max(inc_2, na.rm = T), 
            inc_3 = max(inc_3, na.rm = T),
            inc_4 = max(inc_4, na.rm = T)) 

na_metdiv %<>%
  mutate(inc_1 = NA,
         inc_2 = NA, 
         inc_3 = NA, 
         inc_4 = NA) %>%
  select(metdiv, inc_1:inc_4)

metdiv1990 <- base::rbind(income1990, na_metdiv)

# here's another way to assign things to variables
metdiv1990$Year <- 1990

# here's how we save files 
write_csv(metdiv1990, "cleaned/metdiv1990.csv")

# remove things we don't need (we still need the crosswalk file so we don't have to load it every time. We need the cpi vector)
rm(income_agg, income1990, metdiv1990, na_metdiv)

# Andrew completes this series for 2000, 2010 (08-12 file), 2011 (09-13 file) etc. until 2017 (15-19 file) ----
