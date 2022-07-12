# -------------------------------------------------------------------------
# Created by: Iris Zhang                     
# Date created: 7/8/2022             
# Last revised:             
# Project: PSID-Racial Stratification         
# Subproject: Identify MSA-level Income Quartile Cutoffs by-year 
# Re: Compare income quartile cutoffs using income category by household-counts to data generated from tract-level median household income data from the Census/ACS in Interpolate-ACS.R
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script cleans and processes 2013-2017 5-Year County-Level ACS data downloaded from SE. First, I adjust the values for inflation (2019 Jan 1 dollars) and find the midpoint. Then, I aggregate up to the metdiv, to get the total # of households in each income category, but also the metdiv-level aggregate # of households. 

# Inputs:
# acs1317_county_income.csv (codebook is named acs1317_county_income.txt)
# cbsa_stcou_2013_cw.csv

# Outputs:
#  For now, just a dplyr tibble that allows me to see the vector of 25th, 50th, and 75th percentile of income cutoffs for the 37964 metdiv, which corresponds to Philly. 

# Updates log: 

# Setup -------------------------------------------------------------------

# Packages: 
library("tidyverse") 
library("readr") 
library("magrittr") 

# Directories: 
homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
workdir <- paste0("/../misc_data/")
setwd(paste0(homedir, workdir))

# Import data: 
cpi <- c(1.04)

income <- read_csv("acs1317_county_income.csv")
cw <- read_csv("cbsa_stcou_2013_cw.csv") %>%
  mutate(stcou = state*1000 + county,
         stcou = str_pad(stcou, 5, "left", "0"), 
         metdiv = ifelse(is.na(msamd), cbsa, msamd)) %>%
  select(stcou, metdiv)

# Parameters:

# Main Script -------------------------------------------------------------
# get metdiv IDs for county-level data 
income %<>%
  mutate(stcou = paste0(Geo_STATE, Geo_COUNTY)) %>%
  left_join(cw, by = "stcou")

income %<>%
  rename(n_hh = SE_A14001_001,
         `5000` = SE_A14001_002,
         `12500` = SE_A14001_003, 
         `17500` = SE_A14001_004, 
         `22500`= SE_A14001_005, 
         `27500` = SE_A14001_006, 
         `32500` = SE_A14001_007, 
         `37500` = SE_A14001_008,
         `42500` = SE_A14001_009,  
         `47500` = SE_A14001_010,  
         `55000` = SE_A14001_011, 
         `67500` = SE_A14001_012, 
         `87500` = SE_A14001_013, 
         `111250` = SE_A14001_014, 
         `137500` = SE_A14001_015,  
         `175000` = SE_A14001_016, 
         `225000` = SE_A14001_017)

# pivot longer such that there's a income_cat variable, count variable, stcou, and metdiv variable 

income <- income %>%
  select(stcou, metdiv, `5000`:`225000`) %>%
  pivot_longer(cols = c(`5000`:`225000`),
               names_to = "income_cat", 
               values_to = "count") %>%
  mutate(income_cat_inf = as.numeric(income_cat)*cpi)

# aggregate counts of the metdiv households so that we can get distribution
income_agg <- income %>%
  group_by(metdiv) %>%
  dplyr::summarize(n_hh = sum(count), 
         hh_1 = round(n_hh * 0.25, 0), 
         hh_2 = round(n_hh * 0.5, 0), 
         hh_3 = round(n_hh * 0.75, 0)) %>%
  ungroup() 

# get cumulative distribution (imagine lining every household up and assigning them a number; we're trying to find the number that corresponds to 25%, 50%, 75%)
income <- income %>%
  group_by(metdiv) %>%
  arrange(metdiv, income_cat_inf) %>%
  mutate(cum_hh_count = cumsum(count)) %>%
  left_join(income_agg, by = "metdiv") %>%
  mutate(income_1st = ifelse(cum_hh_count <= hh_1, 1, 0),
         income_2nd = ifelse(cum_hh_count > hh_1 & cum_hh_count <= hh_2, 1, 0),
         income_3rd = ifelse(cum_hh_count > hh_2 & cum_hh_count <= hh_3, 1, 0),
         income_4th = ifelse(cum_hh_count > hh_3, 1, 0))

# get the income value at the Xth spot of the distribution
income <- income %>%
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

# get it for the whole country ####
income <- read_csv("acs1317_county_income.csv")

income %<>%
  rename(n_hh = SE_A14001_001,
         `5000` = SE_A14001_002,
         `12500` = SE_A14001_003, 
         `17500` = SE_A14001_004, 
         `22500`= SE_A14001_005, 
         `27500` = SE_A14001_006, 
         `32500` = SE_A14001_007, 
         `37500` = SE_A14001_008,
         `42500` = SE_A14001_009,  
         `47500` = SE_A14001_010,  
         `55000` = SE_A14001_011, 
         `67500` = SE_A14001_012, 
         `87500` = SE_A14001_013, 
         `111250` = SE_A14001_014, 
         `137500` = SE_A14001_015,  
         `175000` = SE_A14001_016, 
         `225000` = SE_A14001_017) %>%
  mutate(country = "US")

# pivot longer such that there's a income_cat variable, count variable, stcou, and metdiv variable 
income <- income %>%
  select(country, `5000`:`225000`) %>%
  pivot_longer(cols = c(`5000`:`225000`),
               names_to = "income_cat", 
               values_to = "count") %>%
  mutate(income_cat_inf = as.numeric(income_cat)*cpi)

# aggregate counts of the metdiv households so that we can get distribution
income_agg <- income %>%
  group_by(country) %>%
  dplyr::summarize(n_hh = sum(count), 
                   hh_1 = round(n_hh * 0.25, 0), 
                   hh_2 = round(n_hh * 0.5, 0), 
                   hh_3 = round(n_hh * 0.75, 0)) %>%
  ungroup() 

# get cumulative distribution (imagine lining every household up and assigning them a number; we're trying to find the number that corresponds to 25%, 50%, 75%)
income <- income %>%
  arrange(income_cat_inf) %>%
  mutate(cum_hh_count = cumsum(count)) %>%
  left_join(income_agg, by = "country") %>%
  mutate(income_1st = ifelse(cum_hh_count <= hh_1, 1, 0),
         income_2nd = ifelse(cum_hh_count > hh_1 & cum_hh_count <= hh_2, 1, 0),
         income_3rd = ifelse(cum_hh_count > hh_2 & cum_hh_count <= hh_3, 1, 0),
         income_4th = ifelse(cum_hh_count > hh_3, 1, 0))

# get the income value at the Xth spot of the distribution
income <- income %>%
  group_by(country) %>%
  dplyr::summarize(inc_1 = ifelse(income_1st==1, income_cat_inf, NA),
                   inc_2 = ifelse(income_2nd==1, income_cat_inf, NA),
                   inc_3 = ifelse(income_3rd==1, income_cat_inf, NA),
                   inc_4 = ifelse(income_4th==1, income_cat_inf, NA)) %>% 
  group_by(country) %>%
  dplyr::summarize(inc_1 = max(inc_1, na.rm = T), 
                   inc_2 = max(inc_2, na.rm = T),
                   inc_3 = max(inc_3, na.rm = T),
                   inc_4 = max(inc_4, na.rm = T)) 






