homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
workdir <- paste0("/../misc_data/")
setwd(paste0(homedir, workdir))

library(tidyverse)
library(foreign)
library(readr)

rm(list = ls())

#var
# pop, hu, nhwhite, nhblk, hispanic, asian, hinc, pcol, ppov, pprof, mhmval, mrent, pnew, pocc

neighborhoods <- read_csv('ltdb7010_acs1317_2010b_gentvars_clean_addacs0812.csv')

gent <- read_csv('gent_measures_707a_2010b.csv')
gent <- gent %>%
  select(trtid10, ccflag, (contains("007a.2000") | contains("00.2000"))) %>%
  select(trtid10, ccflag, (contains("fm") | contains("dhd"))) %>%
  select(-((contains("40") | contains("gentcat") | contains("alt"))))

gentlong <- gent %>%
  pivot_longer(cols = contains("."),
               names_to = c(".value", "Year"),
               names_sep = "\\.",
               values_drop_na = F) %>%
  mutate(trtid10 = str_pad(trtid10, 11, "left", "0"))

neighborhoods <- neighborhoods %>%
  select(trtid10, metdiv, (ends_with("00") | ends_with("7a")))

nh <- full_join(neighborhoods, gentlong, by = "trtid10")

#tracts excluded from analysis - should be the same across measures 
table(nh$dhd_abledum50c_00, exclude = NULL)
table(nh$fm_abledum50_00, exclude = NULL)
table(nh$dhd_abledum50m_00, exclude = NULL)

table(nh$ccflag)

# how many tracts should we still have? 
74001 - 7629 

# city tracts excluded
7629/27759

nh <- nh %>%
  filter(!is.na(dhd_abledum50c_00) & !is.na(fm_abledum50_00)) # should be 66372

nh %<>%
  mutate(majbflag = ifelse(pnhblk00 >= 50, 1, 0))

# pct of gent and non-gent tracts that are majority black in 2000
table(nh$dhd_rve_gentdumc_007a)
# 3513 gent and 10060 non-gent 
nh %>% filter(dhd_rve_gentdumc_007a == 1 & majbflag == 1) %>% nrow()
817/3512

nh %>% filter(dhd_rve_gentdumc_007a == 0 & majbflag == 1) %>% nrow()
2617/10060

nh <- nh %>%
  mutate(ntypefm = ifelse(is.na(fm50_gentdum_007a) & ccflag==0, "Nongentrifiable Non-city_FM",
                          ifelse(is.na(fm50_gentdum_007a) & ccflag==1, "Nongentrifiable City_FM",
                                 ifelse(fm50_gentdum_007a==1, "Gentrifying_FM", "Nongentrifying_FM"))),
         ntypefm = factor(ntypefm, levels = c("Nongentrifiable Non-city_FM", "Nongentrifiable City_FM",
                                              "Gentrifying_FM", "Nongentrifying_FM")),
         ntyperve = ifelse(is.na(dhd_rve_gentdumc_007a) & ccflag==0, "Nongentrifiable Non-city_RVE",
                            ifelse(is.na(dhd_rve_gentdumc_007a) & ccflag==1, "Nongentrifiable City_RVE",
                                   ifelse(dhd_rve_gentdumc_007a==1, "Gentrifying_RVE", "Nongentrifying_RVE"))),
         ntyperve = factor(ntyperve, levels = c("Nongentrifiable Non-city_RVE", "Nongentrifiable City_RVE",
                                                  "Gentrifying_RVE", "Nongentrifying_RVE")),
         ntypedhdc = ifelse(is.na(dhdc_gentdum_007a) & ccflag==0, "Nongentrifiable Non-city_DHDC",
                            ifelse(is.na(dhdc_gentdum_007a) & ccflag==1, "Nongentrifiable City_DHDC",
                                   ifelse(dhdc_gentdum_007a==1, "Gentrifying_DHDC", "Nongentrifying_DHDC"))),
         ntypedhdc = factor(ntypedhdc, levels = c("Nongentrifiable Non-city_DHDC", "Nongentrifiable City_DHDC",
                                              "Gentrifying_DHDC", "Nongentrifying_DHDC")),
         ntypedhdm = ifelse(is.na(dhdm_gentdum_007a) & ccflag==0, "Nongentrifiable Non-city_DHDM",
                            ifelse(is.na(dhdm_gentdum_007a) & ccflag==1, "Nongentrifiable City_DHDM",
                                   ifelse(dhdm_gentdum_007a==1, "Gentrifying_DHDM", "Nongentrifying_DHDM"))),
         ntypedhdm = factor(ntypedhdm, levels = c("Nongentrifiable Non-city_DHDM", "Nongentrifiable City_DHDM",
                                              "Gentrifying_DHDM", "Nongentrifying_DHDM")))
cpi <- c(1.53,1.04)

nh <- nh %>%
  mutate(hinci00 = hinc00*cpi[1],
         mhmvali00 = mhmval00*cpi[1],
         mrenti00 = mrent00*cpi[1],
         hinci7a = hinc7a*cpi[2],
         mhmvali7a = mhmval7a*cpi[2],
         mrenti7a = mrent7a*cpi[2])

# nh <- nh %>% filter(ccflag==1) # we do central cities only in UAR

varindex = c("pop00", "pop7a", "hu00", "hu7a", "pnhwht00", "pnhwht7a", "pnhblk00", "pnhblk7a", "phisp00", "phisp7a",
             "pasian00", "pasian7a", "hinci00", "hinci7a", "pcol00", "pcol7a", "ppov00", "ppov7a",
             "pprof00", "pprof7a", "mhmvali00", "mhmvali7a", "mrenti00", "mrenti7a", "p20young00", "p20young7a",
             "pown00", "pown7a", "ntyperve", "ntypefm", "ntypedhdc", "ntypedhdm") #variables to keep

nh <- nh %>%
  select(trtid10, ccflag, all_of(varindex))

varindex = c("pop", "hu", "pnhwht", "pnhblk", "phisp", "pasian", "hinci", "pcol", "ppov", "pprof",
             "mhmvali", "mrenti", "p20young", "pown") # create change variables

for (variable in varindex) {
  varname <- paste0(variable, "_change")
  nh[[varname]] <- nh[[paste0(variable, "7a")]] - nh[[paste0(variable, "00")]]
}

# create ordered index for variables in descriptive table

for (var in varindex) {
  if (var == "pop") {
    varindex_desc <- c(paste0(var, "00"), paste0(var, "_change"))
  } else {
    varindex_desc <- c(varindex_desc, c(paste0(var, "00"), paste0(var, "_change")))
  }
}

# do this for FM
for (i in varindex_desc) {
  test <- as.data.frame(nh %>%
    group_by(ntypefm) %>%
    summarize(mean = mean(.data[[i]], na.rm = T)))
  names(test)[2] = paste0("mean_", i)
 if (i == "pop00") {
   ninfofm <- as.data.frame(test)
   names(ninfofm) <- names(test)
 } else {
   ninfofm <- left_join(ninfofm, test, by = "ntypefm")
 }
}

# do this for dhdc
for (i in varindex_desc) {
  test <- as.data.frame(nh %>%
                          group_by(ntypedhdc) %>%
                          summarize(mean = mean(.data[[i]], na.rm = T)))
  names(test)[2] = paste0("mean_", i)
  if (i == "pop00") {
    ninfodhdc <- as.data.frame(test)
    names(ninfodhdc) <- names(test)
  } else {
    ninfodhdc <- left_join(ninfodhdc, test, by = "ntypedhdc")
  }
}

# do this for dhdm
for (i in varindex_desc) {
  test <- as.data.frame(nh %>%
                          group_by(ntypedhdm) %>%
                          summarize(mean = mean(.data[[i]], na.rm = T)))
  names(test)[2] = paste0("mean_", i)
  if (i == "pop00") {
    ninfodhdm <- as.data.frame(test)
    names(ninfodhdm) <- names(test)
  } else {
    ninfodhdm <- left_join(ninfodhdm, test, by = "ntypedhdm")
  }
}

# rve
for (i in varindex_desc) {
  test <- as.data.frame(nh %>%
                          group_by(ntyperve) %>%
                          summarize(mean = mean(.data[[i]], na.rm = T)))
  names(test)[2] = paste0("mean_", i)
  if (i == "pop00") {
    ninforve <- as.data.frame(test)
    names(ninforve) <- names(test)
  } else {
    ninforve <- left_join(ninforve, test, by = "ntyperve")
  }
}

# transpose everything
ninfofm <- rownames_to_column(as.data.frame(t(ninfofm)), var = "descriptive_name")
ninfodhdc <- rownames_to_column(as.data.frame(t(ninfodhdc)), var = "descriptive_name")
ninfodhdm <- rownames_to_column(as.data.frame(t(ninfodhdm)), var = "descriptive_name")
ninforve <- rownames_to_column(as.data.frame(t(ninforve)), var = "descriptive_name")

# update column names - UAR
names(ninfofm) <- c("gentdum", "Nongentrifiable_FM", "Gentrifying_FM", "Non-Gentrifying_FM")
ninfofm <- ninfofm[-1, ]
names(ninfodhdc) <- c("gentdum", "Nongentrifiable_DHDC", "Gentrifying_DHDC", "Non-Gentrifying_DHDC")
ninfodhdc <- ninfodhdc[-1, ]
names(ninfodhdm) <- c("gentdum", "Nongentrifiable_DHDM", "Gentrifying_DHDM",  "Non-Gentrifying_DHDM")
ninfodhdm <- ninfodhdm[-1, ]
names(ninforve) <- c("gentdum", "Nongentrifiable_RVE", "Gentrifying_RVE",  "Non-Gentrifying_RVE")
ninforve <- ninforve[-1, ]

# update column names full version 
names(ninfofm) <- c("gentdum", "Nongentrifiable Non-city_FM", "Nongentrifiable City_FM", "Gentrifying_FM", "Non-Gentrifying_FM")
ninfofm <- ninfofm[-1, ]
names(ninfodhdc) <- c("gentdum", "Nongentrifiable Non-city_DHDC", "Nongentrifiable City_DHDC", "Gentrifying_DHDC", "Non-Gentrifying_DHDC")
ninfodhdc <- ninfodhdc[-1, ]
names(ninfodhdm) <- c("gentdum", "Nongentrifiable Non-city_DHDM", "Nongentrifiable City_DHDM", "Gentrifying_DHDM",  "Non-Gentrifying_DHDM")
ninfodhdm <- ninfodhdm[-1, ]
names(ninforve) <- c("gentdum", "Nongentrifiable Non-city_RVE", "Nongentrifiable City_RVE", "Gentrifying_RVE",  "Non-Gentrifying_RVE")
ninforve <- ninforve[-1, ]

# get total tally of each level of the gentdum, FM
nfm <- as.data.frame(nh %>%
                       group_by(ntypefm) %>%
                       tally() %>%
                       t()) %>%
  rownames_to_column(., var = "descriptive_name")

names(nfm) <- names(ninfofm)
nfm <- nfm[-1, ]

# get total tally of each level of the gentdum, DHDC
ndhdc <- as.data.frame(nh %>%
                       group_by(ntypedhdc) %>%
                       tally() %>%
                       t()) %>%
  rownames_to_column(., var = "descriptive_name")

names(ndhdc) <- names(ninfodhdc) 
ndhdc <- ndhdc[-1, ]

# get total tally of each level of the gentdum, DHDM
ndhdm <- as.data.frame(nh %>%
                       group_by(ntypedhdm) %>%
                       tally() %>%
                       t()) %>%
  rownames_to_column(., var = "descriptive_name")

names(ndhdm) <- names(ninfodhdm) 
ndhdm <- ndhdm[-1, ]

# get total tally of each level of the gentdum, RVE
nrve <- as.data.frame(nh %>%
                         group_by(ntyperve) %>%
                         tally() %>%
                         t()) %>%
  rownames_to_column(., var = "descriptive_name")

names(nrve) <- names(ninforve) 
nrve <- nrve[-1, ]

# add to each dataset
ninfofm <- base::rbind(ninfofm, nfm)
ninfodhdc <- base::rbind(ninfodhdc, ndhdc)
ninfodhdm <- base::rbind(ninfodhdm, ndhdm)
ninforve <- base::rbind(ninforve, nrve)

# create table - in UAR we do fm and dhdc only; change for papers using dhdm also 
desc_table <- ninfofm %>%
  left_join(ninfodhdc, by = "gentdum") %>%
  left_join(ninfodhdm, by = "gentdum") %>%
  left_join(ninforve, by = "gentdum")

# format row names
formatted_rows <- c(
  "Population, 2000",
  "Change in Population, 2000-2017",
  "Housing Units, 2000",
  "Change in Housing Units, 2000-2017",
  "% Non-Hispanic White, 2000",
  "Change in % Non-Hispanic White, 2000-2017",
  "% Non-Hispanic Black, 2000",
  "Change in % Non-Hispanic Black, 2000-2017",
  "% Hispanic, 2000",
  "Change in % Hispanic, 2000-2017",
  "% Asian, 2000",
  "Change in % Asian, 2000-2017",
  "Median household income (inflation-adjusted), 2000",
  "Changee in Median household income (inflation-adjusted), 2000-2017",
  "% college-educated, 2000",
  "Change in % college-educated, 2000-2017",
  "% below poverty, 2000",
  "Change in % below poverty, 2000-2017",
  "% professionals, 2000",
  "Change in % professionals, 2000-2017",
  "Median home value (inflation-adjusted), 2000",
  "Change in Median home value (inflation-adjusted), 2000-2017",
  "Mean gross rent (inflation-adjusted), 2000",
  "Change in Mean gross rent (inflation-adjusted), 2000-2017",
  "% units built in last 20 years, 2000",
  "Change in % units built in last 20 years, 2000-2017",
  "% units owner-occupied, 2000",
  "Change in % units owner-occupied, 2000-2017",
  "N")

desc_table[c(1:nrow(desc_table)), 1] <- formatted_rows

write_csv(desc_table, "~/Google Drive/Stanford/UAR_Gent_Destinations/Figures_Tables/UAR_Table 1.csv")
write_csv(desc_table, "../results/Table 1.csv")

