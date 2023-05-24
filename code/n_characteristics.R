rm(list = ls())
homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
workdir <- paste0("/../misc_data/")
setwd(paste0(homedir, workdir))

library(tidyverse)
library(foreign)
library(readr)

#var
# pop, hu, nhwhite, nhblk, hispanic, asian, hinc, pcol, ppov, pprof, mhmval, mrent, pnew, pocc

neighborhoods <- read_csv('~/Google Drive/My Drive/Stanford/PROJECT FOLDER_Gentrification and PSID Displacement/Gentrification Measures_RA/US Measures_PSID_replication_RA/ltdb7010_acs1317_2010b_gentvars_clean_addacs0812.csv')

gent <- read_csv('gent_measures_707a_2010b.csv')
gentdat10_msa <- read_csv("gent_measures_907a_allmsa.csv")

names(gent)
names(gentdat10_msa)

gent %<>%
  left_join(gentdat10_msa %>% select(trtid10, fm_abledum50_00, fm50_gentdum_007a, dhd_abledum50m_00, dhd_rve_gentdumm_007a)) %>%
  select(trtid10, metdiv, placefp, ccflag, dhd_abledum50c_00.2000, dhd_rve_gentdumc_007a.2000, fm_abledum50_00, fm50_gentdum_007a, dhd_abledum50m_00, dhd_rve_gentdumm_007a) %>%
  rename(rve_msa_gentdum_007a.2000 = dhd_rve_gentdumm_007a,
         fm50_gentdum_007a.2000 = fm50_gentdum_007a,
         fm_abledum50_00.2000 = fm_abledum50_00,
         dhd_abledum50m_00.2000 = dhd_abledum50m_00
         )

names(gent)
rm(gentdat10_msa)

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

table(nh$rve_msa_gentdum_007a, exclude = NULL)

nh %<>%
  mutate(rve_msa_gentdum_007a = ifelse(dhd_abledum50m_00 == 0, NA,
                                       rve_msa_gentdum_007a))

nh %<>%
  filter(!is.na(dhd_abledum50c_00) & !is.na(fm_abledum50_00) & !is.na(dhd_abledum50m_00)) %>%
  mutate(majbflag = ifelse(pnhblk00 >= 50, 1, 0))

# pct of metro tracts that are gentrifiable and 
# pct of gent and non-gent tracts that are majority black in 2000
table(nh$rve_msa_gentdum_007a, exclude = NULL)
(7813+25111)/(25111+7813+33447)
7813/(7813+25111)
7813/(7813+25111+33447)
# 3513 gent and 10060 non-gent 
nh %>% filter(rve_msa_gentdum_007a == 1 & majbflag == 1) %>% nrow()
1121/7813

nh %>% filter(rve_msa_gentdum_007a == 0 & majbflag == 1) %>% nrow()
4009/25111

nh <- nh %>%
  mutate(ntypefm = ifelse(is.na(fm50_gentdum_007a), "Nongentrifiable_FM",
                         ifelse(fm50_gentdum_007a==1, "Gentrifying_FM", "Nongentrifying_FM")),
         ntypefm = factor(ntypefm, levels = c("Nongentrifiable_FM", 
                                              "Gentrifying_FM", "Nongentrifying_FM")),
         ntyperve = ifelse(is.na(dhd_rve_gentdumc_007a), "Nongentrifiable_RVE",
                           ifelse(dhd_rve_gentdumc_007a==1, "Gentrifying_RVE", "Nongentrifying_RVE")),
         ntyperve = factor(ntyperve, levels = c("Nongentrifiable_RVE",
                                                  "Gentrifying_RVE", "Nongentrifying_RVE")),
         ntypervemsa = ifelse(is.na(rve_msa_gentdum_007a), "Nongentrifiable_RVEMSA",
                           ifelse(rve_msa_gentdum_007a==1, "Gentrifying_RVEMSA", "Nongentrifying_RVEMSA")),
         ntypervemsa = factor(ntypervemsa, levels = c("Nongentrifiable_RVEMSA", "Gentrifying_RVEMSA", "Nongentrifying_RVEMSA"))
         )
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
             "pown00", "pown7a", "ntyperve", "ntypefm", "ntypervemsa") #variables to keep

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

# do this for rve
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

# rvemsa
for (i in varindex_desc) {
  test <- as.data.frame(nh %>%
                          group_by(ntypervemsa) %>%
                          summarize(mean = mean(.data[[i]], na.rm = T)))
  names(test)[2] = paste0("mean_", i)
  if (i == "pop00") {
    ninforvemsa <- as.data.frame(test)
    names(ninforvemsa) <- names(test)
  } else {
    ninforvemsa <- left_join(ninforvemsa, test, by = "ntypervemsa")
  }
}

# transpose everything
ninfofm <- rownames_to_column(as.data.frame(t(ninfofm)), var = "descriptive_name")
ninforve <- rownames_to_column(as.data.frame(t(ninforve)), var = "descriptive_name")
ninforvemsa <- rownames_to_column(as.data.frame(t(ninforvemsa)), var = "descriptive_name")

# update column names 
names(ninfofm) <- c("gentdum", "Nongentrifiable_FM", "Gentrifying_FM", "Non-Gentrifying_FM")
ninfofm <- ninfofm[-1, ]
names(ninforve) <- c("gentdum", "Nongentrifiable_RVE", "Gentrifying_RVE",  "Non-Gentrifying_RVE")
ninforve <- ninforve[-1, ]
names(ninforvemsa) <- c("gentdum", "Nongentrifiable_RVEMSA", "Gentrifying_RVEMSA",  "Non-Gentrifying_RVEMSA")
ninforvemsa <- ninforvemsa[-1, ]

# get total tally of each level of the gentdum, FM
nfm <- as.data.frame(nh %>%
                       group_by(ntypefm) %>%
                       tally() %>%
                       t()) %>%
  rownames_to_column(., var = "descriptive_name")

names(nfm) <- names(ninfofm)
nfm <- nfm[-1, ]

# get total tally of each level of the gentdum, RVE
nrve <- as.data.frame(nh %>%
                         group_by(ntyperve) %>%
                         tally() %>%
                         t()) %>%
  rownames_to_column(., var = "descriptive_name")

names(nrve) <- names(ninforve) 
nrve <- nrve[-1, ]

# get total tally of each level of the gentdum, RVEMSA
nrvemsa <- as.data.frame(nh %>%
                        group_by(ntypervemsa) %>%
                        tally() %>%
                        t()) %>%
  rownames_to_column(., var = "descriptive_name")

names(nrvemsa) <- names(ninforvemsa) 
nrvemsa <- nrvemsa[-1, ]

# add to each dataset
ninfofm <- base::rbind(ninfofm, nfm)
ninforve <- base::rbind(ninforve, nrve)
ninforvemsa <- base::rbind(ninforvemsa, nrvemsa)

# create table - in UAR we do fm and dhdc only; change for papers using dhdm also 
desc_table <- ninfofm %>%
  left_join(ninforve, by = "gentdum") %>%
  left_join(ninforvemsa, by = "gentdum")

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

