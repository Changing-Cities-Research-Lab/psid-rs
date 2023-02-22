`# ------------------------------------------------------------------------------
# Created by: Jackie Hwang           
# Date created: Feb 21, 2023
# Last revised: Feb 21, 2023
# Project: PSID - Racial Stratification   
# Subproject: Analysis v2 
# Re: create gentrification measures for all MSA
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script borrows from "create_all_gent_measures_v2.R" but expands gent 
# measures beyond central cities to include all MSA. This script only creates
# measures for RVE, RVEI, and Freeman for 1990-2000 & 2000-2017 using 2010 
# boundaries, and 2013 MSAMDs

# Inputs:
# -clean gentrification variables from census/ACS for 2010 census tract 
# boundaries for needed decades

# Outputs:
# -selected all MSA gentrification measures for 2010 census tract boundaries 

# Update log: 
# 02/21/2023: n/a

# Setup -------------------------------------------------------------------

rm(list=ls())

# Packages: 
library('readstata13')
library('dplyr')
# library('plyr')
library('foreach')
library('foreign')


# Directories: 
homedir <- ""
workdir <- "G:/My Drive/Changing Cities Research Lab/PROJECT FOLDER  Gentrification and PSID Displacement/Gentrification Measures_RA/Gentrification Measures/"
savedir <- "C:/Users/jacks17/Dropbox/1RESEARCH/Residential Mobility/PSID project/Gentrification and Mobility PSID Data/Gentrification Measures/"
setwd(paste0(homedir, workdir))


# Foreach loop that imports each decade of data and outputs merged dataset for all decades
# Identify filenames for each decade
decadefiles <- 
  list.files("Decade Measures_Iris/decades/") 
decadefiles10 <- decadefiles[grepl("_2010.csv", decadefiles)]
rm(decadefiles)

decades <- c("9000", "007a")

gent_measures_allmsa <- 

  foreach (m = 1:length(decades)) %do% {
  
# Import data: 
gentdat10 <-
  read.csv(
    paste0("Decade Measures_Iris/decades/", 
           decadefiles10[grepl(decades[m], decadefiles10)]))

# Parameters:
# 2017 real dollars, using CPI calculator from bls.gov
cpis <- c(2.01, 1.48, 1) 


# Main Script -------------------------------------------------------------

# combine datasets into a list of dataframes (df)
alldat <- list(gentdat10)
names(alldat) <- c("2010") # name dataframes by boundary years
rm(gentdat10) # remove dataframes and keep list

##################################################################
# calculate change variables needed for various gent measures ####
##################################################################

# THIS IS WHERE YOU LEFT OFF ####
# Need to find code for RVE, RVEI, and Freeman and delete the rest
# Don't want to remove change variables that are needed

# create vector of years needed (1970, 1980, 1990, 2000, 2006/10 ACS, 2013/17 ACS)
years <- c(substr(decades[m], 1, 2), substr(decades[m], 3, 4))

# calculate change variables over period ####

# calculate dollar value changes separately (as percent changes)
# calculate changes in ses index and % col variables as change in values
# calculate changes in college-educated relative to distribution midpoint
cvars1 <- c("pcol") # for change in values
cvars2 <- c("mhmval", "mrent", "hinc") # for percent change
cpi <- c(cpis[m], cpis[m + 1]) # adjustments to 2017 real dollars
# cpi is based on www.bls.gov calculator

cvars <- 
  foreach(k = 1:length(alldat)) %do% { # for each dataset
    gentdat <- alldat[[k]] # grab each dataset
    out3 <- gentdat[, 1] # create a vactor of tract IDs
    
    out2 <- 
      foreach (
        j = 1:(length(years)-1), # for each baseline year
        .combine = cbind.data.frame) %do% {
          # for the first set of variables
          cvar1 <- 
            foreach (
              i = 1:length(cvars1), 
              .combine = cbind.data.frame) %do% {
                var1 <- # grab the variable in the baseline year
                  gentdat[, which(names(gentdat) %in% 
                                    paste(cvars1[i], years[j], sep = ""))]
                var2 <- # grab the variable in the period end year
                  gentdat[, which(names(gentdat) %in% 
                                    paste(cvars1[i], years[j + 1], sep = ""))]
                cvar <- var2 - var1 # calculate the difference over the period
                return(cvar)
              }
          # for the second set of variables
          cvar2 <- 
            foreach (
              i = 1:length(cvars2), 
              .combine = cbind.data.frame) %do% {
                var1 <- # grab the variable at baseline and adjust to 2017 dollars
                  gentdat[, which(names(gentdat) %in% 
                                    paste(cvars2[i], years[j], sep = ""))] * 
                  cpi[j]
                var2 <- # grab the variable at the end of the period and adjust to 2017 dollars
                  gentdat[, which(names(gentdat) %in% 
                                    paste(cvars2[i], years[j + 1], sep = ""))] * 
                  cpi[j + 1]
                cvar <- (var2 - var1) / var1 * 100 # calculate the percent change over the period
                return(cvar)
              }
          
          # combine all of the change variables and name them by pasting the year interval as suffix
          out <- cbind.data.frame(cvar1, cvar2) 
          names(out) <- paste(c(cvars1, cvars2), years[j], years[j + 1], sep = "")
          return(out)
        }
    
    # merge all of the variables with the full set of tract ids
    out3 <- cbind.data.frame(out3, out2)
    names(out3)[1] <- names(gentdat)[1]
    
    # merge with the full dataset
    alldat[[k]] <- left_join(alldat[[k]], out3)
    return(NULL)
  }

# clean up
rm(i, j, k, var1, var2, cvar, cvar1, cvar2, 
   cvars, cvars1, cvars2, 
   out, out2, out3)


##############################
# gentrifiable: below msa ####
##############################
# calculate thresholds for whether tracts are gentrifiable 
# based on different criteria depending on the measure being used: 
##  below median hhinc and % new build for msa
# these calculations are only needed for baseline years of each period
## because tracts need to be gentrifiable at the beginning of the period in order to 
## gentrify over the period

# id variables needed
vars <- c("hinc", "p20young")

# calculate msawide medians for variable-years 
msamedians <- 
  foreach (k = 1:length(alldat)) %do% { # create a list with a dataframe for each dataset
    gentdat <- alldat[[k]] # grab each dataset
    # create a data frame with the median for each msa (metdiv) as rows 
    ## with columns for each variable
    out2 <- 
      foreach (i = 1:length(vars), # for each variable
               .combine = cbind.data.frame) %do% {
                 foreach (j = 1:(length(years)-1), # for each baseline year
                          .combine = cbind.data.frame) %do% {
                            # grab the variable-year
                            var <- gentdat[, which(names(gentdat) %in% 
                                                     paste(vars[i], years[j], sep = ""))]
                            # calculate the median for each msa (metdiv)
                            out <- 
                              as.data.frame(as.vector(
                                by(var, 
                                   gentdat$metdiv, 
                                   median, na.rm = T))) 
                            # name it by variable-year
                            names(out) <- paste(vars[i], years[j], sep = "")
                            return(out)
                          }
               }
    # add a column for the msaid
    out2$metdiv <- sort(unique(gentdat$metdiv))
    # reorder the columns
    out2 <- out2[, c(ncol(out2), 1:(ncol(out2)-1))]
    return(out2)
  }
names(msamedians) <- names(alldat) # name each dataframe by the dataset boundary years

# clean up
rm(i, j, k, var, vars, out, out2, gentdat)

#########################################################################
# create list for all of the gent variables for each of the datasets ####
#########################################################################
alldatgent <- list()

# keep geo identifiers
alldatgent[[1]] <- alldat[[1]][, 1:8]
names(alldatgent) <- names(alldat)

##################################################################
# freeman measure - remove central city restriction, 50p only ####
##################################################################
# .	Measure characteristics: central city only; threshold; binary; % college-educated and median housing value
# .	Gentrifiable: 
#### 1) central city - removed
#### 2) tract median household income < MSA median (or 40th percentile) household income; AND 
#### 3) tract % housing built within last 20 years < MSA median (or 40th percentile) % housing built in last 20 years
# .	Gentrifying: 
#### 1) change in % college-educated > median increase in % college-educated for MSA; AND 
#### 2) any increase in median housing value 


# gentrifiable
gentrifiable_fm <-
  foreach (
    i = 1:length(alldat)) %do% { # create a list with dataframes for each dataset
      gentdat <- alldat[[i]] # grab the dataset
      out2 <- 
        foreach (
          j = 1:(length(years)-1), # for each baseline year
          .combine = cbind.data.frame) %do% {
            
            # below median (or 40th p) of msa inc
            # get name of variable-year needed
            hincvar <- 
              paste0("hinc", years[j]) 
            # grab the msa median for the variable
            hinc50 <- 
              msamedians[[i]][, c(1, which(names(msamedians[[i]]) %in% hincvar))]
            # name variables with "msa" prefix
            names(hinc50) <- c("metdiv", "msa_hinc")
            # grab the variable-year in the dataset + msa id
            hinc <- 
              gentdat[, which(names(gentdat) %in% c("metdiv", hincvar))]
            # merge tract variable-year with the msa medians
            hinc <- left_join(hinc, hinc50)
            
            # create a dummy indicator if below the threshold
            hinc50dum <-  
              # if missing values for the tract or msa median, assign NA value, 
              # otherwise assign it a 1 if less than the MSA median, and 0 if not
              ifelse( 
                is.na(hinc[, 2]) | 
                  is.na(hinc[, 3]), 
                NA, 
                ifelse(
                  hinc[, 2] < 
                    hinc[, 3], 
                  1, 0)
              )
            # clean up
            rm(hincvar, hinc50, hinc)
            
            # construct the same dummy indicator as above for 
            # new build less than median (or 40th p)
            p20youngvar <- 
              paste0("p20young", years[j])
            p20young50 <- 
              msamedians[[i]][, c(1, which(names(msamedians[[i]]) %in% p20youngvar))]
            names(p20young50) <- c("metdiv", "msa_p20young")
            p20young <- 
              gentdat[, which(names(gentdat) %in% c("metdiv", p20youngvar))]
            p20young <- left_join(p20young, p20young50)
            p20young50dum <-   
              ifelse(
                is.na(p20young[, 2]) | 
                  is.na(p20young[, 3]), 
                NA, 
                ifelse(
                  p20young[, 2] < 
                    p20young[, 3], 
                  1, 0)
              )
            rm(p20young50, p20youngvar, p20young)
            
            # create a gentrifiable dummy for medians 
            # if the hinc or p20young dummies are missing, 
            ## assign it NA values, 
            
            # replicate for tract_year
            #tractvar <- 
            #paste0("tract", years[j])
            #tractdum <- gentdat[, which(names(gentdat) %in% c(tractvar))]
            
            dum50 <-
              ifelse((is.na(hinc50dum) | 
                        is.na(p20young50dum)), NA, 
                # otherwise, if in the central city, below the hinc threshold and p20young threshold, 
                ## assign it a 1, otherwise 0  
                ifelse(
                    hinc50dum %in% 1 & 
                    p20young50dum %in% 1, 
                  1, 0))

            # combine columns of each of the gentrifiable dummy indicators
            out <- as.data.frame(dum50)
            # rename columns with "fm_able" prefix
            names(out) <- 
              paste("fm_able", names(out), "_", years[j], sep = "")
            # clean up
            rm(dum50, hinc50dum, p20young50dum)
            return(out)
          }
      # combine tract IDs with the variables
      out2 <- cbind.data.frame(gentdat[, 1], out2)
      names(out2)[1] <- names(gentdat)[1]
      return(out2)
    }

# merge with alldatgent 
foreach (i = 1:length(alldat)) %do% {
  alldatgent[[i]] <-
    left_join(alldatgent[[i]], 
              gentrifiable_fm[[i]])
  return(NULL)
}

# clean up 
rm(i, j, out, out2, gentdat, gentrifiable_fm)

# create the gentrifying dummy indicator 
# tracts are gentrifying by freeman's measure if: 
# gentrifiable +  
#### 1) change in % college-educated > median change in % college-educated for MSA; AND 
#### 2) any increase in median housing value 
gentdum_fm <- 
  foreach ( 
    i = 1:length(alldat)) %do% { # create a list for each dataset
      gentdat <- alldat[[i]] # grab the dataset
      # create a dataframe with columns for each time interval and tracts as rows
      gentdum <- 
        foreach (
          j = 1:(length(years)-1), # for each baseline year
          .combine = cbind.data.frame) %do% {
            # grab the tract id and gentrifiable dummies for the baseline year from the gentrification datasets
            gent_abledum <- 
              alldatgent[[i]][, 
                              c(1, which(grepl(paste0("_", years[j], sep= ""), names(alldatgent[[i]])) & 
                                           grepl("fm_abledum", names(alldatgent[[i]]))))
                              ]
            
            # calculate median increase in pcol (% college-educated)
            # get the variable-change years needed
            pcolvar <- 
              paste0("pcol", years[j], years[j + 1])
            # grab the tract id, msa ID, and variable-change years from the tract dataset 
            pcolvar <- 
              gentdat[, c(1, which(names(gentdat) %in% 
                                     c("metdiv", pcolvar)))]
            # for each msa, calculate the median change over time among tracts that have increases
            msamed <- 
              as.data.frame(as.vector(
                by(pcolvar[pcolvar[, 3] > 0, 3], 
                   pcolvar[pcolvar[, 3] > 0, 2], 
                   median, na.rm = T))) 
            # name the column with "med" suffix for median
            names(msamed) <- 
              paste0("pcol", years[j], years[j + 1], "_med")
            # add msa ID
            msamed$metdiv <- sort(unique(pcolvar[pcolvar[, 3] > 0, 2]))
            # merge with variable data
            pcolvar <- left_join(pcolvar, msamed)
            
            # calculate dummy variable if above the change threshold for the variable
            pcolvar$pcol_dum <- 
              # if missing either the value for the variable or the median msa threshold, 
              ## assign it an NA value
              ifelse(
                is.na(pcolvar[, 3]) |
                  is.na(pcolvar[, 4]), NA, 
                # if greater than the threshold, assign it a 1, otherwise 0
                ifelse(pcolvar[, 3] > pcolvar[, 4], 
                       1, 0)
              )
            
            # for median home value (mhmval), 
            # create a dummy indicator if there is any increase in value 
            
            # get the variable-change years name
            mhmvalvar <- 
              paste0("mhmval", years[j], years[j + 1])
            # grab the variable
            mhmvalvar <- 
              gentdat[, c(1, which(names(gentdat) %in% 
                                     c("metdiv", mhmvalvar)))]
            # create a dummy variable if there is an increase
            ## assign it a NA is missing the variable-change years
            mhmvalvar$mhmval_dum <- 
              ifelse(
                is.na(mhmvalvar[, 3]), 
                NA, 
                ifelse(mhmvalvar[, 3] > 0, 
                       1, 0))
            # merge all data: gentrifiable dummies and gentrifying dummies
            dat <- left_join(gent_abledum, pcolvar)
            dat <- left_join(dat, mhmvalvar)
            
            # create a dummy indicator for if gentrifying based on medians
            out50 <- 
              # Assign NA if not gentrifiable, if gentrifiable variable is NA, 
              ## or if the variable dummies are missing
              ifelse(
                dat[, 2] %in% 0 |
                  is.na(dat[, 2]) | 
                  is.na(dat$pcol_dum) | 
                  is.na(dat$mhmval_dum), 
                NA, 
                # assign it a 1 if the pcol and mhmval dummies are 1, otherwise 0.
                ifelse(
                  dat$pcol_dum %in% 1 & 
                    dat$mhmval_dum %in% 1, 
                  1, 
                  0
                )
              )
            # combine the dummies into a single data frame
            out <- as.data.frame(out50)
            # name them with "gentdum" prefix and the time interval years
            names(out) <- 
              c(paste0("fm", c("50"), "_gentdum_",  
                       years[j], years[j+1]))
            return(out)
          }
      # add tract IDs 
      out2 <- cbind.data.frame(gentdat[, 1], gentdum)
      names(out2)[1] <- names(gentdat)[1]
      return(out2)
    }

# merge with alldatgent 
foreach (i = 1:length(alldat)) %do% {
  alldatgent[[i]] <-
    left_join(alldatgent[[i]], 
              gentdum_fm[[i]])
  return(NULL)
}

# clean up 
rm(i, j, out50, out, out2, pcolvar, mhmvalvar, 
   gentdum, gent_abledum, dat, msamed, gentdum_fm, gentdat)

###########################################################################################
# ding et al measure - remove central city restriction, use MSA threshold, binary only ####
###########################################################################################
# .	Measure characteristics: threshold; binary (and categorical); % college-educated and median housing value or rent
# .	Gentrifiable: 
##  1) central city - removed
##  2) tract median household income < msa median household income; 
##  2a) tract median % col & inc & median rent & home value < msa median
# .	Gentrifying: 
##  1) tract change in % college-educated (or median household income) > MSA median increase in % college-educated (or median household income); 
##  AND 2) tract % increase in home value > MSA median % increase in home value OR tract % increase in rent > city (or MSA) median % increase in rent 
##  1a) only % college-educated 

# gentrifiable 
gentrifiable_dhd <-
  foreach (
    i = 1:length(alldat)) %do% { # create a list with a dataframe for each dataset
      gentdat <- alldat[[i]] # grab the dataset
      # create a dataframe with columns with dummy indicators for if gentrifiable
      ## for each tract (rows)
      out2 <- 
        foreach (
          j = 1:(length(years)-1), # for each baseline year
          .combine = cbind.data.frame) %do% {
            # create a dummy indicator if below median of city or msa for household income (hinc)
            # get variable name for baseline year
            hincvar <- 
              paste0("hinc", years[j])
            
            # create the same dummy indicator as above for if below the msa median household income
            hinc50m <- 
              msamedians[[i]][, c(1, which(names(msamedians[[i]]) %in% hincvar))]
            names(hinc50m) <- c("metdiv", "msa_hinc")
            hinc <- 
              gentdat[, which(names(gentdat) %in% c("metdiv", hincvar))]
            hinc <- left_join(hinc, hinc50m)
            hinc50mdum <-   
              ifelse(
                is.na(hinc[, 2]) | 
                  is.na(hinc[, 3]), 
                NA, 
                ifelse(
                  hinc[, 2] < 
                    hinc[, 3], 
                  1, 0)
              )
            rm(hincvar, hinc50m, hinc)
            
            # create gentrifiable dummy indicators for msa median
            # create same dummy as above for msa thresholds
            dum50m <-
              ifelse(is.na(hinc50mdum), NA, 
                ifelse(
                    hinc50mdum %in% 1, 
                  1, 0))
            # combine the gentrifiable dummies into a single dataframe
            out <- as.data.frame(dum50m)
            # name them with "dhd_able" prefix
            names(out) <- 
              paste("dhd_able", names(out), "_", years[j], sep = "")
            # clean up
            rm(dum50m, hinc50mdum)
            return(out)
          }
      # add tract ids to dataset
      out2 <- cbind.data.frame(gentdat[, 1], out2)
      names(out2)[1] <- names(gentdat)[1]
      return(out2)
    }

# merge with alldatgent 
foreach (i = 1:length(alldat)) %do% {
  alldatgent[[i]] <-
    left_join(alldatgent[[i]], 
              gentrifiable_dhd[[i]])
  return(NULL)
}

# clean up 
rm(i, j, out, out2, gentdat, gentrifiable_dhd)


# create a gentrifying binary measure 
# tracts are gentrifying if they are: 
# gentrifiable +  
#### 1) change in pcol (or hinc) > median increase in pcol (or hinc) for msa AND 
#### 2) change in median % increase in home value (or rent) > median % increase in mhmval or rent for msa
gentdum_dhd <- 
  foreach (
    i = 1:length(alldat)) %do% { # create a list with a dataframe for each dataset
      gentdat <- alldat[[i]] # grab the dataset
      gentdum <- 
        # create a dataframe with dummy variables for if gentrifying (as columns)
        # for each tract (as rows)
        foreach (
          j = 1:(length(years)-1), # for each baseline year 
          .combine = cbind.data.frame) %do% {
            # grab the gentrifiable dummy from the gentrifiable dataset 
            gent_abledum <- 
              alldatgent[[i]][, 
                              c(1, 
                                which(grepl(paste0("_", years[j], sep= ""), names(alldatgent[[i]])) & 
                                      (grepl("dhd_abledum", names(alldatgent[[i]])) |
                                        grepl("dhdalt_abledum", names(alldatgent[[i]])))))
                              ]
            
            # create dummies if above the median incr in pcol for msa
            
            # create a dummy indicator as above for msa median incr
            # get variable name and grab the tract id, msa id, and variable-change years needed
            pcolvarm <- 
              paste0("pcol", years[j], years[j + 1])
            pcolvarm <- 
              gentdat[, c(1, which(names(gentdat) %in% 
                                     c("metdiv", pcolvarm)))]
            # calculate msa median changes among tracts with increases only
            msamed <- 
              as.data.frame(as.vector(
                by(pcolvarm[pcolvarm[, 3] > 0, 3], 
                   pcolvarm[pcolvarm[, 3] > 0, 2], 
                   median, na.rm = T))) 
            names(msamed) <- 
              paste0("pcol", years[j], years[j + 1], "_med")
            msamed$metdiv <- sort(unique(pcolvarm[pcolvarm[, 3] > 0, 2]))
            pcolvarm <- left_join(pcolvarm, msamed)
            pcolvarm$pcolm_dum <- 
              ifelse(
                is.na(pcolvarm[, 3]) |
                  is.na(pcolvarm[, 4]), NA, 
                ifelse(pcolvarm[, 3] > pcolvarm[, 4], 
                       1, 0)
              )
            
            # create dummies if above the median incr in hinc for msa

            # create the same dummy indicator as above but based on the msa increases
            # above msa median incr
            hincvarm <- 
              paste0("hinc", years[j], years[j + 1])
            hincvarm <- 
              gentdat[, c(1, which(names(gentdat) %in% 
                                     c("metdiv", hincvarm)))]
            msamed <- 
              as.data.frame(as.vector(
                by(hincvarm[hincvarm[, 3] > 0, 3], 
                   hincvarm[hincvarm[, 3] > 0, 2], 
                   median, na.rm = T))) 
            names(msamed) <- 
              paste0("hinc", years[j], years[j + 1], "_med")
            msamed$metdiv <- sort(unique(hincvarm[hincvarm[, 3] > 0, 2]))
            hincvarm <- left_join(hincvarm, msamed)
            hincvarm$hincm_dum <- 
              ifelse(
                is.na(hincvarm[, 3]) |
                  is.na(hincvarm[, 4]), NA, 
                ifelse(hincvarm[, 3] > hincvarm[, 4], 
                       1, 0)
              )
            
            # construct similar indicators as above for increases in median home values (mhmval)
          
            # above msa median incr
            mhmvalvarm <- 
              paste0("mhmval", years[j], years[j + 1])
            mhmvalvarm <- 
              gentdat[, c(1, which(names(gentdat) %in% 
                                     c("metdiv", mhmvalvarm)))]
            msamed <- 
              as.data.frame(as.vector(
                by(mhmvalvarm[mhmvalvarm[, 3] > 0, 3], 
                   mhmvalvarm[mhmvalvarm[, 3] > 0, 2], 
                   median, na.rm = T))) 
            names(msamed) <- 
              paste0("mhmval", years[j], years[j + 1], "_med")
            msamed$metdiv <- sort(unique(mhmvalvarm[mhmvalvarm[, 3] > 0, 2]))
            mhmvalvarm <- left_join(mhmvalvarm, msamed)
            mhmvalvarm$mhmvalm_dum <- 
              ifelse(
                is.na(mhmvalvarm[, 3]) |
                  is.na(mhmvalvarm[, 4]), NA, 
                ifelse(mhmvalvarm[, 3] > mhmvalvarm[, 4], 
                       1, 0)
              )
            
            # construct similar dummy indicators as above for median rent values (mrent)
            
            # for above msa median incr
            mrentvarm <- 
              paste0("mrent", years[j], years[j + 1])
            mrentvarm <- 
              gentdat[, c(1, which(names(gentdat) %in% 
                                     c("metdiv", mrentvarm)))]
            msamed <- 
              as.data.frame(as.vector(
                by(mrentvarm[mrentvarm[, 3] > 0, 3], 
                   mrentvarm[mrentvarm[, 3] > 0, 2], 
                   median, na.rm = T))) 
            names(msamed) <- 
              paste0("mrent", years[j], years[j + 1], "_med")
            msamed$metdiv <- sort(unique(mrentvarm[mrentvarm[, 3] > 0, 2]))
            mrentvarm <- left_join(mrentvarm, msamed)
            mrentvarm$mrentm_dum <- 
              ifelse(
                is.na(mrentvarm[, 3]) |
                  is.na(mrentvarm[, 4]), NA, 
                ifelse(mrentvarm[, 3] > mrentvarm[, 4], 
                       1, 0)
              )
            # merge all data: gentrifiable variables, and msa and city change dummies 
            dat <- left_join(gent_abledum, pcolvarm[, c(1:2, 5)])
            dat <- left_join(dat, hincvarm[, c(1, 5)])
            dat <- left_join(dat, mhmvalvarm[, c(1, 5)])
            dat <- left_join(dat, mrentvarm[, c(1, 5)])
            
            # clean up 
            rm(gent_abledum,
               pcolvarm, hincvarm, mhmvalvarm, mrentvarm)
            
            # create dummy indicators for if gentrifying for city and msa thresholds in the period
          
            # same indicators as above based on msa median thresholds
            outm <- 
              ifelse(
                dat[, 3] %in% 0 |
                  is.na(dat[, 3]) | 
                  (is.na(dat$pcolm_dum) & 
                     is.na(dat$hincm_dum)) | 
                  (is.na(dat$mrentm_dum) & 
                     is.na(dat$mhmvalm_dum)), 
                NA, 
                ifelse(
                  (dat$pcolm_dum %in% 1 |
                     dat$hincm_dum %in% 1) & 
                    (dat$mhmvalm_dum %in% 1 | 
                       dat$mrentm_dum %in% 1), 
                  1, 
                  0
                )
              )

            # create alternative gentrifying variable for just rv/e (no income)
            # msa
            outm_rve <- 
              ifelse(
                dat[, 3] %in% 0 |
                  is.na(dat[, 3]) | 
                  (is.na(dat$pcolm_dum)) | 
                  (is.na(dat$mrentm_dum) & 
                     is.na(dat$mhmvalm_dum)), 
                NA, 
                ifelse(
                  (dat$pcolm_dum %in% 1) & 
                    (dat$mhmvalm_dum %in% 1 | 
                       dat$mrentm_dum %in% 1), 
                  1, 
                  0
                )
              )
            # combine the dummy indicators
            out <- 
              cbind.data.frame(outm, outm_rve)
            # name the variables with "dhd_gentdum" prefix and 
            # indicate if based on city or msa median
            # add interval years as suffix
            names(out) <- 
              c(paste0("dhd_gentdum", c("m"), 
                       "_", years[j], years[j+1]),
                paste0("dhd_rve_gentdum", c("m"), 
                       "_", years[j], years[j+1]))
            return(out)
          }
      # add tract ids
      out2 <- cbind.data.frame(gentdat[, 1], gentdum)
      names(out2)[1] <- names(gentdat)[1]
      return(out2)
    }

# merge with alldatgent 
foreach (i = 1:length(alldat)) %do% {
  alldatgent[[i]] <-
    left_join(alldatgent[[i]], 
              gentdum_dhd[[i]])
  return(NULL)
}

# clean up 
rm(i, j, out, out2, outm, outm_rve,
   gentdum, dat, msamed, gentdum_dhd)

return(alldatgent[[1]])
}
# Save Results ------------------------------------------------------------

# merge list output into single data frame
gent_measures_allmsa <- 
  full_join(gent_measures_allmsa[[1]], gent_measures_allmsa[[2]])

##save measures
filenames <- 
  paste0("gent_measures_907a_allmsa.csv")
write.csv(gent_measures_allmsa, 
          file = paste0(savedir, filenames), 
          row.names = F, 
          na = '')

