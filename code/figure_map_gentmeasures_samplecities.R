# -------------------------------------------------------------------------
# Created by: Jackie Hwang                          
# Date created: Apr 22, 2021              
# Last revised: May 5, 2023, Iris Zhang                
# Project: PSID Gentrification-Displacement         
# Subproject: Racial Strat  
# Re: Create maps of gentrification measures       
# -------------------------------------------------------------------------

# Script Description: ----------------------------------------------------------

# This script creates maps for Figure 1 in the racial strat draft. The 
# measures files is "gent_measures_707a_2010b.csv" and was created by merging 
# together datasets for decades (in the input folder).
# These files were originally created by Iris Zhang and were updated by JH. 
# All files to create these measures are in the PSID gentrification measures folder.

# Log updates:
# 04/22/21: NA
# 07/27/22: create map for RVE measure only
# 05/05/22: create map for RVE_MSA measure only, and then RVE (with ccflag == 1) and FM50_MSA (without ccflag == 1) in a separate plot.

# Setup: -----------------------------------------------------------------------

# Clear environment
rm(list=ls())

# Packages: 
library('readr')
library('dplyr')
library('foreach')
library('openxlsx')
library('sp')
library('rgdal')
library('ggmap')
library('sf')
# library('rgeos')
# library('RColorBrewer')
# library('maps')
# library('mapdata')
# library('devtools')
# library('proj4')
# library('RgoogleMaps')
library('ggplot2')
library('gridExtra')


# Set working directory: 
homedir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(paste0(homedir, "/.."))
mapdir <- "~/Google Drive/My Drive/Stanford/PROJECT FOLDER_Gentrification and PSID Displacement/Gentrification Measures_RA"

# Import data:

# Measures in 2010 census boundaries
gentdat10 <- read_csv("misc_data/gent_measures_707a_2010b.csv") 
gentdat10_msa <- read_csv("misc_data/gent_measures_907a_allmsa.csv")

names(gentdat10)
names(gentdat10_msa)

gentdat10 %<>%
  left_join(gentdat10_msa %>% select(trtid10, fm50_gentdum_007a, dhd_rve_gentdumm_007a)) %>%
  select(trtid10, metdiv, placefp, ccflag, dhd_rve_gentdumc_007a.2000, fm50_gentdum_007a, dhd_rve_gentdumm_007a) %>%
  rename(rve_msa_gentdum_007a.2000 = dhd_rve_gentdumm_007a,
         rve_city_gentdum_007a.2000 = dhd_rve_gentdumc_007a.2000,
         fm50_gentdum_007a.2000 = fm50_gentdum_007a)

names(gentdat10)
rm(gentdat10_msa)

# These measures are based on tracts in metro areas with populations > 100 and 
# housing units > 50 at the beginning and end of each relevant decade.

# check files for 74,0001 unique tracts
length(unique(gentdat10$trtid10))

# Census tract shapefile 
setwd(mapdir)
shapefile <- sf::st_read("Basemaps", "US_tract_2010")

# Mapping API key (needed for Google Maps): 
register_google("AIzaSyCO-hk4AjUgTdMKDuv18f66py8NIdrf4qU")

# Parameters: 

# vectors for gent measures and years 
measures <- c("rve_city", "fm50")
measures_names <- c("HW City Gentrification Measure", "FM Gentrification Measure")
measures_rve <- c("rve_msa_gentdum")
measures_rve_names <- c("HW Gentrification Measure")
gentyears <- c("2000")
gentyears_names <- c("2000-2017")

# vector for gent categories
cats <- c("citygent", "citynogent", "citynongent")

years <- c("00", "7a") # data years

# create vectors for sample cities to map
# Metdivs: 
## Detroit #19804, Philadelphia #37964 San Francisco #41884
# Place IDs:
## Detroit #22000, Philadelphia #60000 San Francisco #67000
cities <- c("det", "phl", "sfo")
metdivs_id <- c(19804, 37964, 41884)
cities_id <- c(22000, 60000, 67000)
cities_names <- c("Detroit, MI", "Philadelphia, PA", "San Francisco, CA")
cities_zoom <- c(11, 11, 12)
msa_names <- c("Detroit-Livonia-Dearborn, MI", "Philadelphia, PA", "San Francisco-San Mateo-Redwood City, CA")

# Main Script -------------------------------------------------------------

# Map Measures in Sample Cities #-----------------------------------------------
# create panels of maps for each measures for 2000-2017 in sample cities
# sample cities (looseness, size): Detroit, Philly, SF
# For 2010 boundaries only

# get data for needed cities
gentdat <-   
  gentdat10 %>% filter(metdiv %in% metdivs_id) %>%
  mutate(msa_name = case_when(metdiv == 19804 ~ "Detroit-Livonia-Dearborn, MI", 
                         metdiv == 37964 ~ "Philadelphia, PA",
                         metdiv == 41884 ~ "San Francisco-San Mateo-Redwood City, CA"))

# create list with each city's data
gentdat_city <- 
  foreach (i = 1:length(cities)) %do% {
    gentdat %>% filter(metdiv %in% metdivs_id[i])
  }
names(gentdat_city) <- msa_names

# create shapefiles for each city 
# reduce file size to tracts needed
dat_sp <- subset(shapefile, shapefile$GEOID10S %in% gentdat$trtid10)

# Transform to Google Map Coordinates 
dat_sp <- sf::st_transform(dat_sp, CRS("+proj=longlat +datum=WGS84"))

# Create shapefile for each city (divide shapefiles into each city)
plots <- 
  foreach (i = 1:length(gentdat_city)) %do% {
    subset(dat_sp, dat_sp$GEOID10S %in% as.character(gentdat_city[[i]]$trtid10))
  }
names(plots) <- msa_names

all_map_dat = rbind(plots[[1]],
                    plots[[2]],
                    plots[[3]])

# add google maps for each city 

# get center coordinates of each city
centers <- 
  foreach (i = 1:length(cities)) %do% {
    sf::st_coordinates(
      sf::st_centroid(sf::st_union(plots[[msa_names[i]]]))
    )
  }
names(centers) <- msa_names

# get maps for each city
gmaps <- 
  foreach(i = 1:length(cities)) %do% {
    gmap <- 
      get_map(
        c(lon=centers[[i]][1], lat=centers[[i]][2]),
        zoom = cities_zoom[i], 
        maptype = "roadmap", 
        source = "google", 
        color = "bw", 
        alpha = 0.1)
    # convert to google map
    gmap <- ggmap(gmap)
    return(gmap)
  }
names(gmaps) <- msa_names

# clean up 
rm(gmap, i)

# plot all gent measures for each city by decade
# write script that results in 2x3 panels of each measure and only includes dhd_rve
# for each decade for each city

# set directory
setwd(paste0(homedir, "/.."))

alldat <- gentdat

plot_dat = all_map_dat %>% 
  left_join(alldat, 
            by = c("GEOID10" = "trtid10"))
plot_dat <- st_transform(plot_dat, CRS("+proj=longlat +datum=WGS84"))

## Create maps ----
maps <- list()

# Map Set 1: Maps of gentrifying/gentrifiable tracts
for (measure in measures) {
  for (city in cities) {
    mapname <- paste0(measure, "_", city)
    # create map
    map <-
      gmaps[[city]] +
      geom_sf(
        data = plot_dat %>% filter(city_label == city) %>% rename("var" = measure),
        aes(fill = var),    
        alpha = .9,
        size = 0.1,
        color = 'grey45',
        inherit.aes = F) +
      ggtitle(city) + 
      map_theme
    
    # Colors for continous measure map
    if (measure == "gent_cont_002a") {
      map <- map +
        scale_fill_gradient(
          low = "lightpink", 
          high = "darkred",
          na.value = "white") +
        guides(fill = guide_colorbar(barwidth = 10))
      
      # Colors for cat measures
    } else {
      map <- map +
        scale_fill_manual(
          values = gentcolors,
          limits = force,
          guide = "legend")
    }
    maps[[mapname]] <- map
  }
}

for (measure in measures) {
  
  measure_name <- names(measures)[measures == measure]
  map_names <- paste0(measure, "_", cities)
  
  plotlist <- maps[names(maps) %in% map_names]
  
  map_panel <- ggpubr::ggarrange(
    plotlist = plotlist,
    common.legend = T,
    legend = "bottom",
    ncol = 3,
    nrow = 1
  ) %>%
    
    annotate_figure(
      top = text_grob(measure_name,
                      color = "black", 
                      face = "bold", 
                      size = 8)
    )
  
  filename <- paste0("gent-maps/", 
                     measure,
                     ".png")
  
  ggsave(filename = filename, 
         plot = map_panel, 
         width = 8.5, height = 3.5)
}
# write a loop to produce maps: 
# for each city
# for each period
# for each measure 
plots_out <- 
foreach (
  i = 1:length(cities)
) %do% {
  # create fortified data for each measure (except dhd) for each year
  foreach(
    k = 1:length(gentyears)
  ) %do% {
      foreach(
        j = c(1:length(measures))  
      ) %do% {
        # id measure and year
          col <- gentdat_city[[i]] %>% select(which(
            grepl(paste0(measures[j], "_gentdum"), names(alldat)) & 
              grepl(paste0(".", gentyears[k]), names(alldat))))
        # fortify data with this measure and year to assign categories for mapping
        dat_out <-
          foreach(l = c(0, 1),
                  .combine = rbind.data.frame) %do% {
                    out <-
                      fortify(subset(plots[[i]],
                                     plots[[i]]$GEOID10S %in%
                                       gentdat_city[[i]]$trtid10[
                                         which(col == l)]))
                    if (nrow(out) != 0) {
                      out$cat <- as.character(l)}
                    return(out)
                  }
        # add NA (nongentrifiable) to dataset
        out <-
          fortify(subset(plots[[i]],
                         plots[[i]]$GEOID10S %in%
                           gentdat_city[[i]]$trtid10[
                             which(is.na(col))]))
        if (nrow(out) != 0) {
          out$cat <- "nongent"}
        dat_out <- rbind.data.frame(dat_out, out)
        # create map 
        map_out <- 
          gmaps[[i]] + 
          geom_polygon(
            aes(x=long, y=lat, group=group),
            fill = NA,
            size = .2,
            color = 'black',
            data = plots[[i]],
            alpha = 1) +
          geom_polygon(
            aes(x = long, y = lat, group = group, fill = cat), 
            data = dat_out, 
            size = .2,
            alpha = .5) + 
          ggtitle(paste0(cities_names[i], "\n", measures_names[j], ", ", 
                         gentyears_names[k])) + 
          theme(plot.title = element_text(hjust = .5, size = 8), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(), 
                axis.text.x = element_blank(), 
                axis.text.y = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.ticks.y = element_blank(), 
                legend.title = element_blank(), 
                legend.position = "bottom", 
                legend.text = element_text(size = 6),
                legend.key.size = unit(0.2, "cm")) + 
          scale_fill_manual(
            breaks = c(0, 1, "nongent"), 
            values = c("pink", "red",  
                       "white"),
            labels = c("Nongentrifying", "Gentrifying", 
                       "Nongentrifiable"), 
            guide = "legend")
        return(map_out)
      }
  }
}
# export maps
maps_out <- 
  arrangeGrob(
    plots_out[[1]][[1]][[1]], 
    plots_out[[1]][[1]][[2]],
    plots_out[[2]][[1]][[1]], 
    plots_out[[2]][[1]][[2]], 
    plots_out[[3]][[1]][[1]], 
    plots_out[[3]][[1]][[2]], 
    nrow = 3
  )
filename <- "maps_hw_fm_samplecities_2000_2017.pdf"
ggsave(filename, plot = maps_out, width = 15, height = 15, dpi = 300)

# RVE_msa only maps ----
# plot RVE gent measures for each city by decade
# write script that results in 1x3 panel
# for each decade for each city

# set directory
setwd(paste0(homedir, "/.."))

alldat <- gentdat

# write a loop to produce maps: 
# for each city
# for each period
# for each measure 
plots_out <- 
  foreach (
    i = 1:length(cities)
  ) %do% {
    # create fortified data for each measure (except dhd) for each year
    foreach(
      k = 1:length(gentyears)
    ) %do% {
      foreach(
        j = c(1:length(measures_rve))  
      ) %do% {
        # id measure and year
        col <- gentdat_city[[i]] %>% select(which(
          grepl(paste0(measures_rve[j]), names(alldat)) & 
            grepl(paste0(".", gentyears[k]), names(alldat))))
        # fortify data with this measure and year to assign categories for mapping
        dat_out <-
          foreach(l = c(0, 1),
                  .combine = rbind.data.frame) %do% {
                    out <-
                      fortify(subset(dat_sp_city[[i]],
                                     dat_sp_city[[i]]$GEOID10S %in%
                                       gentdat_city[[i]]$trtid10[
                                         which(col == l)]))
                    if (nrow(out) != 0) {
                      out$cat <- as.character(l)}
                    return(out)
                  }
        # add NA (nongentrifiable) to dataset
        out <-
          fortify(subset(dat_sp_city[[i]],
                         dat_sp_city[[i]]$GEOID10S %in%
                           gentdat_city[[i]]$trtid10[
                             which(is.na(col))]))
        if (nrow(out) != 0) {
          out$cat <- "nongent"}
        dat_out <- rbind.data.frame(dat_out, out)
        # create map 
        map_out <- 
          gmaps[[i]] + 
          geom_polygon(
            aes(x=long, y=lat, group=group),
            fill = NA,
            size = .2,
            color = 'black',
            data = plots[[i]],
            alpha = 1) +
          geom_polygon(
            aes(x = long, y = lat, group = group, fill = cat), 
            data = dat_out, 
            size = .2,
            alpha = .5) + 
          ggtitle(paste0(cities_names[i], ", ", 
                         gentyears_names[k])) + 
          theme(plot.title = element_text(hjust = .5, size = 8), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(), 
                axis.text.x = element_blank(), 
                axis.text.y = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.ticks.y = element_blank(), 
                legend.title = element_blank(), 
                legend.position = "bottom", 
                legend.text = element_text(size = 6),
                legend.key.size = unit(0.2, "cm")) + 
          scale_fill_manual(
            breaks = c(0, 1, "nongent"), 
            values = c("pink", "red",  
                       "white"),
            labels = c("Nongentrifying", "Gentrifying", 
                       "Nongentrifiable"), 
            guide = "legend")
        return(map_out)
      }
    }
  }
# export maps
maps_out <- 
  arrangeGrob(
    plots_out[[1]][[1]][[1]], 
    plots_out[[2]][[1]][[1]], 
    plots_out[[3]][[1]][[1]], 
    nrow = 1
  )
filename <- "maps_rve_samplecities_2000_2017.pdf"
ggsave(filename, plot = maps_out, width = 15, height = 5, dpi = 300)

rm(list = ls())
