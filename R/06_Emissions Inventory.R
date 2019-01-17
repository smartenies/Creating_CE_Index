#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: May 22, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes the emissions inventory data (2016) provided by DDPHE
#' 
#' For other areas, this will be NEI data
#' 
#' NOTE: don't forget the ./ before the directory when reading in files!
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)


albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#' -----------------------------------------------------------------------------
#' Read in the emissions inventory data
#' -----------------------------------------------------------------------------

nei_inventory_name <- "2014v2facilities.csv"
nei_output_name <- "NEI_2014v2_AEA.csv"

#' Get shapefile and project to Albers Equal Area
#' Depending on the study area, will need to change this
unit_name <- "CO_Tracts_AEA.csv"
spatial_units <- read_csv(here::here("Data", unit_name)) %>%
  st_as_sf(wkt = "WKT", crs = albers)
plot(st_geometry(spatial_units))

#' 10 km buffer around the grid (to account for buffers around monitoring pts)
spatial_bound <- st_buffer(st_union(spatial_units), dist = 10000)

nei <- read_csv(here::here("Data/NEI_Data", nei_inventory_name)) %>% 
  
  #' Filter by state
  filter(st_usps_cd == "CO")

#' Subset to the criteria air pollutants
unique(nei$pollutant_cd)
caps <- c("VOC", "EC", "NO3", "OC", "PM-CON", "PM10-FIL", "PM10-PRI", "7439921",
          "PM25-FIL", "PM25-PRI", "PMFINE", "SO4", "NOX", "SO2", "CO", "NH3")

nei2 <- filter(nei, pollutant_cd %in% caps) %>% 
  filter(!is.na(longitude_msr)) %>% 
  st_as_sf(., coords = c("longitude_msr", "latitude_msr"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)

nei2 <- nei2[spatial_bound ,]
plot(st_geometry(nei2))

#' Is this a major source for that pollutant?
#' Using thresholds listed in "priority_facility_list_for2014.csv"
nei2 <- nei2 %>% 
  mutate(major_source = ifelse(pollutant_cd == "VOC" & total_emissions >= 100, 1, 
                               ifelse(pollutant_cd == "7439921" & total_emissions > 0, 1, 
                                      ifelse(pollutant_cd == "NOX" & total_emissions >= 750, 1,
                                             ifelse(pollutant_cd == "PM10-FIL" & total_emissions >= 50, 1,
                                                    ifelse(pollutant_cd == "PM10-PRI" & total_emissions >= 100, 1,
                                                           ifelse(pollutant_cd == "PM25-FIL" & total_emissions >= 50, 1,
                                                                  ifelse(pollutant_cd == "PM25-PRI" & total_emissions >= 100, 1,
                                                                         ifelse(pollutant_cd == "SO2" & total_emissions >= 7500, 1,
                                                                                0)))))))))

plot(st_geometry(filter(nei2, pollutant_cd == "PM25-PRI")), pch = 16,
     main="NEI Sites: Primary PM2.5", col=as.factor(nei2$major_source))
legend("right", legend = c("1" = "Major Source (2014 Thresholds)",
                           "2" = "Non-major Source"), 
       col = 1:2, cex = 0.8, pch = 16, title = "Source Category")

st_write(nei2, here::here("Data", nei_output_name),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

