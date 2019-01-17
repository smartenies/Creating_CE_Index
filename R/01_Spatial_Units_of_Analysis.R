#' =============================================================================
#' Date created: January 14, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script creates objects for the census tracts used in the analysis
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


#' =============================================================================
#' Census tract shapefiles and spatial objects for R
#' =============================================================================

#' Specify the geodatabase name and output name
acs_gdb_name <- "ACS_2014_5YR_TRACT_08_COLORADO.gdb"
acs_output_name <- "CO_Tracts_AEA.csv"

#' get shapefile and project to Albers Equal Area
acs_units <- st_read(dsn = here::here("Data/ACS_Data", acs_gdb_name),
                     layer = str_remove(acs_gdb_name, ".gdb"),
                     stringsAsFactors = F) %>%
  st_transform(crs=albers) %>% 
  
  #' Just Denver county for now
  filter(COUNTYFP == "031") %>% 
  select(GEOID, COUNTYFP)
plot(st_geometry(acs_units))

st_write(acs_units, here::here("Data", acs_output_name),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
















