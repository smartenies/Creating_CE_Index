#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: May 24, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script summarizes the social variables at the census tract level
#' to be used in the cumulative exposure index
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
library(viridis)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Create the data frame to hold all of the census tract variables

#' Get shapefile and project to Albers Equal Area
#' Depending on the study area, will need to change this
unit_name <- "CO_Tracts_AEA.csv"
spatial_units <- read_csv(here::here("Data", unit_name)) %>%
  st_as_sf(wkt = "WKT", crs = albers)
plot(st_geometry(spatial_units))

ct_soc <- spatial_units

rm(spatial_units)
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Sensitive populations: see 10_Health Outcome Rates.R
#' -----------------------------------------------------------------------------

hosp_rates <- read_csv(here::here("Data", "Hospitalizations_AEA.csv")) %>% 
  select(GEOID, cvd_rate_adj, res_rate_adj, cpm_rate_adj)

ct_soc <- left_join(ct_soc, hosp_rates, by="GEOID")

#' -----------------------------------------------------------------------------
#' ACS Variables: see 2_ACS Variables.R
#' -----------------------------------------------------------------------------

acs <- read_csv(here::here("Data", "ACS_AEA.csv")) %>% 
  select(GEOID, pop_dens, contains("pct_"), med_income)

ct_soc <- left_join(ct_soc, acs, by="GEOID")

#' -----------------------------------------------------------------------------
#' Crime rates: 3_Crime Statistics.R
#' Spatially join using the CT centroid
#' -----------------------------------------------------------------------------

crime_rates <- read_csv(here::here("Data", "Crime_Rates_AEA.csv")) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  select(crime_rate, violent_rate, property_rate) %>%
  rename(all_crime_rate = crime_rate, 
         violent_crime_rate = violent_rate,
         property_crime_rate = property_rate)

ct_crime <- st_centroid(ct_soc) %>%
  select(GEOID) %>%
  st_join(crime_rates) %>%
  st_set_geometry(NULL)

ct_soc <- left_join(ct_soc, ct_crime, by="GEOID")

st_write(ct_soc, here::here("Data", "CT_SOC.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

