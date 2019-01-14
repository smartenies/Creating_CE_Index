#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: May 22, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes the toxic release inventory (2010) for the state of CO
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
#' Read in the TRI data for CO 2010-2014
#' Downloaded from https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016
#' Data Dictionary: https://www.epa.gov/sites/production/files/2016-11/documents/tri_basic_data_file_format_v15.pdf
#' -----------------------------------------------------------------------------

tri_2010 <- read.csv("./Data/TRI_Data/TRI_2010_CO.csv", header=T,
                     stringsAsFactors = F)
tri_2011 <- read.csv("./Data/TRI_Data/TRI_2011_CO.csv", header=T,
                     stringsAsFactors = F)
tri_2012 <- read.csv("./Data/TRI_Data/TRI_2012_CO.csv", header=T,
                     stringsAsFactors = F)
tri_2013 <- read.csv("./Data/TRI_Data/TRI_2013_CO.csv", header=T,
                     stringsAsFactors = F)
tri_2014 <- read.csv("./Data/TRI_Data/TRI_2014_CO.csv", header=T,
                     stringsAsFactors = F)

tri <- bind_rows(tri_2010, tri_2011, tri_2012, tri_2013, tri_2014) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = ll_wgs84) %>%
  st_transform(crs=albers)
plot(st_geometry(tri))

#' Remove individual years
rm(tri_2010, tri_2011, tri_2012, tri_2013, tri_2014)

#' convert measurements in grams to pounds
#' convert measurements in pounts to tons
unique(tri$UNIT_OF_MEASURE)

g_to_lb <- 0.00220462
lb_to_ton <- 0.0005

tri <- tri %>%
  mutate(TOTAL_LBS = ifelse(UNIT_OF_MEASURE == "Pounts", TOTAL_RELEASES, 
                            TOTAL_RELEASES * g_to_lb),
         TOTAL_TONS = TOTAL_LBS * lb_to_ton)

summary(tri$TOTAL_TONS)

#' Total emissions by classification and carcinogenicity
#' TRI = regular TRI chemical, PBT = persistent bioaccumulative toxin, 
#' Dioxin = dioxin or dioxin-like chemicals

tri_totals <- tri %>%
  group_by(CLASSIFICATION, YEAR) %>%
  summarize(total_ppy = sum(TOTAL_LBS),
            total_tpy = sum(TOTAL_TONS))

#' Aggregate emissions at each facility
tri_by_facility <- tri %>%
  group_by(TRI_FACILITY_ID, FACILITY_NAME, CLASSIFICATION) %>%
  summarize(total_ppy = sum(TOTAL_LBS),
            total_tpy = sum(TOTAL_TONS)) 
  
st_write(tri_by_facility, here::here("Data", "TRI_Data_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
