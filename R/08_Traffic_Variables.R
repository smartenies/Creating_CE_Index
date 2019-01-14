#' =============================================================================
#' Date created: January 14, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes the National Highways Performance Measurement System
#' data (also used in the ECHO LUR)
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
#' NHPMS data available from: https://www.fhwa.dot.gov/policyinformation/hpms/shapefiles.cfm
#' -----------------------------------------------------------------------------

#' Specify the geodatabase name and output name
acs_gdb_name <- "ACS_2014_5YR_TRACT_08_COLORADO.gdb"
acs_output_name <- str_replace(acs_gdb_name, ".gdb", ".csv")

#' get shapefile and project to Albers Equal Area
acs_units <- st_read(dsn = here::here("Data/ACS_Data", acs_gdb_name),
                     layer = str_remove(acs_gdb_name, ".gdb")) %>%
  st_transform(crs=albers)
plot(st_geometry(acs_units))

boundary <- st_make_grid(acs_units, n=1) %>%
  st_buffer(., dist = 2000)

#' Read in NHPS 2014 shapefile, project to Albers equal area, clip to grid
nhpms_name <- "Colorado_Sections_2014.shp"

nhpms_f <- st_read(here::here("Data/Traffic_Data", nhpms_name)) %>% 
  st_zm(., drop = T)
#plot(st_geometry(nhpms_f))

nhpms <- st_transform(nhpms_f, crs=albers) %>%
  st_crop(., st_bbox(grid_bound)) %>% 
  mutate(f_system = as.numeric(F_System),
         aadt = as.numeric(AADT)) %>% 
  select(-c(F_System, AADT))

plot(st_geometry(nhpms))

#' F system classifications:
#'    "1" = "Interstates"
#'    "2" = "PA- Freeways"
#'    "3" = "PA- Other"
#'    "4" = "Minor Arterial"
#'    "5" = "Major Collector"
#'    "6" = "Minor Collector"
#'    "7" = "Local Road"

hist(nhpms$f_system)
summary(nhpms$aadt)
hist(nhpms$aadt)

#' NHPMS- Just roads with AADT data
nhpms_aadt <- nhpms %>%
  filter(aadt > 0)

plot(st_geometry(nhpms_aadt),
     main="NHPMS Links with AADT > 0", col=as.factor(nhpms_aadt$f_system))
legend("right", legend = c("1" = "Interstates",
                           "2" = "PA- Freeways",
                           "3" = "PA- Other",
                           "4" = "Minor Arterial",
                           "5" = "Major Collector",
                           "6" = "Minor Collector",
                           "7" = "Local Road"), 
       col = 1:7, cex = 0.8, pch = 1, title = "Functional Class")

st_write(nhpms_aadt, here::here("Data", "NHPMS_AADT_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

