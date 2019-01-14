#' =============================================================================
#' Date created: January 11, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes ACS variables for each of the census tracts
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

#' For spatial data
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' =============================================================================
#' Reading in the ACS files and mapping key demographic/SES variables
#' .txt files extracted from the ACS TIGER/Line geodatabase
#' See /Data/ACS_Data for the original .gbd
#' =============================================================================

#' Specify the geodatabase name and output name
acs_gdb_name <- "ACS_2014_5YR_TRACT_08_COLORADO.gdb"
acs_output_name <- ("ACS_AEA.csv")

#' get shapefile and project to Albers Equal Area
acs_units <- st_read(dsn = here::here("Data/ACS_Data", acs_gdb_name),
                     layer = str_remove(acs_gdb_name, ".gdb")) %>%
  st_transform(crs=albers)
plot(st_geometry(acs_units))

#' Extract data tables from the geodatabase
#' #' Combine each layer into a single data frame
acs_layers <- st_layers(here::here("Data/ACS_Data", acs_gdb_name))
file_list <- acs_layers[[1]][str_detect(acs_layers[[1]], pattern = "X")]

acs_dataset <- data.frame()
  
for (i in 1:length(file_list)){
  temp <- st_read(dsn = here::here("Data/ACS_Data", acs_gdb_name),
                  layer = file_list[i])
  temp$OBJECTID <-NULL
  temp$GEOID <- as.character(temp$GEOID)
  temp <- temp[order(temp$GEOID),]
    
  if (i == 1) {
    acs_dataset <- temp
    print(file_list[i])
    rm(temp)
  } else {
    acs_dataset <- left_join(acs_dataset, temp, by = "GEOID") #' joining columns
    print(file_list[i])
    rm(temp)
  }
}

acs_dataset$GEOID <- str_remove(acs_dataset$GEOID, "14000US")

#' -----------------------------------------------------------------------------
#' Creating new demographic and SES variables for mapping
#' Using data dictionary for the ACS TIGER/Line files
#'  
#'  ACS Data Dictionary (2010-2014)
#'  Variable     Definition
#'  B01001e1     Total population
#'  B01001e3     Male population under 5 years
#'  B01001e27    Female population under 5 years
#'  
#'  B01001e20    Male population 65-66
#'  B01001e21    Male population 67-69
#'  B01001e22    Male population 70-74
#'  B01001e23    Male population 75-79
#'  B01001e24    Male population 80-84
#'  B01001e25    Male populiation 85+
#'  B01001e44    Female population 65-66
#'  B01001e45    Female population 67-69
#'  B01001e46    Female population 70-74
#'  B01001e47    Female population 75-79
#'  B01001e48    Female population 80-84
#'  B01001e49    Female populiation 85+  
#'  
#'  B03002e3     Non-Hispanic white
#'  
#'  B05002e13    Foreign born
#'  
#'  B15003e1     Population 25 years and older
#'  B15003e2     No schooling
#'  B15003e3     Nursery school
#'  B15003e4     K
#'  B15003e5     1st grade
#'  B15003e6     2nd grade
#'  B15003e7     3rd grade
#'  B15003e8     4th grade
#'  B15003e9     5th grade
#'  B15003e10    6th grade
#'  B15003e11    7th grade
#'  B15003e12    8th grade
#'  B15003e13    9th grade
#'  B15003e14    10th grade
#'  B15003e15    11th grade
#'  B15003e16    12th grade, no diploma
#'  B15003e17    High school diploma
#'  B15003e18    GED or alternative
#'  B15003e19    Some college (1 year), no degree
#'  B15003e20    Some college (>1 year), no degree
#'  B15003e21    Associate's
#'  B15003e22    Bachelor's
#'  B15003e23    Master's
#'  B15003e24    Professional
#'  B15003e25    Doctorate
#'    
#'  B16002e1     Total households
#'  B16002e4     Spanish speaking households, limited English
#'  B16002e7     Other Indo_European speaking households, limited English
#'  B16002e10    Asian/PI speaking households, limited English
#'  B16002e13    Other language speaking households, limited English
#'  
#'  B17017e2     Households in poverty
#'  
#'  B23025e3     Population 16+ in the civilian work force
#'  B23025e5     Population 16+ in the civilian work force that are unemployed
#'  
#'  B19013e1     Median household income (2014 inflation-adjusted $)
#'  
#'  B25035e1	   Median year built for housing
#' -----------------------------------------------------------------------------

#' list of the ACS variable names
#' going to drop these later
acs_vars <- colnames(acs_dataset)[-1]
  
#' Merge ACS data with polygons and to get the spatial data in the df
acs_data <- left_join(acs_units, acs_dataset, by="GEOID") %>%
  mutate(area_km2 = as.vector(unclass(st_area(.))) / 1000^2) %>%
  mutate(total_pop = B01001e1,
         pop_dens = total_pop / area_km2,
         under5 = B01001e3 + B01001e27,
         over64 = B01001e20 + B01001e21 + B01001e22 + B01001e23 + 
                  B01001e24 + B01001e25 + B01001e44 + B01001e45 + 
                  B01001e46 + B01001e47 + B01001e48 + B01001e49,
         nhw = B03002e3, # non-Hispanic White only
         non_nhw = total_pop - nhw,
         foreign_born = B05002e13,
         over24 = B15003e1,
         less_hs = B15003e2 + B15003e3 + B15003e4 +
                   B15003e5 + B15003e6 + B15003e7 + B15003e8 +
                   B15003e9 + B15003e10 + B15003e11 + B15003e12 +
                   B15003e13 + B15003e14 + B15003e15 +
                   B15003e16,
         hs_grad_only = B15003e17 + B15003e18,
         some_college = B15003e19 + B15003e20,
         associates = B15003e21,
         bachelors = B15003e22,
         advanced_degree = B15003e23 + B15003e24 + B15003e25,
         civ_workforce = B23025e3,
         unemployed = B23025e5,
         total_hh = B16002e1,
         hh_limited_eng = B16002e4 + B16002e7 + B16002e10 + B16002e13,
         hh_pov = B17017e2,
         med_income = B19013e1,
         med_year_blt_housing = B25035e1) %>%
  mutate(pct_under5 = (under5 / total_pop) * 100,
         pct_over64 = (over64 / total_pop) * 100,
         pct_nhw = (nhw / total_pop) * 100,
         pct_non_nhw = (non_nhw / total_pop) * 100,
         pct_foreign_born = (foreign_born / total_pop) * 100,
         pct_less_hs_grad = (less_hs / over24) * 100,
         pct_civ_unemployed = (unemployed / civ_workforce) * 100,
         pct_hh_limited_eng = (hh_limited_eng / total_hh) * 100,
         pct_hh_pov = (hh_pov / total_hh) * 100) %>%
  mutate(pct_hs_grad = (100 - pct_less_hs_grad),
         pct_civ_employed = (100 - pct_civ_unemployed),
         pct_hh_above_pov = (100 - pct_hh_pov),
         pct_hh_not_limited_eng = (100 - pct_hh_limited_eng)) %>% 
  select(-one_of(acs_vars))
head(acs_data)
summary(acs_data)  
  
st_write(acs_data, here::here("Data", acs_output_name),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

