#' =============================================================================
#' Date created: January 11, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script summarizes crime statistics
#' 
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data/"
utm_13 <- "+init=epsg:26913"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
co_state_plane <- "+proj=lcc +lat_1=38.45 +lat_2=39.75 +lat_0=37.83333333333334 +lon_0=-105.5 +x_0=914401.8289 +y_0=304800.6096 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Read in crime data from Michigan ICPSR, subset to metro counties, and combine 
#' into a single df
#' Variable names are different for 2013 and 2014, so there are two loops
#' 
#' NOTE: The files read in as part of this loop were generated from the SAS 
#' programs provided by ICPSR
#' -----------------------------------------------------------------------------

crime_output_name <- "temp_crime.csv"

years <- c("2010", "2011", "2012")
crime <- data.frame()

for (i in 1:length(years)) {
  temp <- read_table2(here::here("Data/FBI_Data", 
                     paste0("co_crime_", years[i], ".txt"))) 
  temp2 <- temp[,c("STATE", "ORI", "INCNUM", "INCDATE",
                   "B1007", "B1008", "B1009", "B1012", "B1013",
                   "B2005", "B2006", "B2007", "B2008", "B3023",
                   "V1006", "V1007", "V20061", "V20062", "V20063")]
  
  colnames(temp2) <- c("STATE", "ORI", "INCNUM", "INCDATE",
                       "CITY", "STATE_AB", "POP_GROUP", "AGENCY", "CORE_CITY",
                       "CURRENT_POP", "UCR_COUNTY_CODE", "MSA_CODE", "LAST_POP", "FIPS_COUNTY",
                       "DATE", "HOUR", "UCR_CODE_1", "UCR_CODE_2", "UCR_CODE_3")
  
  crime <- rbind(crime, temp2)
  print(years[i])
  rm(temp, temp2)
}

years2 <- c("2013", "2014")
crime2 <- data.frame()

for (i in 1:length(years2)) {
  temp <- read_table2(here::here("Data/FBI_Data", 
                     paste0("co_crime_", years2[i], ".txt")))
  temp2 <- temp[,c("STATE", "ORI", "INCNUM", "INCDATE",
                   "BH007", "BH008", "BH009", "BH012", "BH013",
                   "BH019", "BH020", "BH021", "BH022", "BH054",
                   "V1006", "V1007", "V20061", "V20062", "V20063")]
  colnames(temp2) <- c("STATE", "ORI", "INCNUM", "INCDATE",
                       "CITY", "STATE_AB", "POP_GROUP", "AGENCY", "CORE_CITY",
                       "CURRENT_POP", "UCR_COUNTY_CODE", "MSA_CODE", "LAST_POP", "FIPS_COUNTY",
                       "DATE", "HOUR", "UCR_CODE_1", "UCR_CODE_2", "UCR_CODE_3")
  crime2 <- rbind(crime2, temp2)
  print(years2[i])
  rm(temp, temp2)
}

names(crime)
names(crime2)

crime <- bind_rows(crime, crime2)
crime$year <- substr(as.character(crime$INCDATE), 1, 4)

rm(crime2, years, years2)

#' -----------------------------------------------------------------------------
#' Calculate rates
#' -----------------------------------------------------------------------------

#' Denominator for rates
per_pop <- 1000

crime$FIPS_COUNTY <- str_pad(crime$FIPS_COUNTY, 3, pad = "0")

glimpse(crime)

#' How many are missing the offense code?
nrow(crime[which(crime$UCR_CODE_1 < 0),])

#' V20061 = UCR_CODE_1 = UCR Offense Code"
#' Violent: homicide (91,92,93), kidnapping/abduction (100), 
#' sex offenses (111-114), robbery (120), assault (131, 132, 133)

#' Property/non-violent: arson (200), extortion (210), burglary/B&E (220),
#' larceny (231-238), motor vehicle theft (240), counterfieting (250), fraud
#' (261-265), embezzelment (270), stolen property (280), vandalism (290), drugs 
#' (351, 352), nonforcible sex offenses (361, 362), porn/obscene materials (370),
#' gambling (391-394), prostitution (401-403), bribery (510), weapon law (520),
#' human trafficking (641, 642)

#' Others: undetermined (-9),  NA LT 3 records (-8), unknown/missing (-7),
#' not applicable (-6), NA window/Grp B record (-5)

#' Indicator for violent crime and property/non-violent crimes
violent_crimes <- c(91:93, 100, 111:114, 120, 131:133)
crime$violent <- ifelse(crime$UCR_CODE_1 %in% violent_crimes, 1, 0)
crime$property <- ifelse(crime$violent == 1, 0, 1)

#' How many agencies are there?
agencies <- unique(crime[,c("FIPS_COUNTY", "CITY", "MSA_CODE", "AGENCY")])
agencies <- agencies[which(agencies$AGENCY != 3),] #' drop universities
city_agencies <- agencies[which(agencies$AGENCY == 1),]

#' AGENCY - type of agency
#'  0 = Covered by another agency
#'  1 = city
#'  2 = county
#'  3 = university or college
#'  4 = state police
#'  5 = special agency
#'  6 = other state agencies
#'  7 = tribal agencies
#'  8 = federal agencies

#' population by agency
agency_pop <- unique(crime[,c("FIPS_COUNTY", "CITY", "AGENCY", 
                              "CURRENT_POP", "LAST_POP")])

agency_pop <- agency_pop[which(agency_pop$AGENCY != 3),] #' drop universities
summary(agency_pop)
head(agency_pop)

agency_avg_pop <- agency_pop %>% 
  group_by(CITY) %>% 
  summarize(pop = mean(CURRENT_POP, na.rm=T)) %>% 
  rename(GEOID = CITY) %>% 
  ungroup()

#' calculate average crimes per year
n_yrs <- length(unique(crime$year))
sum_crime <- crime %>%
  group_by(CITY) %>%
  count() %>%
  rename(total_crimes = n)

sum_violent_crime <- crime %>%
  filter(violent == 1) %>%
  group_by(CITY) %>%
  count() %>%
  rename(total_violent = n)

sum_prop_crime <- crime %>%
  filter(property == 1) %>%
  group_by(CITY) %>%
  count() %>%
  rename(total_property = n)

sum_crime <- left_join(sum_crime, sum_violent_crime, by="CITY") %>%
  left_join(sum_prop_crime, by="CITY") %>%
  rename(GEOID = CITY) %>%
  mutate(avg_total_crimes = total_crimes / n_yrs,
         avg_violent_crimes = total_violent / n_yrs,
         avg_property_crimes = total_property / n_yrs) %>% 
  ungroup()
rm(sum_violent_crime, sum_prop_crime)

#' Calculate rate per 1000
crime_rate <- left_join(sum_crime, agency_avg_pop, by="GEOID") %>%
  filter(pop != 0) %>%
  mutate(crime_rate = (avg_total_crimes / pop * per_pop),
         violent_rate = (avg_violent_crimes / pop) * per_pop,
         property_rate = (avg_property_crimes / pop) * per_pop) %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  mutate(crime_rate = ifelse(crime_rate < 0, NA, crime_rate),
         violent_rate = ifelse(violent_rate < 0, NA, violent_rate),
         property_rate = ifelse(property_rate < 0, NA, property_rate))
summary(crime_rate)

#' -----------------------------------------------------------------------------
#' Assign crime rates to spatial units
#' Need crosswalk: https://www.icpsr.umich.edu/icpsrweb/NACJD/series/366
#' 
#' 
#' -----------------------------------------------------------------------------




write_csv(crime_rate, here::here("Data", crime_output_name))

