#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: May 17, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script summarizes crime statistics at the census tract, city, or county
#' level (depending on the jurisdiction and data availability)
#' 
#' NOTE: don't forget the ./ before the directory when reading in files!
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

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data/"
utm_13 <- "+init=epsg:26913"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
co_state_plane <- "+proj=lcc +lat_1=38.45 +lat_2=39.75 +lat_0=37.83333333333334 +lon_0=-105.5 +x_0=914401.8289 +y_0=304800.6096 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Denver Metro counties: Adams (001), Arapahoe (005), Boulder (013), Broomfield
#' (014), Denver (031), Douglas (035), Jefferson (059), Larimer (069), Weld (123)  
metro <- c("001", "005", "013", "014", "031", "035", "059", "069", "123")

crime_path <- "T:/Rsch-MRS/ECHO/SEM Large Data/Crime Data/"
geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data/"
per_pop <- 1000

#' -----------------------------------------------------------------------------
#' Read in crime data from Jesse Burkhardt
#' Counts by crime type, county, and year
#' -----------------------------------------------------------------------------


#' -----------------------------------------------------------------------------
#' Read in crime data from Michigan ICPSR, subset to metro counties, and combine 
#' into a single df
#' Variable names are different for 2013 and 2014, so there are two loops
#' -----------------------------------------------------------------------------

years <- c("2010", "2011", "2012")
crime <- data.frame()

for (i in 1:length(years)) {
  temp <- read.table(paste(crime_path, "co_crime_", years[i], ".txt", sep=""),
                     header=T, sep="\t") 
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
  temp <- read.table(paste(crime_path, "co_crime_", years2[i], ".txt", sep=""),
                     header=T, sep="\t")
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

crime <- rbind(crime, crime2)
crime$year <- substr(as.character(crime$INCDATE), 1, 4)
save(crime, file="./Data/Crime Data/full crime data.RData")

rm(crime, crime2, years, years2)


#' -----------------------------------------------------------------------------
#' Calculate 5-year average rates outside of Denver city/county
#' Crime data outside of Denver comes from the National Incident-Based Reporting
#' System Extract Files compiled by ICPSR at UM
#' 
#' -----------------------------------------------------------------------------

load("./Data/Crime Data/full crime data.RData") #1213263 records

crime$FIPS_COUNTY <- str_pad(crime$FIPS_COUNTY, 3, pad = "0")

glimpse(crime)

#' #' subset to just 2013-2014 to match what's available in Denver
#' crime <- crime[which(crime$year %in% c("2013", "2014")),]

#' How many are missing the offense code?
nrow(crime[which(crime$UCR_CODE_1 < 0),]) #none!

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

#' Drop lakeside, which has a theme park but a TINY population--- rates are 
#' unrealistic
agency_pop <- agency_pop[which(agency_pop$CITY != "LAKESIDE"),] 

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
  mutate(GEOID = as.character(GEOID))
summary(crime_rate)

#' NOTE: Highest crime rate is Black Hawk, which has a tiny population and is 
#' home to a casino. It's outside the study area and shouldn't impact results

#' -----------------------------------------------------------------------------
#' Next, crime data for Denver 
#' Based on data provided by Jesse Burkhardt
#' 
#' Data on the Denver Open Data website does not include crimes involving 
#' children or sexual assaults, but Dr Burkhardt had the full dataset, so that's 
#' what I'm working with now
#' 
#' Coordinates are in CO Central State Plane NAD 1983
#' -----------------------------------------------------------------------------

#' Census tracts
load("./Data/Spatial Data/co_tracts.RData")
co_tracts <- select(co_tracts, GEOID)
head(co_tracts)

#' Census tract populations
load("./Data/ACS_2010_2014/ACS.RData")
tract_pops <- select(acs, GEOID, total_pop) %>%
  st_set_geometry(NULL) %>%
  rename(pop = total_pop)

#' Read in crime data
d_crime <- read_csv("./Data/Crime Data/Crime1016NIBRS.csv") %>%
  filter(!is.na(xcoord) & xcoord > 1) %>% 
  filter(year %in% c(2010:2014)) %>%
  st_as_sf(coords=c("xcoord", "ycoord"), crs = co_state_plane) %>%
  st_transform(crs = st_crs(co_tracts))

head(d_crime)
glimpse(d_crime)  

plot(st_geometry(co_tracts))
plot(st_geometry(d_crime), col="red", add=T)

#' Spatially join to census tracts
d_crime <- st_join(d_crime, co_tracts)
head(d_crime)
glimpse(d_crime)

save(d_crime, file="./Data/Crime Data/full denver crime data.RData")

#' Indicator for violent crimes
#' See: https://www.denvergov.org/content/dam/denvergov/Portals/720/documents/statistics/2018/UCR_Citywide_Reported_Offenses_2018.pdf
#' See above for list of offense codes

#' Indicator for violent crime and property/non-violent crimes
violent_crimes <- c(91:93, 100, 111:114, 120, 131:133)

d_crime <- d_crime %>% 
  mutate(UCR_code = as.numeric(str_sub(rucr, 1, 3))) %>% 
  mutate(violent = ifelse(UCR_code %in% violent_crimes, 1, 0)) %>% 
  mutate(property = ifelse(violent == 1, 0, 1))  

#' calculate average crimes per year
n_yrs <- length(unique(d_crime$year))
sum_d_crime <- d_crime %>%
  group_by(GEOID) %>%
  count() %>%
  rename(total_crimes = n)

sum_violent_d_crime <- d_crime %>%
  filter(violent == 1) %>%
  group_by(GEOID) %>%
  count() %>%
  rename(total_violent = n) %>% 
  st_set_geometry(NULL)

sum_prop_d_crime <- d_crime %>%
  filter(property == 1) %>%
  group_by(GEOID) %>%
  count() %>%
  rename(total_property = n)%>% 
  st_set_geometry(NULL)

sum_d_crime <- left_join(sum_d_crime, sum_violent_d_crime, by="GEOID") %>%
  left_join(sum_prop_d_crime, by="GEOID") %>%
  mutate(avg_total_crimes = total_crimes / n_yrs,
         avg_violent_crimes = total_violent / n_yrs,
         avg_property_crimes = total_property / n_yrs)
rm(sum_violent_d_crime, sum_prop_d_crime)

#' Calculate rate per 1000
d_crime_rate <- left_join(sum_d_crime, tract_pops, by="GEOID") %>%
  filter(pop != 0) %>%
  mutate(crime_rate = (avg_total_crimes / pop * per_pop),
         violent_rate = (avg_violent_crimes / pop) * per_pop,
         property_rate = (avg_property_crimes / pop) * per_pop) %>%
  st_set_geometry(NULL)
summary(d_crime_rate)
glimpse(d_crime_rate)
#' -----------------------------------------------------------------------------
#' Assign crime rates to spatial units
#' 
#' Spatial unit shapefiles were generated in ArcMap
#' File name: "./Data/Aim 1 Geodatabase.mdx"
#' -----------------------------------------------------------------------------

#' combined crime rates
crime_rate <- bind_rows(crime_rate, d_crime_rate) %>%
  ungroup()
crime_rate$geometry <- NULL
glimpse(crime_rate)
class(crime_rate)

#' The highest crime rate is for Blackhawk
#' Outside of the study area, and contains a casino
summary(crime_rate)

#' Combine the geographic units
tracts <- st_read(paste(geo_data, "/police_tracts.shp", sep=""),
                  stringsAsFactors = F) %>%
  select(GEOID) %>%
  st_transform(crs=albers)
head(tracts)
summary(tracts)

plot(st_geometry(tracts), col="red")

cities <- st_read(paste(geo_data, "/police_cities.shp", sep=""),
                  stringsAsFactors = F) %>%
  select(GEOID) %>%
  st_transform(crs=albers)
head(cities)
summary(cities)

plot(st_geometry(tracts), col="red")
plot(st_geometry(cities), col="green", add=T)

plot(st_geometry(cities), col="green")
plot(st_geometry(tracts), col="red", add=T)

counties <- st_read(paste(geo_data, "/police_counties.shp", sep=""),
                    stringsAsFactors = F) %>%
  mutate(GEOID = gsub(" County", "", as.character(NAMELSAD10))) %>%
  select(GEOID) %>%
  st_transform(crs=albers)
head(counties)
summary(counties)

plot(st_geometry(tracts), col="red")
plot(st_geometry(cities), col="green", add=T)
plot(st_geometry(counties), col="blue", add=T)

plot(st_geometry(counties), col="blue")
plot(st_geometry(cities), col="green", add=T)
plot(st_geometry(tracts), col="red", add=T)

units <- rbind(tracts, cities, counties)
units$GEOID <- toupper(units$GEOID)
plot(st_geometry(units))

crime_rates <- units %>% 
  left_join(crime_rate, by="GEOID")

plot(st_geometry(crime_rates))
summary(crime_rates)
save(crime_rates, file="./Data/Spatial Data/crime_rates.RData")

#' -----------------------------------------------------------------------------
#' Map the crime rates
#' -----------------------------------------------------------------------------

ggplot() +
  ggtitle("Crime rate per 1,000 (all reported incidents)") +
  geom_sf(data = crime_rates, aes(fill = crime_rate), col=NA) +
  scale_fill_viridis(name = "All crimes\nRate per 1,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Crime/All crimes 2010-2014.jpeg", 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Crime rate per 1,000 (violent crimes)") +
  geom_sf(data = crime_rates, aes(fill = violent_rate), col=NA) +
  scale_fill_viridis(name = "Violent crimes\nRate per 1,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Crime/Violent crimes 2010-2014.jpeg", 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Crime rate per 1,000 (property and non-violent crimes)") +
  geom_sf(data = crime_rates, aes(fill = property_rate), col=NA) +
  scale_fill_viridis(name = "Property and\nnon-violent crimes\nRate per 1,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Crime/Property crimes 2010-2014.jpeg", 
       device = "jpeg", dpi=600)







