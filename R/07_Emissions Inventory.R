#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: May 22, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script summarizes the emissions inventory data (2016) provided by
#' DDPHE
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
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#' -----------------------------------------------------------------------------
#' Read in the emissions inventory data for 2016
#' Table from the Access database provided by DDPHE
#' -----------------------------------------------------------------------------

#' Read in excel file
#' Drop sites with missing coordinates
inventory <- read_excel("./Data/Emissions Inventory/emissions_inventory_2016.xlsx") %>%
  mutate_if(is.factor, as.character)
unique(inventory$data_year)

#' Data for the 5-year period matching the ACS data
data_years <- c("2010", "2011", "2012", "2013", "2014")

inventory <- inventory %>%
  filter(site_x_coordinate != 0) %>% #40484 total, 65 missing location data
  filter(data_year %in% data_years) %>%
  mutate(site_id = paste(site_state_fips, site_county_fips, site_site_id, sep="")) %>%
  st_as_sf(coords = c("site_x_coordinate", "site_y_coordinate"), crs=ll_wgs84) %>%
  st_transform(crs = albers)
  
plot(st_geometry(inventory))
summary(inventory)

#' Subset of emissions inventory just for criteria pollutants
#' ozone not included (secondary pollutant)
#' inventory only accounts for directly emitted PM
#' Add an idicator for major emittors (> 100 tpy for any pollutant)

criteria <- c("PM2.5", "PM10", "NO2", "NOX", "SO2", "CO", "PB")

inventory_criteria <- filter(inventory, pollutant_code %in% criteria) %>%
  mutate(major = ifelse(site_emis_estim >= 100, 1, 0))

inventory_other <- filter(inventory, !(pollutant_code %in% criteria)) %>%
  mutate(major = ifelse(site_emis_estim >= 100, 1, 0))

major_sources <- select(inventory_criteria, site_id, major) %>%
  filter(major == 1) %>%
  distinct() %>%
  st_set_geometry(NULL)

emissions_totals <- inventory_criteria %>%
  group_by(pollutant_code, data_year) %>%
  summarize(total = sum(site_emis_estim))

summary(emissions_totals)

#' Total criteria pollutants by facility
#' Indicator for major sources
emissions_by_facility <- inventory_criteria %>%
  group_by(pollutant_code, site_id, facility_name) %>%
  summarize(total = sum(site_emis_estim)) %>%
  left_join(major_sources, by="site_id") %>%
  mutate(major = ifelse(is.na(major), 0, major))

save(inventory, inventory_criteria, inventory_other, emissions_totals,
     emissions_by_facility,
     file="./Data/Spatial Data/emissions_inventory.RData")

#' -----------------------------------------------------------------------------
#' Histogram and time series of emissions data
#' -----------------------------------------------------------------------------

ggplot(data=emissions_totals, aes(x = pollutant_code, y = total)) +
  geom_bar(aes(fill = pollutant_code), stat="identity") +
  ylab("Total emissions (TPY)") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Emissions Inventory/Criteria_Histogram.jpeg",
       device = "jpeg", dpi=600)

ggplot(data=emissions_totals, aes(x=data_year, y=total)) +
  geom_point(aes(group = pollutant_code, color = pollutant_code)) +
  geom_line(aes(group = pollutant_code, color = pollutant_code)) +
  #scale_x_datetime(date_breaks = "6 months", date_labels =  "%b %y") +
  xlab("Data Year") + ylab("Emissions by year (TPY)") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Emissions Inventory/Criteria_Timeseries.jpeg",
       device = "jpeg", dpi=600)
