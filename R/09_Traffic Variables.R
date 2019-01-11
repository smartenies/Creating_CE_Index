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
#' This script summarizes the National Highways Performance Measurement System
#' data (also used in the ECHO LUR)
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
#' NHPMS data available from: https://www.fhwa.dot.gov/policyinformation/hpms/shapefiles.cfm
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/dm_tracts.RData")
boundary <- st_make_grid(dm_tracts, n=1) %>%
  st_buffer(., dist = 2000)

#' Read in NHPS 2014 shapefile, project to Albers equal area, clip to grid
nhpms <- st_read(paste(geo_data, "Colorado_Sections_2014.shp", sep=""),
                 stringsAsFactors = F) 

#' Drop M coordinate, and crop to boundary
nhpms <- st_transform(st_zm(nhpms, drop=T), crs=albers) %>%
  st_crop(boundary)
plot(st_geometry(nhpms))

#' Idenify highways and major roads using the F_System classificatons
#' See Apendix H in the Field Manual
#'      1 = Interstate
#'      2 = Principal arterial - other freeways and expressways
#'      3 = Principal arterial - other
#'      4 = Minor arterial
#'      5 = major collector
#'      6 = minor collector
#'      7 = local road

nhpms_aadt <- nhpms %>%
  filter(!(f_system %in% c(6, 7))) %>%
  mutate(highway = ifelse(f_system %in% c(1, 2, 3), 1, 0),
         major = ifelse(f_system %in% c(4, 5), 1, 0))

save(nhpms, nhpms_aadt,
     file="./Data/Spatial Data/aadt_data.RData")

#' -----------------------------------------------------------------------------
#' Map of AADT across the study area
#' -----------------------------------------------------------------------------

ggplot() +
  ggtitle("Annual Average Daily Traffic (vehicles per day)") +
  geom_sf(data = nhpms_aadt, aes(col = aadt)) +
  scale_color_gradient(name = "AADT\n(vehicles per day)",
                       low = "lightblue1", high="darkblue") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Traffic/AADT 2014.jpeg", sep=""), 
       device = "jpeg", dpi=600)

