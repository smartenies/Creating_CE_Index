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
#' This script creates objects for the census tracts used in the analysis
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


#' =============================================================================
#' Census tract shapefiles and spatial objects for R
#' =============================================================================

#' Denver Metro counties: Adams (001), Arapahoe (005), Boulder (013), Broomfield
#' (014), Denver (031), Douglas (035), Jefferson (059), Larimer (069), Weld (123)  

# metro <- c("001", "005", "013", "014", "031", "035", "059", "069", "123")

#' Colorado Census Tracts
co_tracts <- st_read(paste(geo_data, "tl_2014_08_tract.shp", sep=""),
                     stringsAsFactors = F) %>%
  st_transform(crs=albers)
plot(st_geometry(co_tracts))
save(co_tracts, file="./Data/Spatial Data/co_tracts.RData")

#' Colorado block groups
co_bgroups <- st_read(paste(geo_data, "ACS_2014_5YR_BG_08_Colorado.shp", sep=""),
                     stringsAsFactors = F) %>%
  st_transform(crs=albers)
plot(st_geometry(co_bgroups))
save(co_bgroups, file="./Data/Spatial Data/co_bgroups.RData")

#' Union
co_bound <- st_union(co_tracts)
plot(st_geometry(co_tracts))
plot(st_geometry(co_bound), border="red", add=T)
save(co_bound, file="./Data/Spatial Data/co_bound.RData")

#' Centroids
co_cent <- st_centroid(co_tracts)
plot(st_geometry(co_tracts))
plot(st_geometry(co_bound), border="red", add=T)
plot(st_geometry(co_cent), col="blue", add=T)
save(co_cent, file="./Data/Spatial Data/co_centroids.RData")

#' Denver Metro Census Tracts (just Denver, Adams, and Arapahoe counties)
#' Denver Metro Block Groups
metro <- c("001", "005", "031")

dm_tracts <- co_tracts[which(co_tracts$COUNTYFP %in% metro),]
plot(st_geometry(dm_tracts))
save(dm_tracts, file="./Data/Spatial Data/dm_tracts.RData")

metro2 <- c("001", "005", "013", "014", "031", "035", "059", "069", "123")
dm_bgroups <- co_bgroups[which(co_bgroups$COUNTYFP %in% metro2),]
plot(st_geometry(dm_bgroups))
save(dm_bgroups, file="./Data/Spatial Data/dm_bgroups.RData")

#' Union
dm_bound <- st_union(dm_tracts)
plot(st_geometry(dm_tracts))
plot(st_geometry(dm_bound), border="red", add=T)
save(dm_bound, file="./Data/Spatial Data/dm_bound.RData")

#' Centroids
dm_cent <- st_centroid(dm_tracts)
plot(st_geometry(dm_tracts))
plot(st_geometry(dm_bound), border="red", add=T)
plot(st_geometry(dm_cent), col="blue", add=T)
save(dm_cent, file="./Data/Spatial Data/dm_centroids.RData")

#' -----------------------------------------------------------------------------
#' Generate map of the study area
#' -----------------------------------------------------------------------------

map_bbox <- st_bbox(st_transform(dm_bound, ll_wgs84))
base_map <- get_map(location="Bennett, CO", maptype="roadmap",
                    source="google", zoom=9)
# save(base_map, file="./Data/Spatial Data/google base map.RData")
# load("./Data/Spatial Data/google base map.RData")

ggmap(base_map) +
  ggtitle("Study Area Boundary") +
  geom_sf(data=st_sf(st_transform(dm_bound, ll_wgs84)), 
          inherit.aes = F, color="black", fill=NA, size=1) +
  simple_theme
ggsave(filename = "./Figures/Study Boundaries.jpeg", device = "jpeg",
       dpi = 600)
















