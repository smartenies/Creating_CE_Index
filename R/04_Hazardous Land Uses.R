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
#' This script reads in the hazardous land use shapefiles and saves them as 
#' R objects
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
#' Environmental variables
#' Sources: Colorado Department of Public Health and Environment
#'          https://www.colorado.gov/pacific/cdphe/hm-gis-data
#'          
#'          Ben Allshouse's geodatabase
#'          
#'          COGCC:
#'          http://cogcc.state.co.us/data2.html#/downloads
#'          
#'          Tree canopy and impervious surfaces from NLCD 2011
#'          https://www.mrlc.gov/nlcd2011.php
#' =============================================================================

load("./Data/Spatial Data/dm_tracts.RData")
head(dm_tracts)

#National Priorities List (Superfund) sites (polygons)
npl <- st_read(paste(geo_data, "/NPL_NRD_20161110.shp", sep=""),
               stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(npl)
save(npl, file="./Data/Spatial Data/npl.RData")

#Oil and Gas well-- active and nonactive (points)
wells <- st_read(paste(geo_data, "/Wells.shp", sep=""),
               stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(wells)
save(wells, file="./Data/Spatial Data/wells.RData")

#Brownfields (points)
bf <- st_read(paste(geo_data, "/CDPHE_Brownfield_2017.shp", sep=""),
                 stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(bf)
save(bf, file="./Data/Spatial Data/bf.RData")

#Landfills open to the public (points)
lf <- st_read(paste(geo_data, "/SW_OpenMuniLFL_12_2016.shp", sep=""),
              stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(lf)
save(lf, file="./Data/Spatial Data/lf.RData")

#' Voluntary cleanup sites (points)
vc <- st_read(paste(geo_data, "/Voluntary_Cleanup.shp", sep=""),
              stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(vc)
save(vc, file="./Data/Spatial Data/vc.RData")

#' CAFOs (points)
cafo <- st_read(paste(geo_data, "/CAFOs.shp", sep=""),
              stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(cafo)
save(cafo, file="./Data/Spatial Data/cafo.RData")

#' Compost facilities (points)
compost <- st_read(paste(geo_data, "/compost.shp", sep=""),
              stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(compost)
save(compost, file="./Data/Spatial Data/compost.RData")

#' Mines (points)
mines <- st_read(paste(geo_data, "/mines.shp", sep=""),
              stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(mines)
save(mines, file="./Data/Spatial Data/mines.RData")

#' WWTPs (points)
wwtf <- st_read(paste(geo_data, "/wwtf.shp", sep=""),
              stringsAsFactors = F) %>%
  st_transform(crs=albers)
head(wwtf)
save(wwtf, file="./Data/Spatial Data/wwtf.RData")

#' -----------------------------------------------------------------------------
#' NLCD rasters need to be clipped based on the bounding box for the study area
#' Doing this here to avoid having to read in large rasters
#' -----------------------------------------------------------------------------

library(sp)

bounding_box <- st_make_grid(dm_tracts, n=1) %>%
  st_buffer(dist = 2000) %>%
  as(., "Spatial")
plot(bounding_box)
plot(as(dm_tracts, "Spatial"), add=T)

#' Read in the NLDC data, clip, and summarize for each grid cell
impervious_path <- "/nlcd_2011_impervious_2011_edition_2014_10_10/nlcd_2011_impervious_2011_edition_2014_10_10.img"
land_use_path <- "/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img"
tree_cover_path <- "/CONUSAnalytical_2_8_16/Analytical/nlcd2011_usfs_conus_canopy_analytical.img"

#' Impervious surfaces
impervious <- raster(paste(geo_data, impervious_path, sep="")) %>%
  crop(bounding_box)
plot(impervious)
writeRaster(impervious, filename="./Data/Spatial Data/impervious.grd",
            overwrite = T)

#' Percent Tree cover
tree_cover <- raster(paste(geo_data, tree_cover_path, sep="")) %>%
  crop(bounding_box)
plot(tree_cover)
writeRaster(tree_cover, filename="./Data/Spatial Data/tree_cover.grd",
            overwrite = T)

#' Land use categorization
land_use <- raster(paste(geo_data, land_use_path, sep="")) %>%
  crop(bounding_box)
plot(land_use)
writeRaster(land_use, filename="./Data/Spatial Data/land_use.grd",
            overwrite = T)
