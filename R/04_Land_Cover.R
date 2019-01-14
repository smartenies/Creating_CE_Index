#' =============================================================================
#' Date created: January 11, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' Summarized NLCD data for the CE
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
#' Tree cover, impervious surfaces
#' -----------------------------------------------------------------------------


#' Get shapefile and project to Albers Equal Area
#' Depending on the study area, may need to change this
acs_gdb_name <- "ACS_2014_5YR_TRACT_08_COLORADO.gdb"
spatial_units <- st_read(dsn = here::here("Data/ACS_Data", acs_gdb_name),
                         layer = str_remove(acs_gdb_name, ".gdb")) %>%
  st_transform(crs=albers)
plot(st_geometry(spatial_units))

#' 10 km buffer around the grid (to account for buffers around monitoring pts)
spatial_bound <- st_buffer(st_union(spatial_units), dist = 10000)

#' Plot objects
plot(st_geometry(spatial_units), col = NA, border = "red")
plot(st_geometry(spatial_bound), col = NA, border = "black", add=T)

#' raster clipping requires an sp object
grid_bound_sp <- as(spatial_bound, "Spatial")

#' National Land Cover Database raster files

impervious_name <- "nlcd_2011_impervious_2011_edition_2014_10_10/nlcd_2011_impervious_2011_edition_2014_10_10.img"
land_use_name <- "nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img"
tree_cover_name <- "CONUSAnalytical_2_8_16/Analytical/nlcd2011_usfs_conus_canopy_analytical.img"

#' Impervious surfaces
impervious_f <- raster(here::here("Data/NLCD_Data", impervious_name))
impervious_f
plot(impervious_f, main="Percent Impervious Surfaces: United States 2011")

#' Check range; shouldn't have anything over 100%
impervious <- crop(impervious_f, grid_bound_sp)
impervious
plot(impervious, main="Percent Impervious Surface: Study Area 2011")

writeRaster(impervious, file=here::here("Data", "Impervious_AEA.tif"),
            format="GTiff", overwrite=T)

#' Percent Tree cover
tree_f <- raster(here::here("Data/NLCD_Data", tree_cover_name))
tree_f
plot(tree_f, main="Percent Tree Cover: United States 2011")

tree <- crop(tree_f, grid_bound_sp)
tree
plot(tree, main="Percent Tree Cover: Study Area 2011")

writeRaster(tree, file=here::here("Data", "Tree_Cover_AEA.tif"),
            format="GTiff", overwrite=T)

#' Land use categorization
land_use_f <- brick(here::here("Data/NLCD_Data", land_use_name))
land_use_f
plotRGB(land_use_f, main="Land Use Characterization: United States 2011")

land_use <- crop(land_use_f, grid_bound_sp)
land_use
plot(land_use, main="Land Use Characterization: Study Area 2011")

writeRaster(land_use, file=here::here("Data", "Land_Use_AEA.tif"),
            format="GTiff", overwrite=T)

