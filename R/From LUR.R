#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 Land Use Regression
#' 
#' Task: Clean and format the spatial data used in the LUR
#' 
#' Author: Sheena Martenies
#' 
#' Date Created: October 16, 2018
#' 
#' Contact: sheena.martenies@colostate.edu
#' -----------------------------------------------------------------------------

#' Load required libraries
library(sf)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(readxl)

#' Coordinate reference systems
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm_13 <- "+init=epsg:26913"

#' -----------------------------------------------------------------------------
# 1) Load the grid and create a boundary for clipping the data ----
grid_250 <- st_read(here::here("Data", "Grid_250_m_AEA.csv"),
                    stringsAsFactors = F, wkt = "WKT", crs = albers) 

#' 10 km buffer around the grid (to account for buffers around monitoring pts)
grid_bound <- st_buffer(st_union(grid_250), dist = 10000)

#' Plot objects
plot(st_geometry(grid_250), col = NA, border = "red")
plot(st_geometry(grid_bound), col = NA, border = "black", add=T)

#' raster clipping requires an sp object
grid_bound_sp <- as(grid_bound, "Spatial")

#' -----------------------------------------------------------------------------
# 2) Format the spatial data to fit the study boundary ----

#' A) National Land Cover Database raster files
#' 
impervious_name <- "nlcd_2011_impervious_2011_edition_2014_10_10/nlcd_2011_impervious_2011_edition_2014_10_10.img"
land_use_name <- "nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img"
tree_cover_name <- "CONUSAnalytical_2_8_16/Analytical/nlcd2011_usfs_conus_canopy_analytical.img"

#' Impervious surfaces
impervious_f <- raster(here::here("Data/Raw_Geo_Data", impervious_name))
impervious_f
plot(impervious_f, main="Percent Impervious Surfaces: United States 2011")

#' Check range; shouldn't have anything over 100%
impervious <- crop(impervious_f, grid_bound_sp)
impervious
plot(impervious, main="Percent Impervious Surface: Study Area 2011")

writeRaster(impervious, file=here::here("Data", "Impervious_AEA.tif"),
            format="GTiff", overwrite=T)

#' Percent Tree cover
tree_f <- raster(here::here("Data/Raw_Geo_Data", tree_cover_name))
tree_f
plot(tree_f, main="Percent Tree Cover: United States 2011")

tree <- crop(tree_f, grid_bound_sp)
tree
plot(tree, main="Percent Tree Cover: Study Area 2011")

writeRaster(tree, file=here::here("Data", "Tree_Cover_AEA.tif"),
            format="GTiff", overwrite=T)

#' Land use categorization
land_use_f <- brick(here::here("Data/Raw_Geo_Data", land_use_name))
land_use_f
plotRGB(land_use_f, main="Land Use Characterization: United States 2011")

land_use <- crop(land_use_f, grid_bound_sp)
land_use
plot(land_use, main="Land Use Characterization: Study Area 2011")

writeRaster(land_use, file=here::here("Data", "Land_Use_AEA.tif"),
            format="GTiff", overwrite=T)


#' B) Road network
highways_f <- st_read(here::here("Data/Raw_Geo_Data", "HIGHWAYS.shp")) %>% 
  # drop Z dimension
  st_zm(., drop=T)
plot(st_geometry(highways_f))

highways <- st_transform(highways_f, crs=albers) %>%
  st_crop(., st_bbox(grid_bound))
plot(st_geometry(highways))

st_write(highways, here::here("Data", "Highways_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

major_f <- st_read(here::here("Data/Raw_Geo_Data", "MAJOR_ROADS.shp")) %>% 
  # drop Z dimension
  st_zm(., drop=T)
plot(st_geometry(major_f))

major <- st_transform(major_f, crs=albers) %>%
  st_crop(., st_bbox(grid_bound))
plot(st_geometry(major))

st_write(major, here::here("Data", "Major_Roads_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' National Highways Performance Monitoring System (2017)
nhpms_f <- st_read(here::here("Data/Raw_Geo_Data", "Colorado2017.shp")) %>% 
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

#' C) Hazardous Land Uses and other features of the built environment
#' Sources: Colorado Department of Public Health and Environment
#'          https://www.colorado.gov/pacific/cdphe/hm-gis-data
#'          Ben Allshouse's geodatabase
#'          COGCC: http://cogcc.state.co.us/data2.html#/downloads
#'          US Census Bureau: https://www.census.gov/geo/maps-data/data/tiger-line.html


st_layers(here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"))

#National Priorities List (Superfund) sites (polygons): Superfund_NPL
npl <- st_read(dsn = here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"),
               layer = "Superfund_NPL") %>%
  st_transform(crs=albers)
npl <- npl[grid_bound,]
plot(st_geometry(npl))
st_write(npl, here::here("Data", "NPL_Sites_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#Oil and Gas well-- active and nonactive (points): OG_Wells
og_wells <- st_read(dsn = here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"),
                    layer = "OG_Wells") %>%
  st_transform(crs=albers)
og_wells <- og_wells[grid_bound,]
plot(st_geometry(og_wells))
st_write(og_wells, here::here("Data", "OG_Wells_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#Landfills open to the public (points): Solid_Waste
landfills <- st_read(dsn = here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"),
                     layer = "Solid_Waste") %>%
  st_transform(crs=albers)
landfills <- landfills[grid_bound,]
plot(st_geometry(landfills))
st_write(landfills, here::here("Data", "Landfills_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' CAFOs (points): CAFO
cafo <- st_read(dsn = here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"),
                     layer = "CAFO") %>%
  st_transform(crs=albers)
plot(st_geometry(cafo))
st_write(cafo, here::here("Data", "CAFOs_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Compost facilities (points): Composting
compost <- st_read(dsn = here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"),
                   layer = "Composting") %>%
  st_transform(crs=albers)
plot(st_geometry(compost))
st_write(compost, here::here("Data", "Compost_Facilities_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Waste water treatment plants (points): WWTF
wwtp <- st_read(dsn = here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"),
                   layer = "WWTF") %>%
  st_transform(crs=albers)
wwtp <- wwtp[grid_bound,]
plot(st_geometry(wwtp))
st_write(wwtp, here::here("Data", "Waste_Water_Plants_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Mines (points): Mines
mines <- st_read(dsn = here::here("Data/Raw_Geo_Data", "DenverMetroAirPollution.gdb"),
                 layer = "Mines") %>%
  st_transform(crs=albers)
mines <- mines[grid_bound,]
plot(st_geometry(mines))
st_write(mines, here::here("Data", "Mines_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Emissions Inventory
#' Locations and TPY of primary PM2.5 emitted
nei <- read_csv(here::here("Data/Raw_Geo_Data", "2014v2facilities.csv")) %>% 
  filter(st_usps_cd == "CO")

#' Subset to the criteria air pollutants
unique(nei$pollutant_cd)
caps <- c("VOC", "EC", "NO3", "OC", "PM-CON", "PM10-FIL", "PM10-PRI", "7439921",
          "PM25-FIL", "PM25-PRI", "PMFINE", "SO4", "NOX", "SO2", "CO", "NH3")

nei2 <- filter(nei, pollutant_cd %in% caps) %>% 
  filter(!is.na(longitude_msr)) %>% 
  st_as_sf(., coords = c("longitude_msr", "latitude_msr"), crs = ll_wgs84) %>% 
  st_transform(crs = albers)
nei2 <- nei2[grid_bound,]
plot(st_geometry(nei2))

#' Is this a major source for that pollutant?
#' Using thresholds listed in "priority_facility_list_for2014.csv"
nei2 <- nei2 %>% 
  mutate(major_source = ifelse(pollutant_cd == "VOC" & total_emissions >= 100, 1, 
                         ifelse(pollutant_cd == "7439921" & total_emissions > 0, 1, 
                          ifelse(pollutant_cd == "NOX" & total_emissions >= 750, 1,
                           ifelse(pollutant_cd == "PM10-FIL" & total_emissions >= 50, 1,
                            ifelse(pollutant_cd == "PM10-PRI" & total_emissions >= 100, 1,
                             ifelse(pollutant_cd == "PM25-FIL" & total_emissions >= 50, 1,
                              ifelse(pollutant_cd == "PM25-PRI" & total_emissions >= 100, 1,
                               ifelse(pollutant_cd == "SO2" & total_emissions >= 7500, 1,
                               0)))))))))

plot(st_geometry(filter(nei2, pollutant_cd == "PM25-PRI")), pch = 16,
     main="NEI Sites: Primary PM2.5", col=as.factor(nei2$major_source))
legend("right", legend = c("1" = "Major Source (2014 Thresholds)",
                           "2" = "Non-major Source"), 
       col = 1:2, cex = 0.8, pch = 16, title = "Source Category")

st_write(nei2, here::here("Data", "NEI_2014v2_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Railroads
rail <- st_read(dsn = here::here("Data/Raw_Geo_Data", "tl_2018_us_rails.shp")) %>%
  st_transform(crs=albers)
rail <- rail[grid_bound,]
plot(st_geometry(rail))
plot(st_geometry(grid_bound), border = "red", add=T)

st_write(rail, here::here("Data", "Rail_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Military installations
military <- st_read(dsn = here::here("Data/Raw_Geo_Data", "tl_2018_us_mil.shp")) %>%
  st_transform(crs=albers)
military <- military[grid_bound,]

plot(st_geometry(grid_bound), border = "red")
plot(st_geometry(military), add=T)

st_write(military, here::here("Data", "Military_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Landmarks
area_landmarks <- st_read(dsn = here::here("Data/Raw_Geo_Data", "tl_2018_08_arealm.shp")) %>%
  st_transform(crs=albers) %>% 
  mutate_if(is.factor, as.character)
plot(st_geometry(area_landmarks))

glimpse(area_landmarks)

#' Airports MTFCC = K2451, K2456, K2457
area_airports <- filter(area_landmarks, MTFCC %in% c("K2451", "K2456", "K2457"))
area_airports <- area_airports[grid_bound, ]
plot(st_geometry(area_airports))

st_write(area_airports, here::here("Data", "Airports_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Park MTFCC = K2180:K2190
area_parks <- filter(area_landmarks, MTFCC %in% paste0("K", 2180:2190))
area_parks <- area_parks[grid_bound, ]
plot(st_geometry(area_parks))

st_write(area_airports, here::here("Data", "Parks_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' D) Elevation
#' Source: https://www.sciencebase.gov/catalog/item/581d0539e4b08da350d52552

elevation_f <- raster(here::here("Data/Raw_Geo_Data/elev48i0100a_tif", 
                                 "elev48i0100a.tif"))
elevation_f
plot(elevation_f, main="Elevation: Conterminous United States")

elevation <- crop(elevation_f, grid_bound_sp)
elevation
plot(elevation, main="Elevation in meters: Study Area")

writeRaster(elevation, file=here::here("Data", "Elevation_AEA.tif"),
            format="GTiff", overwrite=T) 

#' E) Population Count and Density
#' source: SEDAC
population_ct_f <- raster(here::here("Data/Raw_Geo_Data", 
                                     "gpw_v4_population_count_rev10_2015_30_sec.tif"))
population_ct_f
plot(population_ct_f, main="Population: GPWv4 2015")

population_ct <- crop(population_ct_f, spTransform(grid_bound_sp, 
                                                   CRSobj = raster::crs(population_ct_f)))
population_ct <- projectRaster(population_ct, crs = albers)
population_ct
plot(population_ct, main="Population (n): Study Area")

writeRaster(population_ct, file=here::here("Data", "Population_Count_AEA.tif"),
            format="GTiff", overwrite=T) 

population_den_f <- raster(here::here("Data/Raw_Geo_Data", 
                                      "gpw_v4_population_density_rev10_2015_30_sec.tif"))
population_den_f
plot(population_den_f, main="Population Density: GPWv4 2015")

population_den <- crop(population_den_f, spTransform(grid_bound_sp, 
                                                     CRSobj = raster::crs(population_den_f)))
population_den <- projectRaster(population_den, crs = albers)
population_den
plot(population_den, main="Population density (n/km2): Study Area")

writeRaster(population_den, file=here::here("Data", "Population_Density_AEA.tif"),
            format="GTiff", overwrite=T) 

#' -----------------------------------------------------------------------------
# 3) Format the ACS block-group level data from  ----

#' Get block groups in study area based on the ACS 2016 5-year estimates
st_layers(here::here("Data/Raw_Geo_Data", "ACS_2016_5YR_BG_08_COLORADO.gdb"))
bg_f <- st_read(dsn = here::here("Data/Raw_Geo_Data", "ACS_2016_5YR_BG_08_COLORADO.gdb"),
                layer = "ACS_2016_5YR_BG_08_COLORADO")

plot(st_geometry(bg_f))

bg <- st_transform(bg_f, crs=albers)
bg <- bg[grid_bound,]

plot(st_geometry(bg))
plot(st_geometry(grid_bound), border = "blue", add=T)

st_write(bg, here::here("Data", "Block_Groups_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)

#' Population and housing density
bg_ids <- as.character(unique(bg$GEOID))

#' Population:
#' B010001e1 = estimate of total population
pop <- st_read(dsn = here::here("Data/Raw_Geo_Data", "ACS_2016_5YR_BG_08_COLORADO.gdb"),
               layer = "X01_AGE_AND_SEX") %>%
  mutate(GEOID = gsub("15000US", "", as.character(GEOID))) %>% 
  select(GEOID, B01001e1) %>%
  filter(GEOID %in% bg_ids) %>%
  rename(total_pop = B01001e1)

#' Housing:
#'     B25024e1,UNITS IN STRUCTURE: Total: Housing units
#'     B25024e2,"UNITS IN STRUCTURE: 1, detached: Housing units
#'     B25024e3,"UNITS IN STRUCTURE: 1, attached: Housing units
#'     B25024e4,UNITS IN STRUCTURE: 2: Housing units
#'     B25024e5,UNITS IN STRUCTURE: 3 or 4: Housing units
#'     B25024e6,UNITS IN STRUCTURE: 5 to 9: Housing units
#'     B25024e7,UNITS IN STRUCTURE: 10 to 19: Housing units
#'     B25024e8,UNITS IN STRUCTURE: 20 to 49: Housing units
#'     B25024e9,UNITS IN STRUCTURE: 50 or more: Housing units
#'     B25024e10,UNITS IN STRUCTURE: Mobile home: Housing units
#'     B25024e11,"UNITS IN STRUCTURE: Boat, RV, van, etc.: Housing units

housing <- st_read(dsn = here::here("Data/Raw_Geo_Data", "ACS_2016_5YR_BG_08_COLORADO.gdb"),
               layer = "X25_HOUSING_CHARACTERISTICS") %>% 
  select(GEOID, paste("B25024e", 1:11, sep="")) %>%
  mutate(GEOID = gsub("15000US", "", as.character(GEOID)),
         single_family_n = B25024e2 + B25024e3,
         multi_unit_n = B25024e4 + B25024e5 + B25024e6 + B25024e7 + B25024e8 + B25024e9,
         mobile_home_n = B25024e10 + B25024e11) %>% 
  mutate(housing_n = single_family_n + multi_unit_n + mobile_home_n) %>%
  filter(GEOID %in% bg_ids) %>%
  select(GEOID, single_family_n, multi_unit_n, mobile_home_n, housing_n)

#' Polygon area in sq km
#' Map units are m, so need to convert to km
bg$area_km2 <- unclass(st_area(bg)) / (1000^2)
bg$area_km2_calculated <- (bg$ALAND + bg$AWATER) / (1000^2)

head(bg[,c("Shape_Area", "area_km2", "area_km2_calculated")])

#' Merge census data and polygons
bg <- left_join(bg, pop, by = "GEOID") %>% 
  left_join(housing, by = "GEOID")
glimpse(bg)

#' Calculate population and housing denisty
#' Persons per sq km and units per sq km
bg$pop_density <- bg$total_pop / bg$area_km2
bg$single_family_density <- bg$single_family_n / bg$area_km2
bg$multi_unit_density <- bg$multi_unit_n / bg$area_km2
bg$mobile_home_density <- bg$mobile_home_n / bg$area_km2
bg$housing_density <- bg$housing_n / bg$area_km2

glimpse(bg)

bg <- bg %>% 
  select(GEOID, area_km2, total_pop, pop_density, 
         single_family_n, single_family_density,
         multi_unit_n, multi_unit_density, 
         mobile_home_n, mobile_home_density,
         housing_n, housing_density)

st_write(bg, here::here("Data", "Population_and_Housing_AEA.csv"),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
