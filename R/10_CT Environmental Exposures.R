#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: May 23, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script summarizes the environmental variables at the census tract level
#' to be used in the cumulative exposure index
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
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Create the data frame to hold all of the census tract variables

load("./Data/Spatial Data/dm_tracts.RData")
ct_env <- select(dm_tracts, GEOID) %>%
  arrange(GEOID) %>%
  mutate(area_km2 = as.vector(unclass(st_area(.)) / (1000^2)))

rm(dm_tracts)
#' -----------------------------------------------------------------------------

#' Function for doughnut buffers
doughnut <- function(poly, outer, inner) {
  buff_outer <- st_buffer(poly, dist = outer)
  buff_inner <- st_buffer(poly, dist = inner) 
  e <- st_difference(buff_outer, buff_inner)
  return(e)
}

#' -----------------------------------------------------------------------------
#' Built environment and hazardous land use variables
#' see: 4_Hazardous Land Uses.R
#'      7_Emissions Inventory.R
#'      8_Toxic Releases.R
#'      9_Traffic Variables.R
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Average percent tree cover (NLCD 2011) and average percent impervious 
#' surface (NLCD 2011)
#' 
#' Note: raster::extract requires sp objects!
#' -----------------------------------------------------------------------------

ct_sp <- as(ct_env, "Spatial")
ct_sp@data$area_km2 <- NULL

tree_cover <- raster("./Data/Spatial Data/tree_cover.grd")
impervious <- raster("./Data/Spatial Data/impervious.grd")

ct_sp$pct_tree_cover <- raster::extract(tree_cover, ct_sp, fun=mean, na.rm=T)[,1]
ct_sp$pct_impervious <- raster::extract(impervious, ct_sp, fun=mean, na.rm=T)[,1]

ct_sp <- as.data.frame(ct_sp) 

ct_env <- left_join(ct_env, ct_sp, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("Percent tree cover") +
  geom_sf(data = ct_env, aes(fill = pct_tree_cover), col=NA) +
  scale_fill_viridis(name = "Percent") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/tree cover.jpeg", sep=""), 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Percent impervious surface") +
  geom_sf(data = ct_env, aes(fill = pct_impervious), col=NA) +
  scale_fill_viridis(name = "Percent") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/impervious surfaces.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(tree_cover, impervious, ct_sp)
gc()

#' -----------------------------------------------------------------------------
#' length of interstates and major roads within the census tract
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/aadt_data.RData")

highways <- filter(nhpms_aadt, highway == 1)
major <- filter(nhpms_aadt, major == 1)

plot(st_geometry(highways), col="red")
plot(st_geometry(major), col="blue", add=T)
plot(st_geometry(ct_env), border="grey50", col=NA, size=1, add=T)

#' Highway lengths
highway_intersect <- st_intersection(ct_env, highways) 

plot(st_geometry(highway_intersect), col="red")

highway_length <- highway_intersect %>%
  mutate(highway_length = unclass(st_length(.))) %>%
  group_by(GEOID) %>%
  summarise(highway_km = sum(highway_length) / 1000,
            sum_highway_aadt = sum(aadt),
            mean_highway_aadt = mean(aadt)) %>%
  st_set_geometry(NULL)

head(highway_length)
summary(highway_length)

#' Checks!
#' Length of intersect and extra should == length of highways
#' Length of intersect should == length in census tracts
sum(highway_length$highway_km)
sum(unclass(st_length(highway_intersect))) / 1000
sum(unclass(st_length(highways))) / 1000

#' Major road lengths, sum AADT, and mean AADT along those roads
major_intersect <- st_intersection(ct_env, major) 

plot(st_geometry(major_intersect), col="blue")

major_length <- major_intersect %>%
  mutate(major_length = unclass(st_length(.))) %>%
  group_by(GEOID) %>%
  summarise(major_km = sum(major_length) / 1000,
            sum_major_aadt = sum(aadt),
            mean_major_aadt = mean(aadt)) %>%
  st_set_geometry(NULL)

head(major_length)
summary(major_length)

ct_env <- left_join(ct_env, highway_length, by="GEOID") %>%
  left_join(major_length, by="GEOID") %>%
  mutate(highway_km = ifelse(is.na(highway_km), 0, highway_km),
         sum_highway_aadt = ifelse(is.na(sum_highway_aadt), 0, sum_highway_aadt),
         mean_highway_aadt = ifelse(is.na(mean_highway_aadt), 0, mean_highway_aadt),
         major_km = ifelse(is.na(major_km), 0, major_km),
         sum_major_aadt = ifelse(is.na(sum_major_aadt), 0, sum_major_aadt),
         mean_major_aadt = ifelse(is.na(mean_major_aadt), 0, mean_major_aadt)) %>%
  mutate(sum_aadt = sum_highway_aadt + sum_major_aadt,
         mean_aadt = (mean_highway_aadt + mean_major_aadt) / 2) %>%
  mutate(sum_aadt_intensity = sum_aadt / area_km2,
         mean_aadt_intensity = mean_aadt / area_km2)

head(ct_env)
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("AADT (total vehicles per day)") +
  geom_sf(data = ct_env, aes(fill = sum_aadt/1000), col=NA) +
  scale_fill_viridis(name = "vehicles/day\n(1000's)") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/total_aadt.jpeg", sep=""), 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("AADT (average vehicles per day)") +
  geom_sf(data = ct_env, aes(fill = mean_aadt/1000), col=NA) +
  scale_fill_viridis(name = "vehicles/day\n(1000's)") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/mean_aadt.jpeg", sep=""), 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("AADT Intensity (total vehicles per day per sq km)") +
  geom_sf(data = ct_env, aes(fill = sum_aadt_intensity), col=NA) +
  scale_fill_viridis(name = "vehicles/day/km2") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/total aadt intensity.jpeg", sep=""), 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("AADT Intensity (mean no. vehicles per day per sq km)") +
  geom_sf(data = ct_env, aes(fill = mean_aadt_intensity), col=NA) +
  scale_fill_viridis(name = "vehicles/day/km2") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/mean aadt intensity.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(highways, highway_intersect, highway_length,
   major, major_intersect, major_length,
   nhpms, nhpms_aadt)

#' -----------------------------------------------------------------------------
#' Number of NPL sites within the census tract (polygons!)
#' Weighted by distance to census tract
#'     1) Drew 250, 500, 750, and 1000 m buffers around polygon
#'     2) Assigned weights of 1, 0.5, 0.25, and 0.1 for sites within these 
#'        buffers
#'     3) Removed NPL Deleted sites, but did not weight by type of facility 
#'        (as done in CalEnviroScreen 3.0) because we did not have information 
#'        on activities (only had locations and status)
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/npl.RData")
plot(st_geometry(npl))

#' Remove "NPL Deleted" sites
#' N = 3 that are filtered out
npl <- filter(npl, SiteStatus != "NPL DELETED")
plot(st_geometry(npl))

#' Function for polygon counts weighted by distance
count_in_buffer <- function(data_sf, sf_object, data_id_col = 1) {
  temp_df <- data.frame()
  
  poly_ids <- st_set_geometry(data_sf, NULL) %>%
    select(data_id_col) %>%
    distinct
  
  for (i in 1:nrow(poly_ids)) {
    poly <- filter(data_sf, GEOID == poly_ids[i,1])
    #poly <- filter(data_sf, GEOID == "08001008709")
    poly_250 <- doughnut(poly, 250, 0)
    poly_500 <- doughnut(poly, 500, 250)
    poly_750 <- doughnut(poly, 750, 500)
    poly_1000 <- doughnut(poly, 1000, 750)
    
    buff_list <- list(poly, poly_250, poly_500, poly_750, poly_1000)
    buff_name <- as.character(c(0, 250, 500, 750, 1000))
    weight_list <- c(1, 1, 0.5, 0.25, 0.1)
    
    poly_wt_count <- vector()
    
    for (j in 1:length(buff_list)) {
      buff <- buff_list[[j]]
      name <- buff_name[j]
      wt <- weight_list[j]
      
      #' identify features that overlap
      overlap <- st_intersection(buff, sf_object) 
      count_by_poly <- nrow(overlap) * wt 
      poly_wt_count <- c(poly_wt_count, count_by_poly) 
    }
    
    temp <- data.frame(GEOID = poly_ids[i,1],
                       wt_count = sum(poly_wt_count))
    temp_df <- rbind(temp_df, temp)
    
    rm(temp, poly)
    if(i %% 100 == 0) print(i)
  }
  
  return(temp_df)
}

npl_counts <- count_in_buffer(ct_env, npl) %>%
  mutate_if(is.factor, as.character) %>%
  rename(npl_count = wt_count)

ct_env <- left_join(ct_env, npl_counts, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("Number of NPL sites within the census tract") +
  geom_sf(data = ct_env, aes(fill = npl_count), col=NA) +
  scale_fill_viridis(name = "No. of NPL sites") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/NPL sites.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(npl_counts, npl)

#' -----------------------------------------------------------------------------
#' Number of TRI sites and 5 year average tpy releases 
#' Weighted by distance to the CT (as above)
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/tri_inventory.RData")
plot(st_geometry(tri_by_facility))

#' Function for counts and total emissions weighted by distance
count_emissions_in_buffer <- function(data_sf, sf_object, emissions_var,
                                      data_id_col = 1, sf_id_col = 1) {
  temp_df <- data.frame()
  
  poly_ids <- st_set_geometry(data_sf, NULL) %>%
    select(data_id_col) %>%
    distinct
  
  unique_points <- ungroup(sf_object) %>%
    select(sf_id_col) %>%
    distinct()
  
  poly_wt_count <- vector()
  poly_wt_sum <- vector()
  
  for (i in 1:nrow(poly_ids)) {
    poly <- filter(data_sf, GEOID == poly_ids[i,1])
    #poly <- filter(data_sf, GEOID == "08001008401")
    poly_250 <- doughnut(poly, 250, 0)
    poly_500 <- doughnut(poly, 500, 250)
    poly_750 <- doughnut(poly, 750, 500)
    poly_1000 <- doughnut(poly, 1000, 750)
    
    buff_list <- list(poly, poly_250, poly_500, poly_750, poly_1000)
    buff_name <- as.character(c(0, 250, 500, 750, 1000))
    weight_list <- c(1, 1, 0.5, 0.25, 0.1)
    
    poly_wt_count <- vector()

    for (j in 1:length(buff_list)) {
      buff <- buff_list[[j]]
      name <- buff_name[j]
      wt <- weight_list[j]
      
      #' identify unique points within the polygon
      overlap <- st_intersection(buff, unique_points)
      count_by_poly <- nrow(overlap) * wt 
      poly_wt_count <- c(poly_wt_count, count_by_poly) 
      
      #' sum emissions of included sites
      #' use all points (account for different pollutants)
      overlap2 <- st_intersection(buff, sf_object) %>%
        st_set_geometry(NULL)
      sites_sum <- ifelse(nrow(overlap2)==0, 0, sum(overlap2[,emissions_var])) * wt
      poly_wt_sum <- c(poly_wt_sum, sites_sum) 
    }
    
    temp <- data.frame(GEOID = poly_ids[i,1],
                       wt_count = sum(poly_wt_count),
                       wt_sum = sum(poly_wt_sum))
    temp_df <- rbind(temp_df, temp)
    
    rm(temp, poly)
    if(i %% 100 == 0) print(i)
  }
  
  return(temp_df)
}

tri_counts_emissions <- count_emissions_in_buffer(ct_env, tri_by_facility, "total_tpy") %>%
  mutate_if(is.factor, as.character) %>%
  rename(tri_count = wt_count,
         tri_tpy = wt_sum)

ct_env <- left_join(ct_env, tri_counts_emissions, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("Number of TRI sites within the census tract") +
  geom_sf(data = ct_env, aes(fill = tri_count), col=NA) +
  scale_fill_viridis(name = "No. of TRI sites") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/TRI sites.jpeg", sep=""), 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("TRI site releases (tpy) within the census tract") +
  geom_sf(data = ct_env, aes(fill = tri_tpy), col=NA) +
  scale_fill_viridis(name = "Tons per year") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/TRI releases.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(tri, tri_totals, tri_by_facility, tri_counts_emissions)

#' -----------------------------------------------------------------------------
#' Number of solid waste facilites, waste water treatment plants, and compost
#' facilities within the census tract 
#' Weighted as the NPL sites above
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/wwtf.RData")
load("./Data/Spatial Data/lf.RData")
load("./Data/Spatial Data/compost.RData")

wwtf <- select(wwtf, Permit_Nam) %>%
  rename(facility_id = Permit_Nam)

lf <- select(lf, PGM_SITE_N) %>%
  rename(facility_id = PGM_SITE_N)

compost <- select(compost, FACILITY_N) %>%
  rename(facility_id = FACILITY_N)

waste_sites <- rbind(wwtf, lf, compost)
plot(st_geometry(waste_sites))

waste_site_counts <- count_in_buffer(ct_env, waste_sites) %>%
  mutate_if(is.factor, as.character) %>%
  rename(waste_site_count = wt_count)

ct_env <- left_join(ct_env, waste_site_counts, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("Number of WWTF, landfills, and composting sites within the census tract") +
  geom_sf(data = ct_env, aes(fill = waste_site_count), col=NA) +
  scale_fill_viridis(name = "No. of sites") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/waste sites.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(compost, lf, wwtf, waste_sites, waste_site_counts)

#' -----------------------------------------------------------------------------
#' Number of major criteria pollutant sources within the census tract
#' Weighted by distance as above
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/emissions_inventory.RData")

inventory_major <- filter(inventory_criteria, major == 1) %>%
  select(site_id) %>%
  distinct

major_emit_counts <- count_in_buffer(ct_env, inventory_major) %>%
  mutate_if(is.factor, as.character) %>%
  rename(major_emit_count = wt_count)

ct_env <- left_join(ct_env, major_emit_counts, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("Number of major criteria pollutant emitters within the census tract") +
  geom_sf(data = ct_env, aes(fill = major_emit_count), col=NA) +
  scale_fill_viridis(name = "No. of sites") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/major emitters.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(emissions_by_facility, emissions_totals, inventory, inventory_criteria,
   inventory_major, inventory_other, major_emit_counts)

#' -----------------------------------------------------------------------------
#' Number of CAFOs within the census tract
#' Weighed by distance as above
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/cafo.RData")

cafo_counts <- count_in_buffer(ct_env, cafo) %>%
  mutate_if(is.factor, as.character) %>%
  rename(cafo_count = wt_count)

ct_env <- left_join(ct_env, cafo_counts, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("Number of CAFOs within the census tract") +
  geom_sf(data = ct_env, aes(fill = cafo_count), col=NA) +
  scale_fill_viridis(name = "No. of sites") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/CAFOs.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(cafo, cafo_counts)

#' -----------------------------------------------------------------------------
#' Number of mines and oil and gas wells within the census tract 
#' Weighed as above
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/mines.RData")
load("./Data/Spatial Data/wells.RData")

mines <- select(mines, rec_no) %>%
  rename(id = rec_no) %>%
  distinct
  
wells <- select(wells, API) %>%
  rename(id = API) %>%
  distinct

mines_wells <- rbind(mines, wells)

mine_well_counts <- count_in_buffer(ct_env, mines_wells) %>%
  mutate_if(is.factor, as.character) %>%
  rename(mine_well_count = wt_count)

ct_env <- left_join(ct_env, mine_well_counts, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")

ggplot() +
  ggtitle("Number of mines and wells within the census tract") +
  geom_sf(data = ct_env, aes(fill = mine_well_count), col=NA) +
  scale_fill_viridis(name = "No. of sites") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = paste("./Figures/CEI Figures/Environmental Variables/mines and wells.jpeg", sep=""), 
       device = "jpeg", dpi=600)

rm(mines, wells, mines_wells, mine_well_counts)

#' -----------------------------------------------------------------------------
#' Median year built
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/acs.RData")

yr_blt <- select(acs, GEOID, med_year_blt_housing) %>% 
  filter(GEOID %in% ct_env$GEOID) %>% 
  st_set_geometry(NULL)

ct_env <- left_join(ct_env, yr_blt, by="GEOID")
save(ct_env, file="./Data/CEI Data/CT_Environmental.RData")