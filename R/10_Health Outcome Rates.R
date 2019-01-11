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
#' This script summarizes the mortality and hospitalization rates for the 
#' study area.
#' 
#' Follows methods used by CalEnviroScreen: 
#'      ZIP codes -> census blocks -> census tracts
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
#' Sensitive populations: cardiopulmonary hospitalizations per 10,000
#' Data were summarized at the ZCTA level for the entire state for the period
#' of 2010-2014
#' 
#' Based on code proided by Ryan Gan Dec 6, 2017
#' 
#' Following methods in CalEnviroScreen 3.0
#' Need age-adjusted rates at the census tract level
#'      1) Calulcate age-adjusted rates at the ZCTA level using the US Standard
#'         Population from 2000
#'      2) Assign census blocks the average age-adjusted rate of the ZCTA it
#'         falls in
#'      3) Caculate a population-weighted rate for each census tract based on
#'         the block populations and average rate
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' First, calculate age-adjusted rates for each ZCTA
#' 
#' noticed broomfield county_geo 014 was sometimes assigned county_final 159
#' there is no county 159. All other spatial indicators suggest broomfield 014
#' set wrfgrid values of 0 to missing
#' remove those missing a WRFGRID ID
#' remove hospitalizations from 2015
#' -----------------------------------------------------------------------------

hosp_path <- "U:/Research/2017_ALA_HIA/Data/CHA Data/co_hosp_w_outcome_df.csv"
co_hosp <- read.csv(hosp_path, header=T, stringsAsFactors = F) %>%
  mutate(county_final = ifelse(county_geo == "014" & county_final == "159", 
                               "014", county_final),
         FIPS = paste("08", county_final, sep=""),
         WRFGRID_ID = ifelse(WRFGRID_ID == 0, NA, WRFGRID_ID)) %>%
  filter(!(is.na(WRFGRID_ID))) %>%
  filter(ADMYY != 15) 
  
age_labels <- c("p0_1", "p1_4", "p5_9", "p10_14", "p15_19", "p20_24", "p25_29", 
                "p30_34", "p35_39", "p40_44", "p45_49", "p50_54", "p55_59", 
                "p60_64", "p65_69", "p70_74", "p75_79", "p80_84", "p85_99") 

co_hosp$age_cat <- cut(co_hosp$AGEYRS, 
                       breaks = c(0,1,5,9,14,19,24,29,34,39,44,49,54,59,64,69,
                                  74,79,84,max(co_hosp$AGEYRS, na.rm=T)),
                       labels = age_labels,
                       include.lowest=T)
co_hosp$age_cat <- as.character(co_hosp$age_cat)

head(co_hosp[,c("AGEYRS", "age_cat")])

#' ZCTA Populations (estimated as part of the ALA HIA)
zip_pop <- read.csv("./Data/ACS_2010_2014/co_populations.csv") %>%
  rename(ZIP = GEOID) %>%
  mutate(ZIP = as.character(ZIP),
         p1_4 = p0_4 - p0_1) %>%
  select(-OBJECTID) %>%
  select(-contains("_se")) %>%
  gather(key = "age_cat", value = "pop", total:p1_4) %>%
  filter(age_cat %in% age_labels)


#' standard populations (2000 US Population, 19 age groups)
#' Data dictionary: https://seer.cancer.gov/stdpopulations/stdpopdic.html
std_pop <- read.table("./Data/ACS_2010_2014/stdpop.19ages.txt", header=F) %>%
  mutate(Standard = as.numeric(substr(V1, start=1, stop=3)),
         age_group = as.numeric(substr(V1, start=4, stop=6)),
         std_pop_mil = as.numeric(substr(V1, start=7, stop=8))) %>%
  filter(Standard == 203) %>%
  mutate(age_cat = age_labels,
         std_pct = std_pop_mil / sum(std_pop_mil)) %>%
  select(-V1, -Standard)
 
# calculate rate by zipcode -----
zip_rate_crude <- co_hosp %>% 
  group_by(ZIP, age_cat) %>% 
  summarise(cvd_n = sum(cvd_dx), res_n = sum(resp_dx),
            cpm_n = sum(cvd_dx, resp_dx)) %>% 
  
  # filter to zip codes in colorado
  filter(as.numeric(ZIP) >= 80001 & as.numeric(ZIP) <= 81658) %>% 
  
  # join with age-stratified census and standard pop
  right_join(zip_pop, by = c("ZIP", "age_cat")) %>% 
  right_join(std_pop, by = "age_cat") %>% 
  
  # removing zip codes with 0 population
  filter(pop != 0) %>%
  
  # calculate 5-year rate 
  mutate(cvd_n = ifelse(is.na(cvd_n), 0, cvd_n / 5),
         res_n = ifelse(is.na(res_n), 0, res_n / 5),
         # set NA cvd resp n to 0
         cpm_n = cvd_n + res_n,
         
         # calculate age-stratified rates per 10,000
         cvd_rate = cvd_n / pop * 10000,
         res_rate = res_n / pop * 10000,
         cpm_rate = cpm_n / pop * 10000,
         
         # multiply rate by proportion of standard pop
         cvd_adj = cvd_rate * std_pct,
         res_adj = res_rate * std_pct,
         cpm_adj = cpm_rate * std_pct)

zip_rate_adj <- zip_rate_crude %>% 
  group_by(ZIP) %>% 
  summarise(cvd_rate_adj = sum(cvd_adj), 
            res_rate_adj = sum(res_adj),
            cpm_rate_adj = sum(cpm_adj))

save(zip_rate_adj, file="./Data/CHA Data/age-adjusted rates ZCTA.RData")

summary(zip_rate_crude)
summary(zip_rate_adj)


#' -----------------------------------------------------------------------------
#' Next, assign census blocks a ZCTA rate and then calculate a population-
#' weighted average rate for the census tracts
#' -----------------------------------------------------------------------------

# load("./Data/Spatial Data/dm_tracts.RData")
# boundary <- st_make_grid(dm_tracts, n=1) %>%
#   st_buffer(., dist = 2000)

blocks <- st_read(paste(geo_data, "tabblock2010_08_pophu.shp", sep=""),
                  stringsAsFactors = F) %>%
  st_transform(crs=albers) %>%
  select(BLOCKID10, POP10) %>%
  rename(BLOCK = BLOCKID10, pop = POP10)
head(blocks)

block_centroids <- st_centroid(blocks)
head(block_centroids)

# plot(st_geometry(blocks), border="red", fill=NA)
# plot(st_geometry(block_centroids), col="blue", add=T)

tracts <- st_read(paste(geo_data, "tl_2014_08_tract.shp", sep=""),
                  stringsAsFactors = F) %>%
  st_transform(crs=albers) %>%
  select(GEOID) %>%
  rename(TRACT = GEOID)

plot(st_geometry(blocks), col=NA, border="blue")
plot(st_geometry(tracts), col=NA, border="red", add=T)

zcta <- st_read(paste(geo_data, "ACS_2014_5YR_ZCTA.shp", sep=""),
                stringsAsFactors = F) %>%
  st_transform(crs=albers) %>%
  select(ZCTA5CE10) %>%
  rename(ZIP = ZCTA5CE10)
head(zcta)

#' Join census block centroids (with population), tracts, and ZCTA
btz <- block_centroids %>%
  st_join(tracts) %>%
  st_join(zcta) %>%
  st_set_geometry(NULL)

load("./Data/CHA Data/age-adjusted rates ZCTA.RData")

#' Join rates to census blocks by ZCTA ID
btz <- left_join(btz, zip_rate_adj, by="ZIP")
summary(btz)

#' Get population-weighted census tract rates
hosp_rates <- btz %>%
  # filter(!is.na(ZCTA5CE10)) %>%
  # filter(!is.na(GEOID)) %>%
  
  #' Just need tract IDs and block populations
  select(TRACT, pop, cvd_rate_adj, res_rate_adj, cpm_rate_adj) %>%
  
  #' #' replace NaN and Inf values with NA
  #' mutate(cvd_rate_adj = ifelse((is.nan(cvd_rate_adj) | is.infinite(cvd_rate_adj)),
  #'                              NA, cvd_rate_adj),
  #'        res_rate_adj = ifelse((is.nan(res_rate_adj) | is.infinite(res_rate_adj)),
  #'                              NA, res_rate_adj),
  #'        cpm_rate_adj = ifelse((is.nan(cpm_rate_adj) | is.infinite(cpm_rate_adj)),
  #'                              NA, cpm_rate_adj)) %>%
  
  #' calculate weighted means for each hospitalization rate
  group_by(TRACT) %>% 
  summarise(cvd_rate_adj = weighted.mean(x = cvd_rate_adj, w = pop, na.rm=T),
            res_rate_adj = weighted.mean(x = res_rate_adj, w = pop, na.rm=T),
            cpm_rate_adj = weighted.mean(x = cpm_rate_adj, w = pop, na.rm=T)) %>%
  rename(GEOID = TRACT)
summary(hosp_rates)

#' Join with CO Tracts
load("./Data/Spatial Data/co_tracts.RData")
co_tracts <- select(co_tracts, GEOID)

hosp_rates <- left_join(co_tracts, hosp_rates, by="GEOID")

save(hosp_rates, file="./Data/Spatial Data/hospitalization_rates.RData")

#' -----------------------------------------------------------------------------
#' Map the hospitalization rates
#' -----------------------------------------------------------------------------

ggplot() +
  ggtitle("CVD Hospitalizations per 10,000") +
  geom_sf(data = hosp_rates, aes(fill = cvd_rate_adj), col=NA) +
  scale_fill_viridis(name = "Hospitalization\nRate per 10,000",
                     option = "C") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme

ggsave(filename = paste("./Figures/CEI Figures/Hospitalization/CVD Rates per 10,000.jpeg", sep=""), 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Respiratory Hospitalizations per 10,000") +
  geom_sf(data = hosp_rates, aes(fill = cvd_rate_adj), col=NA) +
  scale_fill_viridis(name = "Hospitalization\nRate per 10,000",
                     option = "C") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme

ggsave(filename = paste("./Figures/CEI Figures/Hospitalization/RES Rates per 10,000.jpeg", sep=""), 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Cardiopulmonary Hospitalizations per 10,000") +
  geom_sf(data = hosp_rates, aes(fill = cvd_rate_adj), col=NA) +
  scale_fill_viridis(name = "Hospitalization\nRate per 10,000",
                     option = "C") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme

ggsave(filename = paste("./Figures/CEI Figures/Hospitalization/CPM Rates per 10,000.jpeg", sep=""), 
       device = "jpeg", dpi=600)