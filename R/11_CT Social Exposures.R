#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: May 24, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script summarizes the social variables at the census tract level
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

acs_gdb_name <- "ACS_2014_5YR_TRACT_08_COLORADO.gdb"

acs_units <- st_read(dsn = here::here("Data/ACS_Data", acs_gdb_name),
                     layer = str_remove(acs_gdb_name, ".gdb")) %>%
  st_transform(crs=albers)
plot(st_geometry(acs_units))
ct_soc <- acs_units

rm(acs_units)
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Sensitive populations: see 10_Health Outcome Rates.R
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/hospitalization_rates.RData")

hosp_rates <- st_set_geometry(hosp_rates, NULL)

ct_soc <- left_join(ct_soc, hosp_rates, by="GEOID")
save(ct_soc, file="./Data/CEI Data/CT_Social.RData")

ggplot() +
  ggtitle("Age-adjusted cardiovascular disease hospitalization rate") +
  geom_sf(data = ct_soc, aes(fill = cvd_rate_adj), col=NA) +
  scale_fill_viridis(name = "Cases per\n10,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Social Variables/cvd hospitalization.jpeg", 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Age-adjusted respiratory disease hospitalization rate") +
  geom_sf(data = ct_soc, aes(fill = res_rate_adj), col=NA) +
  scale_fill_viridis(name = "Cases per\n10,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Social Variables/res hospitalization.jpeg", 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Age-adjusted cardiopulmonary disease hospitalization rate") +
  geom_sf(data = ct_soc, aes(fill = cpm_rate_adj), col=NA) +
  scale_fill_viridis(name = "Cases per\n10,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Social Variables/cpm hospitalization.jpeg", 
       device = "jpeg", dpi=600)

rm(hosp_rates)

#' -----------------------------------------------------------------------------
#' ACS Variables: see 2_ACS Variables.R
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/acs.RData")

acs <- st_set_geometry(acs, NULL) %>%
  select(GEOID, pop_dens, contains("pct_"), med_income)

ct_soc <- left_join(ct_soc, acs, by="GEOID")
save(ct_soc, file="./Data/CEI Data/CT_Social.RData")

ses_vars <- c("pop_dens", "pct_poc", "pct_fb", "pct_less_hs", "pct_unemp", 
              "pct_limited_eng", "pct_hh_pov", "med_income")
titles <- c("Population density (persons per km2)",
            "Persons of color", 
            "Foreign born population",
            "Adults 25 or older with less than a high school diploma",
            "Percentage of the civilan workforce (ages 16+) that are unemployed",
            "Households that speak limited English",
            "Households with last year income below the poverty level",
            "Median household income (2014$)")

#' Plots
ses_plot_fn <- function(df, var_string, name_string) {
  ggplot() +
    ggtitle(titles[i]) +
    geom_sf(data = df, aes_string(fill = var_string), col=NA) +
    scale_fill_viridis(name = name_string) +
    xlab("") + ylab("") +
    theme(legend.position = "right") +
    simple_theme
}

for (i in 1:length(ses_vars)) {
  ses_var <- ses_vars[i]
  ses_name <- ifelse(ses_var=="med_income", "2014$", 
                     ifelse(ses_var=="pop_dens", "Persons per km\u00B2",
                            "Percentage\nof census tract\npopulation:"))
  
  ses_plot_fn(df = ct_soc, var_string = ses_var, name_string = ses_name)
  ggsave(filename = paste("./Figures/CEI Figures/Social Variables/", ses_var, 
                          ".jpeg", sep=""), 
         device = "jpeg", dpi=600)
}

rm(acs)

#' -----------------------------------------------------------------------------
#' Crime rates: 3_Crime Statistics.R
#' Spatially join based on CT centroid
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/crime_rates.RData")

crime_rates <- select(crime_rates, crime_rate, violent_rate, property_rate) %>%
  rename(all_crime_rate = crime_rate, 
         violent_crime_rate = violent_rate,
         property_crime_rate = property_rate)

ct_crime <- st_centroid(ct_soc) %>%
  select(GEOID) %>%
  st_join(crime_rates) %>%
  st_set_geometry(NULL)

ct_soc <- left_join(ct_soc, ct_crime, by="GEOID")
save(ct_soc, file="./Data/CEI Data/CT_Social.RData")

ggplot() +
  ggtitle("Crime rate per 1,000 population (all incidents)") +
  geom_sf(data = ct_soc, aes(fill = all_crime_rate), col=NA) +
  scale_fill_viridis(name = "Incidents per\n1,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Social Variables/crime rates.jpeg", 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Violent crime rate per 1,000 population (all incidents)") +
  geom_sf(data = ct_soc, aes(fill = violent_crime_rate), col=NA) +
  scale_fill_viridis(name = "Incidents per\n1,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Social Variables/violent crime rates.jpeg", 
       device = "jpeg", dpi=600)

ggplot() +
  ggtitle("Property and non-violent crime rate per 1,000 population (all incidents)") +
  geom_sf(data = ct_soc, aes(fill = property_crime_rate), col=NA) +
  scale_fill_viridis(name = "Incidents per\n1,000") +
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Social Variables/property crime rates.jpeg", 
       device = "jpeg", dpi=600)