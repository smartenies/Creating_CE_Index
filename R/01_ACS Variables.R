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
#' This script summarizes ACS variables for each of the census tracts
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
#' Reading in the ACS files and mapping key demographic/SES variables
#' .txt files extracted from the ACS TIGER/Line geodatabase
#' See /Data/ACS_* for the ArcMap file and original .gbd
#' Make sure there are no XML files in the data folder (they mess with the loop)
#' =============================================================================

years <- c("2010_2014")

file_list <- list.files(paste("./Data/ACS_", years, sep=""), pattern="X")
dataset <- data.frame()
  
for (i in 1:length(file_list)){
  file <- file_list[i]
  temp <- read.table(paste("./Data/ACS_", years, "/", file, sep=""),
                     header=TRUE, sep=",")
  temp$OBJECTID <-NULL
  temp$GEOID <- as.character(temp$GEOID)
  temp <- temp[order(temp$GEOID),]
    
  if (i == 1) {
    dataset <- temp
    print(file)
  } else {
    dataset <- merge(dataset, temp, "GEOID") #' joining columns
    print(file)
    rm(temp)
  }
}
  
acs <- dataset
acs$GEOID <- gsub("14000US", "", acs$GEOID)
rm(dataset, file_list, file)
  
#' -----------------------------------------------------------------------------
#' Creating new demographic and SES variables for mapping
#' Using data dictionary for the ACS TIGER/Line files
#' Available:
#' 
#'  ACS Data Dictionary
#'  Variable     Definition
#'  B01001e1     Total population
#'  B01001e3     Male population under 5 years
#'  B01001e27    Female population under 5 years
#'  
#'  B01001e20    Male population 65-66
#'  B01001e21    Male population 67-69
#'  B01001e22    Male population 70-74
#'  B01001e23    Male population 75-79
#'  B01001e24    Male population 80-84
#'  B01001e25    Male populiation 85+
#'  B01001e44    Female population 65-66
#'  B01001e45    Female population 67-69
#'  B01001e46    Female population 70-74
#'  B01001e47    Female population 75-79
#'  B01001e48    Female population 80-84
#'  B01001e49    Female populiation 85+  
#'  
#'  B03002e3     Non-Hispanic white
#'  
#'  B05002e13    Foreign born
#'  
#'  B15003e1     Population 25 years and older
#'  B15003e2     No schooling
#'  B15003e3     Nursery school
#'  B15003e4     K
#'  B15003e5     1st grade
#'  B15003e6     2nd grade
#'  B15003e7     3rd grade
#'  B15003e8     4th grade
#'  B15003e9     5th grade
#'  B15003e10    6th grade
#'  B15003e11    7th grade
#'  B15003e12    8th grade
#'  B15003e13    9th grade
#'  B15003e14    10th grade
#'  B15003e15    11th grade
#'  B15003e16    12th grade, no diploma
#'  B15003e17    High school diploma
#'  B15003e18    GED or alternative
#'  B15003e19    Some college (1 year), no degree
#'  B15003e20    Some college (>1 year), no degree
#'  B15003e21    Associate's
#'  B15003e22    Bachelor's
#'  B15003e23    Master's
#'  B15003e24    Professional
#'  B15003e25    Doctorate
#'    
#'  B16002e1     Total households
#'  B16002e4     Spanish speaking households, limited English
#'  B16002e7     Other Indo_European speaking households, limited English
#'  B16002e10    Asian/PI speaking households, limited English
#'  B16002e13    Other language speaking households, limited English
#'  
#'  B17017e2     Households in poverty
#'  
#'  B23025e3     Population 16+ in the civilian work force
#'  B23025e5     Population 16+ in the civilian work force that are unemployed
#'  
#'  B19013e1     Median household income (2014 inflation-adjusted $)
#'  
#'  B25035e1	   Median year built for housing
#' -----------------------------------------------------------------------------
  
load("./Data/Spatial Data/co_tracts.RData")

#' list of the ACS variable names
#' going to drop these later
acs_vars <- colnames(acs)[-1]
  
#' Merge ACS data with polygons and to get the spatial data in the df
acs <- left_join(co_tracts, acs, by="GEOID") %>%
  mutate(area_km2 = as.vector(unclass(st_area(.))) / 1000^2) %>%
  mutate(total_pop = B01001e1,
         pop_dens = total_pop / area_km2,
         under5 = B01001e3 + B01001e27,
         over64 = B01001e20 + B01001e21 + B01001e22 + B01001e23 + 
                  B01001e24 + B01001e25 + B01001e44 + B01001e45 + 
                  B01001e46 + B01001e47 + B01001e48 + B01001e49,
         nhw = B03002e3, # non-Hispanic White only
         poc = total_pop - nhw,
         fb = B05002e13,
         over24 = B15003e1,
         less_hs = B15003e2 + B15003e3 + B15003e4 +
                   B15003e5 + B15003e6 + B15003e7 + B15003e8 +
                   B15003e9 + B15003e10 + B15003e11 + B15003e12 +
                   B15003e13 + B15003e14 + B15003e15 +
                   B15003e16,
         hs_grad = B15003e17 + B15003e18,
         some_col = B15003e19 + B15003e20,
         assoc = B15003e21,
         bach = B15003e22,
         advanced = B15003e23 + B15003e24 + B15003e25,
         civ_wf = B23025e3,
         unemp = B23025e5,
         total_hh = B16002e1,
         limited_eng = B16002e4 + B16002e7 + B16002e10 + B16002e13,
         hh_pov = B17017e2,
         med_income = B19013e1,
         med_year_blt_housing = B25035e1) %>%
  mutate(pct_under5 = (under5 / total_pop) * 100,
         pct_over64 = (over64 / total_pop) * 100,
         pct_nhw = (nhw / total_pop) * 100,
         pct_poc = (poc / total_pop) * 100,
         pct_fb = (fb / total_pop) * 100,
         pct_less_hs = (less_hs / over24) * 100,
         pct_hs = (hs_grad / over24) * 100,
         pct_college = ((bach + advanced) / over24) * 100,
         pct_unemp = (unemp / civ_wf) * 100,
         pct_limited_eng = (limited_eng / total_hh) * 100,
         pct_hh_pov = (hh_pov / total_hh) * 100) %>%
  mutate(pct_hs_grad = 1 - pct_less_hs,
         pct_employed = 1 - pct_unemployed,
         pct_hh_above_pov = 1 = pct_hh_pov,
         pct_not_limited_eng = 1 - pct_limited_eng) %>% 
  select(-one_of(acs_vars))
head(acs)
summary(acs)  
  
save(acs, file="./Data/Spatial Data/acs.RData")

#' -----------------------------------------------------------------------------
#' Mapping some key demographic and SES variables for Colorado
#' -----------------------------------------------------------------------------
library(viridis)

load(file="./Data/Spatial Data/acs.RData")
acs <- mutate_if(acs, is.integer, as.numeric)
  
ses_vars <- c("pop_dens", "pct_under5", "pct_over64", "pct_poc", "pct_fb",
              "pct_less_hs", "pct_unemp", "pct_limited_eng", "pct_hh_pov",
              "med_income", "med_year_blt_housing")
titles <- c("Population density (persons per km2)",
            "Population under 5 years of age",
            "Population 65 years of age or older",
            "Persons of color", "Foreign born population",
            "Adults 25 or older with less than a high school diploma",
            "Percentage of the civilan workforce (ages 16+) that are unemployed",
            "Households that speak limited English",
            "Households with last year income below the poverty level",
            "Median household income (2014$)",
            "Housing median year built")

#' First, across all of CO, then, in the three county area
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
                            ifelse(ses_var=="med_year_blt_housing", "Year",
                                   "Percentage\nof census tract\npopulation:")))
  
  ses_plot_fn(df = acs, var_string = ses_var, name_string = ses_name)
  ggsave(filename = paste("./Figures/CEI Figures/ACS Variables/", "co ", ses_var, " ", years, ".jpeg", sep=""), 
         device = "jpeg", dpi=600)
}

load("./Data/Spatial Data/dm_tracts.RData")
dm_ct_list <- unique(dm_tracts$GEOID)
acs_dm <- acs %>%
  filter(GEOID %in% dm_ct_list)

for (i in 1:length(ses_vars)) {
  ses_var <- ses_vars[i]
  ses_name <- ifelse(ses_var=="med_income", "2014$", 
                     ifelse(ses_var=="pop_dens", "Persons per km\u00B2",
                            ifelse(ses_var=="med_year_blt_housing", "Year",
                                   "Percentage\nof census tract\npopulation:")))
  
  ses_plot_fn(df = acs_dm, var_string = ses_var, name_string = ses_name)
  ggsave(filename = paste("./Figures/CEI Figures/ACS Variables/", "dm ", ses_var, " ", years, ".jpeg", sep=""), 
         device = "jpeg", dpi=600)
}