#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: June 1, 2018
#' Date Updated: June 25, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script creates the cumulative exposure index used in the study of the
#' relationship between environmental/social exposures and adiposity at birth
#' in the Healthy Start cohort
#' 
#' June 25, 2018: added categorical exposure variables based on the median
#' of ENV and SOC
#' 
#' Based on methods in Cushing et al. (2016) and CalEnviroScreen 3.0
#' 
#' NOTE: don't forget the ./ before the directory when reading in files!
#' NOTE: Must be connected to the LEAD server
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)
library(IC2)
library(haven)

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
#' 1) Read in HS_I participants and assign them a census tract
#' 2) Join with HSI dataset
#' 3) Identify dates (using week_ending) for pregnancy (conception to delivery)
#' 
#' From HS Data Dictionary:
#' PID = participant ID
#' conception_date = estimated date of conception ()
#' di1 = delivery date (YYYY-MM-DD)
#' -----------------------------------------------------------------------------

#' Read in HS_I dataset
#' N = 1410 participants

# p_path <- "P:/LEAD/Outside-Users/Martenies/DataPull180318/"
# hs_data <- read_sas(data_file = paste(p_path, "p0137.sas7bdat", sep=""))
# save(hs_data, file="./Data/CEI Data/hs_data_for_cei.RData")

load("./Data/CEI Data/hs_data_for_cei.RData")

#' Get conception and delivery dates, as well as "week_ending" dates
hs_data <- select(hs_data, pid, conception_date, di1) %>%
  mutate(conception_date = format(as.Date(conception_date, origin="1960-01-01"),"%Y-%m-%d")) %>%
  mutate(conception_date = as.Date(conception_date)) %>%
  mutate(exp_start = ceiling_date(conception_date, unit = "week"),
         exp_end = ceiling_date(di1, unit = "week")) %>%
  mutate(pregnancy_weeks = difftime(di1, conception_date, unit="week")) %>%
  distinct()

summary(as.numeric(hs_data$pregnancy_weeks))
hist(as.numeric(hs_data$pregnancy_weeks))

#' Drop negative pregnancy weeks (conception date is wrong)
#' and NAs for pregnancy duration (no delivery date)
#' N = 1359 (96.4%) with plausible pregnancy durations
hs_data <- filter(hs_data, !(is.na(pregnancy_weeks))) %>%
  filter(pregnancy_weeks > 0) %>%
  filter(pregnancy_weeks < 50)

summary(as.numeric(hs_data$pregnancy_weeks))
hist(as.numeric(hs_data$pregnancy_weeks))
nrow(hs_data)

#' Get rid of the SAS formats and labels
hs_data <- zap_formats(hs_data) %>%
  zap_labels()

#' Read in geocoded participant addresses
#' N = 1334 geocoded addresses (95%)

#' p_path <- "P:/LEAD/Outside-Users/Martenies/"
#' hs_geocode <- read_xlsx(paste(p_path,
#'                               "hs1_geocoded_2011_2016_proj_nodups.xlsx",
#'                               sep="")) %>%
#'   #' Create the sf object
#'   st_as_sf(coords = c("longitude", "latitude"), crs = ll_wgs84) %>%
#'   st_transform(crs=albers) %>%
#'   select(PID) %>%
#'   rename(pid = PID) %>%
#'   mutate_if(is.integer, as.numeric)
#' head(hs_geocode)
#' 
#' save(hs_geocode, file="./Data/CEI Data/hs_geocode.RData")
load("./Data/CEI Data/hs_geocode.RData")

#' Merge HS data with HS locations
hs_all <- left_join(hs_geocode, hs_data, by="pid") %>%
  filter(!(is.na(pregnancy_weeks)))

#' Assign a CT value
load("./Data/Spatial Data/dm_tracts.RData")
dm_tracts <- select(dm_tracts, GEOID)
head(dm_tracts)

plot(dm_tracts)
nrow(dm_tracts)

#' N = 1202 participants (85%) have geocoded addresses AND live in
#' census tracts within the area
hs <- hs_all %>%
  st_intersection(dm_tracts)
head(hs)

plot(st_geometry(dm_tracts))
plot(st_geometry(hs), col="red", add=T)

summary(as.numeric(hs$pregnancy_weeks))
hist(as.numeric(hs$pregnancy_weeks))

#' -----------------------------------------------------------------------------
#' Assign AP exposures based on census tract, conception, and delivery
#' -----------------------------------------------------------------------------

load("./Data/CEI Data/CT_Air Pollution.RData")
ct_air_pollution <- st_set_geometry(ct_air_pollution, NULL) %>%
  mutate(week_ending = as.Date(week_ending))

hs_df <- st_set_geometry(hs, NULL) %>%
  select(pid, GEOID, exp_start, exp_end) %>%
  mutate(exp_start = as.Date(exp_start),
         exp_end = as.Date(exp_end)) %>%
  as.data.frame()

#' Add exposure concentrations to hs dataset based on the two-week periods
#' when conception and delivery occurred
for (i in 1:nrow(hs_df)) {
  
  #' subset air pollution data based on which census tract the mom lives in
  ap <- filter(ct_air_pollution, GEOID == as.character(hs_df[i, "GEOID"]))
  
  #' get concentrations for the week-ending dates of her pregnancy
  date_seq <- seq.Date(from = as.Date(hs_df[i, "exp_start"]),
                       to = as.Date(hs_df[i, "exp_end"]),
                       by  = "week")
  pm <- select(ap, week_ending, biweekly_average_pm_pred) %>%
    filter(week_ending %in% date_seq)
  
  o3 <- select(ap, week_ending, biweekly_average_o3_pred) %>%
    filter(week_ending %in% date_seq)
  
  #' Average exposures over the duration of the pregnancy
  hs_df[i,"mean_pm"] <- mean(pm$biweekly_average_pm_pred, na.rm=T)
  hs_df[i,"mean_o3"] <- mean(o3$biweekly_average_o3_pred, na.rm=T)
  hs_df[i,"max_o3"] <- max(o3$biweekly_average_o3_pred, na.rm=T)
}

hs_df <- select(hs_df, pid, mean_pm, mean_o3, max_o3)
summary(hs_df)

#' Drop participants (N=11) with missing air pollution data 
#' (living the the farthest east census tracts-- too far for kriging)
#' N = 1149 (81.4%) participants with full air pollution data
hs <- left_join(hs, hs_df, by="pid") %>%
  filter(!(is.na(mean_pm)))
summary(hs)

#' -----------------------------------------------------------------------------
#' Assign ENV and SOC exposures based on census tract, conception, and delivery
#' -----------------------------------------------------------------------------

load("./Data/CEI Data/CT_Environmental.RData")
ct_env <- st_set_geometry(ct_env, NULL)

hs <- left_join(hs, ct_env, by="GEOID") %>% 
  #' reverse code tree cover so that less tree cover is higher ranked
  mutate(pct_no_tree_cover = 100 - pct_tree_cover)

load("./Data/CEI Data/CT_Social.RData")
ct_soc <- st_set_geometry(ct_soc, NULL) %>%
  #' Inverse median income so that smaller incomes are ranked higher
  #' (more disadvantaged)
  mutate(inv_med_income = 1/med_income)

hs <- left_join(hs, ct_soc, by="GEOID")

summary(hs)

#' -----------------------------------------------------------------------------
#' Convert exposure metrics to percentile scores
#' -----------------------------------------------------------------------------

names(hs)

hs_percentile <- select(hs, -c(2:7)) %>%
  st_set_geometry(NULL) %>%
  mutate_at(-1, .funs = funs(percent_rank)) %>%
  mutate_at(-1, funs(. * 100))
colnames(hs_percentile)[-1] <- paste("ptile_", colnames(hs_percentile)[-1], sep="")
summary(hs_percentile)

hs <- left_join(hs, hs_percentile, by="pid")
summary(hs)

#' -----------------------------------------------------------------------------
#' Creating the cumulative exposure index
#' 
#' Based on methods from CalEnviroScreen 3.0:
#' https://oehha.ca.gov/media/downloads/calenviroscreen/report/ces3report.pdf
#'      
#'      ENV is the weighted average of air pollution and built environment
#'      percentile scores
#'           PM2.5, O3, traffic, and TRI exposures are given full weight
#'           Other built environment variables are given 0.5 weight
#'      
#'      SOC is the average of ACS, crime, and susceptibility percentile scores
#'           Vulnerable population variables are not weighted
#'      
#'      CEI = (ENV/10) x (SOC/10)
#' -----------------------------------------------------------------------------

#' Rowmeans function from stackoverflow:
#' https://stackoverflow.com/questions/33401788/dplyr-using-mutate-like-rowmeans/35553114
my_rowmeans = function(...) Reduce(`+`, list(...))/length(list(...))

hs_cei <- hs %>%

  #' Air pollution score and built environment score
  #'     Note: use pct_no_tree_cover so that lower tree cover has higher scores
  mutate(air_pol_score = my_rowmeans(ptile_mean_pm, ptile_mean_o3, 
                              ptile_tri_tpy, ptile_sum_aadt_intensity, na.rm=T),
         blt_env_score = my_rowmeans(ptile_pct_no_tree_cover, ptile_pct_impervious,
                              ptile_npl_count, ptile_waste_site_count, 
                              ptile_major_emit_count, ptile_cafo_count, 
                              ptile_mine_well_count, na.rm=T)) %>%

  #' SES and biological susceptibility scores
  #' ses_score does not include pct_poc or median income
  #' ses_score_2 will be used for sensitivity analyses
  #'     NOte: use inverse median income so that high incomes have lower rankings
  mutate(ses_score = my_rowmeans(ptile_pct_less_hs, ptile_pct_unemp, 
                                 ptile_pct_hh_pov, ptile_pct_limited_eng, 
                                 ptile_violent_crime_rate, 
                                 ptile_property_crime_rate, na.rm=T),
         ses_score_2 = my_rowmeans(ptile_pct_less_hs, ptile_pct_unemp, 
                                   ptile_pct_hh_pov, ptile_pct_limited_eng, 
                                   ptile_violent_crime_rate, 
                                   ptile_property_crime_rate,
                                   ptile_inv_med_income, ptile_pct_poc, na.rm=T),
         suscept_score = my_rowmeans(ptile_cvd_rate_adj, ptile_res_rate_adj, 
                                     na.rm=T)) %>%
  
  #' ENV is weighted mean of air pollution and built environment
  #' SOC is the mean of ses and susceptibility
  #' SOC_2 is the mean of the two scores using ses_score_2
  mutate(env = ((air_pol_score * 1) + (blt_env_score * 0.5)) / (1 + 0.5),
         soc = (ses_score + suscept_score) / 2,
         soc_2 = (ses_score_2 + suscept_score) / 2)

hist(hs_cei$env)
hist(hs_cei$env_2, add=T)
  
max_env <- max(hs_cei$env)
max_soc <- max(hs_cei$soc)
max_soc_2 <- max(hs_cei$soc_2)

hs_cei <- hs_cei %>%
  #' Create a scaled ENV and SOC (based on the max CT score)
  mutate(env_scaled = (env / max_env) * 10,
         soc_scaled = (soc / max_soc) * 10,
         soc_2_scaled = (soc_2 / max_soc_2) * 10) %>% 
  
  #' CEI is the product of the ENV and SOC scores (converted to deciles) 
  mutate(cei = (env/10) * (soc/10),
         cei_2 = (env/10) * (soc_2/10),
         cei_scaled = env_scaled * soc_scaled,
         cei_2_scaled = env_scaled * soc_2_scaled) %>%
  distinct()

summary(hs_cei)

hist(hs_cei$mean_pm)
hist(hs_cei$mean_o3)
hist(hs_cei$env)
hist(hs_cei$env_scaled)
hist(hs_cei$soc)
hist(hs_cei$soc_scaled)
hist(hs_cei$soc_2)
hist(hs_cei$soc_2_scaled)
hist(hs_cei$cei)
hist(hs_cei$cei_scaled)
hist(hs_cei$cei_2)
hist(hs_cei$cei_2_scaled)
hist(hs_cei$cei_3)
hist(hs_cei$cei_3_scaled)

rank_check <- hs_cei %>% 
  select(pid, env, env_scaled, soc, soc_scaled, cei, cei_scaled) %>% 
  mutate(env_rank = dense_rank(env),
         env_scaled_rank = dense_rank(env_scaled),
         soc_rank = dense_rank(soc),
         soc_scaled_rank = dense_rank(soc_scaled),
         cei_rank = dense_rank(cei),
         cei_scaled_rank = dense_rank(cei_scaled))

#' -----------------------------------------------------------------------------
#' High/Low groups of exposure for sensitivity analyses 
#' Note: was going to use tertiles, but there weren't enough people
#' in each of the categories; for eacmple, no one was high environmental/low 
#' social, etc.
#' -----------------------------------------------------------------------------

#' cutoffs for tertiles
env_33 <- quantile(hs_cei$env, probs = 0.33)
env_66 <- quantile(hs_cei$env, probs = 0.66)
soc_33 <- quantile(hs_cei$soc, probs = 0.33)
soc_66 <- quantile(hs_cei$soc, probs = 0.66)

hs_cei <- hs_cei %>%
  #' Categorical for tertiles
  mutate(env_tert_cat = ifelse(env < env_33, 1, 
                               ifelse(env >= env_33 & env < env_66, 2, 3)),
         soc_tert_cat = ifelse(soc < soc_33, 1, 
                               ifelse(soc >= soc_33 & soc < soc_66, 2, 3))) %>% 
  
  #' Indicators for tertiles                             
  mutate(env_low = ifelse(env < env_33, 1, 0),
         env_mid = ifelse(env >= env_33 & env < env_66, 1, 0),
         env_high = ifelse(env >= env_66, 1, 0),
         soc_low = ifelse(soc < soc_33, 1, 0),
         soc_mid = ifelse(soc >= soc_33 & soc < soc_66, 1, 0),
         soc_high = ifelse(soc >= soc_66, 1, 0)) %>%
  
  #' Indicators for each combination of exposures
  mutate(env_low_soc_low = ifelse(env_low == 1 & soc_low == 1, 1, 0),
         env_low_soc_mid = ifelse(env_low == 1 & soc_mid == 1, 1, 0),
         env_low_soc_high = ifelse(env_low == 1 & soc_high == 1, 1, 0),
         env_mid_soc_low = ifelse(env_mid == 1 & soc_low == 1, 1, 0),
         env_mid_soc_mid = ifelse(env_mid == 1 & soc_mid == 1, 1, 0),
         env_mid_soc_high = ifelse(env_mid == 1 & soc_high == 1, 1, 0),
         env_high_soc_low = ifelse(env_high == 1 & soc_low == 1, 1, 0),
         env_high_soc_mid = ifelse(env_high == 1 & soc_mid == 1, 1, 0),
         env_high_soc_high = ifelse(env_high == 1 & soc_high == 1, 1, 0))

glimpse(hs_cei)

table(hs_cei$env_tert_cat, hs_cei$soc_tert_cat)

table(hs_cei$env_low_soc_low)
table(hs_cei$env_low_soc_mid)
table(hs_cei$env_low_soc_high)
table(hs_cei$env_mid_soc_low)
table(hs_cei$env_mid_soc_mid)
table(hs_cei$env_mid_soc_high)
table(hs_cei$env_high_soc_low)
table(hs_cei$env_high_soc_mid)
table(hs_cei$env_high_soc_high)

#' Medians
env_50 <- quantile(hs_cei$env, probs = 0.50)
soc_50 <- quantile(hs_cei$soc, probs = 0.50)

hs_cei <- hs_cei %>%
  #' Categorical for median cutpoints
  mutate(env_med_cat = ifelse(env < env_50, 1, 2),
         soc_med_cat = ifelse(soc < soc_50, 1, 2)) %>% 
  
  #' Indicators for median cutpointss
  mutate(env_below = ifelse(env < env_50, 1, 0),
         env_above = ifelse(env >= env_50, 1, 0),
         soc_below = ifelse(soc < soc_50, 1, 0),
         soc_above = ifelse(soc >= soc_50, 1, 0)) %>%
  
  #' Indicators for each combination of exposures
  mutate(env_below_soc_below = ifelse(env_below == 1 & soc_below == 1, 1, 0),
         env_below_soc_above = ifelse(env_below == 1 & soc_above == 1, 1, 0),
         env_above_soc_below = ifelse(env_above == 1 & soc_below == 1, 1, 0),
         env_above_soc_above = ifelse(env_above == 1 & soc_above == 1, 1, 0))

glimpse(hs_cei)

table(hs_cei$env_med_cat, hs_cei$soc_med_cat)

table(hs_cei$env_below_soc_below)
table(hs_cei$env_below_soc_above)
table(hs_cei$env_above_soc_below)
table(hs_cei$env_above_soc_above)

save(hs, hs_cei, file="./Data/CEI Data/hs_cei.RData")
