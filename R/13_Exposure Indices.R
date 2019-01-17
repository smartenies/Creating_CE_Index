#' =============================================================================
#' Date created: January 17, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script creates the cumulative exposure index used in the study
#' Based on methods in Cushing et al. (2016) and CalEnviroScreen 3.0
#' 
#' NOTE: This script uses dummy participant data 
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

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Create dummy data for the participants based on dates in 2014 and the 
#' census tract IDs
#' -----------------------------------------------------------------------------

unit_name <- "CO_Tracts_AEA.csv"
spatial_units <- read_csv(here::here("Data", unit_name)) %>%
  st_as_sf(wkt = "WKT", crs = albers)
plot(st_geometry(spatial_units))

ct_ids <- unique(spatial_units$GEOID)

conception_dates <- seq.Date(from = as.Date("01/01/2014", format = "%m/%d/%Y"), 
                             to = as.Date("03/15/2014", format = "%m/%d/%Y"), 
                             by = "day")

participant_df <- data.frame(participant_id = seq(1:20))

#' Randomly assign a GEOID and a conception date
participant_df$GEOID <-  sample(ct_ids, size = nrow(participant_df), replace = F)
participant_df$conception_date <- sample(conception_dates, 
                                        size = nrow(participant_df), replace = T)

#' Get delivery date by adding 280 days to conception
participant_df$delivery_date <- participant_df$conception_date + 280

#' Exposure start and end dates using the ceiling_date function
participant_df <- participant_df %>% 
  mutate(exp_start = ceiling_date(conception_date, unit = "week"),
         exp_end = ceiling_date(delivery_date, unit = "week")) %>%
  mutate(pregnancy_weeks = difftime(delivery_date, conception_date, unit="week")) %>%
  distinct()

#' -----------------------------------------------------------------------------
#' Assign AP exposures based on census tract, conception, and delivery
#' -----------------------------------------------------------------------------

ct_air_pollution <- read_csv(here::here("Data", "CT_Air_POllution.csv")) %>%
  mutate(week_ending = as.Date(week_ending))

hs_df <- participant_df %>%
  select(participant_id, GEOID, exp_start, exp_end) %>%
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

#' -----------------------------------------------------------------------------
#' Assign ENV and SOC exposures based on census tract, conception, and delivery
#' -----------------------------------------------------------------------------

ct_env <- read_csv(here::here("Data", "CT_ENV.csv")) %>% 
  select(-WKT)

hs_df <- left_join(hs_df, ct_env, by="GEOID") %>% 
  #' reverse code tree cover so that less tree cover is higher ranked
  mutate(pct_no_tree_cover = 100 - pct_tree_cover)

ct_soc <- read_csv(here::here("Data", "CT_SOC.csv")) %>% 
  select(-WKT) %>% 

  #' Inverse median income so that smaller incomes are ranked higher
  #' (more disadvantaged)
  mutate(inv_med_income = 1/med_income)

hs_df <- left_join(hs_df, ct_soc, by="GEOID")

summary(hs_df)

#' -----------------------------------------------------------------------------
#' Convert exposure metrics to percentile scores
#' -----------------------------------------------------------------------------

names(hs_df)
drop <- c("GEOID", "exp_start", "exp_end", "area_km2")

hs_percentile <- select(hs_df, -drop) %>%
  mutate_at(-1, .funs = funs(percent_rank)) %>%
  mutate_at(-1, funs(. * 100))
colnames(hs_percentile)[-1] <- paste("ptile_", colnames(hs_percentile)[-1], sep="")
summary(hs_percentile)

hs_df <- left_join(hs_df, hs_percentile, by="participant_id")
summary(hs_df)

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

hs_cei <- hs_df %>%

  #' Air pollution score and built environment score
  #'     Note: use pct_no_tree_cover so that lower tree cover has higher scores
  mutate(air_pol_score = my_rowmeans(ptile_mean_pm, ptile_mean_o3, 
                              ptile_tri_tpy, ptile_mean_aadt_intensity, na.rm=T),
         blt_env_score = my_rowmeans(ptile_pct_no_tree_cover, ptile_pct_impervious,
                              ptile_npl_count, ptile_major_emit_count,
                              na.rm=T)) %>%

  #' SES and biological susceptibility scores
  #' ses_score does not include pct_poc or median income
  #' ses_score_2 will be used for sensitivity analyses
  #'     NOte: use inverse median income so that high incomes have lower rankings
  mutate(ses_score = my_rowmeans(ptile_pct_less_hs_grad, ptile_pct_civ_unemployed, 
                                 ptile_pct_hh_pov, ptile_pct_hh_limited_eng, 
                                 ptile_violent_crime_rate, 
                                 ptile_property_crime_rate, na.rm=T),
         ses_score_2 = my_rowmeans(ptile_pct_less_hs_grad, ptile_pct_civ_unemployed, 
                                   ptile_pct_hh_pov, ptile_pct_hh_limited_eng, 
                                   ptile_violent_crime_rate, 
                                   ptile_property_crime_rate,
                                   ptile_inv_med_income, ptile_pct_non_nhw, na.rm=T),
         suscept_score = my_rowmeans(ptile_cvd_rate_adj, ptile_res_rate_adj, 
                                     na.rm=T)) %>%
  
  #' ENV is weighted mean of air pollution and built environment
  #' SOC is the mean of ses and susceptibility
  #' SOC_2 is the mean of the two scores using ses_score_2
  mutate(env = ((air_pol_score * 1) + (blt_env_score * 0.5)) / (1 + 0.5),
         soc = (ses_score + suscept_score) / 2,
         soc_2 = (ses_score_2 + suscept_score) / 2)

hist(hs_cei$env)
hist(hs_cei$soc)
hist(hs_cei$soc_2, add=T)

max_env <- max(hs_cei$env)
max_soc <- max(hs_cei$soc)
max_soc_2 <- max(hs_cei$soc_2)

hs_cei <- hs_cei %>%
  #' CEI is the product of the ENV and SOC scores (converted to deciles) 
  mutate(cei = (env/10) * (soc/10),
         cei_2 = (env/10) * (soc_2/10))

summary(hs_cei)

hist(hs_cei$mean_pm)
hist(hs_cei$mean_o3)
hist(hs_cei$env)
hist(hs_cei$soc)
hist(hs_cei$soc_2)
hist(hs_cei$cei)
hist(hs_cei$cei_2)

#' -----------------------------------------------------------------------------
#' Categorical exposures based on tertiles
#' -----------------------------------------------------------------------------

#' cutoffs for tertiles
env_33 <- quantile(hs_cei$env, probs = 0.33, na.rm=T)
env_66 <- quantile(hs_cei$env, probs = 0.66, na.rm=T)
soc_33 <- quantile(hs_cei$soc, probs = 0.33, na.rm=T)
soc_66 <- quantile(hs_cei$soc, probs = 0.66, na.rm=T)

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

write_csv(hs_cei, here::here("Data", "CT_CE_Index.csv"))
