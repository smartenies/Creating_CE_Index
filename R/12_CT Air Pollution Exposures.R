#' -----------------------------------------------------------------------------
#' Date created: January 14, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: This script generates the kriged estimates at CT centroids
#' -----------------------------------------------------------------------------

library(sf)
library(sp)
library(spatialEco)
library(gstat)
library(automap)
library(tidyverse)
library(lubridate)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' Set up objects for kriging
#' #' Note: gstat (for kriging) requires sp objects (not sf)
#' -----------------------------------------------------------------------------

#' Census tract centroids for kriging
#' Get shapefile and project to Albers Equal Area
#' Depending on the study area, will need to change this
unit_name <- "CO_Tracts_AEA.csv"
spatial_units <- read_csv(here::here("Data", unit_name)) %>%
  st_as_sf(wkt = "WKT", crs = albers)
plot(st_geometry(spatial_units))

ct_ap <- select(spatial_units, GEOID) %>%
  arrange(GEOID) %>%
  st_centroid

ct_air_pollution <- select(spatial_units, GEOID) %>%
  arrange(GEOID)

#' Converting the census tract centroids from sf to sp 
ct_sp <- as(ct_ap, "Spatial")
plot(ct_sp)

#' Monitoring data--
#' Read in files
#' Calculate 2-week averages
#' Use daily means for PM2.5 and daily8h max for ozone
pm_files <- list.files(here::here("Data/AQS_Data"), pattern = "daily_88101",
                       full.names = T)
pm_monitors <- do.call(bind_rows, lapply(pm_files, read_csv))
colnames(pm_monitors) <- str_replace(colnames(pm_monitors), " ", "_")

pm_monitors <- filter(pm_monitors, State_Code == "08") %>% 
  mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
         Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
  mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
  filter(POC == 1) %>% 
  mutate(year = strftime(Date_Local, format = "%Y"),
         week_starting = as.Date(floor_date(Date_Local, unit = "week")),
         week_ending = as.Date(ceiling_date(Date_Local, unit = "week"))) %>%
  mutate(Date_Local = as.Date(Date_Local)) %>%
  arrange(Date_Local, monitor_id) %>%
  distinct()

pm_average <- pm_monitors %>%
  dplyr::group_by(monitor_id, week_ending) %>%
  summarize(weekly_average = mean(Arithmetic_Mean)) %>%
  mutate(pollutant = "pm")
 
################################################## 
o3_files <- list.files(here::here("Data/AQS_Data"), pattern = "8hour_44201",
                       full.names = T)
o3_monitors <- do.call(bind_rows, lapply(o3_files, read_csv))
colnames(o3_monitors) <- gsub(" ", "_", colnames(o3_monitors))

o3_monitors <- filter(o3_monitors, State_Code == "08") %>% 
  mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
         Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
  mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
  filter(POC == 1) %>% 
  group_by(monitor_id, Date_Local) %>% 
  summarize(Max_Measurement = max(Mean_Excluding_All_Flagged_Data)) %>% 
  mutate(year = strftime(Date_Local, format = "%Y"),
         week_starting = as.Date(floor_date(Date_Local, unit = "week")),
         week_ending = as.Date(ceiling_date(Date_Local, unit = "week"))) %>%
  mutate(Date_Local = as.Date(Date_Local)) %>%
  arrange(Date_Local, monitor_id) %>%
  distinct()

o3_average <- o3_monitors %>%
  dplyr::group_by(monitor_id, week_ending) %>%
  summarize(weekly_average = mean(Max_Measurement)) %>%
  mutate(pollutant = "pm")

head(pm_monitors)
head(o3_monitors)

tail(pm_monitors)
tail(o3_monitors)

summary(pm_monitors)
summary(o3_monitors)

hist(pm_monitors$weekly_average)
hist(o3_monitors$weekly_average)

plot(st_geometry(ct_ap), add=T, col="black")
plot(st_geometry(pm_monitors), add=T, col="green")
plot(st_geometry(o3_monitors), add=T, col="blue")

#' Check date lists
#' Not all dates are included, so need a full list for later
length(unique(pm_monitors$week_ending)) == length(unique(o3_monitors$week_ending))

start <- ceiling_date(as.Date("2009-01-01"), unit="week")  + 7
ap_dates <- data.frame(week_ending = seq.Date(from = start, 
                                              to = as.Date("2017-12-31"),
                                              by="2 weeks"))
week_list <- sort(unique(ap_dates$week_ending))

#' -----------------------------------------------------------------------------
#' Biweekly mean PM2.5
#' ------------------------------------------------------------------------------

library(gstat)
library(automap)

show.vgms()
all_models <- c("Exp", "Sph", "Gau", "Mat", "Ste", "Lin")

#' cutoff distance is 40 km
#' Choosing a kriging cutoff distance:
#'     - At 30 km we lose some of the eastern census tracts
#'     - At 40 km we lose the two eastern-most census tracts, but they only 
#'       have a couple participants
#'     - Going to go with 40 km for now and see how it looks
c_dist = 40000

pm_ct_data <- data.frame()
pm_cv_results <- data.frame()
pm_diagnostics <- data.frame()

for (i in 1:length(week_list)) {
  print(paste("Week", i, "of", length(week_list)))
  
  #' use second week ending date to identify concentrations
  week_start <- week_list[i] - 7
  week_end <- week_list[i]
  
  #' Weekly concentration at monitors
  #' Drop rows with NA values
  pm_week <- filter(pm_monitors, week_ending %in% c(week_start, week_end)) %>%
    filter(!is.na(weekly_average))
  
  if(nrow(pm_week) == 0) {next}
  
  #' Biweekly average for each monitor
  pm_week <- pm_week %>%
    select(-week_ending, -year) %>%
    group_by(monitor_id) %>%
    summarize(pollutant = "pm",
              biweekly_average = mean(weekly_average),
              week_ending = week_end)
  
  #' Converting the monitor points from sf to sp 
  pm_week <- as(pm_week, "Spatial")
  
  #' Kriging using gstat
  #' First, fit the empirical variogram
  vgm <- variogram(biweekly_average ~ 1, pm_week, cutoff = c_dist)
  #plot(vgm)
  
  #' Second, fit the model
  vgm_fit <- fit.variogram(vgm, model=vgm(all_models), fit.kappa = seq(.3,5,.01))
  model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
  #plot(vgm, vgm_fit)
  
  #' Third, krige
  ok_result <- krige(biweekly_average ~ 1, pm_week, ct_sp, vgm_fit,
                     maxdist = c_dist)
  
  #' Fourth, leave-one out cross validation
  cv_result <- krige.cv(biweekly_average ~ 1, pm_week, vgm_fit)
  summary(cv_result)
  
  #' me_mean = mean error divided by mean observed
  #' MSNE = mean square normalized error (mean of squared z scores)
  #' RMSE = root mean squared error (same units as pollutant)
  #' cor_obs_pred = correlation between observed and predicted, should be 1
  #' cor_pred_res = correlation between predicted and residual, should be 0
  cv_compare <- compare.cv(list(krige.cv_output = cv_result))
  
  #' Data frame of results
  temp <- data.frame(GEOID = ct_sp@data$GEOID,
                     week_ending = week_end,
                     biweekly_average_pred = ok_result$var1.pred,
                     biweekly_average_var = ok_result$var1.var)
  pm_ct_data <- rbind(pm_ct_data, temp)
  
  #' Data frame of cross-validation results
  cv_result <- as.data.frame(cv_result)
  cv_result$week_ending <- week_end
  pm_cv_results <- rbind(pm_cv_results, cv_result)
  
  temp2 <- data.frame(week_ending = week_end,
                      monitor_n = nrow(pm_week),
                      monitor_min = min(pm_week$biweekly_average, na.rm=T),
                      monitor_max = max(pm_week$biweekly_average, na.rm=T),
                      monitor_mean = mean(pm_week$biweekly_average, na.rm=T),
                      model = model,
                      modeled_min = min(ok_result$var1.pred, na.rm=T),
                      modeled_max = max(ok_result$var1.pred, na.rm=T),
                      modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                      mean_error = unname(unlist(cv_compare[1,1])),
                      me_mean = unname(unlist(cv_compare[2,1])),
                      msne = unname(unlist(cv_compare[5,1])),
                      rmse = unname(unlist(cv_compare[8,1])),
                      cor_obs_pred = unname(unlist(cv_compare[6,1])),
                      cor_pred_res = unname(unlist(cv_compare[7,1])))
  pm_diagnostics <- rbind(pm_diagnostics, temp2)
  
  rm(pm_week, vgm, vgm_fit, model, ok_result, cv_result, cv_compare,
     temp, temp2, week_end)
}

pm_ct_data <- pm_ct_data %>%
  rename(biweekly_average_pm_pred = biweekly_average_pred,
         biweekly_average_pm_var = biweekly_average_var) %>%
  mutate_if(is.factor, as.character)

pm_ct_data_full <- full_join(ap_dates, pm_ct_data, by="week_ending")

save(pm_ct_data, pm_ct_data_full, pm_cv_results, pm_diagnostics,
     file = "./Data/Air Quality/pm kriging results.RData")
write_xlsx(pm_diagnostics,
           path="./Data/Air Quality/PM Kriging Diagnostics.xlsx")

#' ------------------------------------------------------------------------------
#' Biweekly mean daily 8-hour max O3
#' ------------------------------------------------------------------------------

o3_ct_data <- data.frame()
o3_cv_results <- data.frame()
o3_diagnostics <- data.frame()

for (i in 1:length(week_list)) {
  print(paste("Week", i, "of", length(week_list)))
  
  #' use second week ending date to identify concentrations
  week_start <- week_list[i] - 7
  week_end <- week_list[i]
  
  #' Weekly concentration at monitors
  #' Drop rows with NA values
  o3_week <- filter(o3_monitors, week_ending %in% c(week_start, week_end)) %>%
    filter(!is.na(weekly_average))
  
  if(nrow(o3_week) == 0) {next}
  
  #' Biweekly average for each monitor
  o3_week <- o3_week %>%
    select(-week_ending, -year) %>%
    group_by(monitor_id) %>%
    summarize(pollutant = "o3",
              biweekly_average = mean(weekly_average),
              week_ending = week_end)
  
  #' Converting the monitor points from sf to sp 
  o3_week <- as(o3_week, "Spatial")
  
  #' Kriging using gstat
  #' First, fit the empirical variogram
  vgm <- variogram(biweekly_average ~ 1, o3_week, cutoff = c_dist)
  #plot(vgm)
  
  #' Second, fit the model
  vgm_fit <- fit.variogram(vgm, model=vgm(all_models), fit.kappa = seq(.3,5,.01))
  model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
  #plot(vgm, vgm_fit)
  
  #' Third, krige
  ok_result <- krige(biweekly_average ~ 1, o3_week, ct_sp, vgm_fit,
                     maxdist = c_dist)
  
  #' Fourth, leave-one out cross validation
  cv_result <- krige.cv(biweekly_average ~ 1, o3_week, vgm_fit)
  summary(cv_result)
  
  #' me_mean = mean error divided by mean observed
  #' MSNE = mean square normalized error (mean of squared z scores)
  #' RMSE = root mean squared error (same units as pollutant)
  #' cor_obs_pred = correlation between observed and predicted, should be 1
  #' cor_pred_res = correlation between predicted and residual, should be 0
  cv_compare <- compare.cv(list(krige.cv_output = cv_result))
  
  #' Data frame of results
  temp <- data.frame(GEOID = ct_sp@data$GEOID,
                     week_ending = week_end,
                     biweekly_average_pred = ok_result$var1.pred,
                     biweekly_average_var = ok_result$var1.var)
  o3_ct_data <- rbind(o3_ct_data, temp)
  
  #' Data frame of cross-validation results
  cv_result <- as.data.frame(cv_result)
  cv_result$week_ending <- week_end
  o3_cv_results <- rbind(o3_cv_results, cv_result)
  
  temp2 <- data.frame(week_ending = week_end,
                      monitor_n = nrow(o3_week),
                      monitor_min = min(o3_week$biweekly_average, na.rm=T),
                      monitor_max = max(o3_week$biweekly_average, na.rm=T),
                      monitor_mean = mean(o3_week$biweekly_average, na.rm=T),
                      model = model,
                      modeled_min = min(ok_result$var1.pred, na.rm=T),
                      modeled_max = max(ok_result$var1.pred, na.rm=T),
                      modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                      mean_error = unname(unlist(cv_compare[1,1])),
                      me_mean = unname(unlist(cv_compare[2,1])),
                      msne = unname(unlist(cv_compare[5,1])),
                      rmse = unname(unlist(cv_compare[8,1])),
                      cor_obs_pred = unname(unlist(cv_compare[6,1])),
                      cor_pred_res = unname(unlist(cv_compare[7,1])))
  o3_diagnostics <- rbind(o3_diagnostics, temp2)
  
  rm(o3_week, vgm, vgm_fit, model, ok_result, cv_result, cv_compare,
     temp, temp2, week_end)
}

o3_ct_data <- o3_ct_data %>%
  rename(biweekly_average_o3_pred = biweekly_average_pred,
         biweekly_average_o3_var = biweekly_average_var) %>%
  mutate_if(is.factor, as.character)

o3_ct_data_full <- full_join(ap_dates, o3_ct_data, by="week_ending")

save(o3_ct_data, o3_ct_data_full, o3_cv_results, o3_diagnostics,
     file = "./Data/Air Quality/o3 kriging results.RData")
write_xlsx(o3_diagnostics,
           path="./Data/Air Quality/O3 Kriging Diagnostics.xlsx")


#' ------------------------------------------------------------------------------
#' Combined both sets of data into a single data frame
#' ------------------------------------------------------------------------------

load("./Data/Spatial Data/dm_tracts.RData")
ct <- select(dm_tracts, GEOID) %>%
  arrange(GEOID)

ct_df <- st_set_geometry(ct, NULL)

start <- ceiling_date(as.Date("2009-01-01"), unit="week")  + 7
ap_dates <- data.frame(week_ending = seq.Date(from = start, 
                                              to = as.Date("2017-12-31"),
                                              by="2 weeks"))
week_list <- sort(unique(ap_dates$week_ending))

#' create data frame with all of the dates needed
temp <- data.frame()
for (i in 1:nrow(ap_dates)) {
  temp1 <- data.frame(week_ending = rep(ap_dates[i,1], times = nrow(ct)))
  temp1 <- bind_cols(ct_df, temp1)
  temp <- bind_rows(temp, temp1)
  rm(temp1)
}

ct_air_pollution <- st_as_sf(left_join(temp, ct, by="GEOID"))
head(ct_air_pollution)

rm(dm_tracts)

load("./Data/Air Quality/pm kriging results.RData")
load("./Data/Air Quality/o3 kriging results.RData")

ct_air_pollution <- left_join(ct_air_pollution, pm_ct_data_full,
                              by=c("GEOID", "week_ending")) %>%
  full_join(o3_ct_data_full, by=c("GEOID", "week_ending")) %>%
  arrange(week_ending, GEOID)

save(ct_air_pollution, file="./Data/CEI Data/CT_Air Pollution.RData")

# rm(o3, o3_average, o3_ct_data, o3_cv_results, o3_diagnostics,
#    pm, pm_average, pm_ct_data, pm_cv_results, pm_diagnostics,
#    monitors)
