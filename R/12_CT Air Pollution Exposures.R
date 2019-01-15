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

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' Set up objects for kriging
#' #' Note: gstat (for kriging) requires sp objects (not sf)
#' -----------------------------------------------------------------------------

#' Census tract centroids for kriging
acs_gdb_name <- "ACS_2014_5YR_TRACT_08_COLORADO.gdb"

acs_units <- st_read(dsn = here::here("Data/ACS_Data", acs_gdb_name),
                     layer = str_remove(acs_gdb_name, ".gdb")) %>%
  st_transform(crs=albers)
plot(st_geometry(acs_units))

ct_ap <- select(acs_units, GEOID) %>%
  arrange(GEOID) %>%
  st_centroid

ct_air_pollution <- select(acs_units, GEOID) %>%
  arrange(GEOID)

#' Converting the census tract centroids from sf to sp 
ct_sp <- as(ct_ap, "Spatial")
plot(ct_sp)

#' Monitoring data
load("./Data/CEI Data/pm_monitor_metrics.RData")
load("./Data/CEI Data/o3_monitor_metrics.RData")
load("./Data/CEI Data/monitors.RData")

pm_monitors <- select(pm_monitors, monitor_id) %>%
  left_join(pm_average, by = "monitor_id") %>%
  arrange(week_ending) %>%
  mutate(year = year(week_ending)) %>%
  mutate(week_ending = as.Date(week_ending)) %>%
  filter(!(year == 2018)) %>%
  filter(!(is.na(week_ending)))

o3_monitors <- select(o3_monitors, monitor_id) %>%
  left_join(o3_average, by = "monitor_id") %>%
  arrange(week_ending) %>%
  mutate(year = year(week_ending)) %>%
  mutate(week_ending = as.Date(week_ending)) %>%
  filter(!(year == 2018)) %>%
  filter(!(is.na(week_ending))) %>%
  #' convert ppm to ppb
  mutate(weekly_average = weekly_average * 1000)

head(pm_monitors)
head(o3_monitors)

tail(pm_monitors)
tail(o3_monitors)

summary(pm_monitors)
summary(o3_monitors)

hist(pm_monitors$weekly_average)
hist(o3_monitors$weekly_average)

plot(st_geometry(dm_tracts), border="grey50")
plot(st_geometry(ct_ap), add=T, col="black")
plot(st_geometry(pm_monitors), add=T, col="green")
plot(st_geometry(o3_monitors), add=T, col="blue")

#' write out shapefiles to check distances
# st_write(dm_tracts[,4], "./Data/Spatial Data/Shapefiles/dm_tracts.shp",
#          delete_dsn = T)
# st_write(ct_ap, "./Data/Spatial Data/Shapefiles/dm_tracts_centroids.shp",
#          delete_dsn = T)
# st_write(pm_monitors, "./Data/Spatial Data/Shapefiles/pm_monitors.shp",
#          delete_dsn = T)
# st_write(o3_monitors, "./Data/Spatial Data/Shapefiles/o3_monitors.shp",
#          delete_dsn = T)

#' Choosing a kriging cutoff distance:
#'     - At 30 km we lose some of the eastern census tracts
#'     - At 40 km we lose the two eastern-most census tracts, but they only 
#'       have a couple participants
#'     - Going to go with 40 km for now and see how it looks

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
