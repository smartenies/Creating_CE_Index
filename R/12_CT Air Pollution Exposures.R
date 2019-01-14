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
#' Read in pollutant data and summarize to daily means
#' Source: AQS Data Mart (Script: 01_AQS_Data_Scrape.R)
#' -----------------------------------------------------------------------------

#' For which years and which state do we want data?
years <- c(2010:2014)
state <- "08" #Colorado
time_zone <- "America/Denver"

#' #' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' #' Criteria pollutants and carbon parameters
#' 
#' pol <- c("88101", "44201") # PM2.5 and O3

#++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Points for krigings
#' gstat requires sp objects, not sf objects
pts_name <- "gridded_aircraft_ozone.csv" 

krige_pts <- read_csv(here::here("Data", pts_name)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) %>% 
  st_transform(albers) %>% 
  select(WRFGRID_ID) %>% 
  st_centroid() %>% 
  mutate_if(is.character, as.numeric) 

head(krige_pts)
plot(st_geometry(krige_pts), pch = ".")

#' Converting from sf to sp 
krige_pts_sp <- as(krige_pts, "Spatial")
plot(krige_pts_sp)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' -----------------------------------------------------------------------------
#' Set up objects for kriging
#' #' Note: gstat (for kriging) requires sp objects (not sf)
#' -----------------------------------------------------------------------------

#' List of files to krige
krige_files <- data.frame(krige_files = list.files(path = here::here("Data/AQS_Data"))) %>%
  mutate(krige_files = as.character(krige_files)) %>%
  filter(str_detect(krige_files,
                    paste(as.character(years),collapse = '|')))
krige_files <- krige_files$krige_files

for (i in 1:length(krige_files)) {

  #' Output file names
  aqs_krige_name <- paste0(str_replace(krige_files[i], ".csv", ""), 
                           "_Kriged.csv")
  
  #' Output file names
  aqs_cv_name <- paste0(str_replace(krige_files[i], ".csv", "")
                        , "_CV.csv")
  
  #' Output file names
  aqs_diagnostics_name <- paste0(str_replace(krige_files[i], ".csv", ""), 
                                 "_Diagnostics.csv")
  
  
  daily <- read_csv(paste0(aqs_data_path, krige_files[i]))
  colnames(daily) <- gsub(" ", ".", colnames(daily))
  daily <- daily %>%  
    filter(State.Code == state) %>% 
    mutate(County.Code = str_pad(County.Code, 3, pad = "0"),
           Site.Num = str_pad(Site.Num, 4, pad = "0")) %>% 
    mutate(monitor_id = paste0(County.Code, Site.Num)) %>% 
    
    #' using the mean with flagged data excluded for 8h ozone
    #' Arithmetic mean for all other measures
    rowwise() %>% 
    mutate(mean = ifelse(str_detect(krige_files[i], "8hour_44201"), 
                         Mean.Excluding.All.Flagged.Data, 
                         Arithmetic.Mean)) %>% 
    
    #' convert ppm to ppb
    mutate(mean = ifelse(Units.of.Measure == "Parts per million", 
                         mean * 1000, mean)) %>% 
    mutate(Units.of.Measure = ifelse(Units.of.Measure == "Parts per million", 
                                     "ppb", Units.of.Measure)) %>%
    
    #' Just get the first measurement, not the co-located one
    filter(POC == 1)
  
  #' Monitor locations
  monitor_pts <- select(daily, monitor_id, Longitude, Latitude) %>%
    filter(!is.na(Longitude)) %>% 
    distinct() %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
    st_transform(albers)
  
  # plot(st_geometry(krige_pts))
  # plot(st_geometry(monitor_pts), col="red", add=T)
  
  #' if using 8h ozone data, need to select daily 8h max
  if(str_detect(krige_files[i], "8hour_44201")) {
    ozone <- select(daily, monitor_id, Date.Local, mean) %>% 
      group_by(monitor_id, Date.Local) %>% 
      summarize(mean = max(mean))
    
    daily <- select(daily, monitor_id, Date.Local, Units.of.Measure) %>%
      left_join(ozone, by=c("monitor_id", "Date.Local")) %>% 
      distinct()
    
    rm(ozone)
  }
  
  #' ------------------------------------------------------------------
  #' Interpolation and Ordinary kriging
  
  #' List of models to try to fit
  #' The ones listed in "all models" are the standard ones to try
  show.vgms()
  all_models <- c("Exp", "Sph", "Gau", "Cir", "Lin", "Log")
  
  #' cutoff distance is 50 km
  #' May need to play with this
  c_dist = 50000
  
  #' list of dates, pollutants to loop through
  dates <- unique(daily$Date.Local)
  
  #' data frames to collect results
  krige_data <- data.frame()
  cv_data <- data.frame()
  cv_diagnostics <- data.frame()
  
  for (j in 1:length(dates)) {
    daily2 <- filter(daily, Date.Local == dates[j])
    #if (nrow(daily2 == 0)) next
    
    monitors <- left_join(monitor_pts, daily2, by="monitor_id") %>% 
      as("Spatial")
    
    monitors <- monitors[!is.na(monitors@data$mean),]
    monitors@data$mean <- ifelse(is.infinite(monitors@data$mean), NA,
                                 monitors@data$mean)
    summary(monitors@data$mean)
    
    hist(monitors@data$mean)
    qqnorm(monitors@data$mean);qqline(monitors@data$mean, col=2)
    
    if (nrow(monitors) > 3) {
      data_norm_test <- shapiro.test(monitors@data$mean)
    } else {
      data_norm_test <- NA
    }
    
    #' Some IDW estimates for comparison
    idw_pwr2 <- idw(mean ~ 1, monitors, krige_pts_sp, idp = 2)
    idw_pwr2.5 <- idw(mean ~ 1, monitors, krige_pts_sp, idp = 2.5)
    idw_pwr3 <- idw(mean ~ 1, monitors, krige_pts_sp, idp = 3)
    
    #' if data are not normally distributed (based on Shapiro Wilk test), 
    #' use log-transformation-- this can sometimes help, but not always
    #' can change this criterion if needed
    if(all(!is.na(data_norm_test))) {
      if(data_norm_test$p.value < 0.05) {
        monitors@data$mean <- log(monitors@data$mean)
        monitors@data$mean <- ifelse(is.infinite(monitors@data$mean), NA,
                                     monitors@data$mean)
        monitors <- monitors[!is.na(monitors@data$mean),]
        hist(monitors@data$mean)
        qqnorm(monitors@data$mean);qqline(monitors@data$mean, col=2)
      }
    }
    
    #' Kriging using gstat
    #' First, fit the empirical variogram
    vgm <- variogram(mean ~ 1, monitors, cutoff = c_dist)
    # plot(vgm)
    
    #' Second, fit the model
    
    #' ERROR HANDLING
    fit_error <- tryCatch(
      fit.variogram(vgm, model=vgm(all_models),
                    fit.kappa = seq(.3,5,.01)),
      error = function(e) e
    )
    
    range_check <- "range" %in% colnames(fit_error)
    fit_method <- 7
    
    if(range_check == F) {
      fit_error <- tryCatch(
        fit.variogram(vgm, model=vgm(all_models),
                      fit.kappa = seq(.3,5,.01), fit.method = 6),
        error = function(e) e
      )
      fit_method <- 6
    } 
    
    range_check <- "range" %in% colnames(fit_error)
    range_val_check <- ifelse(range_check == F, F, fit_error$range[length(fit_error$range)] > 0)
    error_check <- !inherits(fit_error, "error")
    
    if(range_check == T & range_val_check == T & error_check == T) {
      
      vgm_fit <- fit.variogram(vgm, model=vgm(all_models),
                               fit.kappa = seq(.3,5,.01), fit.method = fit_method)
      
      
      model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
      # plot(vgm, vgm_fit)
      
      #' Third, krige
      ok_result <- krige(mean ~ 1, monitors, krige_pts_sp, vgm_fit)
      
      #' Format kriged data
      #' if data weren't normal, back-transform to original units
      #' need to apply a correction (See Oliver and Webster 2007, PAGE 185)
      #' https://books.google.com/books?hl=en&lr=&id=WBwSyvIvNY8C&oi=fnd&pg=PR5&ots=CCLmSNqK1c&sig=lFZanxv2eVSKec6nPdESzuIFrA4#v=onepage&q&f=false
      #' A back-transformed variance estimate for OK cannot be calculated because
      #' the mean is not known (page 185)
      if(all(!is.na(data_norm_test))) {
        if(data_norm_test$p.value < 0.05) {
          ok_result$var1.pred <- exp(ok_result$var1.pred + (0.5*ok_result$var1.var))
          ok_result$var1.var <- NA
        }
      }
    } else {
      ok_result <- data.frame(var1.pred = rep(NA, nrow(krige_pts_sp)),
                              var1.var = rep(NA, nrow(krige_pts_sp)))
      model <- NA
    }
    
    #' Summary data frame
    temp <- data.frame(WRFGRID_ID = krige_pts_sp@data$WRFGRID_ID,
                       pollutant = str_replace(krige_files[i], ".csv", ""),
                       date = dates[j],
                       conc_pred_idw_pwr2 = idw_pwr2$var1.pred,
                       conc_pred_idw_pwr2.5 = idw_pwr2.5$var1.pred,
                       conc_pred_idw_pwr3 = idw_pwr3$var1.pred,
                       conc_pred_ok = ok_result$var1.pred,
                       conc_var_ok = ok_result$var1.var)
    krige_data <- bind_rows(krige_data, temp)
    
    #' Fourth, leave-one out cross validation
    if(range_check == T & range_val_check == T & error_check == T) {
      cv_result <- krige.cv(mean ~ 1, monitors, vgm_fit)
      summary(cv_result)
      
      if(sum(is.na(cv_result$var1.pred)) == 0) {
        hist(cv_result$residual)
        qqnorm(cv_result$residual);qqline(cv_result$residual, col=2)
        
        if(nrow(monitors) >= 3) {
          cv_res_norm_test <- shapiro.test(cv_result$residual)
        } else {
          cv_res_norm_test <- NA
        }
        
        #' Data frames of cross-validation and diagnostic results
        
        #' See Li and Heap for a nice explanation of diagnostics
        #' https://pdfs.semanticscholar.org/686c/29a81eab59d7f6b7e2c4b060b1184323a122.pdf
        #' Mean error (same units as pollutants), measures bias should be small
        #' RMSE = root mean squared error (same units as pollutant), should be small
        #' cor_obs_pred = correlation between observed and predicted, should be 1
        #' cor_pred_res = correlation between predicted and residual, should be 0
        cv_compare <- compare.cv(list(krige.cv_output = cv_result))
        cv_result <- as.data.frame(cv_result) %>% 
          mutate(pollutant = str_replace(krige_files[i], ".csv", ""),
                 date = dates[j])
        cv_data <- bind_rows(cv_data, cv_result)
        
        temp2 <- data.frame(pollutant = str_replace(krige_files[i], ".csv", ""),
                            date = dates[j],
                            log_transformed = ifelse(is.na(data_norm_test), F, 
                                                     data_norm_test$p.value < 0.05),
                            monitor_n = nrow(monitors),
                            monitor_mean = mean(monitors$mean, na.rm=T),
                            monitor_min = min(monitors$mean, na.rm=T),
                            monitor_max = max(monitors$mean, na.rm=T),
                            model = ifelse(exists("model"),model, NA),
                            modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                            modeled_min = min(ok_result$var1.pred, na.rm=T),
                            modeled_max = max(ok_result$var1.pred, na.rm=T),
                            mean_error = unname(unlist(cv_compare[1,1])),
                            me_mean = unname(unlist(cv_compare[2,1])),
                            msne = unname(unlist(cv_compare[5,1])),
                            rmse = unname(unlist(cv_compare[8,1])),
                            cor_obs_pred = unname(unlist(cv_compare[6,1])),
                            cor_pred_res = unname(unlist(cv_compare[7,1])),
                            data_norm_test_p = ifelse(is.na(cv_res_norm_test), NA, 
                                                      cv_res_norm_test$p.value),
                            cv_res_norm_test_p = ifelse(is.na(cv_res_norm_test), NA, 
                                                        cv_res_norm_test$p.value))
        cv_diagnostics <- bind_rows(cv_diagnostics, temp2)
        
        rm(vgm, vgm_fit, cv_result, cv_compare, temp2)
      }
    } else {
      temp2 <- data.frame(pollutant = str_replace(krige_files[i], ".csv", ""),
                          date = dates[j],
                          log_transformed = ifelse(is.na(data_norm_test), F, 
                                                   data_norm_test$p.value < 0.05),
                          monitor_n = nrow(monitors),
                          monitor_mean = mean(monitors$mean, na.rm=T),
                          monitor_min = min(monitors$mean, na.rm=T),
                          monitor_max = max(monitors$mean, na.rm=T),
                          model = ifelse(exists("model"),model, NA),
                          modeled_mean = NA,
                          modeled_min = NA,
                          modeled_max = NA,
                          model = model,
                          modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                          modeled_min = min(ok_result$var1.pred, na.rm=T),
                          modeled_max = max(ok_result$var1.pred, na.rm=T))
      cv_diagnostics <- bind_rows(cv_diagnostics, temp2)
      rm(temp2)
    }
    
    rm(daily2, monitors, model, ok_result, temp)
  }
  #' Write out results
  write_csv(krige_data, here::here("Data", aqs_krige_name))
  write_csv(cv_data, here::here("Data", aqs_cv_name))
  write_csv(cv_diagnostics, here::here("Data", aqs_diagnostics_name))
}