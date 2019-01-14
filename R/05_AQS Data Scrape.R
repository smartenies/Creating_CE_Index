#' -----------------------------------------------------------------------------
#' Date created: January 14, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Download air pollution data from the EPA AQS website
#' ----------------------------------------------------------------------------


library(tidyverse)
library(readxl)

if(!dir.exists("./Data/Temp")) dir.create("./Data/Temp")
if(!dir.exists("./Data/AQS_Data")) dir.create("./Data/AQS_Data")
if(!dir.exists("./Data/Met_Data")) dir.create("./Data/Met_Data")

years <- c(2010:2014)
met_vars <- c("WIND", "PRESS", "TEMP", "RH_DP")

for (i in 1:length(years)) {
  
  #' Daily PM2.5
  aqs_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_88101_", 
                    years[i], ".zip")
  
  download.file(aqs_url, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/AQS_Data"))
  
  #' Daily met variables
  for (j in 1:length(met_vars)) {
    met_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_", met_vars[j],  
                      "_", years[i], ".zip")
    
    #' Download zipfile from the EPA website and unzip
    download.file(met_url, destfile = here::here("Data/Temp", "temp.zip"))
    unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/Met_Data"))
  }
  
  #' daily mean ozone data
  aqs_url2 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url2, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/AQS_Data"))
  
  #' daily 8h max ozone data
  aqs_url3 <- paste0("https://aqs.epa.gov/aqsweb/airdata/8hour_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url3, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/AQS_Data"))
}