#' -----------------------------------------------------------------------------
#' Project: UPAS Monitor Evaluation
#' Date created: May 17, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: scrape air pollution data from the EPA AQS website
#' https://aqs.epa.gov/api
#' Username: sheena.martenies@colostate.edu
#' Password: khakifrog54
#' ----------------------------------------------------------------------------
#' ----------------------------------------------------------------------------
#' Original AQS Scraper code:
#' AUTHOR: Chad W. Milando (Univ of Michigan; cmilando@umich.edu)
#' DATE: 5/18/2016
#' PURPOSE: scraper of AQS
#' 
#' Modified 09.12.17 by SEM to work for the Denver Metro area
#' ----------------------------------------------------------------------------
#' ----------------------------------------------------------------------------

#' info for AQS website:
user_name <- "sheena.martenies@colostate.edu"
pw <- "khakifrog54"


library(Hmisc)
library(stringr)

years <- c(2009:2017)
state <- "08" #Colorado

#' Denver Metro counties: Adams (001), Arapahoe (005), Boulder (013), Broomfield
#' (014), Denver (031), Douglas (035), Jefferson (059), Larimer (069), Weld (123) 
all_counties <-c("001", "005", "013", "014", "031", "035", "059", "069", "123") 

#' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' Criteria pollutants and carbon parameters
params <- c("14129", "42101", "42401", "42602", "44201", "88101", 
            "16111", "88317", "88321")

output <- data.frame()

#' might not work the first time. if not just try typing in the link first yourself
for(county in all_counties) {
  print(paste("County:", county))
  for(param in params) {
    for(year in years) {
      #for(month in c(1:12)) {
      
      #     last_day <- monthDays(as.Date(paste(year,sprintf("%02i",month),"01",sep="-")))
      #     bdate <- paste0(year,sprintf("%02i",month),"01")
      #     edate <- paste0(year,sprintf("%02i",month),last_day)
      bdate <- paste0(year,sprintf("%02i",1),"01")
      edate <- paste0(year,sprintf("%02i",12),31)
      
      prefix <- paste0("https://aqs.epa.gov/api/rawData?user=",
                       user_name, "&pw=", pw, "&format=DMCSV&param=")
      aqs_link <- paste(prefix,param,
                        "&bdate=",bdate,"&edate=",edate,"&state=",state,
                        "&county=",county,collapse = "",sep="")
      error_catch <- F; warn_catch <- F
      tryCatch(read.csv(aqs_link),error = function(e) error_catch <- T, 
               warning = function(w) warn_catch <- T)
      if(!error_catch) {
        aqs_data <- read.csv(aqs_link)[-1,]
        if(length(which(aqs_data$Latitude == "END OF FILE")) > 0) {
          aqs_data <- aqs_data[-which(aqs_data$Latitude == "END OF FILE"),]
        }
        
        if(nrow(aqs_data) > 0) {
          if(nrow(output) > 0) {
            output <- rbind(output,aqs_data)
          }
          else {
            output <- aqs_data
          }
          rm(aqs_data)
        }
      }
      
      cat("param = ",param,year,"; error?",error_catch,
          "; warn?", warn_catch,"\n")
      #   }
    }
  }
}


output$datetime <- as.POSIXct(paste(output$Date.Local, output$X24.Hour.Local), 
                              format="%Y-%m-%d %H",tz = "America/Denver")
output$Latitude <- as.numeric(as.character(output$Latitude))
output$County.Code <- str_pad(output$County.Code, 3, pad = "0")
output$Site.Num <- str_pad(output$Site.Num, 4, pad = "0")
output$monitor_id <- paste(output$County.Code, output$Site.Num, sep="")

save(output, file = "./Data/CEI Data/aqs_data.RData")
write.csv(output, file = "./Data/Air Quality/AQS Data 2009-2017.csv",
          row.names = F)

# load("./Data/Air Quality/AQS Data 2010.RData")
monitors <- output[,c("County.Code", "Site.Num", "Parameter.Code", "Latitude", "Longitude")]
monitors <- unique(monitors)
monitors$Latitude <- as.numeric(as.character(monitors$Latitude))
monitors$County.Code <- str_pad(monitors$County.Code, 3, pad = "0")
monitors$Site.Num <- str_pad(monitors$Site.Num, 4, pad = "0")
monitors$monitor_id <- paste(monitors$County.Code, monitors$Site.Num, sep="")

save(monitors, file="./Data/CEI Data/aqs_monitors.RData")
write.csv(monitors, file = "./Data/Air Quality/AQS Monitors.csv",
          row.names = F)