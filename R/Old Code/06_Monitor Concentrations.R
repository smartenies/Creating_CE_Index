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
#' This script generates weekly metrics at each monitor in the study area.
#' Cold season ozone concentrations are imputed using multiple imputation
#' with predictive mean matching (PMM) from the mice package
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
library(mice)

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
#' Criteria pollutants
#' Sources: AQS Data Mart (Script: 4_AQS Data Scrape.R)
#'          https://aqs.epa.gov/api
#' =============================================================================

# Air monitors
load("./Data/CEI Data/aqs_monitors.RData") 
monitors <- st_as_sf(monitors, 
                     coords = c("Longitude", "Latitude"), crs=ll_wgs84) %>%
  st_transform(crs=albers) %>%
  mutate(County.Code = str_pad(County.Code, width=3, side="left", pad="0"),
         Site.Num = str_pad(Site.Num, width=4, side="left", pad="0")) %>%
  mutate(monitor_id = paste("08", County.Code, Site.Num, sep=""))
plot(st_geometry(monitors))

pm_monitors <- filter(monitors, Parameter.Code == "88101")
nrow(pm_monitors)
plot(st_geometry(pm_monitors))

o3_monitors <- filter(monitors, Parameter.Code == "44201")
nrow(o3_monitors)
plot(st_geometry(o3_monitors))

save(monitors, pm_monitors, o3_monitors,
     file="./Data/CEI Data/monitors.RData")

#' -----------------------------------------------------------------------------
#' Mapping the air monitors
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/monitors.RData")
load("./Data/Spatial Data/dm_tracts.RData")

ggplot() +
  ggtitle("PM\u2082.\u2085 and O\u2083 monitors in the Northern Front Range region") +
  geom_sf(data=dm_tracts, fill=NA, color="grey50") +
  geom_sf(data=pm_monitors, aes(fill="pm", color="pm")) +
  geom_sf(data=o3_monitors, aes(fill="o3", color="o3")) +
  scale_fill_manual(name="Pollutant",
                    values=c("o3" = "red", "pm" = "blue"),
                    labels=c("O\u2083", "PM\u2082.\u2085")) +
  scale_color_manual(name="Pollutant",
                     values=c("o3" = "red", "pm" = "blue"),
                     labels=c("O\u2083", "PM\u2082.\u2085")) +
  #coord_sf(ylim=c(39.5, 40.25), xlim=c(-105.5, -104.5)) +
  simple_theme

ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/Area Monitors.jpeg", device = "jpeg", 
       dpi=600)

#' -----------------------------------------------------------------------------
#' Prepare R objects for census tract level exposures
#' Change from previous CEI (which used 5 year averages)
#' 
#' Pm2.5: weekly average at each monitor 
#' O3: weekly average daily 8-h max at each monitor during the warm season
#' -----------------------------------------------------------------------------

load("./Data/CEI Data/aqs_data.RData")
output <- mutate(output, monitor_id = paste("08", monitor_id, sep="")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(datetime = as.POSIXct(paste(as.character(Date.Local), 
                                     as.character(X24.Hour.Local)), 
                               format="%Y-%m-%d %H",tz = "America/Denver"))

#' Subset PM2.5, add an identifier for weeks, and take the average
#' week_ending is the date for sunday of that week
#' Will be used for kiging and to match pregnancy dates in the CEI
pm <- filter(output, Parameter.Code == "88101" & POC ==1) %>%
  mutate(year = strftime(datetime, format = "%Y"),
         week_starting = as.Date(floor_date(datetime, unit = "week")),
         week_ending = as.Date(ceiling_date(datetime, unit = "week"))) %>%
  mutate(datetime = as.Date(datetime)) %>%
  arrange(datetime, monitor_id) %>%
  distinct()

pm_average <- pm %>%
  group_by(monitor_id, week_ending) %>%
  summarize(weekly_average = mean(Sample.Measurement)) %>%
  mutate(pollutant = "pm")

save(pm, pm_average, file="./Data/CEI Data/pm_monitor_metrics.RData")

load("./Data/CEI Data/pm_monitor_metrics.RData")
length(unique(pm_average$monitor_id))

#' Subset O3, add an identifier for hour, day, week, month, and year
#' Impute "cold season" ozone concentraitons
o3 <- filter(output, Parameter.Code == "44201" & POC ==1) %>%
  mutate(mdy = strftime(datetime, format = "%m%d%y"),
         year = strftime(datetime, format = "%Y"),
         month = strftime(datetime, format = "%m"),
         hour = strftime(datetime, format = "%H"),
         week_starting = floor_date(datetime, unit = "week"),
         week_ending = ceiling_date(datetime, unit = "week")) %>%
  arrange(datetime, monitor_id) %>%
  #' Drop missing hours (daylight savings)
  filter(!is.na(datetime))

#' Need a wide dataset for imputation
o3_wide <- select(o3, datetime, monitor_id, Sample.Measurement) %>%
  spread(key = monitor_id, value = Sample.Measurement)

#' About half of the monitors collect "cold season" ozone
head(o3_wide)
colnames(o3_wide)[-1] <- paste("O3_", colnames(o3_wide)[-1], sep="")
summary(o3_wide)

#' Should drop 080130007 (Boulder County) and 080690006 (Larimer County) 
#' due to lack of data collected (imputation does not look great)
o3_wide <- select(o3_wide, -O3_080130007, -O3_080690006)

#'Date list for full imputed dataset
o3_datetime <- select(o3_wide, datetime)

#' Imputation using Predictive Mean Matching
#' Using the default of 5 iterations
#' Note: this can take some time
o3_imp_temp <- mice(o3_wide[,-1], method = "pmm")

#' Check to see if imputed values are plausible
densityplot(o3_imp_temp)
stripplot(o3_imp_temp)

#' Complete the dataset (fill in missing values)
o3_imp_complete <- complete(o3_imp_temp)
o3_datetime <- cbind(o3_datetime, o3_imp_complete) %>%
  gather(key=monitor_id, value=Sample.Measurement, -datetime) %>%
  mutate(monitor_id = gsub("O3_", "", monitor_id))

o3_imputed <- o3 %>%
  #' replace sample measurement column with the measured & imputed values
  select(-Sample.Measurement) %>%
  left_join(o3_datetime, by=c("monitor_id", "datetime"))

summary(o3_imputed)
head(o3_imputed)

#' Second, need to calculate the daily 8-hr max 
#' using the imputed dataset here
#' (highest 8 h mean in the 24 h period)
#' See CFR Title 40 Vol 2 Part 50 Appendix P for details
#' https://www.gpo.gov/fdsys/pkg/CFR-2017-title40-vol2/xml/CFR-2017-title40-vol2-part50.xml

save(o3, o3_imputed, file="./Data/CEI Data/o3_monitor_metrics.RData")
load("./Data/CEI Data/o3_monitor_metrics.RData")

load("./Data/CEI Data/o3_monitor_metrics.RData")
length(unique(o3_imputed$monitor_id))

monitor_list <- unique(o3_imputed$monitor_id)
temp_df <- data.frame()

for (i in 1:length(monitor_list)) {
  print(paste("Monitor", i, "of", length(monitor_list)))
  df1 <- filter(o3_imputed, monitor_id == monitor_list[i]) %>%
    arrange(datetime)
  
  day_list <- unique(df1$mdy)
  
  for (j in 1:length(day_list)) {
    days <- c(day_list[j], day_list[j+1])
    df2 <- filter(df1, mdy %in% days)
    
    avg_list <- list()
    a <- 24 #should have 24 moving averages starting at 0:00 and ending at 23:00
    
    for (k in 1:a) {
      hours <- seq(from=k, to=k+7)
      df3 <- slice(df2, hours) 
      
      #' need at least 6 hourly concentrations (75%) to calculate mean
      avg_list[k] <- ifelse(sum(!is.na(df3$Sample.Measurement)) >= 6, 
                            mean(df3$Sample.Measurement, na.rm=T), 
                            NA)
    }
    rm(df3)
    b <- unlist(avg_list)
    
    #' only valid if there are >= 18 (75%) 8-hr means
    max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
    
    temp <- data.frame(monitor_id = unique(o3$monitor_id)[i],
                       mdy = days[1], 
                       d8hmax = max)
    temp_df <- rbind(temp_df, temp)
    rm(temp, df2)
  }
  rm(df1)
}

temp_df <- mutate_if(temp_df, is.factor, as.character)
o3_imputed <- left_join(o3_imputed, temp_df, by=c("monitor_id", "mdy"))

#' Average daily 8 h max concentraitons for each monitor
o3_average <- o3_imputed %>%
  group_by(monitor_id, week_ending) %>%
  summarize(weekly_average = mean(d8hmax)) %>%
  mutate(pollutant = "o3")

save(o3, o3_imputed, o3_average, file="./Data/CEI Data/o3_monitor_metrics.RData")

load("./Data/CEI Data/o3_monitor_metrics.RData")
length(unique(o3_average$monitor_id))

summary(pm_average)
summary(o3_average)

#' -----------------------------------------------------------------------------
#' Time series plots of weekly pm2.5 and ozone at each monitor
#' -----------------------------------------------------------------------------

load("./Data/CEI Data/pm_monitor_metrics.RData")
load("./Data/CEI Data/o3_monitor_metrics.RData")

class(pm_average)
class(o3_average)

ggplot(data=pm_average, aes(x=week_ending, y=weekly_average, color=monitor_id)) +
  geom_point(aes(group=monitor_id)) +
  geom_line(aes(group=monitor_id)) +
  #geom_smooth(method = "loess", size=1) +
  geom_hline(aes(yintercept = 12), lty=4, color="blue", size=1) +
  #scale_x_datetime(date_breaks = "6 months", date_labels =  "%b %y") +
  xlab("Date") + ylab("PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/PM_2009-2017.jpeg",
       device = "jpeg", dpi=600)

ggplot(data=o3_average, aes(x=week_ending, y=weekly_average)) +
  geom_point(aes(group=monitor_id, color=monitor_id)) +
  geom_smooth(method = "loess", color="black", size=1) +
  geom_hline(aes(yintercept = 0.075), lty=4, color="blue", size=1) +
  scale_x_datetime(date_breaks = "6 months", date_labels =  "%b %y") +
  xlab("Date") + ylab("O\u2083 (ppm)") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/O3_2009-2017.jpeg",
       device = "jpeg", dpi=600)

#' -----------------------------------------------------------------------------
#' Histograms of weekly pm2.5 and ozone at each monitor
#' -----------------------------------------------------------------------------

ggplot(data=pm_average, aes(x = weekly_average)) +
  geom_histogram(aes(group=monitor_id, fill=monitor_id)) +
  xlab("PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  theme(legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/PM_Histogram.jpeg",
       device = "jpeg", dpi=600)

ggplot(data=o3_average, aes(x = weekly_average)) +
  geom_histogram(aes(group=monitor_id, fill=monitor_id)) +
  xlab("O\u2083 (ppm)") +
  theme(legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/O3_Histogram.jpeg",
       device = "jpeg", dpi=600)

