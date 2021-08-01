install.packages("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lubridate")
library("tibble")
crime <- read.csv("RMS_Crime_Incidents.csv")
greenlight <- read.csv("Project_Green_Light_Locations.csv")
weather <- read.csv("Detroit_Weather.csv")
unique_offenses <- unique(crime[c("offense_category")])
#1711140010
unique_charges <- unique(crime [c("charge_description")])

crime2 <- crime %>%
  mutate(offense_description = replace(offense_description, offense_category == "AGGRAVATED ASSAULT", "AGGRAVATED / FELONIOUS ASSAULT")) 


violent_crime <- scan("violent_crimes.txt", what ="character")

property_crime <- scan("property_crimes.txt", what = "character")
`%notin%` <- Negate(`%in%`)
crime2 <- within(crime2, {
  v_crime = "N"
  v_crime[offense_category %in% (violent_crime)] = "Y"
  v_crime[offense_category %notin% (violent_crime)] = "N"
  p_crime = "N"
  p_crime[offense_category %in% (property_crime)] = "Y"
  p_crime[offense_category %notin% (property_crime)] = "N"
  
})

crime_violent <- crime2 %>% 
  filter(v_crime == "Y" & year >= 2017 ) %>% 
  select(offense_category, year, longitude, latitude)

crime_property <- crime2 %>% 
  filter(p_crime == "Y") %>%
  mutate(incident_timestamp = ymd_hms(incident_timestamp)) %>% 
  mutate(incident_timestamp = date(incident_timestamp)) %>% 
  select(offense_category, year, longitude, latitude ,hour_of_day,incident_timestamp) %>% 
  rename( DATE = incident_timestamp) 


weather_time <- weather %>% 
  select(DATE, HourlyDryBulbTemperature)

weather_time$DATE <- gsub("-", "/", as.character(weather_time$DATE))
weather_time$DATE <- gsub("T", " ", as.character(weather_time$DATE)) 
weather_time <- weather_time %>% mutate(DATE = ymd_hms(DATE))
weather_time$DATE <- round_date(weather_time$DATE, unit="15 minutes") 


weather_time$hour_of_day <- hour(weather_time$DATE)
weather_time$DATE <- date(weather_time$DATE)
weather_time <- unique(weather_time[c("hour_of_day", "DATE", "HourlyDryBulbTemperature")]) %>% 
weather_time <- weather_time[!(is.na(weather_time$HourlyDryBulbTemperature) | weather_time$HourlyDryBulbTemperature==""), ]



crime_weather2 <- inner_join(crime_property, weather_time, by =c("DATE", "hour_of_day"))

weather_filtered <- weather %>% 
  select (DATE, HourlyDryBulbTemperature) %>%
  drop_na()
crime_violent %>% 
  ggplot(aes(x=year))+
  geom_bar(fill = "#97B3C6")