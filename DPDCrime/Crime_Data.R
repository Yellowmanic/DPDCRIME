install.packages("tidyverse")
install.packages("leaflet")
install.packages("data.table")
library("data.table")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lubridate")
library("tibble")
library("leaflet")
library("scales")
crime_raw <- read.csv("RMS_Crime_Incidents.csv")
weather_raw <- read.csv("Detroit_Weather.csv")

unique_offenses <- unique(crime[c("offense_category")])
#1711140010

violent_crime <- c("ASSAULT","AGGRAVATED ASSAULT","SEXUAL ASSAULT","HOMICIDE","KIDNAPPING")
property_crime <- c("ROBBERY","LARCENY","ARSON","STOLEN VEHICLE","BURGLARY","DAMAGE TO PROPERTY","STOLEN PROPERTY")
#violent_crime <- scan("violent_crimes.txt", what ="character")
#property_crime <- scan("property_crimes.txt", what = "character")

#crime <- crime_raw %>%
  #mutate(offense_description = replace(offense_description, offense_category == "AGGRAVATED ASSAULT", "AGGRAVATED / FELONIOUS ASSAULT")) 

crime_raw %>%
  group_by(year) %>%
  summarize(count = n()) %>% 
  arrange(desc(year))

crime <- crime_raw %>% 
  rename(date = incident_timestamp) %>% 
  mutate(date = ymd_hms(date)) %>% 
  mutate(date = date(date)) %>% 
  filter(year > 2016 & year <= 2020) %>% 
  add_column(crime_type = NA) %>% 
  add_column(month = NA) %>% 
  mutate(month = month(date, TRUE))
 

`%notin%` <- Negate(`%in%`)
crime <- within(crime, {
  crime_type[offense_category %notin% (violent_crime) || (property_crime)] = "other"
  crime_type[offense_category %in% (violent_crime)] = "violent"
  crime_type[offense_category %in% (property_crime)] = "property"
  
})

crime %>% 
  group_by(crime_type) %>% 
  summarize(count = n())

crime <- crime %>% 
  filter(crime_type != "other")

data <- crime %>% filter(year == 2020)
data$popup <- paste("<b>Incident #: </b>", data$crime_id, "<br>", "<b>Category: </b>", data$offense_category,
                    "<br>", "<b>Description: </b>", data$offense_description,
                    "<br>", "<b>Day of week: </b>", data$day_of_week,
                    "<br>", "<b>Date: </b>", data$date,
                    "<br>", "<b>Time: </b>", data$incident_time,
                    "<br>", "<b>Neighborhood: </b>", data$neighborhood,
                    "<br>", "<b>Longitude: </b>", data$longitude,
                    "<br>", "<b>Latitude: </b>", data$latitude)

leaflet(data, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  #addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  #addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%
  addMarkers(lng = ~longitude, lat = ~latitude, popup = data$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap"),
    options = layersControlOptions(collapsed = FALSE)
  )

crime_daily <- crime %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  arrange(date)

crime_daily %>% 
  ggplot(aes(date, count))+  
  geom_line(color = "orange")+
  geom_smooth(color = "black") 

crime %>% 
  group_by(month) %>% 
  summarize(count= n()) %>% 
  arrange(month) 

crime_month <- crime %>% 
  group_by(month, year) %>% 
  summarize(count= n()) %>% 
  arrange(month)
crime_month %>% 
  ggplot(aes(month, count, group = 1))+
  geom_area(fill = "orange")+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  facet_wrap(~year)

crime_month <-crime %>% 
  mutate(days_in_month = days_in_month(date)) %>% 
  group_by(month, year, days_in_month) %>% 
  summarize(count= n()) %>% 
  mutate(daily_crime_rate = count/days_in_month)
crime_month %>% 
  ggplot(aes(month, daily_crime_rate, group = 1))+
  geom_area(fill = "orange")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  facet_wrap(~year)
weather <- weather_raw %>% 
  select(DATE, HourlyDryBulbTemperature) %>%
  rename(temperature = HourlyDryBulbTemperature, date = DATE)

weather <- weather %>% mutate(date = ymd_hms(date))
weather$date <- round_date(weather$date, unit="15 minutes") 


weather$hour_of_day <- hour(weather$date)
weather$date <- date(weather$date)
weather <- unique(weather[c("hour_of_day", "date", "temperature")])  
weather <- weather[!(is.na(weather$temperature) | weather$temperature==""), ]
weather$temperature = as.integer(weather$temperature)

crime <- left_join(crime, weather, by = c("date", "hour_of_day")) %>%   
  distinct(arrest_charge, offense_category, date, crime_id, .keep_all = TRUE) %>% 
  select(-arrest_charge, -crime_id) %>% 
  fill(temperature)

crime_temp_month <- crime %>% 
  mutate(pop = case_when(
    year == 2017 ~ 679865,
    year == 2018 ~ 677155,
    year == 2019 ~ 674841,
    year == 2020 ~ 664139
  )) %>% 
  group_by(month, year, pop) %>%
  summarize(mean_temp = mean(temperature, na.rm =T), count = n()) %>% 
  mutate(crime_rate = count/pop*10000)

crime_temp_month %>% 
  ggplot(aes(mean_temp, crime_rate)) + 
  geom_point()+
  geom_smooth(color = "orange", method = "lm")
  

crime_dow <- crime %>% 
  group_by(day_of_week, offense_category, hour_of_day) %>% 
  summarize(count = n())

hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
dow_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
crime_dow$day_of_week <- factor(crime_dow$day_of_week, labels = rev(dow_format), ordered = TRUE)
crime_dow$hour_of_day <- factor(crime_dow$hour_of_day, level = 0:23, label = hour_format)

ggplot(crime_dow, aes(x = hour_of_day, y = day_of_week, fill = count))+
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(2, "cm"), legend.key.height=unit(0.25, "cm")) +
  labs(x = "Hour of Reported Crime", y = "Day of Week ", title = "Reported Property and Violent Crimes in Detroit (2016-2020)") +
  scale_fill_gradient(low = "white", high = "#101ade")


crime_dow <- crime_dow %>% 
  mutate(norm = count/sum(count))
ggplot(crime_dow, aes(x = hour_of_day, y = day_of_week, fill = norm)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Number of Police Arrests in San Francisco from 2007 – 2016, by Offense") +
  scale_fill_gradient(low = "white", high = "#2980B9") +
  facet_wrap(~ offense_category)

crime_dow_zipcode <- crime %>% 
  group_by(day_of_week, zip_code, hour_of_day) %>% 
  summarise(count = n())



crime_dow_zipcode$day_of_week <- factor(crime_dow_zipcode$day_of_week, labels = rev(dow_format), ordered = TRUE)
crime_dow_zipcode$hour_of_day <- factor(crime_dow_zipcode$hour_of_day, level = 0:23, label = hour_format)


ggplot(crime_dow_zipcode, aes(x = hour_of_day, y = day_of_week, fill = count)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Number of Police Arrests in San Francisco from 2007 – 2016, by Offense") +
  scale_fill_gradient(low = "white", high = "#2980B9") +
  facet_wrap(~ zip_code)  

zip_code_database<- read.csv("detroit_zipcodes.csv")

filtered_zipcodes <-c(48203, 48212, 48236, 48239, 48243)

crime_zipcode <- crime %>% 
  filter(zip_code %notin% filtered_zipcodes) %>% 
  group_by(zip_code, crime_type) %>% 
  summarise(count = n())



crime_zipcode <- left_join(crime_zipcode, zip_code_database, by = "zip_code") %>% 
  drop_na() %>% 
  mutate(crime_rate = (count/Population)*1000)
crime_zipcode$zip_code <- factor(crime_zipcode$zip_code)

ggplot(crime_zipcode, aes(zip_code,crime_rate, fill = crime_type))+
  geom_col() +
  scale_fill_manual("crime_type", values = c("property"= "#2980B9", "violent" = "orange")) +
  coord_flip()+
  labs(title = "Crime Rate in Detroit by Zipcode (2016-2020)",x = "Zip Code",
       y = "Crime Rate per 1000",
       fill = "Crime Type")


downtown_zipcodes <- c(48243, 48226, 48201, 48202, 48216, 48207)
crime_zipcode %>% 
  filter(zip_code %notin% downtown_zipcodes) %>% 
  ggplot(aes(zip_code,crime_rate, fill = crime_type))+
  geom_col() +
  scale_fill_manual("crime_type", values = c("property"= "#2980B9", "violent" = "orange")) +
  coord_flip()+
  labs(title = "Crime Rate in Detroit by Zipcode* (2016-2020)",
       caption = "* Downtown, Midtown and surrounding areas removed",
       x = "Zip Code",
       y = "Crime Rate per 1000",
       fill = "Crime Type")

crime_dow_zipcode %>% 
  filter(zip_code == 48238) %>% 
  ggplot(aes(x = hour_of_day, y = day_of_week, fill = count)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 10)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Number of Police Arrests in San Francisco from 2007 – 2016, by Offense") +
  scale_fill_gradient(low = "white", high = "#101ade") 

crime_dow_zipcode %>% 
  filter(zip_code == 48205) %>% 
  ggplot(aes(x = hour_of_day, y = day_of_week, fill = count)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 10)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Number of Police Arrests in San Francisco from 2007 – 2016, by Offense") +
  scale_fill_gradient(low = "white", high = "#101ade") 

crime$day_of_week <- factor(crime$day_of_week, labels = rev(dow_format), ordered = TRUE)
crime$hour_of_day <- factor(crime$hour_of_day, level = 0:23, label = hour_format)







