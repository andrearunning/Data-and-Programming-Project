#link for LA Crimes historic dataset - https://drive.google.com/file/d/1JQHWr_bo6-0x8cdAdFuJ6y_-1HHQkFxH/view?usp=sharing
#link for LA Crimes dataset- https://drive.google.com/file/d/1feoOUUDfSsnIRKahv7LaLrmww9umCXTb/view?usp=sharing

library(tidyverse)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rvest)


#create clean csv for NYC shootings in 2022 before October 19
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/")
shootings_nyc <- read_csv(file.path(path, "NYPD_Shooting_Incident_Data__Year_To_Date_.csv"))
shootings_clean <- shootings_nyc[complete.cases(shootings_nyc), ]
write.csv(shootings_clean, "shootings_nyc_2022.csv")

#create clean csv for NYC shootings in 2019 before October 19
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/")
shootings_nyc_pre <- read_csv(file.path(path, "NYPD_Shooting_Incident_Data__Historic_.csv"))
shootings_nyc_pre_1 <- shootings_nyc_pre[,-c(7, 9, 10, 11)]
shootings_nyc_pre_clean <- shootings_nyc_pre_1[complete.cases(shootings_nyc_pre_1), ]
shootings_nyc_pre_clean[['OCCUR_DATE']] <- strptime(shootings_nyc_pre_clean[['OCCUR_DATE']],
                                                    format = "%m/%d/%Y")
shootings_nyc_2019 <- shootings_nyc_pre_clean %>% filter(shootings_nyc_pre_clean$OCCUR_DATE > "2019-01-01" & shootings_nyc_pre_clean$OCCUR_DATE < "2019-10-20")
write.csv(shootings_nyc_2019, "shootings_nyc_2019.csv")

#create clean csv for Chi shootings in 2022 before October 19
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
shootings_chi  <- read_csv(file.path(path, "chi_shootings.csv"))
shootings_chi_clean <- shootings_chi[complete.cases(shootings_chi), ]
shootings_chi_clean[['Date']] <- strptime(shootings_chi_clean[['Date']],
                                          format = "%m/%d/%Y %H:%M:%S")
shootings_chi_2022 <- shootings_chi_clean %>% filter(shootings_chi_clean$Date > "2022-01-01 00:00:00" & shootings_chi_clean$Date < "2022-10-20 00:00:00")
write.csv(shootings_chi_2022, "shootings_chi_2022.csv")

#create clean csv for Chi shootings in 2019 before October 19
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
shootings_chi_pre <- read.csv(file.path(path, "Shootings_since_2001.csv"))
shootings_chi_pre_clean <- shootings_chi_pre[complete.cases(shootings_chi_pre), ]
shootings_chi_pre_clean[['Date']] <- strptime(shootings_chi_pre_clean[['Date']],
                                              format = "%m/%d/%Y %H:%M:%S")
shootings_chi_2019 <- shootings_chi_pre_clean %>% filter(shootings_chi_pre_clean$Date > "2019-01-01 00:00:00" & shootings_chi_pre_clean$Date < "2019-10-20 00:00:00")
write.csv(shootings_chi_2019, "shootings_chi_2019.csv")

#create clean csv for LA shootings in 2022 before October 19
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data/")
crimes_la <- read_csv(file.path(path, "Crime_Data_from_2020_to_Present.csv"))
crimes_clean <- crimes_la[ , colSums(is.na(crimes_la))==0]
#crimes_cleaner <- crimes_clean[complete.cases(crimes_clean), ] #not needed because no other observations NA
crimes_clean[['DATE OCC']] <- strptime(crimes_clean[['DATE OCC']],
                                       format ="%m/%d/%Y %H:%M:%S")
crimes_la_2022 <- crimes_clean %>% filter(crimes_clean$`DATE OCC` > "2022-01-01 00:00:00" & crimes_clean$`DATE OCC` < "2022-10-20 00:00:00")
shootings_la_2022 <- crimes_la_2022[crimes_la_2022$`Crm Cd Desc` == "DISCHARGE FIREARMS/SHOTS FIRED",]
#write as csv 
write.csv(shootings_la_2022, "shootings_la_2022.csv")

#create clean csv for LA Shootings in 2019 before October 19
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data/")
crimes_la_pre <- read_csv(file.path(path, "Crime_Data_from_2010_to_2019.csv"))
crimes_la_pre_clean <- crimes_la_pre[, colSums(is.na(crimes_la_pre))==0]
crimes_la_pre_clean[['DATE OCC']] <- strptime(crimes_la_pre_clean[['DATE OCC']],
                                              format ="%m/%d/%Y %H:%M:%S")
crimes_la_2019 <- crimes_la_pre_clean %>% filter(crimes_la_pre_clean$`DATE OCC` > "2019-01-01 00:00:00" & crimes_la_pre_clean$`DATE OCC` < "2019-10-20 00:00:00")
shootings_la_2019 <- crimes_la_2019[crimes_la_2019$`Crm Cd Desc` == "DISCHARGE FIREARMS/SHOTS FIRED",]
write.csv(shootings_la_2019, "shootings_la_2019.csv")

#clean LA police Station dataframe
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data/")
police_stations_la  <- read_csv(file.path(path, "Sheriff_and_Police_Stations.csv"))
police_stations_la_clean <- police_stations_la[ , colSums(is.na(police_stations_la))==0]
police_stations_only_la <- police_stations_la_clean[police_stations_la_clean$city == "Los Angeles" | police_stations_la_clean$city == "Inglewood" | police_stations_la_clean$city == "Beverly Hills" | police_stations_la_clean$city == "West Hollywood"| police_stations_la_clean$city == "Torrance" | police_stations_la_clean$city == "Culver City" | police_stations_la_clean$city == "Santa Monica"| police_stations_la_clean$city == "Marina Del Rey"| police_stations_la_clean$city == "Compton"| police_stations_la_clean$city == "Pasadena"| police_stations_la_clean$city == "Glendale"| police_stations_la_clean$city == "Burbank"| police_stations_la_clean$city == "San Fernando",]
write.csv(police_stations_only_la, "police_station_locations_la.csv")

#clean chicago police station dataframe
path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
police_stations_chi  <- read_csv(file.path(path, "Police_Stations_-_Map.csv"))
police_stations_chi[c('Latitude', 'Longitude')] <- str_split_fixed(police_stations_chi$LOCATION, ",", 2)
police_stations_chi_clean <- police_stations_chi %>% mutate(Longitude = str_replace_all(Longitude, "\\*|\\(|\\)", ""))
police_stations_chi_final <- police_stations_chi_clean %>% mutate(Latitude = str_replace_all(Latitude, "\\*|\\(|\\)", ""))
police_stations_chi_very_clean <- police_stations_chi_final[complete.cases(police_stations_chi_final), ]
write.csv(police_stations_chi_very_clean, "police_station_locations_chi.csv")

#scrape data from NYC Police website with addresses of police stations
#to then later manually enter coordinates found using the addresses
url <- "https://www.nyc.gov/site/nypd/bureaus/patrol/precincts-landing.page"
request <- read_html(url)
table <- html_table(request, fill = TRUE)
View(table[[1]])
nyc_precinct_no_coords <- table[[1]]
write.csv(nyc_precinct_no_coords, "nyc_precinct_no_coords.csv")
