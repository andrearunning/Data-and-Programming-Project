library(tidyverse)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)

#create Chicago distance data 
#2022
get_chicago_police_stations <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  police_stations_chi  <- read_csv(file.path(path, "police_station_locations_chi.csv"))
  police_station_chi_sf<- st_as_sf(police_stations_chi,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(police_station_chi_sf)
}
get_shooting_locations_chi_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  shootings_chi_2022  <- read_csv(file.path(path, "shootings_chi_2022.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_chi_sf)
}
chicago_police_sf <- get_chicago_police_stations()
chicago_shootings_sf_2022 <- get_shooting_locations_chi_2022()
get_sf_with_distance_chi_2022 <- function() {
  id <- st_nearest_feature(chicago_shootings_sf_2022, chicago_police_sf)
  chicago_shootings_sf_2022$closest_station <- chicago_police_sf$DISTRICT[id]
  chicago_shootings_sf_2022$distance_to_station <- st_distance(chicago_shootings_sf_2022, chicago_police_sf[id,], by_element = TRUE)
 return(chicago_shootings_sf_2022)
}
shootings_with_distance_chi_2022 <- get_sf_with_distance_chi_2022()
#2019
get_shooting_locations_chi_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  shootings_chi_2019  <- read_csv(file.path(path, "shootings_chi_2019.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_chi_sf)
}
chicago_shooting_sf_2019 <- get_shooting_locations_chi_2019()

get_sf_with_distance_chi_2019 <- function() {
  id <- st_nearest_feature(chicago_shooting_sf_2019, chicago_police_sf)
  chicago_shooting_sf_2019$closest_station <- chicago_police_sf$DISTRICT[id]
  chicago_shooting_sf_2019$distance_to_station <- st_distance(chicago_shooting_sf_2019, chicago_police_sf[id,], by_element = TRUE)
  return(chicago_shooting_sf_2019)
}
shootings_with_distance_chi_2019 <- get_sf_with_distance_chi_2019()
distances_2019_chicago <- shootings_with_distance_chi_2019$distance_to_station
chicago_2019 <- as.data.frame(distances_2019_chicago)
distances_2022_chicago <- shootings_with_distance_chi_2022$distance_to_station
chicago_2022 <- as.data.frame(distances_2022_chicago)
Chicago_ttest_results <- t.test(chicago_2019, chicago_2022, var.equal =  FALSE)
t.test(chicago_2019, chicago_2022, var.equal =  FALSE)

#create nyc distance data 
#2022
get_nyc_police_stations <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  police_stations_nyc  <- read_csv(file.path(path, "NYC police stations.csv"))
  police_stations_nyc_df <- as.data.frame.table(police_stations_nyc)
  police_stations_nyc_sf <- st_as_sf(police_stations_nyc, coords = c("Longitude", "Latitude"), crs = 4326)
  return(police_stations_nyc_sf)
}
get_shooting_locations_nyc_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  shootings_nyc_2022 <- read_csv(file.path(path, "shootings_nyc_2022.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_sf)
}
nyc_police_stations <- get_nyc_police_stations()
nyc_shooting_locations_2022 <- get_shooting_locations_nyc_2022()
get_sf_with_distance_nyc_2022 <- function() {
  id <- st_nearest_feature(nyc_shooting_locations_2022, nyc_police_stations)
  nyc_shooting_locations_2022$closest_station <- nyc_police_stations$Precinct[id]
  nyc_shooting_locations_2022$distance_to_station <- st_distance(nyc_shooting_locations_2022, nyc_police_stations[id,], by_element = TRUE)
  return(nyc_shooting_locations_2022)
}
shootings_with_distance_nyc_2022 <- get_sf_with_distance_nyc_2022()
#2019
get_shooting_locations_nyc_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  shootings_nyc_2019 <- read_csv(file.path(path, "shootings_nyc_2019.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_sf)
}
nyc_shooting_locations_2019 <- get_shooting_locations_nyc_2019()
get_sf_with_distance_nyc_2019 <- function() {
  id <- st_nearest_feature(nyc_shooting_locations_2019, nyc_police_stations)
  nyc_shooting_locations_2019$closest_station <- nyc_police_stations$Precinct[id]
  nyc_shooting_locations_2019$distance_to_station <- st_distance(nyc_shooting_locations_2019, nyc_police_stations[id,], by_element = TRUE)
  return(nyc_shooting_locations_2019)
}
shootings_with_distance_nyc_2019 <- get_sf_with_distance_nyc_2019()
distances_2019_nyc <- shootings_with_distance_nyc_2019$distance_to_station
nyc_2019 <- as.data.frame(distances_2019_nyc)
distances_2022_nyc <- shootings_with_distance_nyc_2022$distance_to_station
nyc_2022 <- as.data.frame(distances_2022_nyc)
nyc_ttest_results <- t.test(nyc_2019, nyc_2022, var.equal =  FALSE)
t.test(nyc_2019, nyc_2022, var.equal =  FALSE)

#create LA distance data
#2022
get_la_police_stations <- function(){
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  police_stations_la <- read_csv(file.path(path, "police_station_locations_la.csv"))
  police_stations_la_sf<- st_as_sf(police_stations_la,  coords = c("longitude", "latitude"), crs = 4326)
  return(police_stations_la_sf)
}
get_shooting_locations_la_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  shootings_la_2022 <- read_csv(file.path(path, "shootings_la_2022.csv"))
  shootings_sf <- st_as_sf(shootings_la_2022,  coords = c("LON", "LAT"), crs = 4326)
  return(shootings_sf)
}
la_police_stations <- get_la_police_stations()
shooting_locations_la_2022 <- get_shooting_locations_la_2022()
get_sf_with_distance_la_2022 <- function() {
  id <- st_nearest_feature(shooting_locations_la_2022, la_police_stations)
  shooting_locations_la_2022$closest_station <- la_police_stations$Name[id]
  shooting_locations_la_2022$distance_to_station <- st_distance(shooting_locations_la_2022, la_police_stations[id,], by_element = TRUE)
  return(shooting_locations_la_2022)
}
shootings_with_distance_la_2022 <- get_sf_with_distance_la_2022()
#2019
get_shooting_locations_la_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  shootings_la_2019 <- read_csv(file.path(path, "shootings_la_2019.csv"))
  shootings_sf <- st_as_sf(shootings_la_2019,  coords = c("LON", "LAT"), crs = 4326)
  return(shootings_sf)
}
shooting_locations_la_2019 <- get_shooting_locations_la_2019()
get_sf_with_distance_la_2019 <- function() {
  id <- st_nearest_feature(shooting_locations_la_2019, la_police_stations)
  shooting_locations_la_2019$closest_station <- la_police_stations$Name[id]
  shooting_locations_la_2019$distance_to_station <- st_distance(shooting_locations_la_2019, la_police_stations[id,], by_element = TRUE)
  return(shooting_locations_la_2019)
}
shootings_with_distance_la_2019 <- get_sf_with_distance_la_2019()
distances_2019_la <- shootings_with_distance_la_2019$distance_to_station
la_2019 <- as.data.frame(distances_2019_la)
distances_2022_la <- shootings_with_distance_la_2022$distance_to_station
la_2022 <- as.data.frame(distances_2022_la)
la_ttest_results <- t.test(la_2019, la_2022, var.equal =  FALSE)
t.test(la_2019, la_2022, var.equal =  FALSE)
