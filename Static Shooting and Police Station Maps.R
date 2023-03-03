library(tidyverse)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)

#create static maps displaying shootings and crimes in 2019 and 2022 in three cities

#Chicago
#create three functions to call shapefiles
#function that creates shooting by police precinct shapefile 2022
get_shootings_sf_chi_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/Boundaries - Police Districts (current)")
  chi_police_sf <- st_read(file.path(path, "geo_export_ba432f02-c067-4a0f-a392-521838fa9d07.shp"))
  chi_police_map <- st_as_sf(chi_police_sf)
  chi_police_map <- st_set_crs(chi_police_sf, 4326)
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  shootings_chi_2022  <- read_csv(file.path(path, "shootings_chi_2022.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_chi_sf, chi_police_map, sparse = FALSE)
  shootings_chi_sf <- chi_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(shootings_chi_sf)[names(shootings_chi_sf) == 'Count'] <- 'Shootings'
  return(shootings_chi_sf)
}
#function that creates shooting by police precinct shapefile 2019
get_shootings_sf_chi_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/Boundaries - Police Districts (current)")
  chi_police_sf <- st_read(file.path(path, "geo_export_ba432f02-c067-4a0f-a392-521838fa9d07.shp"))
  chi_police_map <- st_as_sf(chi_police_sf)
  chi_police_map <- st_set_crs(chi_police_sf, 4326)
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  shootings_chi_2019  <- read_csv(file.path(path, "shootings_chi_2019.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_chi_sf, chi_police_map, sparse = FALSE)
  shootings_chi_sf <- chi_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(shootings_chi_sf)[names(shootings_chi_sf) == 'Count'] <- 'Shootings'
  return(shootings_chi_sf)
}
#function that creates police department location shapefile
get_chicago_police_stations <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  police_stations_chi  <- read_csv(file.path(path, "police_station_locations_chi.csv"))
  police_station_chi_sf<- st_as_sf(police_stations_chi,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(police_station_chi_sf)
}
#function that creates shooting location shapefile 2022
get_shooting_locations_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  shootings_chi_2022  <- read_csv(file.path(path, "shootings_chi_2022.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_chi_sf)
}
#function that creates shooting location shapefile 2019
get_shooting_locations_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/Chicago Data/")
  shootings_chi_2022  <- read_csv(file.path(path, "shootings_chi_2019.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_chi_sf)
}

#call functions
shooting_locations_2022 <- get_shooting_locations_2022()
shooting_locations_2019 <- get_shooting_locations_2019()
police_stations_chicago <- get_chicago_police_stations()
shootings_sf_chi_2022 <- get_shootings_sf_chi_2022() 
shootings_sf_chi_2019 <- get_shootings_sf_chi_2019()

#create plot using all three shapefiles
#plot showing both shootings and police stations 2022
ggplot() +
  geom_sf(data = shootings_sf_chi_2022, aes(fill = Shootings), color = "black") +
  ggtitle("2022 Shooting Incidences and Police Stations Chicago", subtitle = "shown by police precinct boundary") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white")) +
  scale_fill_continuous(low = "grey", high = "red") +
  labs(fill = "Shootings") +
  geom_sf(data = police_stations_chicago, aes(), shape = 8, size = 1.5, color = "blue") +
  geom_sf(data = shooting_locations_2022, aes(), size = .5, color = "black", alpha = 4/10)
#create plot using all three shapefiles
#plot showing both shootings and police stations 2019
ggplot() +
  geom_sf(data = shootings_sf_chi_2019, aes(fill = Shootings), color = "black") +
  ggtitle("2019 Shooting Incidences and Police Stations Chicago", subtitle = "shown by police precinct boundary") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white")) +
  scale_fill_continuous(low = "grey", high = "red") +
  labs(fill = "Shootings") +
  geom_sf(data = police_stations_chicago, aes(), shape = 8, size = 1.5, color = "blue") +
  geom_sf(data = shooting_locations_2019, aes(), size = .5, color = "black", alpha = 4/10)

#New York City
#write three functions to create shapefiles 2022
#function to retrieve a shapefile showings within police bondaries 2022
get_shootings_sf_nyc_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/Police Precincts")
  nyc_police_sf <- st_read(file.path(path, "geo_export_6e101e15-cda9-4946-8664-97da0938516d.shp"))
  nyc_police_map <- st_as_sf(nyc_police_sf)
  nyc_police_map <- st_set_crs(nyc_police_sf, 4326)
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  shootings_nyc_2022 <- read_csv(file.path(path, "shootings_nyc_2022.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_sf, nyc_police_map, sparse = FALSE)
  nyc_shootings_sf <- nyc_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(nyc_shootings_sf)[names(nyc_shootings_sf) == 'Count'] <- 'Shootings'
  return(nyc_shootings_sf)
}
#function to retrieve a shapefile showings within police bondaries 2019
get_shootings_sf_nyc_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/Police Precincts")
  nyc_police_sf <- st_read(file.path(path, "geo_export_6e101e15-cda9-4946-8664-97da0938516d.shp"))
  nyc_police_map <- st_as_sf(nyc_police_sf)
  nyc_police_map <- st_set_crs(nyc_police_sf, 4326)
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  shootings_nyc_2019 <- read_csv(file.path(path, "shootings_nyc_2019.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_sf, nyc_police_map, sparse = FALSE)
  nyc_shootings_sf <- nyc_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(nyc_shootings_sf)[names(nyc_shootings_sf) == 'Count'] <- 'Shootings'
  return(nyc_shootings_sf)
}

#function to retrieve shapefile of police stations in nyc
get_nyc_police_stations <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  police_stations_nyc  <- read_csv(file.path(path, "NYC police stations.csv"))
  police_stations_nyc_df <- as.data.frame.table(police_stations_nyc)
  police_stations_nyc_sf <- st_as_sf(police_stations_nyc, coords = c("Longitude", "Latitude"), crs = 4326)
  return(police_stations_nyc_sf)
}
#function to retrieve shapefile of 2022 shooting locations in nyc through October 19
get_shooting_locations_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  shootings_nyc_2022 <- read_csv(file.path(path, "shootings_nyc_2022.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_sf)
}
#funciton to retrieve shapefile of 2019 shooting locations in nyc through October 19
get_shooting_locations_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/NYC Data/")
  shootings_nyc_2019 <- read_csv(file.path(path, "shootings_nyc_2019.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_sf)
}

#call functions to create shapefiles 2019
shootings_mapped_sf_2022 <- get_shootings_sf_nyc_2022()  
shooting_location_sf_2022 <- get_shooting_locations_2022()
police_stations_sf <- get_nyc_police_stations()
shootings_mapped__sf_2019 <- get_shootings_sf_nyc_2019()
shooting_location_sf_2019 <- get_shooting_locations_2019()
#create plots using our shapefiles 
#plot showing shootings and police stations
ggplot() +
  geom_sf(data = shootings_mapped_sf_2022, aes(fill = Shootings), color = "black") +
  ggtitle("2022 Shooting Incidences and Police Stations NYC", subtitle = "shown by police precinct boundary") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white")) +
  scale_fill_continuous(low = "grey", high = "red") +
  labs(fill = "Shootings") +
  geom_sf(data = police_stations_sf, aes(), shape = 8, size = 1.5, color = "blue") +
  geom_sf(data = shooting_location_sf_2022, aes(), size = .5, color = "black", alpha = 4/10)
#create plots using our shapefiles 
#plot showing shootings and police stations
ggplot() +
  geom_sf(data = shootings_mapped_sf_2022, aes(fill = Shootings), color = "black") +
  ggtitle("2019 Shooting Incidences and Police Stations NYC", subtitle = "shown by police precinct boundary") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white")) +
  scale_fill_continuous(low = "grey", high = "red") +
  labs(fill = "Shootings") +
  geom_sf(data = police_stations_sf, aes(), shape = 8, size = 1.5, color = "blue") +
  geom_sf(data = shooting_location_sf_2019, aes(), size = .5, color = "black", alpha = 4/10)

#Los Angeles
#write three functions to call shapefiles
#function to call shootings within police precinct boundaries in LA 2022
get_shootings_sf_la_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data/LAPD_Divisions/")
  la_police_sf <- st_read(file.path(path, "LAPD_Divisions.shp"))
  la_police_map <- st_as_sf(la_police_sf)
  la_police_map <- st_set_crs(la_police_sf, 4326)
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  shootings_la_2022 <- read_csv(file.path(path, "shootings_la_2022.csv"))
  shootings_sf <- st_as_sf(shootings_la_2022,  coords = c("LON", "LAT"), crs = 4326)
  temp <- st_within(shootings_sf, la_police_map, sparse = FALSE)
  la_shootings_sf <- la_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(la_shootings_sf)[names(la_shootings_sf) == 'Count'] <- 'Shootings'
  return(la_shootings_sf)
}
#function to call shootings within police precinct boundaries in LA 2019
get_shootings_sf_la_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data/LAPD_Divisions/")
  la_police_sf <- st_read(file.path(path, "LAPD_Divisions.shp"))
  la_police_map <- st_as_sf(la_police_sf)
  la_police_map <- st_set_crs(la_police_sf, 4326)
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  shootings_la_2019 <- read_csv(file.path(path, "shootings_la_2019.csv"))
  shootings_sf <- st_as_sf(shootings_la_2019,  coords = c("LON", "LAT"), crs = 4326)
  temp <- st_within(shootings_sf, la_police_map, sparse = FALSE)
  la_shootings_sf <- la_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(la_shootings_sf)[names(la_shootings_sf) == 'Count'] <- 'Shootings'
  return(la_shootings_sf)
}
#function to call police station shapefile
get_la_police_stations <- function(){
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  police_stations_la <- read_csv(file.path(path, "police_station_locations_la.csv"))
  police_stations_la_sf<- st_as_sf(police_stations_la,  coords = c("longitude", "latitude"), crs = 4326)
  return(police_stations_la_sf)
}
#function to call just shooting locations 2022
get_shooting_locations_2022 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  shootings_la_2022 <- read_csv(file.path(path, "shootings_la_2022.csv"))
  shootings_sf <- st_as_sf(shootings_la_2022,  coords = c("LON", "LAT"), crs = 4326)
  return(shootings_sf)
}
#function to call just shooting locations 2019
get_shooting_locations_2019 <- function() {
  path <- ("~/Documents/GitHub/final-project-brenda-angula-andrea-running/Data/LA Data")
  shootings_la_2019 <- read_csv(file.path(path, "shootings_la_2019.csv"))
  shootings_sf <- st_as_sf(shootings_la_2019,  coords = c("LON", "LAT"), crs = 4326)
  return(shootings_sf)
}

#use functions to call shapefiles 
la_shootings_sf_2022 <- get_shootings_sf_la_2022()
shooting_locations_sf_la_2022 <- get_shooting_locations_2022()
police_stations_la_sf <- get_la_police_stations()
la_shootings_sf_2019 <- get_shootings_sf_la_2019()
shooting_locations_sf_la_2019 <- get_shooting_locations_2019()

#visualize functions 2022
#Visualize showing shootings and police stations
ggplot() +
  geom_sf(data = la_shootings_sf_2022, aes(fill = Shootings), color = "black") +
  ggtitle("2022 Shooting Incidences and Police Stations in LA County", subtitle = "shown by police precinct boundary") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white")) +
  scale_fill_continuous(low = "grey", high = "red") +
  labs(fill = "Shootings") +
  geom_sf(data = police_stations_la_sf, aes(), shape = 8, size = 1.5, color = "blue") +
  geom_sf(data = shooting_locations_sf_la_2022, aes(), size = .5, color = "black", alpha = 4/10)
#visualize functions 2019
#Visualize showing shootings and police stations
ggplot() +
  geom_sf(data = la_shootings_sf_2019, aes(fill = Shootings), color = "black") +
  ggtitle("2019 Shooting Incidences and Police Stations in LA County", subtitle = "shown by police precinct boundary") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white")) +
  scale_fill_continuous(low = "grey", high = "red") +
  labs(fill = "Shootings") +
  geom_sf(data = police_stations_la_sf, aes(), shape = 8, size = 1.5, color = "blue") +
  geom_sf(data = shooting_locations_sf_la_2019, aes(), size = .5, color = "black", alpha = 4/10)
