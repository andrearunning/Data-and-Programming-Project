library(tidyverse)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(shiny)

get_shootings_sf_chi_2022 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/Boundaries - Police Districts (current)")
  chi_police_sf <- st_read(file.path(path, "geo_export_ba432f02-c067-4a0f-a392-521838fa9d07.shp"))
  chi_police_map <- st_as_sf(chi_police_sf)
  chi_police_map <- st_set_crs(chi_police_sf, 4326)
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
  shootings_chi_2022  <- read_csv(file.path(path, "shootings_chi_2022.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_chi_sf, chi_police_map, sparse = FALSE)
  shootings_chi_sf <- chi_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(shootings_chi_sf)[names(shootings_chi_sf) == 'Count'] <- 'Shootings'
  return(shootings_chi_sf)
}

get_shootings_sf_chi_2019 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/Boundaries - Police Districts (current)")
  chi_police_sf <- st_read(file.path(path, "geo_export_ba432f02-c067-4a0f-a392-521838fa9d07.shp"))
  chi_police_map <- st_as_sf(chi_police_sf)
  chi_police_map <- st_set_crs(chi_police_sf, 4326)
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
  shootings_chi_2019  <- read_csv(file.path(path, "shootings_chi_2019.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_chi_sf, chi_police_map, sparse = FALSE)
  shootings_chi_sf <- chi_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(shootings_chi_sf)[names(shootings_chi_sf) == 'Count'] <- 'Shootings'
  return(shootings_chi_sf)
}

get_shootings_sf_nyc_2022 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/Police Precincts")
  nyc_police_sf <- st_read(file.path(path, "geo_export_6e101e15-cda9-4946-8664-97da0938516d.shp"))
  nyc_police_map <- st_as_sf(nyc_police_sf)
  nyc_police_map <- st_set_crs(nyc_police_sf, 4326)
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/")
  shootings_nyc_2022 <- read_csv(file.path(path, "shootings_nyc_2022.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_sf, nyc_police_map, sparse = FALSE)
  nyc_shootings_sf <- nyc_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(nyc_shootings_sf)[names(nyc_shootings_sf) == 'Count'] <- 'Shootings'
  return(nyc_shootings_sf)
}
get_shootings_sf_nyc_2019 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/Police Precincts")
  nyc_police_sf <- st_read(file.path(path, "geo_export_6e101e15-cda9-4946-8664-97da0938516d.shp"))
  nyc_police_map <- st_as_sf(nyc_police_sf)
  nyc_police_map <- st_set_crs(nyc_police_sf, 4326)
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/")
  shootings_nyc_2019 <- read_csv(file.path(path, "shootings_nyc_2019.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  temp <- st_within(shootings_sf, nyc_police_map, sparse = FALSE)
  nyc_shootings_sf <- nyc_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(nyc_shootings_sf)[names(nyc_shootings_sf) == 'Count'] <- 'Shootings'
  return(nyc_shootings_sf)
}

get_shootings_sf_la_2022 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data/LAPD_Divisions/")
  la_police_sf <- st_read(file.path(path, "LAPD_Divisions.shp"))
  la_police_map <- st_as_sf(la_police_sf)
  la_police_map <- st_set_crs(la_police_sf, 4326)
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data")
  shootings_la_2022 <- read_csv(file.path(path, "shootings_la_2022.csv"))
  shootings_sf <- st_as_sf(shootings_la_2022,  coords = c("LON", "LAT"), crs = 4326)
  temp <- st_within(shootings_sf, la_police_map, sparse = FALSE)
  la_shootings_sf <- la_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(la_shootings_sf)[names(la_shootings_sf) == 'Count'] <- 'Shootings'
  return(la_shootings_sf)
}

get_shootings_sf_la_2019 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data/LAPD_Divisions/")
  la_police_sf <- st_read(file.path(path, "LAPD_Divisions.shp"))
  la_police_map <- st_as_sf(la_police_sf)
  la_police_map <- st_set_crs(la_police_sf, 4326)
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data")
  shootings_la_2019 <- read_csv(file.path(path, "shootings_la_2019.csv"))
  shootings_sf <- st_as_sf(shootings_la_2019,  coords = c("LON", "LAT"), crs = 4326)
  temp <- st_within(shootings_sf, la_police_map, sparse = FALSE)
  la_shootings_sf <- la_police_map %>%
    mutate(Count = apply(temp, 2, sum))
  names(la_shootings_sf)[names(la_shootings_sf) == 'Count'] <- 'Shootings'
  return(la_shootings_sf)
}


get_chicago_police_stations <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
  police_stations_chi  <- read_csv(file.path(path, "police_station_locations_chi.csv"))
  police_station_chi_sf<- st_as_sf(police_stations_chi,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(police_station_chi_sf)
}

get_nyc_police_stations <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/")
  police_stations_nyc  <- read_csv(file.path(path, "NYC police stations.csv"))
  police_stations_nyc_df <- as.data.frame.table(police_stations_nyc)
  police_stations_nyc_sf <- st_as_sf(police_stations_nyc, coords = c("Longitude", "Latitude"), crs = 4326)
  return(police_stations_nyc_sf)
}

get_la_police_stations <- function(){
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data")
  police_stations_la <- read_csv(file.path(path, "police_station_locations_la.csv"))
  police_stations_la_sf<- st_as_sf(police_stations_la,  coords = c("longitude", "latitude"), crs = 4326)
  return(police_stations_la_sf)
}

get_shooting_locations_chi_2022 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
  shootings_chi_2022  <- read_csv(file.path(path, "shootings_chi_2022.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_chi_sf)
}

get_shooting_locations_chi_2019 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/Chicago Data/")
  shootings_chi_2022  <- read_csv(file.path(path, "shootings_chi_2019.csv"))
  shootings_chi_sf <- st_as_sf(shootings_chi_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_chi_sf)
}

get_shooting_locations_nyc_2022 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/")
  shootings_nyc_2022 <- read_csv(file.path(path, "shootings_nyc_2022.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2022,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_sf)
}
get_shooting_locations_nyc_2019 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/NYC Data/")
  shootings_nyc_2019 <- read_csv(file.path(path, "shootings_nyc_2019.csv"))
  shootings_sf <- st_as_sf(shootings_nyc_2019,  coords = c("Longitude", "Latitude"), crs = 4326)
  return(shootings_sf)
}

get_shooting_locations_la_2022 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data")
  shootings_la_2022 <- read_csv(file.path(path, "shootings_la_2022.csv"))
  shootings_sf <- st_as_sf(shootings_la_2022,  coords = c("LON", "LAT"), crs = 4326)
  return(shootings_sf)
}

get_shooting_locations_la_2019 <- function() {
  path <- ("~/Documents/GitHub/Data-and-Programming-Project/Data/LA Data")
  shootings_la_2019 <- read_csv(file.path(path, "shootings_la_2019.csv"))
  shootings_sf <- st_as_sf(shootings_la_2019,  coords = c("LON", "LAT"), crs = 4326)
  return(shootings_sf)
}

ui <- fluidPage(
  selectInput(
    inputId = "cities",
    label = "choose a city and year",
    choices = c("Chicago (2019)", "Chicago (2022)", "New York City (2019)", "New York City (2022)", "Los Angeles (2019)", "Los Angeles (2022)")),
  radioButtons(
    inputId = "police",
    label = "choose to plot shooting incidences, police stations, or both",
    choices = c("Shooting Incidences", "Police Stations", "Shooting Incidences and Police Stations")),
  plotOutput("graph")
)

server <- function(input, output) {
  
  output$graph <- renderPlot({ 
    
    if (input$cities == "Chicago (2022)") {
      
      if (input$police == "Shooting Incidences") {
        shootings_sf_chi_2022 <- get_shootings_sf_chi_2022()
        shootings_location_chi_2022 <- get_shooting_locations_chi_2022() 
        ggplot() +
          geom_sf(data = shootings_sf_chi_2022, aes(fill = Shootings), color = "black") +
          ggtitle("2022 Shooting Incidences Chicago", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = shootings_location_chi_2022, aes(), size = .5, color = "black")
        
      } else if(input$police == "Police Stations") {
        chicago_police_stations <- get_chicago_police_stations()
        shootings_sf_chi_2022 <- get_shootings_sf_chi_2022()
        ggplot() +
          geom_sf(data = shootings_sf_chi_2022, aes(fill = Shootings), color = "black") +
          ggtitle("2022 Police Stations Chicago", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = chicago_police_stations, aes(), shape = 8, size = 1.5, color = "blue")
      } else if(input$police == "Shooting Incidences and Police Stations") {
        chicago_police_stations <- get_chicago_police_stations()
        shootings_sf_chi_2022 <- get_shootings_sf_chi_2022()
        shootings_location_chi_2022 <- get_shooting_locations_chi_2022()
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
          geom_sf(data = chicago_police_stations, aes(), shape = 8, size = 1.5, color = "blue") +
          geom_sf(data = shootings_location_chi_2022, aes(), size = .5, color = "black", alpha = 4/10) 
      }
      
    } else if(input$cities == "Chicago (2019)") {
          if(input$police == "Shooting Incidences") {
          shootings_sf_chi_2019 <- get_shootings_sf_chi_2019()
          shootings_location_chi_2019 <- get_shooting_locations_chi_2019() 
          ggplot() +
            geom_sf(data = shootings_sf_chi_2019, aes(fill = Shootings), color = "black") +
            ggtitle("2019 Shooting Incidences Chicago", subtitle = "shown by police precinct boundary") +
            theme(axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "white")) +
            scale_fill_continuous(low = "grey", high = "red") +
            labs(fill = "Shootings") +
            geom_sf(data = shootings_location_chi_2019, aes(), size = .5, color = "black")
        } else if(input$police == "Police Stations") {
          chicago_police_stations <- get_chicago_police_stations()
          shootings_sf_chi_2019 <- get_shootings_sf_chi_2019()
          ggplot() +
            geom_sf(data = shootings_sf_chi_2019, aes(fill = Shootings), color = "black") +
            ggtitle("2019 Police Stations Chicago", subtitle = "shown by police precinct boundary") +
            theme(axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "white")) +
            scale_fill_continuous(low = "grey", high = "red") +
            labs(fill = "Shootings") +
            geom_sf(data = chicago_police_stations, aes(), shape = 8, size = 1.5, color = "blue")
        } else if(input$police == "Shooting Incidences and Police Stations") {
          chicago_police_stations <- get_chicago_police_stations()
          shootings_sf_chi_2019 <- get_shootings_sf_chi_2019()
          shootings_location_chi_2019 <- get_shooting_locations_chi_2019()
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
            geom_sf(data = chicago_police_stations, aes(), shape = 8, size = 1.5, color = "blue") +
            geom_sf(data = shootings_location_chi_2019, aes(), size = .5, color = "black", alpha = 4/10) 
          }
          
    } else if(input$cities == "New York City (2022)") {
      if (input$police == "Shooting Incidences") {
      shootings_sf_nyc_2022 <- get_shootings_sf_nyc_2022()
      shootings_locations_nyc_2022 <- get_shooting_locations_nyc_2022()
      ggplot() +
        geom_sf(data = shootings_sf_nyc_2022, aes(fill = Shootings), color = "black") +
        ggtitle("2022 Shooting Incidences NYC", subtitle = "shown by police precinct boundary") +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(color = "white")) +
        scale_fill_continuous(low = "grey", high = "red") +
        labs(fill = "Shootings") +
        geom_sf(data = shootings_locations_nyc_2022, aes(), size = .5, color = "black")
      } else if (input$police == "Police Stations") {
      shootings_sf_nyc_2022 <- get_shootings_sf_nyc_2022()
      nyc_police_stations <- get_nyc_police_stations()
      ggplot() +
        geom_sf(data = shootings_sf_nyc_2022, aes(fill = Shootings), color = "black") +
        ggtitle("2022 Police Stations NYC", subtitle = "shown by police precinct boundary") +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(color = "white")) +
        scale_fill_continuous(low = "grey", high = "red") +
        labs(fill = "Shootings") +
        geom_sf(data = nyc_police_stations, aes(), shape = 8, size = 1.5, color = "blue")
      } else if (input$police == "Shooting Incidences and Police Stations"){
        shootings_sf_nyc_2022 <- get_shootings_sf_nyc_2022()
        nyc_police_stations <- get_nyc_police_stations()
        shootings_locations_nyc_2022 <- get_shooting_locations_nyc_2022()
        ggplot() +
        geom_sf(data = shootings_sf_nyc_2022, aes(fill = Shootings), color = "black") +
          ggtitle("2022 Shooting Incidences and Police Stations NYC", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = nyc_police_stations, aes(), shape = 8, size = 1.5, color = "blue") +
          geom_sf(data = shootings_locations_nyc_2022, aes(), size = .5, color = "black", alpha = 4/10)
      }
    } else if(input$cities == "New York City (2019)") {
      if(input$police == "Shooting Incidences") {
        shootings_sf_nyc_2019 <- get_shootings_sf_nyc_2019()
        shootings_location_nyc_2019 <- get_shooting_locations_nyc_2019() 
        ggplot() +
          geom_sf(data = shootings_sf_nyc_2019, aes(fill = Shootings), color = "black") +
          ggtitle("2019 Shooting Incidences New York City ", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = shootings_location_nyc_2019, aes(), size = .5, color = "black")
      } else if(input$police == "Police Stations") {
        nyc_police_stations <- get_nyc_police_stations()
        shootings_sf_nyc_2019 <- get_shootings_sf_nyc_2019()
        ggplot() +
          geom_sf(data = shootings_sf_nyc_2019, aes(fill = Shootings), color = "black") +
          ggtitle("2019 Police Stations New York City", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = nyc_police_stations, aes(), shape = 8, size = 1.5, color = "blue")
      } else if(input$police == "Shooting Incidences and Police Stations") {
        nyc_police_stations <- get_nyc_police_stations()
        shootings_sf_nyc_2019 <- get_shootings_sf_nyc_2019()
        shootings_location_nyc_2019 <- get_shooting_locations_nyc_2019()
        ggplot() +
          geom_sf(data = shootings_sf_nyc_2019, aes(fill = Shootings), color = "black") +
          ggtitle("2019 Shooting Incidences and Police Stations New York City", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = nyc_police_stations, aes(), shape = 8, size = 1.5, color = "blue") +
          geom_sf(data = shootings_location_nyc_2019, aes(), size = .5, color = "black", alpha = 4/10) 
      }
    } else if(input$cities == "Los Angeles (2019)"){
      if(input$police == "Shooting Incidences") {
        shootings_sf_la_2019 <- get_shootings_sf_la_2019()
        shootings_locations_la_2019 <- get_shooting_locations_la_2019()
        ggplot() +
          geom_sf(data = shootings_sf_la_2019, aes(fill = Shootings), color = "black") +
          ggtitle("2019 Shooting Incidences Los Angeles", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = shootings_locations_la_2019, aes(), size = .5, color = "black")
      } else if(input$police == "Police Stations"){
        la_police_stations <- get_la_police_stations()
        shootings_sf_la_2019 <- get_shootings_sf_la_2019()
        ggplot() +
          geom_sf(data = shootings_sf_la_2019, aes(fill = Shootings), color = "black") +
          ggtitle("2019 Police Stations LA County", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = la_police_stations, aes(), shape = 8, size = 1.5, color = "blue")
      } else {
        la_police_stations <- get_la_police_stations()
        shootings_sf_la_2019 <- get_shootings_sf_la_2019()
        shootings_locations_la_2019 <- get_shooting_locations_la_2019()
        ggplot() +
          geom_sf(data = shootings_sf_la_2019, aes(fill = Shootings), color = "black") +
          ggtitle("2019 Shooting Incidences in Los Angeles and Police Stations in LA County", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = la_police_stations, aes(), shape = 8, size = 1.5, color = "blue") +
          geom_sf(data = shootings_locations_la_2019, aes(), size = .5, color = "black", alpha = 4/10)
      }
    } else if(input$cities == "Los Angeles (2022)") {
      if(input$police == "Shooting Incidences") {
        shootings_sf_la_2022 <- get_shootings_sf_la_2019()
        shootings_locations_la_2022 <- get_shooting_locations_la_2022()
        ggplot() +
          geom_sf(data = shootings_sf_la_2022, aes(fill = Shootings), color = "black") +
          ggtitle("2022 Shooting Incidences Los Angeles", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = shootings_locations_la_2022, aes(), size = .5, color = "black")
      } else if(input$police == "Police Stations"){
        la_police_stations <- get_la_police_stations()
        shootings_sf_la_2022 <- get_shootings_sf_la_2022()
        ggplot() +
          geom_sf(data = shootings_sf_la_2022, aes(fill = Shootings), color = "black") +
          ggtitle("2022 Police Stations LA County", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = la_police_stations, aes(), shape = 8, size = 1.5, color = "blue")
      } else if(input$police == "Shooting Incidences and Police Stations") {
        la_police_stations <- get_la_police_stations()
        shootings_sf_la_2022 <- get_shootings_sf_la_2022()
        shootings_locations_la_2022 <- get_shooting_locations_la_2022()
        ggplot() +
          geom_sf(data = shootings_sf_la_2022, aes(fill = Shootings), color = "black") +
          ggtitle("2022 Shooting Incidences in Los Angeles and Police Stations in LA County", subtitle = "shown by police precinct boundary") +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "white")) +
          scale_fill_continuous(low = "grey", high = "red") +
          labs(fill = "Shootings") +
          geom_sf(data = la_police_stations, aes(), shape = 8, size = 1.5, color = "blue") +
          geom_sf(data = shootings_locations_la_2022, aes(), size = .5, color = "black", alpha = 4/10)
        
 
      }}})}

shinyApp(ui = ui, server = server)


