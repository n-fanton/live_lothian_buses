# Intro: packages -------------------------------------------------------------
library("lothianBuses")
library("shiny")
library("tidyverse")
library("janitor")
library("shinythemes")
library("shinyWidgets")
library("leaflet")
library("sf")

# Intro: data -----------------------------------------------------------------

search_names <- read_rds(here::here("data", "search_names.rds"))
stop_lookups <- read_rds(here::here("data", "stop_lookups.rds"))
shapefiles   <- read_rds(here::here("data", "route_shapefiles.rds"))
stops        <- read_rds(here::here("data", "stops.rds"))
stops_by_route <- read_rds(here::here("data", "stops_by_route.rds"))
route_colours <- read_rds(here::here("data", "route_colours.rds"))
tram_shapefile <- read_rds(here::here("data", "tram_shapefile.rds"))
routes_shapefile <- st_read(here::here("data", "tfe_shapefile.geojson"))


# Remove Shandwick Place from display for trams
# Trams should show up when searching for Shandwick Place, but display
# as departing from West End


# Intro: helper functions -----------------------------------------------------
source("helpers.R")


