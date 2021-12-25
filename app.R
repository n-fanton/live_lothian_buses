# Intro: packages -------------------------------------------------------------
library("lothianBuses")
library("shiny")
library("tidyverse")
library("janitor")
library("shinythemes")
library("shinyWidgets")
library("leaflet")

# Intro: data -----------------------------------------------------------------

search_names <- read_rds(here::here("data", "search_names.rds"))
stop_lookups <- read_rds(here::here("data", "stop_lookups.rds"))
shapefiles   <- read_rds(here::here("data", "route_shapefiles.rds"))

stop_services <- read_rds("stop_services.rds")

# Remove Shandwick Place from display for trams
# Trams should show up when searching for Shandwick Place, but display
# as departing from West End


# Intro: helper functions -----------------------------------------------------
source("helpers.R")

icons <- awesomeIcons(
  icon = 'bus',
  iconColor = "#E1DFCC",
  library = 'fa',
  markerColor = "#970000"
)

