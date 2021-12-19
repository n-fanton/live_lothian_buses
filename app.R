# Intro: packages -------------------------------------------------------------
library("lothianBuses")
library("shiny")
library("tidyverse")
library("janitor")
library("shinythemes")

# Intro: data -----------------------------------------------------------------

grouped_stops <- read_rds("grouped_stops.rds")
stop_identifiers <- read_rds("stop_identifiers.rds")

# Remove Shandwick Place from display for trams
# Trams should show up when searching for Shandwick Place, but display
# as departing from West End
display_identifiers <- stop_identifiers %>%
  filter(!(name == "Shandwick Place" & identifier == "TRAM"))

# Intro: helper functions -----------------------------------------------------
source("helpers.R")

