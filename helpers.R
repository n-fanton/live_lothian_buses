# Helper functions ------------------------------------------------------------
## Find current time in HH:MM -------------------------------------------------
current_time <- function() {
  Sys.time() %>%
    str_sub(12,16)
}

## Convert HH:MM to minutes since midnight ------------------------------------
to_minutes <- function(hh_mm) {
  ### Number of hours since midnight ------------------------------------------
  hours <- hh_mm %>%
    stringr::str_sub(1, 2) %>%
    as.numeric()

  # If the desired time shows is future (identified as the hours being
  # less than now), add 24 to show since last midnight
  hours_now <- stringr::str_sub(current_time(), 1, 2) %>%
    as.numeric()
  hours <- case_when(hours < hours_now-1 ~ hours + 24,
                     TRUE ~ hours)

  ### Minutes since the last full hour ----------------------------------------
  minutes <- hh_mm %>%
    stringr::str_sub(4, 5) %>%
    as.numeric()

  ### Display minutes since midnight
  magrittr::add((hours * 60), minutes)
}

## Create routes map ----------------------------------------------------------
create_map <- function(services, shapefile = route_shapefle) {

  services <- as.character(services)

  data_to_map <- shapefile %>%
    filter(name %in% services)

  map <- leaflet() %>%
    addTiles()

  for (i in 1:nrow(data_to_map)) {
    map <- map %>%
      addPolylines(data = data_to_map,
                   lat = ~points[[i]]$latitude,
                   lng = ~points[[i]]$longitude)
  }

  map
}
