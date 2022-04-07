# Helper functions ------------------------------------------------------------
## Find current time in HH:MM -------------------------------------------------
current_time <- function() {
  Sys.time() %>%
    with_tz("GB") %>%
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

## Function to add services to mao --------------------------------------------
add_services_to_map <- function(map, services, shp) {
  # Clean services input
  services <- as.character(services)
  # Select data to map
  data_to_map <- shp %>%
    filter(route_name %in% services)
  # Add route to map for each line of selected data
  for (i in 1:nrow(data_to_map)) {
    # Find colour for the route
    route_colour <- data_to_map[i, ] %>%
      select(colour) %>%
      pull()
    # Find label for the route
    route_label <- data_to_map[i, ] %>%
      mutate(label = paste0("Route ", route_name, " to ", destination)) %>%
      select(label) %>%
      pull()
    # Add lines to map
    map <- map %>%
      addPolylines(data = data_to_map,
                   lat = ~points[[i]]$latitude,
                   lng = ~points[[i]]$longitude,
                   color = route_colour,
                   opacity = 1,
                   label = route_label)
  }
  # Return map
  map
}

