# Download and clean data -----------------------------------------------------
## Alternative bus stop names -------------------------------------------------
new_stop_names <- read_csv(here::here("data", "display_names.csv"),
                           col_types = cols(.default = col_character())) %>%
  select(stop_id, new_name)

## Route colours --------------------------------------------------------------
route_colours <- read_rds(here::here("data", "route_colours.rds"))

## Bus stops ------------------------------------------------------------------
stops <- httr::GET(url = "https://tfe-opendata.com/api/v1/stops") %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE) %>%
  magrittr::extract2(2) %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  mutate(stop_id = case_when(stop_id == 36290146 ~ "36290145",
                             TRUE ~ as.character(stop_id))) %>%
  left_join(new_stop_names, by = "stop_id") %>%
  mutate(name =
           case_when(!is.na(new_name) ~ new_name,
                     TRUE ~ name),
         search_name =
           name,
         int_name =
           case_when(service_type == "tram" &
                       !(stop_id %in% c("36290137", "36290138",
                                        "36290104", "36290105")) ~
                       paste(name, "(Tram)"),
                     TRUE ~ name),
         direction =
           case_when(stop_id == "36290145" ~ "",
                     TRUE ~ direction),
         display_name =
           case_when(is.na(identifier) ~ paste(int_name, direction),
                     TRUE ~ paste(int_name, identifier))) %>%
  rowwise() %>%
  mutate(display_services = paste(services, collapse = ", "),
         display_destinations = paste(destinations, collapse = ", ")) %>%
  ungroup()

# Add extra entries for West End tram stop and Atholl Crescent
# bus stop to display when looking up Shandwick Place
stops <- stops %>%
  bind_rows(stops %>%
              filter(stop_id %in% c("36290140", "36290141")) %>%
              mutate(search_name = "Shandwick Place"))%>%
  bind_rows(stops %>%
              filter(stop_id == "36236576") %>%
              mutate(search_name = "Shandwick Place"))

write_rds(stops, here::here("data", "stops.rds"))

## Routes ---------------------------------------------------------------------
routes <- httr::GET(url = "https://tfe-opendata.com/api/v1/services") %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE) %>%
  magrittr::extract2(2) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

# Prepare datasets to actually use in app (light, minimal versions) -----------

## Bus stop names for searching -----------------------------------------------
search_names <- stops %>%
  select(search_name) %>%
  unique() %>%
  pull()

write_rds(search_names, here::here("data", "search_names.rds"))

## Stop lookups for displaying ------------------------------------------------

stop_lookups <- stops %>%
  select(stop_id, search_name, display_name, services)

write_rds(stop_lookups, here::here("data", "stop_lookups.rds"))

## Lookup of stops by service for use in map ----------------------------------
stops_by_route <- stops %>%
  select(stop_id, services) %>%
  unnest(services) %>%
  group_by(services) %>%
  summarise(stop_id = paste(stop_id, collapse = ",")) %>%
  mutate(stop_id = str_split(stop_id, ","))

write_rds(stops_by_route, here::here("data", "stops_by_route.rds"))

## Sort out shapefiles for each route -----------------------------------------

### Extract shapefiles from downloaded route data -----------------------------

# Shapefiles are saved as a dataset within a dataset within the downloaded
# files, so here they're extracted for each route
get_route_shapefiles <- function(route) {
  routes %>%
    filter(name == as.character(route)) %>%
    select(routes) %>%
    pull() %>%
    magrittr::extract2(1) %>%
    as_tibble() %>%
    mutate(route_name = as.character(route))}

# Need to initialise an empty tibble to then add data
route_shapefiles <- tibble()

# Add to that tibble the geospatial data for each route
for (i in 1:nrow(routes)) {
  temp <- get_route_shapefiles(routes[i,1])
  route_shapefiles <- bind_rows(route_shapefiles, temp)
}

route_shapefiles <- route_shapefiles %>%
  left_join(route_colours, by = c("route_name" = "name")) %>%
  rowid_to_column("order") %>%
  arrange(-order) %>%
  select(-order)

write_rds(route_shapefiles, here::here("data", "route_shapefiles.rds"))

### Download tram route shapefile ---------------------------------------------

tramlines_url <- paste0(
  "https://opendata.arcgis.com/api/v3/datasets/",
  "fcd8d11078284316ac4b40244b069a4a_8/downloads/",
  "data?format=geojson&spatialRefId=4326")

tram_shapefile <- sf::st_read(tramlines_url) %>%
  sf::st_zm(drop = T, what = "ZM")

write_rds(tram_shapefile, here::here("data", "tram_shapefile.rds"))


### New way of doing bus maps -------------------------------------------------
# Managed to convince the data to be a simple features thing

busdata <- lothianBuses::list_routes(.details = TRUE)

bus_shapefile <- busdata %>%
  unnest("routes") %>%
  unnest("points") %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84") %>%
  group_by(name, description, destination, service_type) %>%
  summarize(m = mean(as.numeric(name)), do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  left_join(route_colours)

tram_shapefile <- read_csv(here::here("data", "tramline.csv")) %>%
  select(-idk) %>%
  mutate(
    name = "T50",
    description = "Airport - York Place",
    destination = "Airport or York Place",
    colour = "#8C1713",
    service_type = "tram",
    order = 0
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84") %>%
  group_by(name, description, destination, colour, service_type, order) %>%
  summarize(order = mean(order), do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  ungroup()

all_shapefile <- bus_shapefile %>%
  ungroup() %>%
  mutate(order = row_number()) %>%
  bind_rows(tram_shapefile) %>%
  mutate(
    colour = case_when(
      name %in% c("125", "126", "127") ~ "#FF9800",
      name == "45" ~ "#51B8BB",
      TRUE ~ colour
    )
  ) %>%
  arrange(order) %>%
  select(-c(m, order)) %>%
  mutate(
    label = case_when(
      name == "T50" ~ "<b>Tram</b> between <b>City Centre</b> and <b>York Place</b>",
      TRUE ~ paste0("Route <b>", name, "</b> to <b>", destination, "</b>")
    )
  )

geojsonio::geojson_write(all_shapefile,
                         file = here::here("data", "tfe_shapefile.geojson"))
