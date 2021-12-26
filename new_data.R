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
  mutate(name = case_when(!is.na(new_name) ~ new_name,
                          TRUE ~ name),
         search_name = name,
         display_name = case_when(is.na(identifier) ~ paste(name, direction),
                                  TRUE ~ paste(name, identifier))) %>%
  rowwise() %>%
  mutate(display_services = paste(services, collapse = ", "),
         display_destinations = paste(destinations, collapse = ", ")) %>%
  ungroup()

stops <- stops %>%
  bind_rows(stops %>%
              filter(stop_id %in% c("36290140", "36290141")) %>%
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
