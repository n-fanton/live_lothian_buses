# Prepare data for app --------------------------------------------------------
## Save new RDS data? ---------------------------------------------------------
save <- TRUE

## Download all stops from TfE API --------------------------------------------
all_stops <- lothianBuses::get_stops() %>%
  mutate(
    ### Rename tram stops -----------------------------------------------------
    # Stops are renamed to normal names, and often to correspond to and group
    # with nearby bus stops
    name = case_when(
      stop_id == "36290101" ~ "Airport",
      stop_id == "36290104" ~ "Balgreen Tram Stop",
      stop_id == "36290105" ~ "Balgreen Tram Stop",
      stop_id == "36290107" ~ "Bankhead",
      stop_id == "36290108" ~ "Bankhead",
      stop_id == "36290110" ~ "Edinburgh Gateway",
      stop_id == "36290111" ~ "Edinburgh Gateway",
      stop_id == "36290113" ~ "Gogarburn",
      stop_id == "36290114" ~ "Gogarburn",
      stop_id == "36290116" ~ "Gyle Centre",
      stop_id == "36290117" ~ "Gyle Centre",
      stop_id == "36290119" ~ "Haymarket Station",
      stop_id == "36290120" ~ "Haymarket Station",
      stop_id == "36290122" ~ "Ingliston Park & Ride",
      stop_id == "36290123" ~ "Ingliston Park & Ride",
      stop_id == "36290125" ~ "Murrayfield Stadium",
      stop_id == "36290126" ~ "Murrayfield Stadium",
      stop_id == "36290128" ~ "Edinburgh Park Central",
      stop_id == "36290129" ~ "Edinburgh Park Central",
      stop_id == "36290131" ~ "Edinburgh Park Station",
      stop_id == "36290132" ~ "Edinburgh Park Station",
      stop_id == "36290134" ~ "Princes Street (tram)",
      stop_id == "36290135" ~ "Princes Street (tram)",
      stop_id == "36290137" ~ "Saughton",
      stop_id == "36290138" ~ "Saughton",
      stop_id == "36290140" ~ "West End",
      stop_id == "36290141" ~ "West End",
      stop_id == "36290143" ~ "St Andrew Square",
      stop_id == "36290144" ~ "St Andrew Square",
      stop_id == "36290146" ~ "York Place",
      TRUE ~ name),
    stop_id = case_when(
      stop_id == 36290146 ~ "36290145",
      TRUE ~ as.character(stop_id)))

## Grouped stops to search for in the app -------------------------------------
grouped_stops <- all_stops %>%
  mutate(
    ### Use station direction as identifier if there is none ------------------
    identifier = case_when(
      is.na(identifier) ~ direction,
      TRUE ~ identifier)) %>%
  bind_rows(
    ### Add additional names for West End / Shandwick Place tram --------------
    all_stops %>%
      filter(stop_id %in% c("36290140", "36290141")) %>%
      mutate(name = "Shandwick Place")) %>%
  group_by(name) %>%
  summarise(
    ### Group IDs and identifiers for each stop name --------------------------
    stop_ids_all = paste(unique(stop_id), collapse = ", "),
    identifiers = paste(unique(identifier), collapse = ", "),
    number = n()) %>%
  mutate(
    ### Convert grouped IDs and identifiers into vectors ----------------------
    stop_id = stringr::str_split(stop_ids_all, ", "),
    identifier = stringr::str_split(identifiers, ", ")) %>%
  select(name, stop_id, identifier)

### Save ----------------------------------------------------------------------
if (save) write_rds(grouped_stops, "grouped_stops.rds")

## Stop identifiers to display in app -----------------------------------------
stop_identifiers <- all_stops %>%
  mutate(
    ### Use 'TRAM' as identifier for tram stops -------------------------------
    identifier = case_when(
      service_1 == "T50" ~ "TRAM",
      is.na(identifier) ~ direction,
      TRUE ~ identifier)) %>%
  mutate(stop_id = as.character(stop_id)) %>%
  select(stop_id, identifier, name)

### Save ----------------------------------------------------------------------
if (save) write_rds(stop_identifiers, "stop_identifiers.rds")

## List of routes for map filtering -------------------------------------------
routes <- list_routes() %>%
  mutate(route_display = paste(name, description))

if (save) write_rds(routes, "routes.rds")


## Bind stops to services -----------------------------------------------------
stops <- httr::GET(url = "https://tfe-opendata.com/api/v1/stops") %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE) %>%
  magrittr::extract2(2) %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  unnest(services) %>%
  select(stop_id, name, services)

if(save) write_rds(stops, "stop_services.rds")

## Get route colours ----------------------------------------------------------

if (FALSE) {
  get_route_colour <- function(route) {

    route <- as.character(route)
    message(route)

    route_api <- paste0("https://lothianapi.com/routePatterns?route_name=", route)

    colour <- httr::GET(url = route_api) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      magrittr::use_series(route) %>%
      magrittr::use_series(color)

    if(!is.null(colour)) return(colour)
  }

  colours <- routes %>%
    filter(!(name %in% c("281"))) %>%
    select(name) %>%
    rowwise() %>%
    mutate(colour = get_route_colour(name)) %>%
    ungroup() %>%
    mutate(colour = stringr::str_to_upper(colour))

  write_rds(colours, "route_colours.rds")
}

## Route shapefiles -----------------------------------------------------------
routes <- httr::GET(url = "https://tfe-opendata.com/api/v1/services") %>%
  httr::content(as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE) %>%
  magrittr::extract2(2) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

get_route_shapefiles <- function(route) {
  routes %>%
    filter(name == as.character(route)) %>%
    select(routes) %>%
    pull() %>%
    magrittr::extract2(1) %>%
    as_tibble() %>%
    mutate(route_name = as.character(route))
}

shapes <- tibble()

for (i in 1:nrow(routes)) {
  temp <- get_route_shapefiles(routes[i,1])

  shapes <- bind_rows(shapes, temp)
}


all_route_shapefiles <- full_join(routes, shapes,
                                  by = c("name" = "route_name")) %>%
  left_join(colours)

if(save) write_rds(all_route_shapefiles, "route_shapefiles.rds")

