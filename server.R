# App server side calculations ------------------------------------------------
server <- function(input, output) {
  ## Live departures table-----------------------------------------------------
  ### Current time for refresh button -----------------------------------------
  output$last_refreshed <- renderText({
    input$refresh
    paste0("Last refreshed: ", current_time())
  })
  ### Download live departures data -------------------------------------------
  live_departures_data <- reactive({
    input$refresh

    selected_stop_ids <- stop_lookups %>%
      filter(search_name %in% input$selected_stops) %>%
      select(stop_id) %>%
      pull()

    #### Download data from API if services are selected ----------------------
    if (length(selected_stop_ids) > 0) {
      live_departures(selected_stop_ids)
    } else {
      tibble()
    }
  })

  ### Data table to show live departures --------------------------------------
  output$live_departures <- renderDataTable({
    #### Find how much time it's been since midnight --------------------------
    # This is used to calculate the number of minutes until arrival
    minutes_since_midnight <- Sys.time() %>%
      str_sub(12,16) %>%
      to_minutes()

    #### Output table ---------------------------------------------------------
    live_departures_data() %>%
      {if (input$show_terminators) {
        .
      } else if ("is_terminating_here" %in% names(.)) {
        filter(., !is_terminating_here)
      }} %>%
      left_join(stop_lookups, by = c("sms" = "stop_id")) %>%
      mutate(
        route_name = case_when(route_name == "T50" ~ "Tram",
                               TRUE ~ route_name),
        time_after_midnight = to_minutes(display_time),
        departs_in = time_after_midnight - minutes_since_midnight,
        due = case_when(departs_in < 1 ~ "DUE",
                        TRUE ~ as.character(departs_in)),
        destination = case_when(
          destination == "Royal Infirmry" ~ "Royal Infirmary",
          TRUE ~ destination),
        t_diversion = case_when(is_diverted ~ "Diverted. ",
                                TRUE ~ ""),
        t_term = case_when(is_terminating_here ~ "Terminates here. ",
                           TRUE ~ ""),
        note = paste0(t_term, t_diversion)) %>%
      arrange(departs_in, route_name, display_name) %>%
      select(due, route_name, destination, display_name) %>%
      rename(`Route` = route_name,
             `To` = destination,
             `Due` = due,
             `Stop` = display_name)
  },
  options = list(pageLength = 8))

  ### Put output into a wrapper if there's no data to display -----------------
  output$departures_ui <- renderUI({
    if (nrow(live_departures_data()) == 0)
      return("No data to show")

    dataTableOutput("live_departures")
  })

  ## Create live map ----------------------------------------------------------
  output$services_map <- renderLeaflet({

    input$refresh

    if (input$show_all) {
      selected_services <- stop_lookups %>%
        filter(search_name %in% input$selected_stops) %>%
        select(services) %>%
        pull() %>%
        unlist() %>%
        unique()
    } else if (nrow(live_departures_data()) > 0) {
      selected_services <- live_departures_data() %>%
        select(route_name) %>%
        pull() %>%
        unique()
    } else {
      selected_services <- NA
    }

    map <- leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap")

    if (suppressWarnings(!is.na(selected_services)) &
        length(selected_services) > 0) {
      map <- map %>%
        add_services_to_map(services = selected_services,
                            shp = shapefiles)
    } else {
      map <- map %>%
        addAwesomeMarkers(
          icon = icons,
          lat = 55.951741,
          lng = -3.191745,
          label = "One day, there will be buses")
    }

    if (input$show_stops &
        suppressWarnings(!is.na(selected_services)) &
        length(selected_services) > 0) {

      stops_to_display <- stops_by_route %>%
        filter(services %in% selected_services) %>%
        select(stop_id) %>%
        pull() %>%
        unlist() %>%
        unique()

      stop_map_data <- stops %>%
        filter(stop_id %in% stops_to_display) %>%
        mutate(stop_label = paste0(
          "<b>", display_name, "</b><br><b> Routes: </b>",
          display_services, "<br><b> To: </b>",
          display_destinations
        ))

      map <- map %>%
        addAwesomeMarkers(
          icon = awesomeIcons(
            icon = "bus",
            iconColor = "#E1DFCC",
            library = "fa",
            markerColor = "#970000"),
          data = stop_map_data,
          lat = ~latitude,
          lng = ~longitude,
          label = ~lapply(stop_label, htmltools::HTML),
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 33))
    }

    if (input$show_live_buses) {
      temp_locations <- live_locations() %>%
        filter(service_name %in% selected_services) %>%
        left_join(stop_lookups, by = c("next_stop_id" = "stop_id")) %>%
        left_join(route_colours, by = c("service_name" = "name")) %>%
        mutate(
          display_speed = case_when(
            is.na(speed) ~ "Stopped",
            speed == 0 ~ "Stopped",
            TRUE ~ paste0("Moving at ", speed, "mph")),
          bus_label = paste0(
            "Service <b>", service_name, "</b> to <b>", destination, "</b><br>",
            "Next stop: ", display_name, "<br>", display_speed))


      map <- map %>%
        addAwesomeMarkers(
          data = temp_locations,
          lat = ~latitude,
          lng = ~longitude,
          label = ~lapply(bus_label, htmltools::HTML),
          icon = ~awesomeIcons(
            icon = "arrow-up",
            iconColor = colour,
            library = "fa",
            markerColor = "white",
            iconRotate = heading),
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 33))

    }

    map

  })

  output$alerts <- renderUI({
    httr::GET(url = "https://lothianupdates.com/api/public/getServiceUpdates?key=8094E98541294E7AC25491127FAC7A72") %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      magrittr::extract2(1) %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      mutate(label = paste0("<b>", title_en, "</b><br>", description_en, "<br>")) %>%
      select(label) %>%
      pull() %>%
      paste(collapse = "") %>%
      htmltools::HTML()

  })
}

