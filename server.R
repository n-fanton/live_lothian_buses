# App server side calculations ------------------------------------------------
server <- function(input, output) {
  ## Current time for refresh button ------------------------------------------
  output$last_refreshed <- renderText({
    input$refresh
    paste0("Last refreshed: ", current_time())
  })

  ## Download live departures data for selected stops -------------------------
  live_departures_data <- reactive({
    # Reload when refresh button is clicked
    input$refresh

    # Find IDs for selected bus stops to read coming departures
    selected_stop_ids <- stop_lookups %>%
      filter(search_name %in% input$selected_stops) %>%
      select(stop_id) %>%
      pull()

    # Download data from API if services are selected, otherwise return
    # empty tibble
    if (length(selected_stop_ids) > 0) {
      live_departures(selected_stop_ids)
    } else {
      tibble()
    }
  })

  ## Data table to show live departures ---------------------------------------
  output$live_departures <- renderDataTable({
    # Find how much time it's been since midnight
    # Minutes until arrival are calculated as the difference in minutes since
    # midnight of the last refresh, and of the time the bus is due. This also
    # allows for post-midnight services: if requested time is in the 'past',
    # e.g. 01:30 when it's currently 11:20, it'll add 24h to the 01:30.
    minutes_since_midnight <- Sys.time() %>%
      str_sub(12,16) %>%
      to_minutes()

    ### Output table ----------------------------------------------------------
    live_departures_data() %>%
      # Filter out terminating services unless selected otherwise in input
      {if (input$show_terminators) {
        .
      } else if ("is_terminating_here" %in% names(.)) {
        # If there's no terminating services the variable doesn't appear in
        # the API data, so only filtering if the column is present
        filter(., !is_terminating_here)
      }} %>%
      # Add stop ID data for display
      left_join(stop_lookups, by = c("sms" = "stop_id")) %>%
      mutate(
        # The tram route is listed as T50 (ambitious!), so here I'm changing
        # it to just say 'Tram'
        #route_name = case_when(route_name == "T50" ~ "Tram",
        #                       TRUE ~ route_name),
        # Find minutes since midnight of departures
        time_after_midnight = to_minutes(display_time),
        # Calculate departure time in minutes since current time
        departs_in = time_after_midnight - minutes_since_midnight,
        # Sometimes the departure time shows as -1, so here it shows as 'DUE'
        # if due in less than a minute
        due = case_when(departs_in < 1 ~ "DUE",
                        TRUE ~ as.character(departs_in)),
        # Add asterisk if departure is not live
        due = case_when(!is_live ~ paste0(due, "*"),
                        TRUE ~ due),
        destination = case_when(
          # This typo has been annoying me for the three years I've lived
          # in Edinburgh
          destination == "Royal Infirmry" ~ "Royal Infirmary",
          TRUE ~ destination)) %>%
      # Sort order of display by due time, route, and destination
      arrange(departs_in, route_name, display_name) %>%
      # Select only variables that make it to output
      select(due, route_name, destination, display_name) %>%
      unique() %>%
      # Rename into human-friendly variable names
      # Importatnt to display stop because multiple stops can be selected,
      # and display_name also includes identifiers where there's multiple
      # stops grouped together (Princes Street, RIE, etc.)
      rename(`Due` = due,
             `Route` = route_name,
             `To` = destination,
             `Stop` = display_name)
  },
  # Only show eight entries per page of output
  options = list(pageLength = 8))

  ## Put output into a wrapper if there's no data to display ------------------
  output$departures_ui <- renderUI({
    # If there are no upcoming departures, just return the text
    if (nrow(live_departures_data()) == 0)
      return("No departures")

    # If there are some departures to display, show them!
    dataTableOutput("live_departures")
  })

  ## Create live map ----------------------------------------------------------
  output$services_map <- renderLeaflet({

    # Refresh this when the refresh button is clicked
    input$refresh

    # Create baseline map to which to add elements
    map <- leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap")

    ### Display bus routes ----------------------------------------------------
    #### Select routes to display ---------------------------------------------
    if (input$show_all) {
      # Display all routes if that's what's requested
      selected_services <- stop_lookups %>%
        filter(search_name %in% input$selected_stops) %>%
        select(services) %>%
        pull() %>%
        unlist() %>%
        unique()
    } else if (nrow(live_departures_data()) > 0) {
      # If not displaying all routes, display those on departures board,
      # if there are any
      selected_services <- live_departures_data() %>%
        select(route_name) %>%
        pull() %>%
        unique()
    } else {
      # If not displaying all, and departures board is empty, return NULL
      selected_services <- NULL
    }

    # Does the map require the tram line to be drawn?
    map_requires_tram <- "T50" %in% selected_services

    #### Add services to map --------------------------------------------------
    if (length(selected_services[selected_services != "T50"]) > 0) {
      # If there are services to display, add them to the base map
      map <- map %>%
        add_services_to_map(
          services = selected_services[selected_services != "T50"],
          shp = shapefiles)
    } else if (!map_requires_tram) {
      # If there are no services to display, just add a note that says so
      map <- map %>%
        addAwesomeMarkers(
          icon = awesomeIcons(
            icon = "bus",
            iconColor = "red",
            library = "fa",
            markerColor = "white",
            iconRotate = 180,
            spin = TRUE),
          lat = 55.951741,
          lng = -3.191745,
          label = "One day, there will be buses")
    }

    #### Add tram line to map if required -------------------------------------
    if (map_requires_tram) {
      map <- map %>%
        addPolylines(data = tram_shapefile,
                     color = "#980006",
                     label = "Airport - York Place tram")
    }


    #### Add bus stops to map (if requested) ----------------------------------
    if (input$show_stops & length(selected_services) > 0) {
      # Find all stops used by the selected routes
      stops_to_display <- stops_by_route %>%
        filter(services %in% selected_services) %>%
        select(stop_id) %>%
        pull() %>%
        unlist() %>%
        unique()

      # Get extra data for the selected stops: all routes that call there,
      # all destinations, and where the stops actually are
      # TODO: Show all stops in group rather than just the exact stops
      stop_map_data <- stops %>%
        filter(stop_id %in% stops_to_display) %>%
        mutate(stop_label = paste0(
          "<b>", display_name, "</b><br><b> Routes: </b>",
          display_services, "<br><b> To: </b>",
          display_destinations))

      # Add the stop data to map, with custom markers
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
          # Without lapply, each stop will show data for all
          # see https://stackoverflow.com/questions/43144596
          label = ~lapply(stop_label, htmltools::HTML),
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 33))
      } else if (length(selected_services) > 0) {
        # Otherwise just show the selected stops on map

        # Get data for the selected stops: all routes that call there,
        # all destinations, and where the stops actually are
        # TODO: Show all stops in group rather than just the exact stops
        stop_map_data <- stops %>%
          filter(search_name %in% input$selected_stops) %>%
          mutate(stop_label = paste0(
            "<b>", display_name, "</b><br><b> Routes: </b>",
            display_services, "<br><b> To: </b>",
            display_destinations))

        # Add the stop data to map, with custom markers
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
            # Without lapply, each stop will show data for all
            # see https://stackoverflow.com/questions/43144596
            label = ~lapply(stop_label, htmltools::HTML))
      }

    #### Display live bus locations on map (if requested) ---------------------
    if (input$show_live_buses & length(selected_services) > 0) {
      # Load live locations from TfE and then filter for just the services
      # we want displayed on the map
      temp_locations <- live_locations() %>%
        filter(service_name %in% selected_services) %>%
        left_join(stop_lookups, by = c("next_stop_id" = "stop_id")) %>%
        left_join(route_colours, by = c("service_name" = "name")) %>%
        mutate(
          # Fix 'Royal Infirmry' in destinations
          destination = case_when(
            destination == "Royal Infirmry" ~ "Royal Infirmary",
            TRUE ~ destination),
          # Want to avoid displaying NAmph or 'Moving at 0mph'
          display_speed = case_when(
            is.na(speed) ~ "Stopped",
            speed == 0 ~ "Stopped",
            TRUE ~ paste0("Moving at ", speed, "mph")),
          bus_label = paste0(
            "Service <b>", service_name, "</b> to <b>", destination, "</b><br>",
            "Next stop: ", display_name, "<br>", display_speed, "<br>",
            last_gps_fix_secs, " seconds ago"))

      # Add the bus locations to the base map
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
            # Rotate the arrow marker for the direction the bus is heading
            iconRotate = heading),
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 33))
      }

    ### Return the map --------------------------------------------------------
    # This really feels like to small a command after all the buildup
    map
    })

  ## Travel alerts ------------------------------------------------------------
  output$alerts <- renderUI({
    # Travel alerts link - I got this directly from the Lothian website
    alerts_url <- paste0("https://lothianupdates.com/api/public/",
                         "getServiceUpdates")

    # The alerts are returned as JSON, but all I really need is one single
    # bit of HTML-styled text to display
    # First read the JSON data and clean it up a bit
    httr::GET(url = alerts_url) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE) %>%
      magrittr::extract2(1) %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      # New variable to display alerts styled in HTML, bold title, normal
      # text, a new row for each title and bit of text
      mutate(label = paste0("<b>", title_en, "</b><br>",
                            description_en, "<br>")) %>%
      # Now select just this column and extract it into one long string
      select(label) %>%
      pull() %>%
      paste(collapse = "") %>%
      # Finally, apply HTML style to what is now just text with weird
      # <b> and <br> tags in it
      htmltools::HTML()
    })
  }

