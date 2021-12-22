# App server side calculations ------------------------------------------------
server <- function(input, output) {
  ## Live departures table-----------------------------------------------------
  ### Current time for refresh button -----------------------------------------
  output$last_refreshed <- renderText({
    input$refresh
    paste0("Last refreshed: ", current_time())
  })
  ### Selected API data -------------------------------------------------------
  data_input <- reactive({
    input$refresh
    #### Convert selected bus stop names into codes to query with API ---------
    selected_stop_codes <- grouped_stops %>%
      filter(name %in% input$selected_stops) %>%
      select(stop_id) %>%
      magrittr::extract2(1) %>%
      unlist()

    #### Download data from API if services are selected ----------------------
    if (length(selected_stop_codes) > 0) {
      live_departures(selected_stop_codes)
    } else {
      tibble()
    }
  })

  ### Put output into a wrapper if there's no data to display -----------------
  output$departures_ui <- renderUI({
    if(nrow(data_input()) == 0)
      return("No data to show")

    dataTableOutput("live_departures")
  })

  ### Data table to show live departures --------------------------------------
  output$live_departures <- renderDataTable({
    #### Find how much time it's been since midnight --------------------------
    # This is used to calculate the number of minutes until arrival
    minutes_since_midnight <- Sys.time() %>%
      str_sub(12,16) %>%
      to_minutes()

    #### Output table ---------------------------------------------------------
    data_input() %>%
      {if (input$show_terminators) {
        .
      } else if ("is_terminating_here" %in% names(.)) {
        filter(., !is_terminating_here)
      }} %>%
     # {if (input$show_terminators) . else filter(., !is_terminating_here)} %>%
      left_join(display_identifiers, by = c("sms" = "stop_id")) %>%
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
      arrange(departs_in, name, identifier, route_name) %>%
      select(due, route_name, destination, name, identifier, note) %>%
      rename(`Route` = route_name,
             `Destination` = destination,
             `Arrives in (minutes)` = due,
             `Notes` = note,
             `Direction / ID` = identifier,
             `Stop` = name)
  },
  options = list(pageLength = 8))

  ## Create live map ----------------------------------------------------------
  output$services_map <- renderLeaflet({
    selected_services <- stop_services %>%
      filter(name %in% input$selected_stops) %>%
      select(services) %>%
      pull()

    create_map(services = selected_services,
               shapefile = route_shapefile)

  })
}
