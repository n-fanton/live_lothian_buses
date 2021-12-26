library("shinydashboard")
source("app.R")


header <- dashboardHeader(
  title = "Edinburgh bus times",
  titleWidth = "25%"
  # 25% corresponds to the 3/12 width of the settings column in app
)

body <- dashboardBody(
  fluidRow(
    column(
      ## Sidebar column with settings and options -----------------------------
      width = 3,
      h2("Settings"),
      box(
        ### Box with settings for departures display --------------------------
        width = NULL,
        status = "info",
        selectizeInput(
          # Allows for multiple stops to be selected; defaults to RIE
          inputId = "selected_stops",
          label = "Select bus stops",
          choices = search_names,
          multiple = TRUE,
          selected = "Royal Infirmary"
          ),
        checkboxInput(
          # TfE API returns services that terminate, which may not be useful
          inputId = "show_terminators",
          label = "Show services that terminate here",
          value = FALSE
          )
        ),
      box(
        ### Box with settings for map display - doesn't change table ----------
        width = NULL,
        status = "info",
        p(strong("Map settings")),
        helpText(
          "If unselected, only services displayed in the live",
          "departures table will be shown"
          ),
        checkboxInput(
          # If selected, services that use this stop but aren't currently
          # on the departure board will also be shown (e.g. nightbus
          # services during the day).
          inputId = "show_all",
          label = "Show all services that use this stop",
          value = FALSE
          ),
        checkboxInput(
          # Display bus stops on map? Will display all stops used by the
          # displayed services, and give info on routes and destinations
          # available at the stop.
          inputId = "show_stops",
          label = "Show bus stops",
          value = FALSE
          ),
        checkboxInput(
          # Read realtime location data from TfE and display bus locations,
          # route, destination, speed, direction, and next stop
          inputId = "show_live_buses",
          label = "Show live bus locations",
          value = TRUE
          )
        ),
      box(
        ### Box for refreshing departures data and map ------------------------
        # Shiny doesn't continuously read new data, so after a while it needs
        # and update.
        # TODO: automatically refresh after 60 seconds
        width = NULL,
        status = "warning",
        p(strong("Refresh data")),
        helpText(
          "Table will refresh when new stops are selected, or when the",
          "refresh button is clicked."),
        actionButton(
          inputId = "refresh",
          label = "Refresh"
          ),
        textOutput("last_refreshed")
        ),
      ## Part 2 of the sidebar - travel alerts --------------------------------
      # Potentially move below map instead, this takes a lot of room
      # on mobile displays
      h2("Travel alerts"),
      box(
        ### Box displaying travel alerts read form Lothian Buses API ----------
        width = NULL,
        status = "danger",
        htmlOutput("alerts")
      )),
    column(
      ## Main part of the panel, displaying output ----------------------------
      width = 9,
      h2("Live departures"),
      box(
        ### Table displaying next departures from selected stop(s) ------------
        status = "success",
        width = NULL,
        uiOutput("departures_ui")
      ),
      h2("Services at selected stop"),
      p("Some services may not currently display on the map."),
      box(
        ### Map output displaying services from the selected stop -------------
        status = "success",
        width = NULL,
        leafletOutput("services_map",
                      height = "800px")
        ),
      h2("Notes"),
      box(
        ### Add various notes etc. to the app
        status = "info",
        width = NULL,
        p("When buses are stopped, their direction arrows will point up",
          "as the direction cannot be determined."),
        p("Timetable, location, stops, and routes data from Transport for",
          "Edinburgh. Travel alerts and route colours from Lothian Buses."),
        p("App author: Nick Fanton.")
        )
      )
    )
  )


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
