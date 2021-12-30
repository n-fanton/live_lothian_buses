library("shinydashboard")
source("app.R")


header <- dashboardHeader(
  title = "Edinburgh bus times",
  titleWidth = "25%"
  # 25% corresponds to the 3/12 width of the settings column in app
)

sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Departure board",
             tabName = "departures",
             icon = icon("list",
                         lib = "glyphicon")),
    menuItem("Map explorer",
             icon = icon("map", lib = "font-awesome"),
             tabName = "explorer")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "departures",
      h2("Live departures board"),
      fluidRow(
        column(
          ## Sidebar column with settings and options -----------------------------
          width = 3,
          box(
            ### Box with settings for departures display --------------------------
            title = "Departure display",
            solidHeader = TRUE,
            collapsible = FALSE,
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
            title = "Map settings",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = NULL,
            status = "info",
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
            title = "Refresh data",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = NULL,
            status = "warning",
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
          box(
            ### Box displaying travel alerts read form Lothian Buses API ----------
            title = "Travel alerts",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = NULL,
            status = "danger",
            htmlOutput("alerts")
          ),
          box(
            ### Add various notes etc. to the app
            title = "Notes",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            status = "warning",
            width = NULL,
            p("When buses are stopped, their direction arrows will point up",
              "as the direction cannot be determined."),
            p("Timetable, location, stops, and routes data from Transport for",
              "Edinburgh. Travel alerts and route colours from Lothian Buses."),
            p("App author: Nick Fanton.")
            )
          ),
        column(
          ## Main part of the panel, displaying output ----------------------------
          width = 9,
          box(
            ### Table displaying next departures from selected stop(s) ------------
            title = "Live departures",
            solidHeader = TRUE,
            collapsible = FALSE,
            status = "success",
            width = NULL,
            uiOutput("departures_ui"),
            helpText("* Timetabled, but not live")
          ),
          box(
            ### Map output displaying services from the selected stop -------------
            title = "Services map",
            solidHeader = TRUE,
            collapsible = FALSE,
            status = "success",
            width = NULL,
            p("Displays services from selected bus stops. ",
              "Some services may not currently display on the map."),
            leafletOutput("services_map",
                          height = "800px")
          )
        )
      )
    ),
    tabItem(
      tabName = "explorer",
      p("Coming soon!")
      )
    )
  )


dashboardPage(
  header,
  sidebar,
  body
)
