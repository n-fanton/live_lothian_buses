library("shinydashboard")
source("app.R")


header <- dashboardHeader(
  title = "Edinburgh bus times",
  titleWidth = "25%"
)

body <- dashboardBody(
  fluidRow(
    column(#style = "position: fixed; overflow: visible;",
           width = 3,
           h2("Settings"),
           box(
             width = NULL,
             status = "info",
             selectizeInput(
               inputId = "selected_stops",
               label = "Select bus stops",
               choices = search_names,
               multiple = TRUE,
               selected = "Royal Infirmary"),
             checkboxInput(
               inputId = "show_terminators",
               label = "Show services that terminate here",
               value = FALSE
               )),
           box(
             width = NULL,
             status = "info",
             p(strong("Map settings")),
             checkboxInput(
               inputId = "show_all",
               label = "Show all services that use this stop",
               value = FALSE
             ),
             helpText(
               "If unselected, only services displayed in the live",
               "departures table will be shown"),
             checkboxInput(
               inputId = "show_stops",
               label = "Show bus stops on map",
               value = TRUE
             )),
           box(
             width = NULL,
             status = "warning",
             helpText(
               "Table will refresh when new stops are selected, or when the",
               "refresh button is clicked."),
             textOutput("last_refreshed"),
             actionButton(
               inputId = "refresh",
               label = "Refresh")),
           h2("Travel alerts"),
           box(
             width = NULL,
             status = "danger",
             htmlOutput("alerts")
           )),
    column(width = 9,
           h2("Live departures"),
           box(
             status = "success",
             width = NULL,
             uiOutput("departures_ui")
           ),
           h2("Available services"),
           p("Tram services are currently not displayed on the map."),
           box(
             status = "success",
             width = NULL,
             leafletOutput("services_map",
                           height = "800px")
           )))
)


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
