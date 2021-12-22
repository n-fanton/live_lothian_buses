library("shinydashboard")
source("app.R")


header <- dashboardHeader(
  title = "Edinburgh bus times",
  titleWidth = "25%"
)

body <- dashboardBody(
  fluidRow(
    column(width = 3,
           box(
             width = NULL,
             status = "info",
             selectizeInput(
               inputId = "selected_stops",
               label = "Select bus stops",
               choices = grouped_stops$name,
               multiple = TRUE,
               selected = "Royal Infirmary"),
             checkboxInput(
               inputId = "show_terminators",
               label = "Show services that terminate here",
               value = FALSE
               ),
             checkboxInput(
               inputId = "show_nightbus",
               label = "Show night services on map",
               value = FALSE
             ),
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
           box(
             width = NULL,
             status = "danger",
             p("This box will contain warnings downloaded from Lothian.")
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
