library("shinydashboard")
source("app.R")


header <- dashboardHeader(
  title = "Edinburgh bus times",
  titleWidth = "30%"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           h2("Live departures"),
           box(
             status = "success",
             width = NULL,
             uiOutput("departures_ui")
           ),
           h2("Available services"),
           box(
             status = "success",
             width = NULL,
             leafletOutput("services_map")
           )),
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
           )))
)


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
