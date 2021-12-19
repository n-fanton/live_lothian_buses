# Data and helpers ------------------------------------------------------------
source("app.R")

# App user interface ----------------------------------------------------------

ui <- fluidPage(
  ## Main layout --------------------------------------------------------------
  navbarPage(
    "Live Transport for Edinburgh departures",
    ### Live departures (table) panel -----------------------------------------
    tabPanel(
      "Departures",
      #### Sidebar with app inputs --------------------------------------------
      sidebarLayout(
        ##### Select bus stop panel -------------------------------------------
        sidebarPanel(
          width = 3,
          selectizeInput(
            inputId = "selected_stops",
            label = "Select bus stops",
            choices = grouped_stops$name,
            multiple = TRUE,
            selected = "Royal Infirmary"
          ),
          ##### Toggle display of terminating services ------------------------
          checkboxInput(
            inputId = "show_terminators",
            label = "Show services that terminate here",
            value = FALSE
            ),
          ##### Action button to refresh inputs -------------------------------
          helpText(
            "Table will refresh when new stops are selected, or when the",
            "refresh button is clicked."
          ),
          textOutput("last_refreshed"),
          actionButton(
            inputId = "refresh",
            label = "Refresh"
          )
        ),
        #### Main panel showing app outputs -----------------------------------
        mainPanel(
          uiOutput("departures_ui"),
          p("Data: Transport for Edinburgh")
        )
      )
    ),
    ### Live locations (map) panel --------------------------------------------
    tabPanel(
      "Locations"
    )
  ),
)
