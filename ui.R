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
          width = 2,
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
          width = 10,
          uiOutput("departures_ui"),
          p("Data: Transport for Edinburgh")
        )
      )
    ),
    ### Live locations (map) panel --------------------------------------------
    tabPanel(
      "Locations",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          pickerInput(
            inputId = "selected_map_routes",
            label = "Select routes to display",
            choices = routes$route_display,
            selected = routes$route_display,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          actionButton(
            inputId = "refresh_map",
            label = "Refresh"
          )
        ),
        mainPanel(
          width = 8,
          leafletOutput("bus_locations",
                        width = "100%",
                        height = 600)
        )
      )
    )
  ),
)
