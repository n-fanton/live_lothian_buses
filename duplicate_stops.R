ui <- fluidPage(
  # App title ----
  titlePanel("Station locations?"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectizeInput("selected_stops",
                  "Bus stop name",
                  choices = all_ids,
                  selected = "36236926",
                  multiple = T)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      leafletOutput(outputId = "stops_map")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$stops_map <- renderLeaflet({
    display_stops <- all_stops %>%
      filter(stop_id %in% input$selected_stops)

    leaflet(data = display_stops) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude,
                 label = ~paste0(name, " ", stop_id))
  })


}

shinyApp(ui, server)
