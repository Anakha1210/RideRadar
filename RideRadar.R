library(shiny)
library(leaflet)
library(osrm)
library(sf)
library(dplyr)

# Define predefined locations in Bangalore
waypoints <- data.frame(
  id = c("Majestic", "MG Road", "Indiranagar", "Koramangala", "Electronic City"),
  lat = c(12.9784, 12.9740, 12.9790, 12.9352, 12.8392),
  lon = c(77.5720, 77.6060, 77.6400, 77.6220, 77.6784)
)

# Get full route using OSRM
route <- osrmRoute(
  src = waypoints[1, c("lon", "lat")], 
  dst = waypoints[nrow(waypoints), c("lon", "lat")], 
  overview = "full"
)

# Convert route to sf object
route_sf <- st_as_sf(route, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Extract route coordinates
route_coords <- as.data.frame(st_coordinates(route_sf))
colnames(route_coords) <- c("lon", "lat")

# user interface
ui <- fluidPage(
  titlePanel("Real-Time Bus Tracking - Bangalore"),
  leafletOutput("map"),
  tags$script(HTML("setInterval(function(){Shiny.setInputValue('update', Math.random())}, 3000);"))
)

server <- function(input, output, session) {
  bus_index <- reactiveVal(1)  # Start 
  #bus movement logic
  observeEvent(input$update, {
    new_index <- bus_index() + 1
    if (new_index > nrow(route_coords)) new_index <- 1  #  back to start
    bus_index(new_index)
  })
  #initial map reading
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = route_sf, color = "blue", weight = 5, opacity = 0.8)
  })
  #map updating
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(
        lng = route_coords$lon[bus_index()], 
        lat = route_coords$lat[bus_index()], 
        popup = "Bus Location", 
        icon = makeIcon("", "", 24, 24)
      )
  })
}

shinyApp(ui, server)





