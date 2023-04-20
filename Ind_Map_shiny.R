# Load required packages
library(shiny)
library(sf)
library(sp)
library(rgdal)
library(tidyverse)
library(ggplot2)

# Read district shapefile of India
ind_shape <- st_read("C:/Shape files 2020/state.shp")

# Define UI for app
ui <- fluidPage(
  titlePanel("Vaccination Coverage in India"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", 
                  label = "Select variable:",
                  choices = c("BCG", "ROTA","Penta","HepB","MCV1","MCV2"),
                  selected = "BCG")
    ),
    mainPanel(
      plotOutput("ind_map")
    )
  )
)

# Define server logic for app
server <- function(input, output) {
  
  # Generate some random data points
  set.seed(123)
  ind_data <- data.frame(
    OBJECTID = ind_shape$OBJECTID,
    STATE = ind_shape$STATE,
    BCG = runif(length(ind_shape$OBJECTID), 0, 100),
    ROTA = runif(length(ind_shape$OBJECTID), 0, 100),
    Penta = runif(length(ind_shape$OBJECTID), 0, 100),
    HepB = runif(length(ind_shape$OBJECTID), 0, 100),
    MCV1 = runif(length(ind_shape$OBJECTID), 0, 100),
    MCV2 = runif(length(ind_shape$OBJECTID), 0, 100)
  )
  
  # Merge data with shapefile
  ind_map_data <- merge(ind_shape, ind_data, by.x = "OBJECTID", by.y = "OBJECTID")
  
  # Render map
  output$ind_map <- renderPlot({
    ggplot(ind_map_data, aes(fill = !!sym(input$var))) +
      geom_sf(color = "black") +
      scale_fill_viridis_b() +
      labs(title = "Coverage of Vaccination in India",
           subtitle = paste0(input$var, " Coverage"),
           caption = "Source: HMIS - Data upto Feb-2023") +
      theme(title = element_text(face = "bold"),
            legend.position = "left") +
      theme_void()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
