#!/usr/bin/env Rscript

library(leaflet)
library(raster)
library(shiny)

data <- readRDS('average_stacks.rds')
variables <- names(data)

ui <- fillPage(
  tags$head(tags$style('.checkbox {z-index: 999}')),
  absolutePanel(
    top = '5px',
    right = '5px',
    selectizeInput('variable',
                   label = NULL,
                   choices = variables,
                   selected = 'CO',
                   width = 200),
    checkboxInput('log10', 
                  label = 'log10',
                  value = F,
                  width = 200)
  ),
  leafletOutput('map', height = '100vh')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    req(input$variable)
    r <- data[[input$variable]]
    
    if (input$log10) {
      r[r == 0] <- NA
      r <- log10(r)
    }
    
    domain <- values(r)
    colors <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                           domain, na.color = 'transparent')
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addRasterImage(r, colors = colors, opacity = 0.4) %>%
      addLegend(position = 'bottomright',
                pal = colors,
                values = domain)
  })
}

shinyApp(ui = ui, server = server)
