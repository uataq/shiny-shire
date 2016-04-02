# Ben Fasoli
library(leaflet)
library(shiny)

function(input, output, session) {
  output$map1 <- output$map2 <- output$map3 <- output$map4 <- renderLeaflet({
    path <- switch(input$tracer, 
                   'CO2'   = 'maps/leaf_CO2.rds',
                   'CH4'   = 'maps/leaf_CH4.rds',
                   'PM2.5' = 'maps/leaf_PM25.rds',
                   'O3'    = 'maps/leaf_O3.rds')
    readRDS(path)
    })
  
  output$map_hestia <- renderLeaflet({
    readRDS('maps/leaf_hestia.rds')
  })
}