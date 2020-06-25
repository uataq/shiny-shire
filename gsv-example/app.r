#!/usr/bin/env Rscript

library(leaflet)
library(raster)
library(shiny)

hestia <- readRDS('data/hestia.rds')
convolved <- readRDS('data/convolved_sectors.rds')
footprint <- readRDS('data/footprint.rds')

data <- extend(convolved, footprint)
names(data) <- c('Airport',
                 'Commercial_nonpoint',
                 'Commercial_point',
                 'Electricity_production',
                 'Industrial',
                 'Non_road',
                 'On_road',
                 'Railroad',
                 'Residential',
                 'Total')

data$Footprint <- footprint
data$Hestia <- hestia$TOTAL

variables <- names(data)

ui <- fillPage(
  tags$head(tags$style('.checkbox {z-index: 999}')),
  absolutePanel(
    top = '5px',
    right = '5px',
    selectizeInput('variable',
                   label = NULL,
                   choices = variables,
                   selected = 'Total',
                   width = 200),
    checkboxInput('log10',
                  label = 'log10',
                  value = T,
                  width = 200)
  ),
  leafletOutput('map', height = '100vh')
)

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
                           range(domain, na.rm = T), na.color = 'transparent')
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addRasterImage(r, colors = colors, opacity = 0.4) %>%
      addLegend(position = 'bottomright',
                pal = colors,
                values = domain)
  })
  
}

shinyApp(ui = ui, server = server)
