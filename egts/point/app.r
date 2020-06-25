#!/usr/bin/env Rscript

library(googlesheets)
library(leaflet)
library(shiny)

data <- readRDS('geolocated.rds')
variables <- unique(data$variable)

# https://docs.google.com/spreadsheets/d/1MXjqItM9N_yuRIK7yZdoW65onNZlYP9W8WrEe5JjuxU/edit#gid=0
google_sheet <- gs_key('1MXjqItM9N_yuRIK7yZdoW65onNZlYP9W8WrEe5JjuxU', verbose = F)

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
    
    pts <- subset(data, variable == input$variable)
    
    meta <- gs_read(google_sheet, verbose = F)
    pts <- dplyr::left_join(pts, meta, by = 'idx')
    
    pts$name[is.na(pts$name)] <- 'Unknown'
    pts$is_valid[is.na(pts$is_valid)] <- 'Unknown'
    
    popup <- glue::glue('<b>{pts$name}</b><br>',
                        'Valid: {pts$is_valid}<br>',
                        'Index: {pts$idx}<br>',
                        'Value: <b>{pts$value}</b>')
    
    if (input$log10) {
      pts$value[pts$value == 0] <- NA
      pts$value <- log10(pts$value)
    }
    
    domain <- pts$value
    colors <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                           domain, na.color = 'transparent')
    
    color <- colors(pts$value)
    color[pts$is_valid == 1] <- '#31E981'  # green
    color[pts$is_valid == 0] <- '#EF2917'  # red
    color[pts$is_valid == -1] <- '#000000'  # black
    color[is.na(pts$is_valid)] <- '#ffffff'
    
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addCircleMarkers(lng = pts$longitude, 
                 lat = pts$latitude,
                 color = color,
                 fillColor = colors(pts$value),
                 radius = 6,
                 weight = 3,
                 opacity = 1,
                 fillOpacity = 0.5,
                 popup = popup) %>%
      addLegend(position = 'bottomright',
                pal = colors,
                values = domain)
  })
}

shinyApp(ui = ui, server = server)


# input <- list(variable = 'CO', log10 = T)
