# Ben Fasoli
library(leaflet)
library(shiny)

map <- readRDS('/srv/shiny-server/slcinterp/maps_krige.rds')

cols  <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'), 
                      domain=c(0, 1),
                      na.color='#00000000')


function(input, output, session) {
  output$map <- renderLeaflet({
    switch(input$sector,
           'hestia' = map$hestia,
           'krige'  = map$idw,
           'slv'    = map$slv)
  })
}