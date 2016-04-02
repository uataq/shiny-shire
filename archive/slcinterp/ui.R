# Ben Fasoli
library(leaflet)
library(shiny)

ui <- bootstrapPage(title='SLC CO2 Interpolation - Beta',
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                    leafletOutput('map', width='100%', height='100%'),
                    absolutePanel(top=20, right=35, width=215,
                                  selectInput('sector', h5('Choose display'),
                                              c('hestia', 'krige', 'slv')))
)