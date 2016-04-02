# Ben Fasoli

source('global.R')

library(leaflet)
library(shiny)
library(shinydashboard)

# ---------------------------------------------------------------------------------
# Header Layout
# header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'))
header <- dashboardHeader(title='UATAQ Lab')


# ---------------------------------------------------------------------------------
# Sidebar Layout
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Real-time Observations', href='http://air.utah.edu/', 
             newtab=T, icon=icon('home'))),
  fluidRow(column(12,
                  selectInput('city', h5('Choose Hestia City'), c('Salt Lake City')),
                  checkboxGroupInput('sectors', h5('Choose sectors'), sector_opts, sector_opts),
                  sliderInput('hour_range', h5('By hour of day'), 0, 24, c(0, 24), 1, round=T),
                  sliderInput('day_range', h5('By day of year'), 1, 365, c(0, 2), 1, round=T),
                  p('Waiting times may be as long as a few minutes. It\'s a lot of data...'),
                  submitButton('Update map')
  ))
)

# ---------------------------------------------------------------------------------
# Main panel layout, corresponding to sidebar tabs
body <- dashboardBody(
  tags$head(tags$meta(name='apple-mobile-web-app-capable', content='yes'),
            tags$meta(name='apple-mobile-web-app-status-bar-style', content='black'),
            tags$meta(name='viewport', content='width=device-width, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no'),
            HTML('<link rel="apple-touch-icon" href="touch-icon-iphone.png" />'),
            HTML('<link rel="apple-touch-icon" sizes="76x76" href="touch-icon-ipad.png" />'),
            HTML('<link rel="apple-touch-icon" sizes="120x120" href="touch-icon-iphone-retina.png" />'),
            HTML('<link rel="apple-touch-icon" sizes="152x152" href="touch-icon-ipad-retina.png" />')),
  fluidRow(column(12, 
                  box(title='Hestia Emsisions', status='danger', width=NULL,
                      footer='Developed by Ben Fasoli and Daniel Mendoza at the University of Utah.',
                      leafletOutput('map', width='100%', height='600'))
  ))
)



# ---------------------------------------------------------------------------------
dashboardPage(title='Hestia', 
              skin='red',
              header,
              sidebar,
              body
)


# ui <- bootstrapPage(title='Hestia 2012 Annual',
#                     tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#                     leafletOutput('map', width='100%', height='100%')
# )