# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Required for air shiny app
# Ben Fasoli

library(leaflet)
library(shinydashboard)
library(shinythemes)


# ---------------------------------------------------------------------------------
# Header Layout
header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'))


# ---------------------------------------------------------------------------------
# Sidebar Layout
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Real-time Observations', href='http://air.utah.edu/', newtab=T, icon=icon('home')),
    menuItem('Measurement Methods', tabName='fasoli', icon=icon('flask'), selected=T),
    menuItem('Model Emissions', tabName='mendoza', icon=icon('cloud'))
  )
)

# ---------------------------------------------------------------------------------
# Main panel layout, corresponding to sidebar tabs
body <- dashboardBody(
  tabItems(
    tabItem('fasoli',
            tags$head(includeCSS('styles.css'),
                      tags$meta(name='apple-mobile-web-app-capable', content='yes'),
                      tags$meta(name='apple-mobile-web-app-status-bar-style', content='black'),
                      tags$meta(name='viewport', content='width=device-width, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no'),
                      HTML('<link rel="apple-touch-icon" href="touch-icon-iphone.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="76x76" href="touch-icon-ipad.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="120x120" href="touch-icon-iphone-retina.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="152x152" href="touch-icon-ipad-retina.png" />')),
            
            fluidRow(column(12, 
                            box(width=NULL, 
                                title='Mobile Platforms for Continuous Spatial Measurements of Urban Trace Gases and Criteria Pollutants',
                                status='danger',
                                includeMarkdown('fasoli/intro.md'), 
                                tags$a(href = 'fasoli_AGU_F2015.pdf', class='btn btn-default', 
                                       icon('picture-o'), 'See poster!')
                            ),
                            
                            tabBox(id='tracer', width=NULL, title='TRAX Spatio-Temporal Averages', side='right',
                                   tabPanel('CO2', leafletOutput('map1', height=400, width='100%')),
                                   tabPanel('CH4', leafletOutput('map2', height=400, width='100%')),
                                   tabPanel('O3', leafletOutput('map3', height=400, width='100%')),
                                   tabPanel('PM2.5', leafletOutput('map4', height=400, width='100%'))
                            ),
                            
                            tabBox(width=NULL, title='Nerdmobile Mobile Laboratory', side='right',
                                   tabPanel('Dashboard', includeMarkdown('fasoli/nerdmobile.md'),
                                            img(src = 'nerdmobile4.png', width='80%')),
                                   tabPanel('Hardware', includeMarkdown('fasoli/nerdmobile.md'),
                                            img(src = 'nerdmobile3.png', width='50%')),
                                   tabPanel('Mobile data', includeMarkdown('fasoli/nerdmobile.md'),
                                            img(src = 'nerdmobile2.png', width='80%'))
                            ),
                            
                            box(width=NULL, title='Project Info', status='danger', 
                                includeMarkdown('fasoli/abstract.md')#,
                                # img(src = 'LAIR_logo.png', height=35),
                                # img(src = 'GCSC_logo.jpg', height=50)
                            )
            ))
    ),
    
    
    tabItem('mendoza',
            fluidRow(column(12, 
                            box(width=NULL, 
                                title='A Spatial and Temporal Comparison of Highly-resolved Modeled Emissions of Carbon Dioxide, and Criteria Pollutants in Salt Lake City, Utah',
                                status='danger', 
                                includeMarkdown('mendoza/intro.md'), 
                                tags$a(href = 'Mendoza.AGU.Poster.2015.12.11.pdf', class='btn btn-default', 
                                       icon('picture-o'), 'See poster!')
                            ),
                            
                            box(width=NULL, title='Hestia 0.002deg Carbon Emissons', status='danger',
                                   leafletOutput('map_hestia', height=400, width='100%')
                            ),
                            
                            box(width=NULL, title='Project Info', status='danger', 
                                includeMarkdown('mendoza/abstract.md'),
                                img(src = 'LAIR_logo.png', height=35)
                            )
            ))
    )
  )
)



# ---------------------------------------------------------------------------------
dashboardPage(title='Utah AGU F2015', 
              skin='black',
              header,
              sidebar,
              body
)