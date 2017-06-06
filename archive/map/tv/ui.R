# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Required for air tv shiny app
# Ben Fasoli

library(leaflet)

navbarPage(title=div(img(src='utelogo.png', height=18), 'Atmospheric Trace gas & Air Quality (UATAQ)'), 
           windowTitle='Air Quality & Trace Gas Lab',
           theme='bootstrap.css',
           id='tracer',
           tabPanel('Carbon Dioxide (CO2)', 
                    div(class='outer',
                        tags$head(includeCSS("styles.css"),
                                  HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css">'),
                                  HTML('<meta http-equiv="refresh" content="900">')),
                        leafletOutput('map1', height='100%', width='100%'),
                        absolutePanel(top=20, right=35, width=215,
                                      textOutput('last_time1')),
                        absolutePanel(bottom=20, left=35,
                                      actionButton('default1', 'All Sites', icon=icon('expand'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      actionButton('slc1', 'Salt Lake City', icon=icon('search'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      img(src='SCIF_logo.png', width=150, align='middle'))
                    )),
           tabPanel('Methane (CH4)', 
                    div(class='outer',
                        leafletOutput('map2', height='100%', width='100%'),
                        absolutePanel(top=20, right=35, width=215,
                                      textOutput('last_time2')),
                        absolutePanel(bottom=20, left=35,
                                      actionButton('default2', 'All Sites', icon=icon('expand'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      actionButton('slc2', 'Salt Lake City', icon=icon('search'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      img(src='SCIF_logo.png', width=150, align='middle'))
                    )),
           tabPanel('Particulate Matter (PM2.5)', 
                    div(class='outer',
                        leafletOutput('map3', height='100%', width='100%'),
                        absolutePanel(top=20, right=35, width=215,
                                      textOutput('last_time3')),
                        absolutePanel(bottom=20, left=35,
                                      actionButton('default3', 'All Sites', icon=icon('expand'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      actionButton('slc3', 'Salt Lake City', icon=icon('search'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      img(src='SCIF_logo.png', width=150, align='middle'))
                    )),
           tabPanel('Ozone (O3)', 
                    div(class='outer',
                        leafletOutput('map4', height='100%', width='100%'),
                        absolutePanel(top=20, right=35, width=215,
                                      textOutput('last_time4')),
                        absolutePanel(bottom=20, left=35,
                                      actionButton('default4', 'All Sites', icon=icon('expand'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      actionButton('slc4', 'Salt Lake City', icon=icon('search'), width=150,
                                                   class='btn action-button btn-large btn-primary'),
                                      br(),br(),
                                      img(src='SCIF_logo.png', width=150, align='middle'))
                    ))#,
           #            
           #            tags$script(HTML("var header = $('.navbar > .container');
           #                             header.append('<div style=float:right><h4>air.utah.edu</h4></div>');
           #                             console.log(header)"))
)