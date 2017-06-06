# Ben Fasoli
setwd('/srv/shiny-server/map/mobile/')

navbarPage(
  tags$head(HTML('<meta http-equiv="refresh" content="0;url=http://air.utah.edu/index.html">'))
  # title=div(img(src='utelogo.png', height=18), 'ATAQ Lab'), 
  #          windowTitle='Air Quality & Trace Gas Lab',
  #          theme='bootstrap.css',
  #          collapsible=T,
  #          tabPanel(HTML('Map</a></li><li><a href=\"http://air.utah.edu/s/tech/\">Technical Utilities'), 
  #                   div(class='outer',
  #                       tags$head(includeCSS("styles.css"),
  #                                 tags$meta(name='apple-mobile-web-app-capable', content='yes'),
  #                                 tags$meta(name='apple-mobile-web-app-status-bar-style', content='black'),
  #                                 tags$meta(name='viewport', content='width=device-width, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no'),
  #                                 HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css">'),
  #                                 HTML('<link rel="apple-touch-icon" href="touch-icon-iphone.png" />'),
  #                                 HTML('<link rel="apple-touch-icon" sizes="76x76" href="touch-icon-ipad.png" />'),
  #                                 HTML('<link rel="apple-touch-icon" sizes="120x120" href="touch-icon-iphone-retina.png" />'),
  #                                 HTML('<link rel="apple-touch-icon" sizes="152x152" href="touch-icon-ipad-retina.png" />')),
  #                       leafletOutput('map', height='100%', width='100%'),
  #                       absolutePanel(top=15, right=10, width=200,
  #                                     selectInput('tracer', NULL,
  #                                                 c('Carbon Dioxide (CO2)',
  #                                                   'Methane (CH4)',
  #                                                   'Particulate Matter (PM2.5)',
  #                                                   'Ozone (O3)'))),
  #                       absolutePanel(top=60, right=10, width=300, align='right',
  #                                     textOutput('last_time'))
  #                   ))
)