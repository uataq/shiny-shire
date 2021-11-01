# Ben Fasoli
source('global.R')

# Header Layout ----------------------------------------------------------------
header <- dashboardHeader(
  title = HTML(
    '<div>',
    '<img src="http://air.utah.edu/~benfasoli/img/utelogo.png" style="height: 16px; position: relative; bottom:2px;">',
    'Atmospheric Trace gas & Air Quality Lab',
    '</div>'),
  titleWidth = '100%'
)

# Sidebar Layout ---------------------------------------------------------------
sidebar <- dashboardSidebar(disable = T)

# Body Layout ------------------------------------------------------------------
body <- dashboardBody(
  tags$head(includeCSS('styles.css'),
            HTML('<meta http-equiv="refresh" content="7200">')),
  
  valueBoxOutput('value_co2_ppm', width = 2),
  valueBoxOutput('value_ch4_ppb', width = 2),
  valueBoxOutput('value_temp_f', width = 2),
  valueBoxOutput('value_wspd_ms', width = 2),
  valueBoxOutput('value_rh_pct', width = 2),
  valueBoxOutput('value_wchil_c', width = 2),
  
  column(12,
  fluidRow(
    column(6, tags$video(width = '100%', height = 515, autoplay = NA, controls = NA,
               loop = NA, src="http://benfasoli.com/files/snowbird.mp4")),
    column(6, leafletOutput('map', width = '100%', height = 515))
  )
  ),
  # column(6, 
  # leafletOutput('map', width = '100%', height = 500),
  # box(width = NULL, title = HTML('Current Carbon Dioxide (ppm CO<sub>2</sub>)'),
  #     leafletOutput('map')),
  box(width = 12, title = HTML('Carbon Dioxide Concentrations (ppm CO<sub>2</sub>)'),
      status = 'success', plotlyOutput(height = 280, 'timeseries')),
  # ),
  
  
  div(class = 'moreinfo',
      HTML('<i class="fa fa-question-circle"></i> For more information,',
           'visit <b>air.utah.edu</b>'))
)

# Generate Page ----------------------------------------------------------------
dashboardPage(title='UATAQ HDP Dashboard', skin='black',
              header, sidebar, body)