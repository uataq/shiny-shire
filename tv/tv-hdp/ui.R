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
  
  valueBoxOutput('value_temp_c', width = 3),
  valueBoxOutput('value_wspd_ms', width = 3),
  valueBoxOutput('value_rh_pct', width = 3),
  valueBoxOutput('value_wchil_c', width = 3),
  
  fluidRow(
    box(width = 5,
        leafletOutput('map')
    ),
    box(width = 7,
        plotlyOutput('timeseries')
    )
  ),
  div(class = 'moreinfo',
      HTML('<i class="fa fa-question-circle"></i> For more information,',
           'visit <b>air.utah.edu</b>'))
)

# Generate Page ----------------------------------------------------------------
dashboardPage(title='UATAQ HDP Dashboard', skin='black',
              header, sidebar, body)