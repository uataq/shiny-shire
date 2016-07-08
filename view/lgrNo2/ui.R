# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Ben Fasoli

lapply(c('dygraphs', 'leaflet', 'shinydashboard'), 
       library, character.only=T)

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(a(img(src='utelogo.png', height=19), 'ATAQ Lab',
                                      href='http://air.utah.edu'),
                                    class='uataq-head'),
                          titleWidth='100%')

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(id='tab',
              menuItem('Concentration_ppb', tabName='Concentration_ppb', selected=T),
              menuItem('Pressure_torr', tabName='Pressure_torr'),
              menuItem('Temperature_C', tabName='Temperature_C'),
              menuItem('Instrument_Temperature_C', tabName='Instrument_Temperature_C')))

# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  tags$head(includeCSS('styles.css')),
  fluidRow(
    column(12, 
           box(width=NULL, status='danger', title='Prototype LGR NO2 Mobile Testing',
               leafletOutput('leaf', height=500),
               hr(),
               dygraphOutput('ts')
           )
    )
  )
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ LGR NO2', skin='black',
              header, sidebar, body)