# Ben Fasoli
source('global.r')

function(req) {
  
  header <- dashboardHeader(
    title = 'UATAQ'
  )
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem('Network map', icon = icon('map-marker'),
               href = 'https://air.utah.edu', newtab = F),
      div(
        align = 'center',
        dateRangeInput('dates',
                       label = 'Time Period',
                       start = Sys.Date() - 14,
                       end   = Sys.Date(),
                       max   = Sys.Date(),
                       width = '100%'
        ),
        selectInput('stid',
                    label = 'Site ID',
                    choices = c('', stids),
                    width = '100%'
        ),
        selectInput('column',
                    label = 'Data Options',
                    choices = '',
                    width = '100%'
        ),
        actionButton('submit',
                     label = 'Search',
                     icon = icon('search')),
        checkboxInput('include_atmos',
                      label = 'Include Atmosphere',
                      value = T,
                      width = '100%'),
        checkboxInput('include_failed_qc',
                      label = 'Include Failed QC',
                      value = F,
                      width = '100%')
      ),
      menuItem('Network status', icon = icon('tachometer-alt'),
               href = 'https://air.utah.edu/status.html', newtab = F),
      menuItem('Calibrated data', icon = icon('check'),
               href = 'https://air.utah.edu/s/measurements/', newtab = F)
    )
  )
  
  body <- dashboardBody(
    includeStyle <- tagList(
      includeHTML('_header.html'),
      tags$style('#plot {height: calc(100vh - 80px) !important;}
                  #submit {margin: 15px; width: 200px; border: none;}
                 .content-wrapper, .right-side {background-color: #fff;}
                 .main-svg {background-color: transparent !important;')
    ),
    shinyjs::useShinyjs(),
    withSpinner(
      plotlyOutput('plot'),
      color = '#BB261A',
      type = 8
    )
  )
  
  
  dashboardPage(title = 'UATAQ Diagnostics', skin = 'black',
                header, sidebar, body)
}

