# Ben Fasoli
source('global.r')

function(req) {
  
  header <- dashboardHeader(
    title = tags$span(
      HTML('<img src="https://air.utah.edu/~benfasoli/img/utelogo.png" 
              style="height: 20px; position: relative; bottom: 1px;">
              ATAQ Lab')
    )
  )
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem('Network map', icon = icon('map-marked-alt'),
               href = 'https://air.utah.edu', newtab = F),
      div(
        align = 'center',
        dateRangeInput('dates',
                       label = 'Time Period',
                       start = Sys.Date() - 7,
                       end   = Sys.Date(),
                       max   = Sys.Date(),
                       width = '100%'
        ),
        selectInput('stid',
                    label = 'Site ID',
                    choices = c('', stids),
                    width = '100%'
        ),
        actionButton('submit',
                     label = 'Search',
                     icon = icon('search'))
      ),
      menuItem('Network status', icon = icon('tachometer-alt'),
               href = 'https://air.utah.edu/status.html', newtab = F),
      menuItem('Diagnostics', icon = icon('wrench'),
               href = 'https://air.utah.edu/s/diagnostics/', newtab = F)
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
    
    withSpinner(
      plotlyOutput('plot'),
      color = '#BB261A',
      type = 8
    )
  )
  
  
  dashboardPage(title = 'UATAQ Measurements', skin = 'black',
                header, sidebar, body)
}

