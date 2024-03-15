# Ben Fasoli
source('global.r')

function(req) {
  
  header <- dashboardHeader(
    title = div(a(img(src='https://air.utah.edu/img/utelogo.png', height=19),
                'ATAQ',
                href='http://air.utah.edu',
                style = "color: rgb(68, 68, 68);"))
  )
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
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
      )
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

