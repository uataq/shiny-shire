# Ben Fasoli and James Mineau
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
        selectInput('instrument',
                    label = 'Instrument',
                    choices = '',
                    width = '100%'
        ),
        selectInput('lvl',
                    label = 'Diagnostic Level',
                    choices = '',
                    width = '100%'
        ),
        selectInput('column',
                    label = 'Data Options',
                    choices = '',
                    width = '100%'
        ),
        actionButton('submit',
                     label = 'Search',
                     icon = icon('search')
                     ),
        checkboxInput('remove_failed_qc',
                      label = 'Remove Failed QC',
                      value = F,
                      width = '100%'
                      ),
        radioButtons('color_by',
                      label = 'Color By',
                      choices = list('QAQC Flag' = 'QAQC_Flag', 
                                     'ID' = 'ID'),
                      selected = 'QAQC_Flag',
                      inline = T, width = '100%')
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
