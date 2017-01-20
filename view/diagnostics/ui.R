# Ben Fasoli

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'),
                          titleWidth='100%')

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('UATAQ Site Map', href='http://air.utah.edu/',
             newtab=F, icon=icon('home')),
    menuItem('Simple Display', href='http://air.utah.edu/s/view/meas/',
             newtab=F, icon=icon('area-chart')),
    div(align='center',
        selectInput('site', 'Site', 
                    c('', grep('trx', substr(dir('/projects/data/'), 1, 3),
                               value=T, invert=T))),
        conditionalPanel('input.site != ""',
                         dateRangeInput('date_range', 'Date range',
                                        start=Sys.Date()-7, end=Sys.Date()+1),
                         selectInput('dataset', 'Data type', c('Instrument diagnostics',
                                                               'Calibrated dataset'))),
        uiOutput('data_options'),
        actionButton('apply', strong('Show me')),
        conditionalPanel('$("html").hasClass("shiny-busy")',
                         tags$div(style='font-size: 20px;',
                                  HTML('<i class="fa fa-refresh fa-spin"></i>')))
    )
  )
)

# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  tags$head(includeCSS('styles.css'),
            HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css">')),
  fluidRow(
    column(12,
           box(status='danger', width=NULL,
               conditionalPanel('input.site == ""', 'To get started, choose a location on the left.'),
               dygraphOutput('ts', height=500),
               uiOutput('downsample_ui')
           )
    )
  )
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Data', skin='black',
              header, sidebar, body)

