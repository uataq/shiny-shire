# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Ben Fasoli

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'))

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('UATAQ Homepage', href='http://air.utah.edu/', newtab=F, icon=icon('home')),
    menuItem('TRAX Data', href='http://air.utah.edu/s/trax/', newtab=F, icon=icon('train')),
    div(align='center',
        selectInput('site', 'Site', 
                    c('', grep('trx', substr(dir('/home/benfasoli/github/lair-proc/run/'), 1, 3),
                               value=T, invert=T))),
        conditionalPanel('input.site != ""',
                         dateRangeInput('date_range', 'Date range',
                                        start=Sys.Date()-7, end=Sys.Date()),
                         selectInput('dataset', 'Data type', c('Calibrated dataset', 'Instrument diagnostics'))),
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
               dygraphOutput('ts', height=600),
               uiOutput('downsample_ui')
           )
    )
  )
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Data', skin='black',
              header, sidebar, body)