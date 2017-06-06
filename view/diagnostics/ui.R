# Ben Fasoli

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(
  title = HTML(
    '<div class="blklink">',
    '<a href="https://air.utah.edu">',
    '<img src="http://air.utah.edu/~benfasoli/img/utelogo.png" style="height: 16px; position: relative; bottom:2px;">',
    'Atmospheric Trace gas & Air Quality Lab',
    '</a>',
    '</div>'),
  titleWidth = '100%'
)

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('UATAQ Home', href='https://air.utah.edu/',
             newtab=F, icon=icon('home')),
    menuItem('Calibrated Data', href='http://air.utah.edu/s/view/meas/',
             newtab=F, icon=icon('area-chart')),
    shiny::br(),
    div(align='center',
        selectInput('site', 'Site', 
                    c('', grep('trx', substr(dir('/projects/data/'), 1, 3),
                               value=T, invert=T))),
        dateRangeInput('date_range', 'Date range',
                       start=Sys.Date()-7, end=Sys.Date()+1),
        uiOutput('column_ui'),
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
               dygraphOutput('ts', height=500)
           )
    )
  )
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Diagnostics', skin='black',
              header, sidebar, body)
