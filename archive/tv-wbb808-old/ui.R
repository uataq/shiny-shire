# Ben Fasoli
source('global.R')

# Header Layout ----------------------------------------------------------------
header <- dashboardHeader(
  title=div(img(src='utelogo.png', height=19),
            'Atmospheric Trace gas & Air Quality'),
  titleWidth='100%')

# Sidebar Layout ---------------------------------------------------------------
sidebar <- dashboardSidebar(disable=T)

# Body Layout ------------------------------------------------------------------
body <- dashboardBody(
  tags$head(includeCSS('styles.css'),
            HTML('<meta http-equiv="refresh" content="3600">')),
  
  fluidRow(
    column(3, valueBoxOutput('wbb_co2', NULL)),
    column(3, valueBoxOutput('wbb_ch4', NULL)),
    column(3, valueBoxOutput('wbb_o3', NULL)),
    column(3, valueBoxOutput('wbb_pm25', NULL))),

  fluidRow(
    column(8, 
           do.call(function(...) {tabBox(..., id='tab', width='100%')},
                   apply(opts, 1,
                         function(x)
                           tabPanel(x[1], mod_map_ui(x[2]), value=x[2])))
    ),
    column(4,
           box(title=HTML('UATAQ Lab Trends'), width=NULL, status='danger',
               # plotOutput('ts_co2', height=210),
               # plotOutput('ts_ch4', height=210)
               # img(src='http://air.utah.edu/~benfasoli/output/updating/wbb_co2.png', width='100%'),
               # img(src='http://air.utah.edu/~benfasoli/output/updating/wbb_ch4.png', width='100%')
               img(src='http://air.utah.edu/~benfasoli/output/updating/wbb_all.png', width='100%')
           )#,
           
           #        box(title='Reserving for library use', width=NULL, status='danger',
           #            div(height=300)
           #        )
    )
  ),
  div(style='text-align: center;',
      HTML('<i class="fa fa-question-circle"></i> For more information,',
           'visit air.utah.edu'))
)

# Generate Page ----------------------------------------------------------------
dashboardPage(title='UATAQ Dashboard', skin='black',
              header, sidebar, body)