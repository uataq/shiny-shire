# Ben Fasoli
source('global.R')

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'),
                          titleWidth='100%')

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(disable=T)

# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  tags$head(includeCSS('styles.css'),
            HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css">')),
  fluidRow(
    column(12,
           box(status='danger', width=NULL,
               uiOutput('display') 
           )
    )
  )
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Data', skin='black',
              header, sidebar, body)