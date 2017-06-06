# Ben Fasoli
source('global.r')

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(
  title = HTML(
    '<div>',
    '<img src="http://air.utah.edu/~benfasoli/img/utelogo.png" style="height: 16px; position: relative; bottom:2px;">',
    'Atmospheric Trace gas & Air Quality Lab',
    '</div>'),
  titleWidth = '100%'
)

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('UATAQ Site Map', icon = icon('globe'),
             href = 'https://air.utah.edu', newtab = F),
    menuItem('Download Data', icon = icon('download'),
             href = 'https://air.utah.edu/s/data/', newtab = F)),
  div(
    align = 'center',
    dateRangeInput('date_range', 'Time Period',
                   start = Sys.Date() - 5,
                   end   = Sys.Date(),
                   max   = Sys.Date(),
                   width = '100%'),
    selectInput('site_id', 'Site ID',
                c('',
                  'csp - Castle Peak' = 'csp',
                  'dbk - Daybreak' = 'dbk',
                  'fru - Fruitland' = 'fru',
                  'hdp - Hidden Peak' = 'hdp',
                  'heb - Heber' = 'heb',
                  'hpl - Horsepool' = 'hpl',
                  'imc - Murray' = 'imc',
                  'lgn - Logan' = 'lgn',
                  'roo - Roosevelt' = 'roo',
                  'rpk - Rose Park' = 'rpk',
                  'sug - Sugarhouse' = 'sug',
                  'sun - Suncrest' = 'sun',
                  'wbb - University of Utah' = 'wbb'),
                width = '100%'),
    selectInput('quality', 'Dataset',
                c('Calibrated Measurements' = 'calibrated',
                  'Instrument Diagnostics' = 'parsed'),
                width = '100%')
  ),
  
  sidebarMenu(
    menuItem('Site Diagnostics', icon = icon('wrench'),
             href = 'https://air.utah.edu/s/view/diagnostics/', newtab = F),
    menuItem('About UATAQ', icon = icon('question'),
             href = 'https://air.utah.edu/about.html', newtab = F)
  )
)


# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  # Additional html/css dependencies -------------------------------------------
  useShinyjs(),
  includeStyle <- tagList(
    includeHTML('_header.html'),
    includeCSS('styles.css'),
    tags$head(HTML('<meta http-equiv="refresh" content="18000">'))
  ),
  
  uiOutput('dynamic_plot')
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Data', skin='black',
              header, sidebar, body)

