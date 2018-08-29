# Ben Fasoli
source('global.R')

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(a(img(src='utelogo.png', height=19), 'ATAQ Lab',
                                      href='http://air.utah.edu'),
                                    class='uataq-head'),
                          titleWidth='100%')

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(disable=T) 


# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  useShinyjs(),
  bsModal('key_window', 'Enter access token', 'open_key_window',
          HTML('<div class="input-group">',
               '<span class="input-group-addon"><i class="fa fa-key fa-fw"></i></span>',
               '<input class="form-control shiny-bound-input" id="token" type="text" placeholder="1a79a4d60de6718e8e5b326e338ae533">',
               '</div>')
  ),
  
  tags$head(includeCSS('www/styles.css')),
  uiOutput('dash')
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Data API', skin='black',
              header, sidebar, body)