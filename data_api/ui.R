# Ben Fasoli

# Header Layout ---------------------------------------------------------------
header <- dashboardHeader(title=div(a(img(src='utelogo.png', height=19), 'ATAQ Lab',
                                      href='http://air.utah.edu'),
                                    class='uataq-head'),
                          titleWidth='100%')

# Sidebar Layout --------------------------------------------------------------
sidebar <- dashboardSidebar(disable=T) 


# Body Layout -----------------------------------------------------------------
body <- dashboardBody(
  bsModal('key_window', 'Enter access key', 'open_changepass_window',
          HTML('<div class="input-group">',
               '<span class="input-group-addon"><i class="fa fa-key fa-fw"></i></span>',
               '<input class="form-control shiny-bound-input" id="userkey" type="text" placeholder="1a79a4d60de6718e8e5b326e338ae533">',
               '</div>'),
          bsAlert('key_alert')
  ),
  
  tags$head(includeCSS('www/styles.css')),
  uiOutput('dash')
)


# Page Creation ---------------------------------------------------------------
dashboardPage(title='UATAQ Data API', skin='black',
              header, sidebar, body)