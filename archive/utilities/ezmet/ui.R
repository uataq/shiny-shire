# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Ben Fasoli
library(shinydashboard)

# ---------------------------------------------------------------------------------
# Header Layout
header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'))

# ---------------------------------------------------------------------------------
# Sidebar Layout
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('UATAQ Homepage', href='http://air.utah.edu/', newtab=F, icon=icon('home')),
    menuItem('Overview', tabName='Overview', icon=icon('question-circle'), selected=T),
    menuItem('Setup', tabName='Setup', icon=icon('terminal'))
  )
)

# ---------------------------------------------------------------------------------
# Main panel layout, corresponding to sidebar tabs
body <- dashboardBody(
  tabItems(
    tabItem('Overview',
            tags$head(includeCSS('styles.css'),
                      tags$meta(name='apple-mobile-web-app-capable', content='yes'),
                      tags$meta(name='apple-mobile-web-app-status-bar-style', content='black'),
                      tags$meta(name='viewport', content='width=device-width, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no'),
                      HTML('<link rel="apple-touch-icon" href="touch-icon-iphone.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="76x76" href="touch-icon-ipad.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="120x120" href="touch-icon-iphone-retina.png" />'),
                      HTML('<link rel="apple-touch-icon" sizes="152x152" href="touch-icon-ipad-retina.png" />')),
            fluidRow(
              column(12,
                     box(width=NULL, 
                         title='EZMet Overview',
                         status='danger', 
                         includeMarkdown('text/overview.md'),
                         footer='Designed by Ben Fasoli'
                     )
              )
            )
    ),
    
    tabItem('Setup',
            fluidRow(
              column(12,
                     box(width=NULL,
                         title='Setup',
                         status='danger',
                         textInput('project', h5('Project name')),
                         sliderInput('interval', h5('Sampling interval (s)'), 
                                     min=0.1, max=60, step=0.1, value=1),
                         checkboxGroupInput('sensors', h5('Choose desired sensors'),
                                            choices=dir('sensors')),
                         hr(),
#                          checkboxInput('add_analog', 'Custom analog sensors'),
#                          conditionalPanel('input.add_analog == true',
#                                           p('analog')),
                         downloadButton('download', 'Download program!')
                     )
              )
            )
    )
  )
)



# ---------------------------------------------------------------------------------
dashboardPage(title='UATAQ EZMet', 
              skin='black',
              header,
              sidebar,
              body
)
