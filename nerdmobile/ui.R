# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Nerdmobile processing app
# Ben Fasoli

library(dygraphs)
library(leaflet)
library(shinydashboard)

# ---------------------------------------------------------------------------------
# Header Layout
header <- dashboardHeader(title=div(img(src='utelogo.png', height=19), 'ATAQ Lab'))

# ---------------------------------------------------------------------------------
# Sidebar Layout
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('UATAQ Homepage', href='http://air.utah.edu/', newtab=F, icon=icon('home')),
    menuItem('Data Processing', tabName='Nerdmobile', icon=icon('car'), selected=T),
    menuItem('R Source Code', tabName='Code', icon=icon('code'))
  )
)

# ---------------------------------------------------------------------------------
# Main panel layout, corresponding to sidebar tabs
body <- dashboardBody(
  tabItems(
    tabItem('Nerdmobile',
            tags$head(includeCSS('styles.css')),
            uiOutput('process_ui')
    ),
    
    tabItem('Code', 
            fluidRow(
              column(12,
                     box(width=NULL, title='R Processing Secret Sauce', status='danger', solidHeader=T,
                         tags$pre(HTML(paste(system('cat src.R', intern=T), collapse='<br>'))))
              )
            )
    )
  )
)



# ---------------------------------------------------------------------------------
dashboardPage(title='UATAQ Nerdmobile', 
              skin='black',
              header,
              sidebar,
              body
)
