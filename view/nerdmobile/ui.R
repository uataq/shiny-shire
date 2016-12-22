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
    menuItem('R Source Code', newtab=F, icon=icon('code'),
             href='https://github.com/benfasoli/shiny-shire/blob/master/view/nerdmobile/src.R')
  )
)

# ---------------------------------------------------------------------------------
# Main panel layout, corresponding to sidebar tabs
body <- dashboardBody(
  tabItems(
    tabItem('Nerdmobile',
            tags$head(includeCSS('styles.css')),
            uiOutput('process_ui')
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
