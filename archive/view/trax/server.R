# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Ben Fasoli 
source('modules.R')

# Server ----------------------------------------------------------------------
function(input, output, session) {
  # Import data
  md <- reactive({
    invalidateLater(300000, session)
    readRDS('/home/benfasoli/cron/air.utah.edu/_data/airmap.rds')$mobile
  })
  
  # Initialize display modules
  for (elem in opts$short)
    callModule(map, elem, data=md(), tab=input$tab)
}
