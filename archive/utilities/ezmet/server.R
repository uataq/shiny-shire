# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Ben Fasoli 

source('src.R')

function(input, output, session) {
  output$download <- downloadHandler(
    filename = function() {
      'ezmet.zip'
    },
    content = function(file) {
      validate(need(!is.null(input$project) & nchar(input$project) > 0, 
                    'ERROR: Invalid project name. Try again.'))
      validate(need(!is.null(input$sensors) & length(input$sensors) > 0, 
                    'ERROR: No sensors chosen.'))
      ino = make_ino(input$sensors, as.numeric(input$interval) * 1000, input$project)
      setwd('/srv/shiny-server/ezmet/inos')
      zip(file, ino$name, flags='-r9X')
      setwd('/srv/shiny-server/ezmet/')
    }
  )
}
