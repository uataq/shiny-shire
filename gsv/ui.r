library(plotly)
library(rclipboard)

library(shiny)
library(shinycssloaders)
library(shinydashboard)

header <- dashboardHeader(
  title = tags$span(
    HTML('<img src="https://air.utah.edu/~benfasoli/img/utelogo.png" 
              style="height: 20px; position: relative; bottom: 1px;">
              ATAQ Lab')
  )
)

sidebar <- dashboardSidebar(
  div(align = 'center',
      selectInput('system_id', label = 'Vehicle ID', 
                  choices = c('Lone 10416' = 'lone',
                              'Star 10419' = 'star')),
      
      selectInput('device_id', label = 'Device ID', 
                  choices = c('GPS' = 'gps',
                              # 'LGR MGGA' = 'lgr_mgga',
                              'Licor 7000' = 'licor_7000',
                              'Magee AE33' = 'magee_ae33',
                              'Teledyne T200' = 'teledyne_t200',
                              'Teledyne T500u' = 'teledyne_t500u',
                              'Thermo PDR 1500' = 'thermo_pdr_1500')),
      
      checkboxInput('hide_qc', 'Hide QC < 0', value = F),
      
      dateInput('date', label = 'Date',
                value = '2019-06-03', min = '2019-05-01', max = '2020-04-01'),
      
      fluidRow(
        column(6, actionButton('left', '←')),
        column(6, actionButton('right', '→'))
      )

  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML('
      .main-svg {
        background-color: transparent !important;
      }
      
      .content-wrapper {
        background-color: #fff;
      }
      
      #shiny-notification-panel {
        width: 450px;
      }
      
      #timeseries {
        height: calc(100vh - 80px) !important;
        min-height: 600px;
      }
    '))
  ),
  
  rclipboardSetup(),
  
  withSpinner(
    plotlyOutput('timeseries'),
    color = '#BB261A',
    type = 8
  )
)

dashboardPage(header, sidebar, body, title = 'GSV', skin = 'black')
