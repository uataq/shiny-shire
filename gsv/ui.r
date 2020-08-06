source('global.r')

ui <- function(req) {
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
                                'LGR MGGA' = 'lgr_mgga',
                                'Licor 7000' = 'licor_7000',
                                'Magee AE33' = 'magee_ae33',
                                'Picarro G2401' = 'picarro_g2401',
                                'Teledyne T200' = 'teledyne_t200',
                                'Teledyne T500u' = 'teledyne_t500u',
                                'Thermo PDR 1500' = 'thermo_pdr_1500',
                                'Vaisala WXT536' = 'vaisala_wxt536')),
        
        checkboxInput('hide_qc', 'Hide QC < 0', value = F),
        
        dateInput('date', label = 'Date',
                  value = '2019-06-03', min = '2019-05-01', max = '2020-03-15'),
        
        fluidRow(
          column(4, actionButton('left', '←', style = 'margin: 5px auto;')),
          column(4, actionButton('refresh', '↻', style = 'margin: 5px auto;')),
          column(4, actionButton('right', '→', style = 'margin: 5px auto;')),
          style = 'max-width:100%;'
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
}