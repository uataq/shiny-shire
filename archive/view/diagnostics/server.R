# Ben Fasoli

max_obs <- 100000
path <- '/uufs/chpc.utah.edu/common/home/lin-group2/measurements/data'

function(input, output, session) {
  r <- reactiveValues(data = NULL,
                      refresh = 0,
                      site = NULL)
  
  # URL Bookmarking ----------------------------------------------------------------------
  # observe({
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(function(url) {
  #   updateQueryString(url)
  # })
  
  # Column choosing UI -------------------------------------------------------------------
  output$column_ui <- renderUI({
    req(input$site)
    path <- file.path('/projects/data', input$site)
    inst <- dir(path, pattern = 'licor-6262|lgr-ugga')[1]
    opts <- switch(inst,
                   'licor-6262' = c('rawCO2', 'batt_volt', 'PTemp', 'Room_T', 'IRGA_T',
                                    'IRGA_P', 'MF_Controller_mLmin', 'PressureVolt',
                                    'rhVolt', 'gas_T', 'rawCO2_Voltage', 'rawH2O',
                                    'Program'),
                   'lgr-ugga' = c('CO2d_ppm', 'CO2d_ppm_sd', 'CH4_ppm', 'CH4_ppm_sd',
                                  'H2O_ppm', 'H2O_ppm_sd', 'CO2_ppm', 'CO2_ppm_sd',
                                  'CH4d_ppm', 'CH4d_ppm_sd', 'GasP_torr', 'GasP_torr_sd',
                                  'GasT_C', 'GasT_C_sd', 'AmbT_C', 'AmbT_C_sd'))
    selectInput('column', 'Display', opts)
  })
  
  
  # Read data ----------------------------------------------------------------------------
  observeEvent(input$apply, {
    if (is.null(input$site) | nchar(input$site) < 2) {
      showNotification('Choose site...', type = 'message', duration = 3,
                       id = 'msg', closeButton = F)
      return()
    }
    
    showNotification('Generating timeseries...', type = 'message', duration = 3,
                     id = 'msg', closeButton = F)
    
    path <- file.path('/projects/data', input$site)
    inst <- dir(path, pattern = 'licor-6262|lgr-ugga')[1]
    path <- file.path(path, inst, 'parsed', '%Y_%m_parsed.dat')
    col_types <- switch(inst,
                        'licor-6262' = 'T______ddddddddddddc_ddc',
                        'lgr-ugga' = 'Tdddddddddddddddddddddccdd')
    
    time_range <- c(as.POSIXct(input$date_range[1], tz = 'UTC'),
                    as.POSIXct(paste(input$date_range[2], '23:59:59'), tz = 'UTC'))
    
    files <- format(seq(time_range[1], time_range[2], by = 'day'), tz='UTC', format = path)
    files <- intersect(files, dir(dirname(path), full.names = T))
    
    if (length(files) == 0) {
      showNotification('No files found. Try a different time range', duration = 3,
                       id = 'msg', type = 'error', closeButton = F)
      return()
    }
    str(files)
    dat <- files %>%
      lapply(read_csv, col_types = col_types, locale = locale(tz = 'UTC')) %>%
      bind_rows() %>%
      filter(Time_UTC >= time_range[1], Time_UTC <= time_range[2],
             ID_co2 != -99)
    if (nrow(dat) < 1) {
      showNotification('No data found. Try a different time range', duration = 3,
                       id = 'msg', type = 'error', closeButton = F)
      return()
    }
    
    if (nrow(dat) > max_obs) {
      showNotification(paste('Atmospheric data is randomly sampled down to',
                             prettyNum(max_obs, big.mark = ',', scientific = F),
                             'points from', prettyNum(nrow(dat), big.mark = ',', scientific = F),
                             'observations. To see the highest frequency data, try a smaller',
                             'date range.'),
                       duration = 5, id = 'msg', type = 'warning', closeButton = T)
      dat <- filter(dat, ID_co2 != -10) %>%
        bind_rows(filter(dat, ID_co2 == -10) %>%
                    sample_n(max_obs, replace=T) ) %>%
        arrange(Time_UTC)
    }
    
    if (!input$show_atmos) {
      mask <- grepl('-10', dat$ID, fixed = T)
      dat <- dat[!mask, ]
    }

    r$data <- dat %>%
      (function(df){
        out <- data_frame(Time_UTC = df$Time_UTC)
        for (col in sort(unique(df$ID))) {
          col_make <- paste0('~', col)
          out[col_make] <- df[input$column]
          out[df$ID != col, col_make] <- NA
        }
        return(out)
      })
  })
  
  
  # Produce figure ------------------------------------------------------------
  output$ts <- renderDygraph({
    req(r$data)
    
    isolate({
      xts(select(r$data, -Time_UTC), r$data$Time_UTC, tzone = 'UTC') %>%
        dygraph(xlab = 'UTC Time') %>%
        dyOptions(colors = c('#D2352C', '#1A2226','#00ADB5', '#DD6E42', '#4F6D7A',
                             RColorBrewer::brewer.pal(7, 'Set2'))) %>%
        dyOptions(drawPoints = T, pointSize = 2, fillGraph = T, fillAlpha = 0.25,
                  drawGrid = F, useDataTimezone = T, rightGap = 0)
    })
  })
}
