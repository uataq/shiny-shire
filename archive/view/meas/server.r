# Ben Fasoli
source('global.r')

make_subplot <- function(df) {
  # Requires POSIXct column "Time_UTC" and columns for observations. Recommend
  # building df with bind_rows to combine multiple datasets of interest
  nvar <- ncol(df %>% select(-Time_UTC))
  
  vars <- df %>%
    select(-Time_UTC) %>%
    colnames()
  
  fig <- lapply(vars, df, FUN = function(var, df) {
    attributes(df$Time_UTC)$tzone <- 'America/Denver'
    plot_ly(df, x = ~Time_UTC, y = as.formula(paste0('~', var)),
            hoverinfo = 'x+y', fill = 'tozeroy') %>%
      add_lines() %>%
      layout(showlegend = F,
             xaxis = list(title = '', showgrid = F),
             yaxis = list(title = strsplit(var, '_') %>%
                            unlist() %>%
                            grep('cal', x = ., value = T, invert = T) %>%
                            paste(collapse = ' '),
                          gridcolor = 'rgb(220,220,220)',
                          range = range(df[[var]], na.rm = T)))
  }) %>%
    subplot(nrows = nvar, shareX = T, titleX = F, titleY = T, margin = 0)
  
  # Hacky way of setting figure colors to hexcolors defined in _style_setup.r
  for (i in 1:nvar) {
    fig$x$data[[i]]$line$color <- hexcolors[c(1:nvar)[i]]
  }
  fig
}

function(input, output, session) {
  r <- reactiveValues(avgid = NULL,
                      n_subplot = 1)
  
  # URL Query -----------------------------------------------------------------
  observe({
    isolate({
      q <- parseQueryString(isolate(session$clientData$url_search))
      saveRDS(q, '~/temp.rds')
      if ('site_id' %in% names(q)) {
        updateSelectInput(session, 'site_id', selected=q[['site_id']])
      }
    })
  })
  
  output$subplots <- renderPlotly({
    req(input$site_id)
    
    isolate({
      if (!is.null(r$avgid))
        removeNotification(r$avgid)
    })
    
    showNotification(paste('Generating figures...',
                           'This may take a moment for large datasets'),
                     type = 'error', id = 'recalculating')
    
    date_range <- input$date_range %>% as.POSIXct()
    
    days <- seq(input$date_range[1], input$date_range[2], by = 'day')
    ndays <- length(days)
    
    available_files <- dir(file.path('/projects', 'data', input$site_id),
                           pattern = paste0('_calibrated.dat'),
                           recursive = T, full.names = T)
    available_files_base <- basename(available_files)
    matched_files <- available_files_base %>%
      intersect(format(days, tz = 'UTC',
                       format = paste0('%Y_%m_calibrated.dat')))
    filenames <- available_files[available_files_base %in% matched_files]
    
    if (length(filenames) < 1) {
      removeNotification('recalculating')
      showNotification('No data found - try a different site or time range',
                       type = 'error', duration = 5)
      return()
    }
    
    col_types <- read_lines(filenames[1], n_max = 1) %>%
      strsplit(',', fixed = T) %>%
      unlist() %>%
      sapply(switch,
             # Shared variable names
             'Time_UTC' = 'T',
             'CO2d_ppm_cal' = 'd',
             'CH4d_ppm_cal' = 'd',
             'CO2d_ppm' = 'd',
             'CH4d_ppm' = 'd',
             # LGR Variables
             'H2O_ppm' = 'd',
             'GasP_torr' = 'd',
             'GasT_C' = 'd',
             'RD0_us' = 'd',
             'RD1_us' = 'd',
             'ID_co2' = 'd',
             'ID_ch4' = 'd',
             # Licor Variables
             'MF_Controller_mLmin' = 'd',
             'PressureVolt' = 'd',
             'rhVolt' = 'd',
             'gasT' = 'd',
             'rawCO2_Voltage' = 'd',
             'Room_T' = 'd',
             '_') %>%
      paste(collapse = '')
    
    meas <- filenames %>%
      lapply(read_csv, col_types = col_types) %>%
      bind_rows() %>%
      filter(Time_UTC >= date_range[1],
             Time_UTC <= date_range[2] + 86400)
    
    if (nrow(meas) < 6) {
      removeNotification('recalculating')
      showNotification('No data found - try a different site or time range',
                       type = 'error', duration = 5)
      return()
    }
    
    avg_unit <- if (ndays < 3) { 'sec'
    } else if (ndays < 30) { 'min'
    } else if (ndays < 720) { 'hour'
    } else { 'day' }
    
    fig <- meas %>%
      group_by(Time_UTC = trunc(Time_UTC, unit = avg_unit) %>% as.POSIXct()) %>%
      summarize_all(funs(mean(., na.rm = T))) %>%
      do(.[ ,order(names(.))]) %>%
      select(starts_with('CO2'), starts_with('CH4'), everything()) %>%
      make_subplot() %>%
      layout(plot_bgcolor  = 'transparent',
             paper_bgcolor = 'transparent') %>%
      config(displayModeBar = F)
    
    r$n_subplot <- length(fig$x$data)
    removeNotification('recalculating')
    if (avg_unit != 'sec') {
      r$avgid <- showNotification(paste('Data shown are', avg_unit, 'averages'),
                                  type = 'warning', duration = 10)
    }
    
    fig
  })
  
  output$dynamic_plot <- renderUI({
    plotlyOutput('subplots', height = 300 + r$n_subplot * 150) %>% 
      withSpinner(color = '#222C32')
  })
}
