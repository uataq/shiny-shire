# Ben Fasoli 

max_obs <- 100000

network <- function(site) {
  if (is.null(site)) {
    return(NULL)
  } else if (site %in% c('csp', 'fru', 'hpl', 'roo', 'wbb')) {
    return('ch4')
  } else if (site %in% c('dbk', 'heb', 'lgn', 'rpk', 'sug', 'sun')) {
    return('co2')
  } else {
    return(NULL)
  }
}

function(input, output, session) {
  r <- reactiveValues(data = NULL, 
                      downsample = NULL,
                      refresh = 0, 
                      site = NULL)
  
  # URL Query -----------------------------------------------------------------
  observe({
    isolate({
      q <- parseQueryString(isolate(session$clientData$url_search))
      if (r$refresh < 1 && 'site' %in% names(q)) {
        updateSelectInput(session, 'site', selected=q[['site']])
        r$site <- q[['site']]
        r$refresh <- r$refresh + 1
      }
    })
  })
  
  # Update UI on button click -------------------------------------------------
  observeEvent(input$apply, {
    r$site <- input$site
    r$refresh <- r$refresh + 1
  })
  
  # Column choosing UI --------------------------------------------------------
  output$data_options <- renderUI({
    if (input$dataset != 'Instrument diagnostics') return()
    if (network(input$site) == 'ch4') {
      selectInput('data', 'Display', c('CH4_ppm', 'CH4_ppm_sd', 
                                       'H2O_ppm', 'H2O_ppm_sd', 
                                       'CO2_ppm', 'CO2_ppm_sd', 
                                       'CH4d_ppm', 'CH4d_ppm_sd',
                                       'CO2d_ppm', 'CO2d_ppm_sd',
                                       'GasP_torr', 'GasP_torr_sd', 
                                       'GasT_C', 'GasT_C_sd', 
                                       'AmbT_C', 'AmbT_C_sd'), 
                  'CH4d_ppm')
    } else if (network(input$site) == 'co2') {
      selectInput('data', 'Display', c('batt_volt', 'PTemp', 'Room_T',
                                       'IRGA_T', 'IRGA_P',
                                       'MF_Controller_mLmin', 
                                       'PressureVolt', 'rhVolt', 
                                       'gas_T', 'rawCO2_Voltage', 
                                       'rawCO2', 'rawH2O', 'Program'), 
                  'rawCO2')
    }
  })
  
  
  # Read data -----------------------------------------------------------------
  observeEvent(r$refresh, {
    if (is.null(network(r$site))) return()
    if (input$dataset == 'Calibrated dataset') {
      opts <- switch(network(r$site),
                     'co2' = list(type = 'Tddddidc',
                                  col  = 'CO2d_ppm_cal'),
                     'ch4' = list(type = 'Tddddiddddic',
                                  col  = c('CO2d_ppm_cal', 'CH4d_ppm_cal')))
      loc <- file.path('/projects/data/measurements/data', 
                       r$site, 'calibrated/%Y_%m_calibrated.dat')
    } else {
      opts <- switch(network(r$site),
                     'co2' = list(type = 'Tiiiiiidddddddddddddcddc'),
                     'ch4' = list(type = 'Tddddddddddddddddddddiccdd'))
      opts$col <- input$data
      loc <- file.path('/projects/data/measurements/data', 
                       r$site, 'parsed/%Y_%m_parsed.dat')
    }
    time_range <- c(as.POSIXct(input$date_range[1], tz='UTC'),
                    as.POSIXct(paste(input$date_range[2], '23:59:59'), tz='UTC'))
    
    dat <- format(seq(time_range[1], time_range[2], 3600*24), tz='UTC', 
                  format=loc) %>%
      unique() %>%
      lapply(function(f, col_types) {
        if (file.exists(f))
          readr::read_csv(f, locale=locale(tz='UTC'), col_types=col_types)
      }, col_types=opts$type) %>%
      bind_rows() %>%
      do({if (nrow(.) > 1) return(.) else return(data_frame())}) %>%
      filter(Time_UTC >= time_range[1], Time_UTC <= time_range[2])
    
    if (nrow(dat) > max_obs) {
      r$downsample <- nrow(dat)
      if ('ID_co2' %in% names(dat)) {
        dat <- filter(dat, ID_co2 != -10) %>%
          bind_rows( filter(dat, ID_co2 == -10) %>%
                       sample_n(max_obs) ) %>%
          arrange(Time_UTC)
      } else dat <- sample_n(dat, max_obs)
    } else r$downsample <- NULL
    
    dat <- dat %>%
      (function(df){
        if ('ID_co2' %in% names(df)) {
          df <- filter(df, ID_co2 != -99)
          out <- data_frame(Time_UTC = df$Time_UTC)
          for (col in sort(unique(df$ID))) {
            col_make <- paste0('~', col)
            out[col_make] <- df[input$data]
            out[df$ID != col, col_make] <- NA
          }
        } else if (length(names(df)) > 0) {
          out <- select(df, Time_UTC, one_of(opts$col))
        } else out <- df
        return(out)
      })
    

    r$data <- dat
  })
  
  
  # Produce figure ------------------------------------------------------------
  output$ts <- renderDygraph({
    if (is.null(network(r$site))) return()
    validate(need(nrow(r$data) > 1, paste('No data found. Try a different',
                                          'time period or a different site.')))
    isolate({
      dy <- xts(select(r$data, -Time_UTC), r$data$Time_UTC) %>%
        dygraph() %>%
        dyOptions(colors = c('#D2352C', '#1A2226','#00ADB5', '#DD6E42', '#4F6D7A',
                             RColorBrewer::brewer.pal(7, 'Set2')))
      if (input$dataset == 'Calibrated dataset') {
        if ('CH4d_ppm_cal' %in% names(r$data)) {
          dy <- dy %>%
            dySeries('CH4d_ppm_cal', axis='y2') %>%
            dyAxis('y', label='CO2 (ppm)') %>%
            dyAxis('y2', label='CH4 (ppm)') %>%
            dyOptions(drawGrid=F)
        } else {
          dy <- dy %>% dyOptions(fillGraph=T, drawGrid=F, fillAlpha=0.25)
        }
      } else {
        dy <- dy %>%
          dyOptions(drawPoints=T, pointSize=2, fillGraph=T, fillAlpha=0.25,
                    drawGrid=F)
      }
      dy
    })
  })
  
  # Produce downsample text ---------------------------------------------------
  output$downsample_ui <- renderUI({
    if (is.null(r$downsample)) return()
    tagList(
      hr(),
      p(HTML('<i class="fa fa-exclamation-circle"></i>'),
        paste('Atmospheric data is randomly sampled down to', 
              prettyNum(max_obs, big.mark=',', scientific=F),
              'points from', prettyNum(r$downsample, big.mark=',', scientific=F),
              'observations. To see the highest frequency data, try a smaller',
              'date range.'))
    )
  })
}

# DEBUG
# input <- list(
#   site='sun',
#   dataset='Instrument diagnostics',
#   data='CO2d_ppm',
#   date_range = c(Sys.Date()-2, Sys.Date())
# )
# r <- list(site=input$site)
# network <- function() {'co2'}