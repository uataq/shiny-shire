library(bigrquery)
library(dplyr)
library(glue)
library(googlesheets4)
library(plotly)
library(rclipboard)
library(shiny)

bq_auth(path = 'service-account.json')
gs4_auth(path = 'service-account.json')

KEYS <- list(
  gps = c('latitude', 'longitude', 'n_sat'),
  # lgr_mgga = c('co2d_ppm', 'ch4d_ppm', 'h2o_ppm', 'p_torr')
  licor_7000 = c('co2d_ppm', 'h2o_ppt', 'p_kpa', 't_c'),
  magee_ae33 = c('bc_ngm3'),
  # magic
  # picarro
  teledyne_t200 = c('no_ppb', 'flow_ccm', 'moly_t_c'),
  teledyne_t500u = c('no2_ppb', 'samp_pres_inhga', 'samp_temp_c'),
  thermo_pdr_1500 = c('pm25_ugm3', 'p_pa', 'rh_pct', 't_c')
)

GLOBAL_KEYS <- c('time', 'qc')

SHEET_URL <- list(
  lone = 'https://docs.google.com/spreadsheets/d/1JIR0ml6SSWndnPDHdQroqWjO71kTfkz3Db-sDX3zUS4',
  star = 'https://docs.google.com/spreadsheets/d/1Bk4hU0xDUuzxHk57YJjrSIm-q21E7p5I-uJau-DqPYI'
)

get_qc_sheet <- function(system_id, device_id) {
  url <- SHEET_URL[[system_id]]
  range_read(url, device_id)
}


server <- function(input, output, session) {
  session$allowReconnect(T)
  
  observeEvent(input$right,
               updateDateInput(session, 'date', value = input$date + 1))
  
  observeEvent(input$left,
               updateDateInput(session, 'date', value = input$date - 1))
  
  data <- reactive({
    req(input$device_id)
    req(input$system_id)
    
    inputs <- c('vehicle_id', 'device_id', 'date', 'left', 'right')
    input_values <- lapply(inputs, function(x) input[[x]])
    names(input_values) <- inputs
    str(input_values)
    
    con <- DBI::dbConnect(
      bigquery(),
      project = 'gsv-aq-x',
      dataset = 'saltlakecity'
    )
    
    table_id <- glue('2_{input$device_id}')
    table <- tbl(con, table_id)
    
    time_start <- format(as.POSIXct(format(input$date), tz = 'America/Denver'), tz = 'UTC')
    time_stop <- format(as.POSIXct(format(input$date+1), tz = 'America/Denver'), tz = 'UTC')
    
    keys <- c(KEYS[[input$device_id]], GLOBAL_KEYS)
    df <- table %>%
      filter(system_id == !!input$system_id,
             time >= !!time_start,
             time <= !!time_stop) %>%
      arrange(time) %>%
      select(!!keys) %>%
      show_query() %>%
      collect()
    
    str(df)
    
    validate(
      need(nrow(df) > 0, 'No data found. Try another day or vehicle.')
    )
    
    df
  })
  
  output$timeseries <- renderPlotly({
    df <- req(data())

    qc <- get_qc_sheet(input$system_id, input$device_id)
    for (i in 1:nrow(qc)) {
      mask <- (df$time >= qc$t_start[i]) & (df$time <= qc$t_end[i])
      df$qc[mask] <- qc$flag[i]
    }

    if (input$hide_qc) {
      df <- filter(df, qc == 0)
    }
    
    df$qc <- as.factor(df$qc)
    
    print(format(df$time[1], tz = 'UTC', format = '%Y-%m-%dT%H:%M:%S%Z'))
    
    subplots <- list()
    keys <- KEYS[[input$device_id]]
    for (i in 1:length(keys)) {
      k <- keys[i]
      p <- plot_ly(data = df,
                   x = ~time,
                   y = df[[k]],
                   color = ~qc,
                   legendgroup = 'qc',
                   showlegend = i == 1,
                   type = 'scattergl',
                   mode = 'markers') %>%
        layout(yaxis = list(showgrid = F,
                            title = k))
      
      if (i == 1)
        p <- event_register(p, 'plotly_selected')
      
      subplots <- c(subplots, list(p))
    }
    
    subplot(subplots, nrows = length(subplots), shareX = T, titleY = T) %>%
      layout(hovermode = 'compare',
             xaxis = list(title = 'UTC', showgrid = F))
  })
  
  observe({
    req(data())
    
    try({
      event <- event_data(
        event = 'plotly_selected',
        session = session
      )
    })
    
    if (!'event' %in% ls() || is.null(event)) {
      return()
    }
    
    device_id <- isolate(input$device_id)
    system_id <- isolate(input$system_id)
    
    df <- isolate(data())
    
    index <- match(trunc(event$x) * 1e-3, trunc(as.numeric(df$time)))
    
    df$selected <- F
    df$selected[index] <- T
    
    run <- rle(df$selected) %>%
      unclass() %>%
      as.data.frame()
    
    if (nrow(run) == 1) {
      return()
    }
    
    df$selection_group <- run %>%
      mutate(values = 1:n()) %>%
      inverse.rle()
    
    str(df)
    
    df <- filter(df, selected)
    
    selection_groups <- unique(df$selection_group)
    rows <- lapply(selection_groups, function(i) {
      selection <- df[df$selection_group == i, ]
      start <- floor(min(selection$time, na.rm = T))
      stop <- ceiling(max(selection$time, na.rm = T))
      now <- Sys.time()
      data.frame(t_start = format(start, tz = 'UTC'),
                 t_stop = format(stop, tz = 'UTC'),
                 t_added = format(now, tz = 'UTC'),
                 flag = -1)
    }) %>%
      bind_rows()
    
    ellip <- if (nrow(rows) > 10) '...' else ''
    
    showNotification(
      tags$div(
        rclipButton('clipbtn', 'Copy', 
                    paste(apply(rows, 1, function(x) paste(x, collapse = '\t')), collapse = '\n'),
                    icon('clipboard')),
        HTML('<br>', 
             paste(apply(head(rows, 10), 1, function(x) paste(x, collapse = ', ')), collapse = '<br>'),
             '<br>',
             ellip
        )
      ),
      duration = NULL,
      id = 'selection')
  })
}
