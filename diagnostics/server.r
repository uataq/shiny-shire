# Ben Fasoli
source('global.r')

max_rows <- 100000

function(input, output, session) {
  
  # Spawn new server processes for disconnected clients
  session$allowReconnect(T)
  
  # Bookmark state with URL props
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  onRestored(function(state) {
    updateSelectInput(session, 'column',
                      selected = state$input$column,
                      choices = columns())
  })
  
  # Fetch column options for given site
  columns <- reactive({
    if (nchar(input$stid) < 3) return('')
    base_path <- file.path('/data', input$stid)
    paths <- file.path(base_path, dir(base_path, pattern = 'licor|lgr'), 'qaqc')
    headers <- lapply(paths, function(path) {
      file <- dir(path, full.names = T)[1]
      header <- strsplit(readLines(file, n = 1), ',')[[1]]
      return(header)
    })
    header <- unique(unlist(headers))
    not_options <- c('Time_UTC', 'ID', 'ID_CO2', 'ID_CH4', 'Program', 'QAQC_Flag')
    header <- c('', setdiff(header, not_options))
    setNames(header, gsub('_', ' ', header))
  })
  
  # On stid change, ensure column options are up to date
  observeEvent(input$stid, {
    updateSelectInput(session, 'column', choices = columns())
  })
  
  output$plot <- renderPlotly({
    # Execute isolated expression on submit button
    input$submit
    isolate({
      removeNotification('message-no-data')
      removeNotification('message-select-data')
      # Validate that a site has been selected
      if (nchar(input$column) == 0 | nchar(input$stid) < 3 | input$submit == 0) {
        showNotification('Select a dataset to load.',
                         duration = NULL,
                         closeButton = F,
                         type = 'warning',
                         id = 'message-select-data')
        return(NULL)
      }
      
      # Search files and load data
      showNotification('Fetching data...',
                       duration = NULL,
                       closeButton = F,
                       type = 'message',
                       id = 'message-loading')
      
      # Read reactive values prior to async call
      column <- input$column
      dates <- as.POSIXct(c(input$dates[1], input$dates[2] + 1), tz = 'UTC')
      include_atmos <- input$include_atmos
      include_failed_qc <- input$include_failed_qc
      
      stid <- input$stid
      future({
        # Base path to find data
        base_path <- file.path('/data', stid)
        paths <- file.path(base_path, dir(base_path, pattern = 'licor|lgr'), 'qaqc')
        files_in_paths <- dir(paths)
        # File selection by date
        files_by_date <- unique(format(seq(dates[1], dates[2], by = 'day'),
                                       '%Y_%m_qaqc.dat'))
        files <- files_in_paths[grep(paste(files_by_date, collapse='|'), files_in_paths)]
        
        # Validate that files exist
        if (length(files) == 0) return(NULL)
        
        # Read data from matched files
        data <- rbindlist(lapply(files, function(file) {
          suppressWarnings(
            fread(file, showProgress = F, data.table = F,
                  select = c('Time_UTC', column, 'ID', 'QAQC_Flag'))
          )
        })) %>%
          mutate(Time_UTC = fastPOSIXct(Time_UTC, tz = 'UTC')) %>%
          filter(Time_UTC >= dates[1],
                 Time_UTC < dates[2],
                 !grepl('-99', ID)) %>%
          na.omit()
        
        if (!include_failed_qc) data <- filter(data, QAQC_Flag >= 0)
        
        # Optionally remove atmospheric observations
        if (!include_atmos) data <- filter(data, !grepl('-10', ID))
        
        # Validate that data exist
        if (nrow(data) == 0) return(NULL)
        
        # Subsample data rows
        if (nrow(data) > max_rows) {
          ref_idx <- grep('-10', data$ID, invert = T)
          idx <- union(
            trunc(seq.int(1, nrow(data), length.out = max(0, max_rows - length(ref_idx)))),
            ref_idx
          )
          data <- slice(data, idx)
        }
        data <- arrange(data, Time_UTC)
        # attributes(data$Time_UTC)$tzone <- 'America/Denver'
        attributes(data$Time_UTC)$tzone <- 'UTC'
        data
      }) %...>% {
        data <- .
        removeNotification('message-loading')
        
        # Validate that data exist
        if (is.null(data)) {
          showNotification('No data found. Try a different site or date range.',
                           duration = NULL,
                           closeButton = F,
                           type = 'error',
                           id = 'message-no-data')
          return(NULL)
        }
        
        if (nrow(data) == max_rows) {
          showNotification(
            paste('Observations reduced to', max_rows, 'rows.'),
            duration = 10,
            type = 'warning')
        }
        
        data %>%
          mutate(ID = as.factor(ID)) %>%
          plot_ly(x = ~Time_UTC, y = data[[column]], name = ~ID, color = ~ID,
                  type = 'scattergl', mode = 'markers',
                  marker = list(size = 10, 
                                line = list(color = 'rgba(255, 255, 255, .1)',                                          width = 1),
                                opacity = 0.8)) %>%
          layout(hovermode = 'compare',
                 legend = list(orientation = 'h'),
                 xaxis = list(title = 'UTC',
                              showgrid = F),
                 yaxis = list(showgrid = F,
                              title = gsub('_', ' ', column)))
      }
    })
  })
  
}
