# Ben Fasoli
source('global.r')

max_rows <- 10000

function(input, output, session) {
  session$allowReconnect(T)
  
  # Bookmark state with URL props
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  output$plot <- renderPlotly({
    # Execute isolated expression on submit button
    input$submit
    isolate({
      removeNotification('message-no-data')
      removeNotification('message-select-site')
      # Validate that a site has been selected
      if (nchar(input$stid) < 3 | input$submit == 0) {
        showNotification('Select a site to see data.',
                         duration = NULL,
                         closeButton = F,
                         type = 'warning',
                         id = 'message-select-site')
        return(NULL)
      }
      
      # Search files and load data
      showNotification('Fetching data...',
                       duration = NULL,
                       closeButton = F,
                       type = 'message',
                       id = 'message-loading')
      
      # Read reactive values prior to async call
      dates <- as.POSIXct(c(input$dates[1], input$dates[2] + 1))
      stid <- input$stid
      future({
        # Base path to find data
        base_path <- file.path('/data', stid)
        paths <- file.path(base_path, dir(base_path, pattern = 'licor|lgr'), 'calibrated')
        files_in_paths <- dir(paths)
        # File selection by date
        files_by_date <- unique(format(seq(dates[1], dates[2], by = 'day'),
                                       '%Y_%m_calibrated.dat'))
        files <- files_in_paths[grep(paste(files_by_date, collapse='|'), files_in_paths)]
        
        # Validate that files exist
        if (length(files) == 0) return(NULL)
        
        # Read data from matched files
        data <- rbindlist(lapply(files, function(file) {
          suppressWarnings(
            fread(file, showProgress = F, data.table = F,
                  select = c('Time_UTC', 'CO2d_ppm_cal', 'CH4d_ppm_cal', 'ID_CO2'))
          )
        })) %>%
          mutate(Time_UTC = fastPOSIXct(Time_UTC, tz = 'UTC')) %>%
          filter(Time_UTC >= dates[1],
                 Time_UTC < dates[2],
                 ID_CO2 == -10) %>%
          select(-ID_CO2) %>%
          na.omit()
        
        # Validate that data exist
        if (nrow(data) == 0) return(NULL)
        
        # Subsample data rows
        if (nrow(data) > max_rows) {
          data <- data %>%
            slice(trunc(seq.int(1, n(), length.out = max_rows)))
        }
        data <- arrange(data, Time_UTC)
        attributes(data$Time_UTC)$tzone <- 'America/Denver'
        data
      }) %...>% {
        removeNotification('message-loading')
        
        # Validate that data exist
        if (is.null(.)) {
          showNotification('No data found. Try a different site or date range.',
                           duration = NULL,
                           closeButton = F,
                           type = 'error',
                           id = 'message-no-data')
          return(NULL)
        }
        
        if (nrow(.) == max_rows) {
          showNotification(
            paste('Observations reduced to', max_rows, 'rows.'),
            duration = 10,
            type = 'warning')
        }

        variables <- data_frame(
          var_name = c('CO2d_ppm_cal', 'CH4d_ppm_cal'),
          short = c('CO2', 'CH4'),
          long = c('Carbon Dioxide (ppm)', 'Methane (ppm)'),
          color = c('rgba(241, 143, 1, 0.5)', 'rgba(0, 159, 183, 0.5)')
        )
        variables <- variables[variables$var_name %in% colnames(.), ]
        plot_list <- lapply(1:nrow(variables), function(i) {
          y <- .[[variables$var_name[i]]]
          plot_ly(., x = ~Time_UTC, y = y,
                  name = variables$short[i],
                  type = 'scattergl', mode = 'lines',
                  fill = 'tozeroy',
                  fillcolor = variables$color[i],
                  line = list(color = variables$color[i])) %>%
            layout(yaxis = list(range = range(y),
                                showgrid = F,
                                title = variables$long[i]))
        })
        subplot(plot_list, nrows = length(plot_list), shareX = T, titleY = T) %>%
          layout(hovermode = 'compare',
                 showlegend = F,
                 xaxis = list(title = 'Mountain Time',
                              showgrid = F))
      }
    })
  })
}
