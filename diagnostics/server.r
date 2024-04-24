# Ben Fasoli & James Mineau
suppressPackageStartupMessages(source('global.r'))

max_rows <- 100000

invalid_columns <- c('Time_UTC', 'Pi_Time', 'ID', 'ID_CO2', 'ID_CH4',
                     'Program', 'Fix_Quality', 'Status', 'QAQC_Flag')

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
    updateSelectInput(session, 'instrument',
                      selected = state$input$instrument,
                      choices = instruments())

    updateSelectInput(session, 'lvl',
                      selected = state$input$lvl,
                      choices = lvls())

    updateSelectInput(session, 'column',
                      selected = state$input$column,
                      choices = columns())
  })

  # Fetch instrument options for given site
  instruments <- reactive({
    if (nchar(input$stid) < 3) return('')
    base_path <- file.path('/data', input$stid)
    insts <- list.dirs(base_path, full.names = T) %>%
      .[file.exists(file.path(., 'qaqc'))] %>%
      basename()
    insts <- c('', insts)
    setNames(insts, gsub('_', ' ', insts))
  })

  # Fetch diagnostic level options for given site - instrument combination
  lvls <- reactive({
    if (nchar(input$stid) < 3 | nchar(input$instrument) == 0) return('')
    instrument_dir <- file.path('/data', input$stid, input$instrument)
    if (file.exists(file.path(instrument_dir, 'calibrated'))) {
      c('qaqc', 'calibrated')
    } else {
      c('qaqc')
    }
  })

  # Fetch column options for given site - instrument - lvl combination
  columns <- reactive({
    if (nchar(input$stid) < 3 | nchar(input$instrument) == 0 | nchar(input$lvl) == 0) return('')
    data_dir <- file.path('/data', input$stid, input$instrument, input$lvl)
    header <- strsplit(readLines(dir(data_dir, full.names = T)[1],
                                 n = 1), ',')[[1]]
    header <- unique(unlist(header))
    header <- c('', setdiff(header, invalid_columns))
    setNames(header, gsub('_', ' ', header))
  })

  # On stid change, ensure instrument, lvl, & column options are up to date
  observeEvent(input$stid, {
    updateSelectInput(session, 'instrument', choices = instruments())
  })

  # On instrument change, ensure lvl & column options are up to date
  observeEvent(input$instrument, {
    updateSelectInput(session, 'lvl', choices = lvls())
    updateSelectInput(session, 'column', choices = columns())
  })

  # On lvl change, ensure column options are up to date
  observeEvent(input$lvl, {
    updateSelectInput(session, 'column', choices = columns())
  })

  # If remove_failed_qc is checked, color_by is changed to ID
  observeEvent(input$remove_failed_qc, {
    if (input$remove_failed_qc) {
      updateRadioButtons(session, 'color_by', selected = 'ID')
    }
  })

  output$plot <- renderPlotly({
    # Execute isolated expression on submit button
    input$submit
    isolate({
      removeNotification('message-no-data')
      removeNotification('message-select-data')
      # Validate that a site has been selected
      if (nchar(input$instrument) == 0 | nchar(input$column) == 0
          | nchar(input$lvl) == 0 | nchar(input$stid) < 3 | input$submit == 0) {
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
      stid <- input$stid
      instrument <- input$instrument
      lvl <- input$lvl
      column <- input$column
      dates <- as.POSIXct(c(input$dates[1], input$dates[2] + 1), tz = 'UTC')
      remove_failed_qc <- input$remove_failed_qc
      color_by <- input$color_by

      future({
        data_dir <- file.path('/data', stid, instrument, lvl)

        # Identify intersection of files and dates
        files_in_paths <- list.files(data_dir, full.names = T)
        files_by_date <- unique(format(seq(dates[1], dates[2], by = 'day'),
                                       paste0('%Y_%m_', lvl, '.dat')))
        files <- files_in_paths[grep(paste(files_by_date, collapse = '|'),
                                     files_in_paths)]

        # Validate that files exist
        if (length(files) == 0) return(NULL)

        # Read data from matched files
        data <- rbindlist(lapply(files, function(file) {
          suppressWarnings(
            fread(file, showProgress = F, data.table = F,
                  select = c('Time_UTC', column, 'QAQC_Flag',
                             'ID', 'ID_CO2', 'ID_CH4'))  # ID cols for cals
          )
        })) %>%
          mutate(Time_UTC = fastPOSIXct(Time_UTC, tz = 'UTC')) %>%
          filter(Time_UTC >= dates[1],
                 Time_UTC < dates[2]) %>%
          na.omit()

        if (lvl == 'calibrated') {
          if ('ID_CH4' %in% colnames(data)) {
            # rebuild ID from ID_CO2 and ID_CH4
            data <- data %>%
              mutate(ID = paste(ID_CO2, ID_CH4, sep = '~')) %>%
              select(-ID_CO2, -ID_CH4)
          } else {
            data <- data %>%
              rename(ID = ID_CO2)
          }
        }

        # Filter flushing observations
        if ('ID' %in% colnames(data)) {
          data <- data[!grepl('flush|-99', data$ID), ]
        }

        # Filter failed QAQC observations
        if (remove_failed_qc) data <- filter(data, QAQC_Flag >= 0)

        # Validate that data exist
        if (nrow(data) == 0) return(NULL)

        # Subsample data rows
        if (nrow(data) > max_rows) {
          data <- data %>%
            slice(trunc(seq.int(1, n(), length.out = max_rows)))
        }

        data <- arrange(data, Time_UTC)
        attributes(data$Time_UTC)$tzone <- 'UTC'

        return(data)
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

        if (color_by == 'ID' && !('ID' %in% colnames(data))) {
          # Add atmospheric ID column
          message(paste('adding ID to', stid, instrument, lvl))
          data$ID <- 'atmosphere'
        }

        data[[color_by]] <- as.factor(data[[color_by]])

        data %>%
          plot_ly(x = ~Time_UTC, y = data[[column]],
                  name = data[[color_by]], color = data[[color_by]],
                  type = 'scattergl', mode = 'markers',
                  colors = c('#F18F01', '#009FB7', '#2CA02C', '#D62728', '#9467BD', '#8C564B', '#E377C2', '#7F7F7F'),
                  marker = list(size = 10,
                                line = list(color = 'rgba(255, 255, 255, .1)',
                                            width = 1),
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
