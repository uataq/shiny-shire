# Ben Fasoli & James Mineau
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
        # Directory with site data
        site_dir <- file.path('/data', stid)

        # Get instruments that have a final directory
        instruments <- list.dirs(site_dir, full.names = T) %>%
          .[file.exists(file.path(., 'final'))] %>%
          basename()

        data <- lapply(instruments, function(instrument) {
          # File paths in final directory
          path <- file.path(site_dir, instrument, 'final')
          files_in_path <- list.files(path, full.names = T)
          # File selection by date
          files_by_date <- unique(format(seq(dates[1], dates[2], by = 'day'),
                                         '%Y_%m_final.dat'))
          # Intersection between files in directory and files by date
          files <- files_in_path[grep(paste(files_by_date, collapse = '|'),
                                       files_in_path)]

          # Validate that files exist
          if (length(files) == 0) return(NULL)

          # Read data from matched files
          data <- rbindlist(lapply(files, function(file) {
            suppressWarnings(
              fread(file, showProgress = F, data.table = F)
            )
          })) %>%
            mutate(Time_UTC = fastPOSIXct(Time_UTC, tz = 'UTC')) %>%
            filter(Time_UTC >= dates[1],
                   Time_UTC < dates[2]) %>%
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

          return(data)
        }) %>%
          purrr::discard(is.null) %>%
          purrr::reduce(full_join, by = 'Time_UTC') %>%
          arrange(Time_UTC)

        if (nrow(data) == 0) return(NULL)

        return(data)
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

        variables <- tribble(
          ~var_name, ~short, ~long, ~color,
          'CO2d_ppm_cal', 'CO2',   'Carbon Dioxide (ppm)',                                'rgba(241, 143, 1, 0.5)',
          'CH4d_ppm_cal', 'CH4',   'Methane (ppm)',                                       'rgba(0, 159, 183, 0.5)',
          'O3_ppb',       'O3',    'Ozone (ppb)',                                         'rgba(44, 160, 44, 0.5)',
          'NO2_ppb',      'NO2',   'Nitrogen Dioxide (ppb)',                              'rgba(214, 39, 40, 0.5)',
          'NO_ppb',       'NO',    'Nitric Oxide (ppb)',                                  'rgba(148, 103, 189, 0.5)',
          'PM2.5_ugm3',   'PM2.5', 'Particulate Matter<sub>2.5</sub> (μg/m<sup>3</sup>)', 'rgba(140, 86, 75, 0.5)',
          'CO_ppb',       'CO',    'Carbon Monoxide (ppb)',                               'rgba(227, 119, 194, 0.5)',
          'BC6_ngm3',     'BC',    'Black Carbon (ng/m<sup>3</sup>)',                     'rgba(127, 127, 127, 0.5)'
        )

        # Use measured data if calibrated data is all NA
        cal_vars <- grep("_cal$", variables$var_name, value = TRUE)
        for (var in cal_vars) {
          if (all(is.na(.[[var]]))) {
            new_var <- sub("_cal$", "_meas", var)
            variables[variables$var_name == var, 'var_name'] <- new_var
          }
        }

        variables <- variables[variables$var_name %in% colnames(.), ]

        plot_list <- lapply(1:nrow(variables), function(i) {
          y <- .[[variables$var_name[i]]]
          mask <- !is.na(y)
          plot_ly(., x = ~Time_UTC[mask], y = y[mask],
                  name = variables$short[i],
                  type = 'scattergl', mode = 'lines',
                  fill = 'tozeroy',
                  fillcolor = variables$color[i],
                  line = list(color = variables$color[i])) %>%
            layout(yaxis = list(range = range(y[mask]),
                                showgrid = F,
                                title = variables$long[i],
                                side = ifelse((nrow(variables) > 2)
                                                && (i %% 2 == 0),
                                              'right', 'left')))
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
