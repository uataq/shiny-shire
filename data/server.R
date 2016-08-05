# Ben Fasoli
source('global.R')

network <- function(site) {
  if (site %in% c('csp', 'fru', 'hpl', 'roo', 'sno', 'wbb')) {
    return('ch4')
  } else if (site %in% c('dbk', 'heb', 'lgn', 'rpk', 'sug', 'sun')) {
    return('co2')
  }
}

get_data <- function(sites, t_start, t_end, variables) {
  tmp <- lapply(sites, t_start = t_start, t_end = t_end, variables = variables,
                function(site, t_start, t_end, variables) {
                  base <- file.path('/projects/data', site, 'calibrated')
                  files <- strftime(seq(t_start, t_end, by = 'day'), '%Y_%m_calibrated.dat') %>%
                    unique %>%
                    file.path(base, .)
                  col_types <- switch(network(site),
                                      'co2' = 'Tddddidc',
                                      'ch4' = 'Tddddiddddic')
                  tmp <- file.path('/projects/data', site, 'calibrated') %>%
                    dir(full.names = T, pattern = '.*\\.dat') %>%
                    intersect(files) %>%
                    lapply(read_csv, col_types = col_types, locale = locale(tz = 'UTC')) %>%
                    bind_rows()
                }) %>%
    bind_rows() %>%
    filter(Time_UTC >= as.POSIXct(t_start, tz = 'UTC'),
           Time_UTC <= as.POSIXct(paste0(t_end, '23:59:59'),   tz = 'UTC'))
  if (nrow(tmp) > 0) {
    df <- data_frame(Time_UTC = tmp$Time_UTC, site_id=tmp$site_id)
    for (col in variables) {
      if (col %in% colnames(tmp)) {
        df[col] <- select(tmp, match(col, colnames(tmp)))
      } else df[col] <- NA
    }
    df <- df %>%
      mutate(Time_UTC = strftime(Time_UTC, tz = 'UTC', format = '%Y-%m-%d %H:%M:%S %Z'))
  } else {
    info('No data found. Try a different date range, different site, or different set of variables.')
    return(NULL)
  }
}




# Server initialization --------------------------------------------------------
function(input, output, session) {
  valid <- readr::read_csv(tail(dir('user_auth', full.names=T), 1))
  
  auth <- reactiveValues(agree    = F,
                         logged   = F,
                         token    = NA,
                         name     = NA,
                         t_start  = NA,
                         t_end    = NA,
                         sites    = list(NA),
                         btn_cnt  = 0)
  
  # URL query ------------------------------------------------------------------
  observe({
    q <- parseQueryString(isolate(session$clientData$url_search))
    if ('token' %in% names(q)) 
      auth$token <- q['token']
    if ('agree' %in% names(q))
      auth$agree <- T
  })
  
  # Validate login credentials -------------------------------------------------
  observe({
    # auth$token <- digest::digest('benfasoli')
    # auth$agree <- T
    if (!auth$agree) return()
    if (!is.null(input$token) && nchar(input$token) == 32)
      auth$token <- input$token
    
    idx <- match(auth$token, sapply(valid$name, digest))
    
    if (!is.na(idx)) {
      toggleModal(session, 'key_window', toggle = 'hide')
      auth_info    <- valid[idx, ]
      auth$logged  <- T
      auth$name    <- auth_info$name
      auth$t_start <- auth_info$t_start
      auth$t_end   <- auth_info$t_end
      auth$sites   <- auth_info$sites
    } else if (nchar(auth$token) == 32) {
      info('Login error. Check for valid token.')
    }
  })
  
  # Body UI --------------------------------------------------------------------
  observeEvent(input$agree, auth$agree <- T)
  observe({
    if (!is.null(input$agree) && input$agree && is.na(auth$token))
      toggleModal(session, 'key_window', 'show')
  })
  
  output$dash <- renderUI({
    if (!auth$logged) {
      column(12,
             box(width=NULL, status='danger', solidHeader=F,
                 includeMarkdown('www/fair_use.md'),
                 bsButton('agree', 'I agree', icon=icon('check'), 
                          block=T, style='danger')
             )
      )
    } else {
      
      # Manipulate user authentication options
      opts_sites <- auth$sites
      if (!is.na(auth$sites)) {
        opts_sites<- opts_sites %>%
          strsplit('/', fixed=T) %>%
          unlist()
      } else {
        opts_sites <- '/projects/data' %>%
          dir() %>% 
          grep(x = ., pattern = 'trx', value = T, invert = T)
      }
      opts_t_start <- auth$t_start
      opts_t_end <- auth$t_end
      
      fluidRow(
        column(8, offset = 2,
               box(title='Choose data',
                   solidHeader=T, width=NULL,
                   p(
                     strong(auth$name),
                     br(),
                     'API token: ', auth$token
                   ),
                   hr(),
                   fluidRow(
                     column(6, 
                            dateInput('t_start', 'Start date', width='100%',
                                      value=opts_t_start, min=opts_t_start)),
                     column(6, 
                            dateInput('t_end', 'End date', width='100%',
                                      value=opts_t_start, max=opts_t_end))
                   ),
                   selectInput('sites', 'Site identifiers', width = '100%', 
                               choices=c('Choose site(s)' = '', opts_sites),
                               multiple=T),
                   selectInput('variables', 'Variables', width = '100%',
                               choices = c('Choose variable(s)' = '', 
                                           'CO2d_ppm_cal', 'CO2d_ppm_raw', 
                                           'm_co2', 'b_co2', 'n_co2', 'CH4d_ppm_cal', 
                                           'CH4d_ppm_raw', 'm_ch4', 'b_ch4', 'n_ch4'), 
                               multiple = T),
                   tags$a(id = 'btn', class = 'btn btn-default btn-block shiny-download-link disabled',
                          href = '', target = '_blank', icon('download'), 'Download')
               )
        )#,
        
        # column(7, 
        #        box(title='Example data structure', width=NULL, solidHeader=T,
        #            # DT::renderDataTable('table')
        #            'hi'
        #        ))
      )
    }
  })
  
  # Enable or disable example button -------------------------------------------
  observe({
    cond <- !is.null(input$sites) &&
      nchar(input$sites) > 1 &&
      !is.null(input$variables) &&
      nchar(input$variables > 1)
    
    # Classes when button enabled
    toggleClass('btn', 'btn-danger', cond)
    # Classes when button disabled
    toggleClass('btn', 'btn-default disabled', !cond)
  })
  
  # Download handler -----------------------------------------------------------
  output$btn <- downloadHandler(
    contentType = 'text/csv',
    filename = function() {
      paste0('uataq-', strftime(Sys.Date(), '%Y%m%d'), '.csv')
    },
    content = function(file) {
      removeClass('btn', 'btn-default disabled')
      print(input$variables)
      df <- get_data(input$sites, input$t_start, input$t_end, input$variables)
      print(str(df))
      print(colnames(df))
      if (!is.null(df))
        write_csv(df, file)
    }
  )
}
