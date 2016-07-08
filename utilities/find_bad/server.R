# Ben Fasoli 
source('global.R')

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
  
  output$display <- renderUI({
    if (!is.null(input$site) && nchar(input$site) > 1) {
      tagList(
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div(HTML('<i class="fa fa-refresh fa-spin"></i>'))
        ),
        plotOutput('ts', height=600, brush='brush'),
        verbatimTextOutput('string'),
        actionButton('reset', 'Start over', icon=icon('undo'))
      )
    } else {
      selectInput('site', 'Site', 
                  c('', grep('trx', substr(dir('/home/benfasoli/github/lair-proc/run/'), 1, 3),
                             value=T, invert=T)))
    }
  })
  
  which_column <- reactive({
    if (is.null(network(input$site))) return()
    if (network(input$site) == 'ch4') {
      return('CO2d_ppm')
    } else if (network(input$site) == 'co2') {
      return('rawCO2')
    }
  })
  
  dat <- reactive({
    if (is.null(network(input$site))) return()
    col_types <- switch(network(input$site),
                        'co2' = 'Tiiiiiidddddddddddddcddc',
                        'ch4' = 'Tddddddddddddddddddddiccdd')
    loc <- file.path('/projects/data/measurements/data', 
                     input$site, 'parsed')
    
    dat <- dir(loc, full.names=T) %>%
      lapply(function(f, col_types) {
        if (file.exists(f))
          readr::read_csv(f, locale=locale(tz='UTC'), col_types=col_types)
      }, col_types=col_types) %>%
      bind_rows() %>%
      mutate(idx = 1:n()) %>%
      filter(ID_co2 >= 0) 
    dat
  })
  
  # Produce figure ------------------------------------------------------------
  output$ts <- renderPlot({
    if (is.null(network(input$site))) return()
    isolate({
      df <- dat()
      x <- df$Time_UTC
      y <- df[[which_column()]]
      ID <- factor(df$ID_co2)
      qplot(x=x, y=y, color=ID) +
        geom_hline(yintercept=unique(df$ID_co2), alpha=0.2) +
        theme_classic() +
        xlab(NULL) + ylab(NULL)
    })
  })
  
  output$string <- renderText({
    brush <- input$brush
    df <- dat()
    x <- as.numeric(df$Time_UTC)
    y <- as.numeric(df[[which_column()]])
    idx <- x > brush$xmin & x < brush$xmax & y > brush$ymin & y < brush$ymax
    df <- df[idx, ]
    
    t_start <- format(min(df$Time_UTC, na.rm=T)-1, tz='UTC', format='%Y-%m-%d %H:%M:%OS3')
    t_end <- format(max(df$Time_UTC, na.rm=T)+1, tz='UTC', format='%Y-%m-%d %H:%M:%OS3')
    miu_old <- 'all'
    miu_new <- 'NA'
    comment <- 'namecomment'
    paste(sep=', ', t_start, t_end, miu_old, miu_new, comment, collapse='\n')
  })
  
  observeEvent(input$reset, session$reload())
}

# DEBUG
# input <- list(
#   site='csp'
# )
# which_column <- function() {'CO2d_ppm'}
