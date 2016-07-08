# Ben Fasoli

# Server initialization --------------------------------------------------------
function(input, output, session) {
  valid <- readr::read_csv(tail(dir('user_auth', full.names=T), 1))
  
  auth <- reactiveValues(logged   = F,
                         name     = NA,
                         t_start  = NA,
                         t_end    = NA,
                         sites    = list(NA))
  
  # Validate login credentials -------------------------------------------------
  observe({
    if (!is.null(input$userkey) && nchar(input$userkey) == 32) {
      userkey <- tolower(input$userkey)
      idx <- match(userkey, sapply(valid$name, digest))
      
      if (!is.na(idx)) {
        toggleModal(session, 'key_window', 'close')
        auth_info    <- valid[idx, ]
        auth$logged  <- T
        auth$name    <- auth_info$name
        auth$t_start <- auth_info$t_start
        auth$t_end   <- auth_info$t_end
        auth$sites   <- auth_info$sites
      } else {
        closeAlert(session, 'a1')
        createAlert(session, 'key_alert', 'a1', style='danger',
                    content='Login error. Check credentials.')
      }
    }
  })
  
  # Body UI --------------------------------------------------------------------
  observeEvent(input$accept, toggleModal(session, 'key_window', 'open'))
  
  output$dash <- renderUI({
    if (!auth$logged) {
      column(12,
             box(width=NULL, title='Fair Use Agreement', status='danger', solidHeader=F,
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
          dir()
      }
      opts_t_start <- auth$t_start
      opts_t_end <- auth$t_end
      
      
      fluidRow(
        column(5,
               box(title='Choose data', status='danger',
                   solidHeader=T, width=NULL,
                   p(
                     strong(auth$name), 
                     br(),
                     'Sites available: ', strong(paste(opts_sites, collapse=', ')),
                     br(),
                     'From: ', strong(auth$t_start),
                     br(),
                     'To: ', strong(auth$t_end)
                     
                   ),
                   selectInput('sites', 'Site identifiers', width='100%', 
                               choices=c('Choose site(s)'='', opts_sites), multiple=T),
                   dateInput('t_start', 'Start date', width='100%',
                             value=opts_t_start, min=opts_t_start),
                   dateInput('t_end', 'End date', width='100%',
                             value=opts_t_start, max=opts_t_end),
                   bsButton('btn_download', 'Download data',
                            icon=icon('download'), style='danger',
                            block=T, disabled=T)
               )
        ),
        
        column(7, 
               box(title='Example data structure', width=NULL, solidHeader=T,
                   'data table?'))
      )
    }
  })
  
  # Enable or disable download button
  observe({
    if (!is.null(input$sites) &&
        nchar(input$sites) > 2) {
      updateButton(session, 'btn_download', disabled=F)
    } else {
      updateButton(session, 'btn_download', disabled=T)
    }
  })
}
