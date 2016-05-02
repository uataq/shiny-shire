# Field form table refresh control
fft <- reactiveValues(refresh=T)

# Reactive - Fetch note list ---------------------------------------------------
get_notes <- reactive({
  if (fft$refresh)
    readRDS( tail( dir('src/notes', full.names=T), 1) )
})

# UI - Show nitrogen update bar ------------------------------------------------
output$note_nitrogen_ui <- renderUI({
  if (input$note_site %in% c(n2_sites)) {
    ln <- get_notes()[[input$note_site]] %>%
      tail(1)
    if (!is.null(ln$n2_psi) && !is.na(ln$n2_psi)) set <- ln$n2_psi
    else set <- 2200
    sliderInput('note_nitrogen_psi', h5('Nitrogen Tank Pressure (psi)'),
                min=0, max=2500, step=50, value=set)
  } else return()
})

# UI - Show tank pressure and location update ----------------------------------
output$note_tank_update_ui <- renderUI({
  tank_opts <- sort(names(get_tanks()))
  box(title='Reference Tank Status', solidHeader=T, width=NULL,
      fluidRow(
        column(3, 
               selectInput('ff_tank_num', h5('Tank Number'), choices=c('', tank_opts)),
               selectInput('ff_tank_location', h5('Location'), choices=c('', loc_opts))),
        column(8, sliderInput('ff_tank_psi', h5('Pressure (psi)'), min=0, max=2500, step=50, value=2200))),
      fluidRow(
        column(3, actionButton('ff_tank_save', 'Submit', icon=icon('pencil-square-o'))),
        column(9, bsAlert('fftanksave_alert'))))
})



# UI - Site and tank infoboxes -------------------------------------------------
output$site_status <- renderUI({
  if (!is.null(input$note_site) && nchar(input$note_site) > 0) {
    # Find last note time
    lt <- get_notes()[[input$note_site]] %>%
      .$time_out %>%
      max(na.rm=T)
    T_info <- infoBox('Last updated', format(lt, format='%Y-%m-%d\n %H:%M %Z', tz='America/Denver'), 
                      color='yellow', fill=F, icon=icon('calendar', lib='glyphicon'))
    
    # Find lowest tank pressure
    tanks <- get_tanks() %>% 
      lapply(tail, n=1)
    mask <- tanks %>%
      lapply(site=input$note_site, function(tid, site) {
        if (!is.na(tid$Location) && tid$Location == site) return(T)
        else return(F)
      }) %>%
      unlist()
    tanks_onsite <- tanks[mask] %>%
      bind_rows() %>%
      mutate(Num = names(tanks)[mask])
    
    if (nrow(tanks_onsite) > 0) {
      min_idx <- which.min(tanks_onsite$Pressure)
      P_min <- tanks_onsite$Pressure[min_idx]
      
      if(P_min <= 600){
        col <- 'red'
        ic <- 'thumbs-o-down'
      } else if(P_min > 600 && P_min < 1200){
        col <- 'yellow'
        ic <- 'reorder'
      } else {
        col <- 'green'
        ic <- 'thumbs-o-up'
      }
      
      P_info <- infoBox('Tank status', paste(P_min, 'psi'), color=col, fill=T,
                        icon=icon(ic, lib='font-awesome'), 
                        subtitle=paste('Tank', tanks_onsite$Num[min_idx]))
    } else {
      P_info <- infoBox('Tank status', 'Unknown', color='red', fill=T,
                        icon=icon('question', lib='font-awesome'))
    }
    tagList(T_info, P_info)
  } else return()
})


# UI - Tank pressure time series -----------------------------------------------
output$ts_note <- renderPlot({
  if (!is.null(input$note_site) && nchar(input$note_site) > 0) {
    tanks <- get_tanks()
    mask <- tanks %>%
      lapply(site=input$note_site, function(tid, site) {
        loc <- tail(tid$Location, 1)
        if (!is.na(loc) && loc == site) return(T)
        else return(F)
      }) %>%
      unlist()
    if (!any(mask)) return()
    tanks_onsite <- tanks[mask] %>%
      bind_rows() %>%
      mutate(Num = gsub('.*_', '', ID)) %>%
      filter(Location == input$note_site)  %>%
      select(Time, Pressure, Num)
    
    notes <- get_notes()[[input$note_site]]
    ln <- tail(notes, 1)
    if(!is.null(ln$n2_psi) && !is.na(ln$n2_psi)){
      tanks_onsite <- bind_rows(tanks_onsite, 
                                data_frame(Time=notes$time_out,
                                           Pressure=notes$n2_psi,
                                           Num='N2'))
    }
    tanks_onsite <- na.omit(tanks_onsite)
    if(nrow(tanks_onsite) > 0){
      f <- qplot(data=tanks_onsite, x=Time, y=Pressure,
                 color=factor(Num), xmin=Sys.time()-60*60*24*365) +
        geom_line() +
        scale_fill_discrete('') +
        xlab(NULL) + ylab(NULL) +
        theme_classic() +
        theme(legend.position='bottom',
              legend.title=element_blank())
      print(f)
    }
  } else NULL
})

# UI - Datatable of notes ------------------------------------------------------
output$dt_note <- DT::renderDataTable(server=F, rownames=F, style='bootstrap', 
                                      options=list(responsive=T),
                                      extensions=c('responsive'), {
  notes <- get_notes()
  if(input$note_site %in% names(notes)){
    dt <- notes[[input$note_site]]
    dt[order(dt$time_in, decreasing=T), ]
  } else NULL
})

# Event - New note save --------------------------------------------------------
observeEvent(input$note_save, {
  time_in  <- as.POSIXct(input$note_time_in, '%Y-%m-%d %H:%M', tz='UTC')
  time_out <- as.POSIXct(input$note_time_out, '%Y-%m-%d %H:%M', tz='UTC')
  
  if (input$note_site %in% n2_sites) n2_psi <- input$note_nitrogen_psi
  else n2_psi <- NA
  
  text <- input$note_text
  if (nchar(text) < 1) text <- NA
  
  if (!is.na(time_in) && !is.na(time_out)) {
    nd <- data_frame(time_in = time_in,
                     time_out = time_out,
                     n2_psi = n2_psi,
                     note = text,
                     name = auth$name)
    
    notes <- get_notes()
    temp <- saveRDS(list(n1=notes[[input$note_site]],
                         n2=input$note_site,
                         n3=notes), '~/test.rds')
    notes[input$note_site] <- list(bind_rows(notes[[input$note_site]], nd))
    isolate(fft$refresh <- F)
    saveRDS(notes, format(Sys.time(), 'src/notes/%y%m%d_%H%M_notearchive.rds'))
    isolate(fft$refresh <- T)
    
    updateTextInput(session, 'note_text', value='')
    updateTextInput(session, 'note_time', value=format(Sys.time(), '%Y-%m-%d %H:%M %Z', tz='UTC'))
    
    closeAlert(session, 'f1')
    createAlert(session, 'ffsave_alert', 'f1', style='success',
                content='Success! Note appended to archive.')
  } else {
    closeAlert(session, 'f1')
    createAlert(session, 'ffsave_alert', 'f1', style='danger',
                content='Failed to save note. Is the timestamp formatted correctly?.')
  }
})


# Event - Tank info save -------------------------------------------------------
observeEvent(input$ff_tank_save, {
  tank <- input$ff_tank_num
  if (!is.null(tank) && nchar(tank) > 0) {
    tanks <- get_tanks()
    d <- data_frame(Time = Sys.time(), 
                    Pressure = as.numeric(input$ff_tank_psi), 
                    Location = input$ff_tank_location,
                    Name = auth$name)
    d[nchar(d) < 1] <- NA
    if(tank %in% names(tanks)){
      for (column in grep('Note', names(tanks[[tank]]), value=T, fixed=T, invert=T)) {
        if (!(column %in% names(d))) {
          d[column] <- tail(tanks[[tank]], 1)[column]
        }
      }
      tanks[tank] <- list(bind_rows(tanks[[tank]], d))
    } else return()
    isolate(tt$refresh <- FALSE)
    saveRDS(tanks, format(Sys.time(), 'src/tanks/%y%m%d_%H%M_tankdata.rds'))
    isolate(tt$refresh <- TRUE)
  }
})






