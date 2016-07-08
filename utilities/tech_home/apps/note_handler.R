# ---------------------------------------------------------------------------------
# TANK TRACKER INITIALIZATION
ff <- reactiveValues(refresh=TRUE,
                     saveidx=0)

# ---------------------------------------------------------------------------------
# SITE SELECTION
observe(switch(input$note_network,
               'CO2 Network' = updateSelectInput(session, 'note_site',
                                                       choices=c('Daybreak','Draper','Heber','Logan',
                                                                 'Murray','Rose Park','Sugarhouse','WBB')),
               'Uinta Basin' = updateSelectInput(session, 'note_site',
                                                 choices=c('Castle Peak', 'Fruitland','Horsepool','Roosevelt')),
               'Other' = updateSelectInput(session, 'note_site',
                                           choices=c('Nerdmobile','SPL','TRAX','Other'))
))

# ---------------------------------------------------------------------------------
# IMPORT NOTES
getNotes <- reactive({
  if(ff$refresh) readRDS(tail(dir('notes', pattern='.*.{1}rds', full.names=T), 1))
})

# ---------------------------------------------------------------------------------
# SAVE EVENT UI
output$ui_note_passfail <- renderUI({
  if(!is.null(input$note_save) && input$note_save > ff$saveidx){
    ff$saveidx <- ff$saveidx + 1
    network  <- isolate(input$note_network)
    site     <- isolate(input$note_site)
    time_in  <- as.POSIXct(isolate(input$note_time_in), '%Y-%m-%d %H:%M', tz='UTC')
    time_out <- as.POSIXct(isolate(input$note_time_out), '%Y-%m-%d %H:%M', tz='UTC')
    
    if(input$note_network=='CO2 Network') n2_psi   <- isolate(input$note_nitrogen_psi)
    else n2_psi <- NA
    
    text     <- isolate(input$note_text)
    if(nchar(text)<1) text <- NA
    
    if(!is.na(time_in) && !is.na(time_out)) {
      Notes <- getNotes()
      isolate(ff$refresh <- FALSE)
      
      nd <- data.frame(stringsAsFactors=F,
                       time_in=time_in,
                       time_out=time_out,
                       n2_psi=n2_psi,
                       note=text,
                       name=USER$name)
      Notes[site] <- list(rbind.fill(Notes[[site]], nd))
      
      saveRDS(Notes, format(Sys.time(), 'notes/%y%m%d_%H%M_notearchive.rds'))
      isolate(ff$refresh <- TRUE)
      
      updateTextInput(session, 'note_text', value='')
      updateTextInput(session, 'note_time', value=format(Sys.time(), '%Y-%m-%d %H:%M %Z', tz='UTC'))
      
      box(title='Saved', status='success', width=NULL,
          p('Note appended to archive.')
      )
    } else{
      box(title='Error', status='danger', width=NULL,
          p('Failed to save note. Is the timestamp formatted correctly?.')
      )
    }
  }
})

# ---------------------------------------------------------------------------------
# LAST UPDATED INFOBOX
output$site_last_updated <- renderInfoBox({
  site  <- input$note_site
  notes <- getNotes()
  dt <- notes[[site]]
  lt <- max(dt$time_out, na.rm=T)
  infoBox('Last updated', format(lt, '%Y-%m-%d\n %H:%M %Z', tz='UTC'), color='yellow', fill=F,
          icon=icon('calendar', lib='glyphicon')
  )
})

# ---------------------------------------------------------------------------------
# PRESSURE STATUS INFOBOX
output$site_tank_status <- renderInfoBox({
  site  <- input$note_site
  Tanks <- getTanks()
  if(!is.null(site)){
    latest <- ldply(Tanks, .id='Num', function(x) tail(x, 1))
    mask <- latest$Location == site
    sitetanks <- latest[mask, c('Num', 'Time', 'Pressure')]
    notes <- getNotes()
    lastnote <- tail(notes[[site]], 1)
    if(!is.null(lastnote$n2_psi) && !is.na(lastnote$n2_psi)){
      sitetanks <- rbind.fill(sitetanks, data.frame(stringsAsFactors=F,
                                                    Time=lastnote$time_out,
                                                    Pressure=lastnote$n2_psi,
                                                    Num='N2'))
    }
    sitetanks <- subset(sitetanks, !is.na(Pressure))
    if(length(sitetanks$Pressure) > 0){
      minidx <- which.min(sitetanks$Pressure)
      lowest <- sitetanks$Pressure[minidx]
      if(lowest <= 600){
        col <- 'red'
        ic <- 'thumbs-o-down'
      } else if(lowest > 600 && lowest < 1200){
        col <- 'yellow'
        ic <- 'reorder'
      } else{
        col <- 'green'
        ic <- 'thumbs-o-up'
      }
      
      infoBox('Tank status', paste(lowest, 'psi'), color=col, fill=T,
              icon=icon(ic, lib='font-awesome'), subtitle=paste('Tank', sitetanks$Num[minidx])
      )
    } else{
      infoBox('Tank status', 'Unknown', color='red', fill=T,
              icon=icon('question', lib='font-awesome')
      )
    }
  }
})

# ---------------------------------------------------------------------------------
# NOTE TABLE
output$dt_note <- DT::renderDataTable(options=list(scrollX=TRUE), {
  site  <- input$note_site
  notes <- getNotes() 
  if(site %in% names(notes)){
    dt <- notes[[site]]
    dt[order(dt$time_in, decreasing=T), ]
  } else NULL
})

# ---------------------------------------------------------------------------------
# TANK PRESSURES TIME SERIES
output$ts_note <- renderPlot({
  site  <- input$note_site
  if(!is.null(site)){
    Tanks <- getTanks()
    latest <- ldply(Tanks, .id='Num', function(x) tail(x, 1))
    mask <- latest$Location == site
    sitetanks <- latest[mask, 'Num']
    ascii <- ldply(Tanks, .id='Num', .fun=function(x){data.frame(x)})[ , 1:30]
    tankdata <- ascii[ascii$Num %in% sitetanks & ascii$Location==site, ]
    plotdata <- tankdata[ , c('Time', 'Pressure', 'Num')]
    notes <- getNotes()
    lastnote <- tail(notes[[site]], 1)
    if(!is.null(lastnote$n2_psi) && !is.na(lastnote$n2_psi)){
      plotdata <- rbind.fill(plotdata, data.frame(stringsAsFactors=F,
                                                  Time=notes[[site]]$time_out,
                                                  Pressure=notes[[site]]$n2_psi,
                                                  Num='N2'))
    }
    plotdata <- na.omit(plotdata)
    if(nrow(plotdata) > 0){
      f <- qplot(data=plotdata, x=Time, y=Pressure, color=factor(Num), xmin=Sys.time()-60*60*24*365) +
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

# ---------------------------------------------------------------------------------
# NITROGEN PRESSURE UI
output$note_nitrogen_ui <- renderUI({
  site  <- input$note_site
  if(input$note_network=='CO2 Network'){
    notes <- getNotes()
    lastnote <- tail(notes[[site]], 1)
    if(!is.null(lastnote$n2_psi) && !is.na(lastnote$n2_psi)){
      setat <- lastnote$n2_psi
    } else setat <- 2200
    sliderInput('note_nitrogen_psi', h5('Nitrogen Tank Pressure (psi)'),
                min=0, max=2500, step=50, value=setat)
  } else ''
})


# ---------------------------------------------------------------------------------
# STANDARD TANK UPDATE
output$note_tank_update <- renderUI({
  if(USER$level=='user'){
    tankopts <- sort(names(getTanks()))
    locationopts <- sort(c('Draper', 'Heber', 'Logan', 
                           'Daybreak', 'Murray', 'Rose Park', 'Sugarhouse',
                           'Fruitland', 'Horsepool', 'Roosevelt',
                           'Nerdmobile', 'TRAX', 'SPL',
                           'ASB', 'Garden', 'WBB'))
    box(title='Reference Tank Status', solidHeader=T, width=NULL,
        fluidRow(
          column(3, 
                 selectInput('ff_tank_num', h5('Tank Number'), choices=c('', tankopts)),
                 selectInput('ff_tank_location', h5('Location'), choices=c('', locationopts))
          ),
          column(8, sliderInput('ff_tank_psi', h5('Pressure (psi)'), min=0, max=2500, step=50, value=2200))
        ),
        fluidRow(
          column(3, actionButton('ff_tank_save', 'Submit', icon=icon('pencil-square-o'))),
          column(9, uiOutput('ff_tank_save_text'))
        )
    )
  } else ''
})

# ---------------------------------------------------------------------------------
# TANK SAVE MESSAGE
output$ff_tank_save_text <- renderUI({
  if(input$ff_tank_save > 0){
    tank <- isolate(input$ff_tank_num)
    h5(paste('Success. Tank', tank, 'record updated.'))
  } else ''
})








