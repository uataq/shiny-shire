# Tank table refresh control
tt <- reactiveValues(refresh=T)

# Reactive - Fetch tank list ---------------------------------------------------
get_tanks <- reactive({
  if (tt$refresh) 
    readRDS( tail( dir('src/tanks', full.names=T), 1) )
})
get_tank_names <- reactive( sort( names( get_tanks() )))

# Reactive - Reset fields ------------------------------------------------------
observeEvent({input$tank_reset; input$tank_view_reset; input$tank_function}, {
  textfields <- c('tank_id', 'tank_psi', 'tank_location', 'tank_network', 'tank_co2', 'tank_ch4', 'tank_co', 
                  'tank_note', 'tank_serial', 'tank_relative', 'tank_calfile', 'tank_co2_stdev', 'tank_co2_sterr', 
                  'tank_co2_n', 'tank_ch4_stdev', 'tank_ch4_sterr', 'tank_ch4_n', 'tank_co_stdev', 
                  'tank_co_sterr', 'tank_co_n', 'tank_otto_summary', 'tank_d13C', 'tank_d18O', 'tank_d13C_stdev', 'tank_d13C_sterr',
                  'tank_d13C_n', 'tank_d18O_stdev','tank_d18O_sterr','tank_d18O_n')
  selfields <- c('tank_num', 'tank_num_view')
  for (field in textfields) updateTextInput(session, field, value='')
  for (field in selfields)  updateSelectInput(session, field, selected='')
})

# Reactive - Fill edit fields --------------------------------------------------
observe({
  tank <- input$tank_num
  if (!is.null(tank) && nchar(tank) > 0){
    tanks <- get_tanks()
    if (tank %in% names(tanks)){
      last_record <- tail(tanks[[tank]], 1)
      updateTextInput(session, 'tank_id', value=last_record$ID)
      updateTextInput(session, 'tank_psi', value=last_record$Pressure)
      updateTextInput(session, 'tank_location', value=last_record$Location)
      updateTextInput(session, 'tank_network', value=last_record$Network)
      updateTextInput(session, 'tank_co2_analyzer', value=last_record$Network)
      updateTextInput(session, 'tank_co2', value=last_record$CO2)
      updateTextInput(session, 'tank_ch4', value=last_record$CH4)
      updateTextInput(session, 'tank_co', value=last_record$CO)
      updateTextInput(session, 'tank_serial', value=last_record$Serial)
      updateTextInput(session, 'tank_relative', value=last_record$Relative)
      updateTextInput(session, 'tank_calfile', value=last_record$CalFile)
      updateTextInput(session, 'tank_co2_stdev', value=last_record$CO2_stdev)
      updateTextInput(session, 'tank_co2_sterr', value=last_record$CO2_sterr)
      updateTextInput(session, 'tank_co2_stn', value=last_record$CO2_n)
      updateTextInput(session, 'tank_ch4_stdev', value=last_record$CH4_stdev)
      updateTextInput(session, 'tank_ch4_sterr', value=last_record$CH4_sterr)
      updateTextInput(session, 'tank_ch4_stn', value=last_record$CH4_n)
      updateTextInput(session, 'tank_co_stdev', value=last_record$CO_stdev)
      updateTextInput(session, 'tank_co_sterr', value=last_record$CO_sterr)
      updateTextInput(session, 'tank_co_stn', value=last_record$CO_n)
      updateTextInput(session, 'tank_otto_summary', value=last_record$OTTO_summary)
      updateTextInput(session, 'tank_d13C', value=last_record$d13C)
      updateTextInput(session, 'tank_d18O', value=last_record$d18O)
      updateTextInput(session, 'tank_d13C_stdev', value=last_record$d13C_stdev)
      updateTextInput(session, 'tank_d13C_sterr', value=last_record$d13C_sterr)
      updateTextInput(session, 'tank_d13C_stn', value=last_record$d13C_n)
      updateTextInput(session, 'tank_d18O_stdev', value=last_record$d18O_stdev)
      updateTextInput(session, 'tank_d18O_sterr', value=last_record$d18O_sterr)
      updateTextInput(session, 'tank_d18O_stn', value=last_record$d18O_n)
      updateTextInput(session, 'tank_note', value=last_record$Note)
    }
  }
})

# UI - View --------------------------------------------------------------------
output$tank_view_ui <- renderUI({
  tagList(
    h5('Choose tank to view history'),
    selectInput('tank_num_view', NULL, choices=c('', get_tank_names())),
    actionButton('tank_view_reset', 'Reset', icon=icon('repeat')))
})

# UI - Add ---------------------------------------------------------------------
output$tank_add_ui <- renderUI({
  if (grepl('a', auth$level)){
    tagList(
      p('Add a new editable tank to the master list.'),
      textInput('tank_add_num', h5('Tank Number')),
      actionButton('tank_add_save', 'Add new tank'))
  } else{
    p('Sorry, but you don\'t have permissions to do this.')
  }
})

# UI - Edit --------------------------------------------------------------------
output$tank_edit_ui <- renderUI({
  input$tank_save; input$tank_remove_trigger; input$tank_add_save
  
  if (grepl('a', auth$level)){
    isolate(tagList(
      h3('Tank Info'),
      textInput('tank_time', 'Time', value=format(Sys.time(), '%Y-%m-%d %H:%M')),
      fluidRow(
        column(width=4,
               selectInput('tank_num', 'Tank Number', choices=c('', get_tank_names())),
               textInput('tank_psi', 'Pressure (psi)')),
        column(width=4,
               textInput('tank_serial', 'Tank Serial'),
               textInput('tank_location', 'Location')),
        column(width=4, 
               textInput('tank_id', 'ID'),
               textInput('tank_network', 'Network'))),
      fluidRow(column(4, textInput('tank_co2', 'CO2 (ppm)')),
               column(4, textInput('tank_ch4', 'CH4 (ppm)')),
               column(4, textInput('tank_co', 'CO (ppb)'))),
      h5('Notes'),
      tags$textarea(id="tank_note", rows=5),
      hr(),
      h3('Quality'),
      fluidRow(
        column(4, textInput('tank_relative', 'Relative to')),
        column(4, textInput('tank_calfile', 'Calibration file')),
        column(4, selectInput('tank_co2_analyzer', 'Instrument used',
                              c('',
                                'LI7000 IRG4-0279',
                                'LI7000 IRG4-0280',
                                'LGR RM 14-0269')))),
      fluidRow(
        column(4, h4('CO2'),
               textInput('tank_co2_stdev', 'StDev'),
               textInput('tank_co2_sterr', 'StErr'),
               textInput('tank_co2_n', 'N')),
        column(4, h4('CH4'),
               textInput('tank_ch4_stdev', 'StDev'),
               textInput('tank_ch4_sterr', 'StErr'),
               textInput('tank_ch4_n', 'N')),
        column(4, h4('CO'),
               textInput('tank_co_stdev', 'StDev'),
               textInput('tank_co_sterr', 'StErr'),
               textInput('tank_co_n', 'N'))),
      hr(),
      h3('Isotopes'),
      textInput('tank_otto_summary', 'OTTO Flask Summary'),
      fluidRow(
        column(4, textInput('tank_d13C', h3('d13C')),
               textInput('tank_d13C_stdev', 'd13C StDev'),
               textInput('tank_d13C_sterr', 'StErr'),
               textInput('tank_d13C_n', 'N')),
        column(4,textInput('tank_d18O', h3('d18O')),
               textInput('tank_d18O_stdev', 'd18O StDev'),
               textInput('tank_d18O_sterr', 'StErr'),
               textInput('tank_d18O_n', 'N'))),
      hr(),
      fluidRow(column(2, actionButton('tank_save', 'Save', icon=icon('pencil-square-o'))),
               column(2, actionButton('tank_reset', 'Reset', icon=icon('repeat'))),
               column(4, actionButton('tank_emptybutton', 'Tank is empty!', icon=icon('times-circle-o'))))))
  } else p('Sorry, but you don\'t have permissions to do this. Use the Field Form app instead.')
})

# UI - Remove ------------------------------------------------------------------
output$tank_remove_tank_ui <- renderUI({
  if (grepl('a', auth$level)){
    tagList(
      p('Remove a tank record from the database. Warning: this cannot be undone.'),
      h5('Tank number'),
      selectInput('tank_num_remove', NULL, choices=c('', get_tank_names())))
  } else  p('Sorry, but you don\'t have permissions to do this.')
})
output$tank_remove_time_ui <- renderUI({
  remove_tank <- input$tank_num_remove
  if (!is.null(remove_tank) && nchar(remove_tank) > 0){
    times <- as.character(get_tanks()[[remove_tank]]$Time)
    times <- na.omit(times)
    tagList(
      selectInput('tank_remove_time', h5('Time of record'), choices=c('', times)),
      actionButton('tank_remove_trigger', 'Delete record')
    )
  } else return()
})

# Event - Add tank to database -------------------------------------------------
observeEvent(input$tank_add_save, {
  tanks <- getTanks()
  d <- data_frame(ID = as.character(NA), Serial = as.character(NA), Time = as.POSIXct(as.character(NA)), 
                  Pressure = as.numeric(NA), Location = as.character(NA), Network = as.character(NA), CO2_analyzer = as.character(NA),
                  CO2 = as.numeric(NA), CO2_stdev = as.numeric(NA), CO2_sterr = as.numeric(NA), CO2_n = as.numeric(NA),
                  CH4 = as.numeric(NA), CH4_stdev = as.numeric(NA), CH4_sterr = as.numeric(NA), CH4_n = as.numeric(NA),
                  CO = as.numeric(NA), CO_stdev = as.numeric(NA), CO_sterr = as.numeric(NA), CO_n = as.numeric(NA), OTTO_summary = as.character(NA),
                  d13C = as.numeric(NA), d13C_stdev = as.numeric(NA), d13C_sterr = as.numeric(NA), d13C_n = as.numeric(NA),
                  d18O = as.numeric(NA), d18O_stdev = as.numeric(NA), d18O_sterr = as.numeric(NA), d18O_n = as.numeric(NA), 
                  Relative = as.character(NA), CalFile = as.character(NA), Note = as.character(NA), Name = as.character(NA))
  isolate(tt$refresh <- FALSE)
  tanks[input$tank_add_num] <- list(d)
  saveRDS(tanks, format(Sys.time(), 'src/tanks/%y%m%d_%H%M_tankdata.rds'))
  updateTextInput(session, 'tank_add_num', value='')
  isolate(tt$refresh <- TRUE)
})

# Event - Edit tank database ---------------------------------------------------
observeEvent(input$tank_save, {
  Tank <- input$tank_num
  if (!is.null(Tank) && nchar(Tank) > 0) {
    Tanks <- getTanks()
    d <- data_frame(ID=input$tank_id, 
                    Serial=input$tank_serial,
                    Time = as.POSIXct(input$tank_time), 
                    Pressure = as.numeric(input$tank_psi), 
                    Location = input$tank_location,
                    Network = input$tank_network,
                    CO2_analyzer = input$tank_co2_analyzer,
                    CO2 = as.numeric(input$tank_co2),
                    CO2_stdev = as.numeric(input$tank_co2_stdev),
                    CO2_sterr = as.numeric(input$tank_co2_sterr),
                    CO2_n = as.numeric(input$tank_co2_n),
                    CH4 = as.numeric(input$tank_ch4),
                    CH4_stdev = as.numeric(input$tank_ch4_stdev),
                    CH4_sterr = as.numeric(input$tank_ch4_sterr),
                    CH4_n = as.numeric(input$tank_ch4_n),
                    CO = as.numeric(input$tank_co),
                    CO_stdev = as.numeric(input$tank_co_stdev),
                    CO_sterr = as.numeric(input$tank_co_sterr),
                    CO_n = as.numeric(input$tank_co_n),
                    OTTO_summary = as.character(input$tank_otto_summary),
                    d13C = as.numeric(input$tank_d13C),
                    d13C_stdev = as.numeric(input$tank_d13C_stdev),
                    d13C_sterr = as.numeric(input$tank_d13C_sterr),
                    d13C_n = as.numeric(input$tank_d13C_n),
                    d18O = as.numeric(input$tank_d18O),
                    d18O_stdev = as.numeric(input$tank_d18O_stdev),
                    d18O_sterr = as.numeric(input$tank_d18O_sterr),
                    d18O_n = as.numeric(input$tank_d18O_n),
                    Relative = input$tank_relative,
                    CalFile = input$tank_calfile,
                    Note = input$tank_note,
                    Name = auth$name)
    
    d[nchar(d) < 1] <- NA
    isolate(dt$refresh <- FALSE)
    saveRDS(Tanks, format(Sys.time(), 'tanks/%y%m%d_%H%M_tankdata.rds'))
    isolate(dt$refresh <- TRUE)
  }
})

# Event - Remove record from database ------------------------------------------
observeEvent(input$tank_remove_trigger, {
  tank <- input$tank_num_remove
  remove_time <- input$tank_remove_time
  if (!is.null(tank) && nchar(tank) > 0) {
    tanks <- get_tanks()
    if(tank %in% names(tanks)){
      if(!is.null(remove_time) && nchar(remove_time) > 0){
        remove_time <- as.POSIXct(remove_time)
        tanks[[tank]] <- subset(tanks[[tank]], Time != remove_time)
      } else if(nrow(tanks[[tank]]) < 2){
        tanks[tank] <- NULL
      }
    } else return()
    isolate(dt$refresh <- FALSE)
    saveRDS(Tanks, format(Sys.time(), 'tanks/%y%m%d_%H%M_tankdata.rds'))
    updateSelectInput(session, 'tank_num_remove', selected='')
    isolate(dt$refresh <- TRUE)
  }
})

# Event - Empty tank button press ----------------------------------------------
observeEvent(input$tank_emptybutton, {
  textfields <- c('tank_location', 'tank_network', 'tank_co2', 'tank_ch4', 'tank_co', 
                  'tank_note', 'tank_relative', 'tank_co2_stdev', 'tank_co2_sterr', 
                  'tank_co2_n', 'tank_ch4_stdev', 'tank_ch4_sterr', 'tank_ch4_n', 'tank_co_stdev', 
                  'tank_co_sterr', 'tank_co_n', 'tank_otto_summary', 'tank_d13C', 'tank_d18O', 'tank_d13C_stdev', 'tank_d13C_sterr',
                  'tank_d13C_n', 'tank_d18O_stdev','tank_d18O_sterr','tank_d18O_n')
  for (field in textfields) updateTextInput(session, field, value='')
  updateTextInput(session, 'tank_psi', value=0)
})

# UI - Datatable of tanks ------------------------------------------------------
output$dt_tank <- DT::renderDataTable(server=F, options=list(scrollX=TRUE), {
  
  if(input$tank_function=='View') tank <- input$tank_num_view
  else if(input$tank_function=='Remove') tank <- input$tank_num_remove
  else if(input$tank_function=='Edit') tank <- input$tank_num
  else tank <- NA
  tanks <- get_tanks()
  if(!is.null(tank) && nchar(tank) > 0 && tank %in% names(tanks)){
    tbl_dat <- tanks[[tank]]
    o <- order(tbl_dat$Time, decreasing=T)
    dt <- tbl_dat[o, ]
    rownames(dt) <- NULL
  } else{
    tbl_dat <- get_tanks() %>% 
      lapply(tail, n=1) %>%
      bind_rows() %>%
      mutate(Num = names(tanks))
    o <- order(tbl_dat$Num)
    dt <- tbl_dat[o, ]
  }
  dt
})


# Download - access tank database ----------------------------------------------
output$download_trigger <- downloadHandler(
  filename = function() {
    rdsname <- tail(dir('src/tanks', pattern='.*.{1}rds'), 1)
    othername <- substr(rdsname, 1, nchar(rdsname)-4)
    switch(input$download_type,
           'Serialized R dataset (.rds)' = rdsname,
           'Tab separated (.dat)' = paste0(othername, '.dat'),
           'Matlab structure (.mat)' = paste0(othername, '.mat'))
  },
  content = function(file) {
    d <- get_tanks()
    ascii <- bind_rows(d)
    switch(input$download_type,
           'Serialized R dataset (.rds)' = saveRDS(d, file),
           'Tab separated (.dat)' = write.table(ascii, file, sep='\t\t\t', row.names=F, quote=F),
           'Matlab structure (.mat)' = writeMat(file, tank_data=d))
  }
)

# Submit tank request ----------------------------------------------------------

observeEvent(input$request_save, {
  name <- input$request_name
  date <- input$request_date
  email <- input$request_email
  target <- isolate(cbind(c(input$request_qty_1, input$request_qty_2, input$request_qty_3),
                          c(input$request_co2_1, input$request_co2_2, input$request_co2_3),
                          c(input$request_ch4_1, input$request_ch4_2, input$request_ch4_3)))
  
  text <- isolate(input$request_comment)
  msg <- paste(sep='\n',
               paste('Name:\t\t', name),
               paste('Date:\t\t', date),
               paste('Email:\t\t', email),
               paste('Comments:\t', text),
               paste('\nRequest:'),
               paste(input$request_qty_1, input$request_co2_1, input$request_ch4_1, sep='\t'),
               paste(input$request_qty_2, input$request_co2_2, input$request_ch4_2, sep='\t'),
               paste(input$request_qty_3, input$request_co2_3, input$request_ch4_3, sep='\t')
  )
  
  sendmail_options(smtpServer='ASPMX.L.GOOGLE.COM')
  from <- '<donotreply@air.utah.edu>'
  to <- '<tankfillrequest@gmail.com>'
  subject <- paste('Tank Request:', Sys.Date())
  sendmail(from, to, subject, msg)
  
  closeAlert(session, 't1')
  createAlert(session, 'tankrequest_alert', 't1', style='success',
              content='Tank request has been sent. You will be contacted via email regarding the status of your order.')
})
