# ---------------------------------------------------------------------------------
# TANK TRACKER INITIALIZATION
dt <- reactiveValues(refresh=TRUE)

# ---------------------------------------------------------------------------------
# RESET FIELDS EVENT
observeEvent({input$tank_reset; input$tank_view_reset; input$tank_function}, {
  resetFields(session)
})

# ---------------------------------------------------------------------------------
# IMPORT TANKS
getTanks <- reactive({
  # input$tank_save; input$tank_remove_trigger; input$tank_add_save
  if(dt$refresh) readRDS((tail(dir('tanks', full.names=T), 1)))
})
Tanknames <- reactive(sort(names(getTanks())))
# getTanks <- reactivePoll(1000, session, 
#                          function(){
#                            tail(dir('tanks', full.names=T), 1)
#                          },
#                          function(){
#                            readRDS(tail(dir('tanks', full.names=T), 1))
#                          }
# )

# ---------------------------------------------------------------------------------
# FILL FIELDS
observe({
  Tank <- input$tank_num
  if(!is.null(Tank) && nchar(Tank) > 0){
    Tanks <- getTanks()
    if(Tank %in% names(Tanks)){
      last_record <- tail(Tanks[[Tank]], 1)
      updateTextInput(session, 'tank_id', value=last_record$ID)
      updateTextInput(session, 'tank_psi', value=last_record$Pressure)
      updateTextInput(session, 'tank_location', value=last_record$Location)
      updateTextInput(session, 'tank_network', value=last_record$Network)
      updateTextInput(session, 'tank_co2', value=last_record$CO2)
      updateTextInput(session, 'tank_ch4', value=last_record$CH4)
      updateTextInput(session, 'tank_co', value=last_record$CO)
      updateTextInput(session, 'tank_note', value=last_record$Note)
    }
  }
})

observe({
  input$show_adv_edit
  Tank <- input$tank_num
  if(!is.null(Tank) && nchar(Tank) > 0){
    Tanks <- getTanks()
    if(Tank %in% names(Tanks)){
      last_record <- tail(Tanks[[Tank]], 1)
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
      updateTextInput(session, 'tank_d13C', value=last_record$d13C)
      updateTextInput(session, 'tank_d18O', value=last_record$d18O)
      updateTextInput(session, 'tank_d13C_stdev', value=last_record$d13C_stdev)
      updateTextInput(session, 'tank_d13C_sterr', value=last_record$d13C_sterr)
      updateTextInput(session, 'tank_d13C_stn', value=last_record$d13C_n)
      updateTextInput(session, 'tank_d18O_stdev', value=last_record$d18O_stdev)
      updateTextInput(session, 'tank_d18O_sterr', value=last_record$d18O_sterr)
      updateTextInput(session, 'tank_d18O_stn', value=last_record$d18O_n)
    }
  }
})

# ---------------------------------------------------------------------------------
# ADD TANK EVENT
observeEvent(input$tank_add_save, {
  tanknum <- input$tank_add_num
  Tanks <- getTanks()
  d <- data.frame(stringsAsFactors=F,
                  ID = as.character(NA), Serial = as.character(NA), Time = as.POSIXct(as.character(NA)), 
                  Pressure = as.numeric(NA), Location = as.character(NA), Network = as.character(NA),
                  CO2 = as.numeric(NA), CO2_stdev = as.numeric(NA), CO2_sterr = as.numeric(NA), CO2_n = as.numeric(NA),
                  CH4 = as.numeric(NA), CH4_stdev = as.numeric(NA), CH4_sterr = as.numeric(NA), CH4_n = as.numeric(NA),
                  CO = as.numeric(NA), CO_stdev = as.numeric(NA), CO_sterr = as.numeric(NA), CO_n = as.numeric(NA),
                  d13C = as.numeric(NA), d13C_stdev = as.numeric(NA), d13C_sterr = as.numeric(NA), d13C_n = as.numeric(NA),
                  d18O = as.numeric(NA), d18O_stdev = as.numeric(NA), d18O_sterr = as.numeric(NA), d18O_n = as.numeric(NA), 
                  Relative = as.character(NA), CalFile = as.character(NA), Note = as.character(NA), Name = as.character(NA))
  isolate(dt$refresh <- FALSE)
  Tanks[tanknum] <- list(d)
  saveRDS(Tanks, format(Sys.time(), 'tanks/%y%m%d_%H%M_tankdata.rds'))
  updateTextInput(session, 'tank_add_num', value='')
  isolate(dt$refresh <- TRUE)
})

# ---------------------------------------------------------------------------------
# SAVE EVENT
observeEvent(input$tank_save, {
  Tank <- input$tank_num
  if (!is.null(Tank) && nchar(Tank) > 0) {
    Tanks <- getTanks()
    if(input$show_adv_edit){
      d <- data.frame(stringsAsFactors=F,
                      ID=input$tank_id, 
                      Serial=input$tank_serial,
                      Time = as.POSIXct(input$tank_time), 
                      Pressure = as.numeric(input$tank_psi), 
                      Location = input$tank_location,
                      Network = input$tank_network,
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
                      Name = USER$name)
    } else if(!input$show_adv_edit){
      d <- data.frame(stringsAsFactors=F,
                      ID=input$tank_id, 
                      Time = as.POSIXct(input$tank_time), 
                      Pressure = as.numeric(input$tank_psi), 
                      Location = input$tank_location,
                      Network = input$tank_network,
                      CO2 = as.numeric(input$tank_co2),
                      CH4 = as.numeric(input$tank_ch4),
                      CO = as.numeric(input$tank_co),
                      Note = input$tank_note,
                      Name = USER$name)
    } else return()
    
    d[nchar(d) < 1] <- NA
    # Replace columns not defined by user with previous observation
    if(Tank %in% names(Tanks)){
      for (column in grep('Note', names(Tanks[[Tank]]), value=T, fixed=T, invert=T)) {
        if (!(column %in% names(d))) {
          d[column] <- tail(Tanks[[Tank]], 1)[column]
        }
      }
      Tanks[Tank] <- list(rbind.fill(Tanks[[Tank]], d))
    } else if(USER$level=='admin'){
      Tanks[Tank] <- list(d)
    } else return()
    isolate(dt$refresh <- FALSE)
    saveRDS(Tanks, format(Sys.time(), 'tanks/%y%m%d_%H%M_tankdata.rds'))
    isolate(dt$refresh <- TRUE)
  }
})

observeEvent(input$ff_tank_save, {
  Tank <- input$ff_tank_num
  if (!is.null(Tank) && nchar(Tank) > 0) {
    Tanks <- getTanks()
    d <- data.frame(stringsAsFactors=F,
                    Time = Sys.time(), 
                    Pressure = as.numeric(input$ff_tank_psi), 
                    Location = input$ff_tank_location,
                    Name = USER$name)
    d[nchar(d) < 1] <- NA
    isolate(dt$refresh <- FALSE)
    if(Tank %in% names(Tanks)){
      for (column in grep('Note', names(Tanks[[Tank]]), value=T, fixed=T, invert=T)) {
        if (!(column %in% names(d))) {
          d[column] <- tail(Tanks[[Tank]], 1)[column]
        }
      }
      Tanks[Tank] <- list(rbind.fill(Tanks[[Tank]], d))
    } else return()
    saveRDS(Tanks, format(Sys.time(), 'tanks/%y%m%d_%H%M_tankdata.rds'))
    isolate(dt$refresh <- TRUE)
  }
})

# ---------------------------------------------------------------------------------
# TANK TABLE
output$dt_tank <- DT::renderDataTable(server=F, options=list(scrollX=TRUE), {
  advanced <- c(input$show_adv_edit, input$show_adv_add, input$show_adv_view, input$show_adv_remove)
  advanced[is.null(advanced)] <- FALSE
  
  if(input$tank_function=='View') Tank <- input$tank_num_view
  else if(input$tank_function=='Remove') Tank <- input$tank_num_remove
  else if(input$tank_function=='Edit') Tank <- input$tank_num
  else Tank <- NA
  Tanks <- getTanks()
  if(!is.null(Tank) && nchar(Tank) > 0 && Tank %in% names(Tanks)){
    tabledat <- Tanks[[Tank]]
    o <- order(tabledat$Time, decreasing=T)
    dt <- tabledat[o, ]
    rownames(dt) <- NULL
  } else{
    tabledat <- ldply(Tanks, .id='Num', function(x) tail(x, 1))
    rownames(tabledat) <- names(Tanks)
    o <- order(tabledat$Num)
    dt <- tabledat[o, ]
  }
  if (any(advanced)){
    return(dt)
  } else return(dt[ ,simple_cols])
})

# ---------------------------------------------------------------------------------
# PRESSURE TIME SERIES
# output$tank_ts <- renderPlot({
#   input$tank_reset
#   Tank <- input$tank_num
#   Tanks <- getTanks()
#   if(!is.null(Tank) && nchar(Tank) > 0 && Tank %in% names(Tanks)){
#     p <- Tanks[[Tank]]
#     o <- order(p$Time, decreasing=T)
#     p <- p[o, ]
#     with(p, plot(Time, Pressure, type='l', 
#                  ylim=c(0, 2500), main=Tank,
#                  xlab='', ylab='Pressure (psi)'))
#   }
# })
# 
# output$tank_ts_frame <- renderUI({
#   input$tank_reset
#   Tank <- input$tank_num
#   Tanks <- getTanks()
#   if(!is.null(Tank) && nchar(Tank) > 0 && Tank %in% names(Tanks)){
#     box(width=NULL, plotOutput('tank_ts'))
#   } else ''
# })


# ---------------------------------------------------------------------------------
# TANK VIEW UI
output$tank_view_ui <- renderUI({
  tagList(
    h5('Choose tank to view history'),
    selectInput('tank_num_view', NULL, choices=c('', Tanknames())),
    fixedRow(
      column(width=2, actionButton('tank_view_reset', 'Reset', icon=icon('repeat'))),
      column(width=2, checkboxInput('show_adv_view', 'Advanced'))
    )
  )
})



# ---------------------------------------------------------------------------------
# TANK ADD UI
output$tank_add_ui <- renderUI({
  if(USER$level == 'admin'){
    tagList(
      p('Add a new editable tank to the master list.'),
      textInput('tank_add_num', h5('Tank Number')),
      fixedRow(
        column(width=2, actionButton('tank_add_save', 'Add new tank')), 
        column(width=2, checkboxInput('show_adv_add', 'Advanced'))
      )
    )
  } else{
    p("Sorry, but you don't have permissions to do this.")
  }
})


# ---------------------------------------------------------------------------------
# TANK EDIT UI
output$tank_edit_ui <- renderUI({
  input$tank_save; input$tank_remove_trigger; input$tank_add_save
  
  if(USER$level == 'admin'){
    isolate(tagList(
      h3('Tank Info'),
      textInput('tank_time', 'Time', value=format(Sys.time(), '%Y-%m-%d %H:%M')),
      fluidRow(
        column(width=4,
               selectInput('tank_num', 'Tank Number', choices=c('', Tanknames())),
               textInput('tank_psi', 'Pressure (psi)')
        ),
        column(width=4,
               textInput('tank_serial', 'Tank Serial'),
               textInput('tank_location', 'Location')
        ),
        column(width=4, 
               textInput('tank_id', 'ID'),
               textInput('tank_network', 'Network')
        )
      ),
      
      fluidRow(column(4, textInput('tank_co2', 'CO2 (ppm)')),
               column(4, textInput('tank_ch4', 'CH4 (ppm)')),
               column(4, textInput('tank_co', 'CO (ppb)'))),
      h5('Notes'),
      tags$textarea(id="tank_note", rows=5),
      checkboxInput('show_adv_edit', 'Advanced', value=F),
      conditionalPanel('input.show_adv_edit==true', 
                       hr(),
                       h3('Quality'),
                       fluidRow(
                         column(4,
                                textInput('tank_relative', 'Relative to')
                                ),
                         column(4,
                                textInput('tank_calfile', 'Calibration file'))
                         ),
                       fluidRow(
                         column(4, 
                                h4('CO2'),
                                textInput('tank_co2_stdev', 'StDev'),
                                textInput('tank_co2_sterr', 'StErr'),
                                textInput('tank_co2_n', 'N')
                         ),
                         column(4, 
                                h4('CH4'),
                                textInput('tank_ch4_stdev', 'StDev'),
                                textInput('tank_ch4_sterr', 'StErr'),
                                textInput('tank_ch4_n', 'N')
                         ),
                         column(4, 
                                h4('CO'),
                                textInput('tank_co_stdev', 'StDev'),
                                textInput('tank_co_sterr', 'StErr'),
                                textInput('tank_co_n', 'N')
                         )
                       ),
                       hr(),
                       h3('Isotopes'),
                       fluidRow(
                         column(4, 
                                textInput('tank_d13C', h3('d13C')),
                                textInput('tank_d13C_stdev', 'd13C StDev'),
                                textInput('tank_d13C_sterr', 'StErr'),
                                textInput('tank_d13C_n', 'N')
                         ),
                         column(4,
                                textInput('tank_d18O', h3('d18O')),
                                textInput('tank_d18O_stdev', 'd18O StDev'),
                                textInput('tank_d18O_sterr', 'StErr'),
                                textInput('tank_d18O_n', 'N')
                         )
                       )
      ),
      hr(),
      fluidRow(column(2, actionButton('tank_save', 'Save', icon=icon('pencil-square-o'))),
               column(2, actionButton('tank_reset', 'Reset', icon=icon('repeat'))),
               column(4, actionButton('tank_emptybutton', 'Tank is empty!', icon=icon('times-circle-o')))
      )
    ))
  } else{
    p("Sorry, but you don't have permissions to do this. Use the Field Form app instead.")
  }
})

# ---------------------------------------------------------------------------------
# TANK REMOVE UI
output$tank_remove_tank_ui <- renderUI({
  if(USER$level=='admin'){
    tagList(
      p('Remove a tank record from the database. Warning: this cannot be undone.'),
      h5('Tank number'),
      fixedRow(
        column(width=2, selectInput('tank_num_remove', NULL, choices=c('', Tanknames()))),
        column(width=2, checkboxInput('show_adv_remove', 'Advanced'))
      )
    )
  } else p("Sorry, but you don't have permissions to do this.")
})


# ---------------------------------------------------------------------------------
# DELETE RECORD UI
output$tank_remove_time_ui <- renderUI({
  remove_tank <- input$tank_num_remove
  if(!is.null(remove_tank) && nchar(remove_tank) > 0){
    Tanks <- getTanks()
    Times <- as.character(Tanks[[remove_tank]]$Time)
    Times <- na.omit(Times)
    tagList(
      selectInput('tank_remove_time', h5('Time of record'), choices=c('', Times)),
      actionButton('tank_remove_trigger', 'Delete record')
    )
  } else ''
})

# ---------------------------------------------------------------------------------
# DELETE RECORD EVENT
observeEvent(input$tank_remove_trigger, {
  Tank <- input$tank_num_remove
  remove_time <- input$tank_remove_time
  if (!is.null(Tank) && nchar(Tank) > 0) {
    Tanks <- getTanks()
    if(Tank %in% names(Tanks)){
      if(!is.null(remove_time) && nchar(remove_time) > 0){
        remove_time <- as.POSIXct(remove_time)
      Tanks[[Tank]] <- subset(Tanks[[Tank]], Time != remove_time)
      } else if(nrow(Tanks[[Tank]]) < 2){
        Tanks[Tank] <- NULL
      }
    } else return()
    isolate(dt$refresh <- F)
    saveRDS(Tanks, format(Sys.time(), 'tanks/%y%m%d_%H%M_tankdata.rds'))
    updateSelectInput(session, 'tank_num_remove', selected='')
    isolate(dt$refresh <- T)
  }
})

# ---------------------------------------------------------------------------------
# DOWNLOAD TANK
output$download_trigger <- downloadHandler(
  filename = function() {
    rdsname <- tail(dir('tanks', pattern='.*.{1}rds'), 1)
    othername <- substr(rdsname, 1, nchar(rdsname)-4)
    switch(input$download_type,
           'Serialized R dataset (.rds)' = rdsname,
           'Tab separated (.dat)' = paste0(othername, '.dat'),
           'Matlab structure (.mat)' = paste0(othername, '.mat'))
  },
  content = function(file) {
    d <- getTanks()
    ascii <- ldply(d, .id='Num', .fun=function(x){data.frame(x)})[ , 1:30]
    switch(input$download_type,
           'Serialized R dataset (.rds)' = saveRDS(d, file),
           'Tab separated (.dat)' = write.table(ascii, file, sep='\t\t\t', row.names=F, quote=F),
           'Matlab structure (.mat)' = writeMat(file, tank_data=d))
  }
)

# ---------------------------------------------------------------------------------
# EMPTY TANK
observeEvent(input$tank_emptybutton, {
  textfields <- c('tank_location', 'tank_network', 'tank_co2', 'tank_ch4', 'tank_co', 
                  'tank_note', 'tank_relative', 'tank_co2_stdev', 'tank_co2_sterr', 
                  'tank_co2_n', 'tank_ch4_stdev', 'tank_ch4_sterr', 'tank_ch4_n', 'tank_co_stdev', 
                  'tank_co_sterr', 'tank_co_n', 'tank_d13C', 'tank_d18O', 'tank_d13C_stdev', 'tank_d13C_sterr',
                  'tank_d13C_n', 'tank_d18O_stdev','tank_d18O_sterr','tank_d18O_n')
  for (field in textfields) updateTextInput(session, field, value='')
  updateTextInput(session, 'tank_psi', value=0)
})