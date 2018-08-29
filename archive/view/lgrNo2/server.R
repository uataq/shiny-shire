# UNIVERSITY OF UTAH AIR QUALITY AND TRACE GAS LAB
# Ben Fasoli 

lapply(c('dygraphs', 'leaflet', 'shinydashboard', 'xts', 'dplyr', 'readr', 'uataq'), 
       library, character.only=T)

# Server -----------------------------------------------------------------------
function(input, output, session) {
  # Import data
  data <- reactive({
    invalidateLater(300000, session)
    lgr <- '/projects/data/trx02/raw/lgrNo2/' %>%
      dir(full.names=T) %>%
      tail(1) %>%
      read_lines() %>%
      breakstr() %>%
      rename(Time_UTC = V1,
             Concentration_ppb = V4,
             Pressure_torr = V6,
             Temperature_C = V7,
             Instrument_Temperature_C = V9,
             Valve_desc = V22) %>%
      filter(Valve_desc == 'Sample') %>%
      select(Time_UTC, Concentration_ppb, Pressure_torr, 
             Temperature_C, Instrument_Temperature_C) %>%
      mutate(Time_UTC = as.POSIXct(trunc(as.POSIXct(Time_UTC, tz='UTC'), 'secs'))) %>%
      mutate_each(funs(as.numeric(.)), -Time_UTC)
    gps <- '/projects/data/trx02/raw/gps/' %>%
      dir(full.names=T) %>%
      tail(1) %>%
      read_lines() %>%
      grep('GPGGA', x=., fixed=T, value=T) %>%
      breakstr() %>%
      rename(Time_UTC = V1, lat = V4, lon = V6) %>%
      select(Time_UTC, lat, lon) %>%
      mutate(Time_UTC = as.POSIXct(trunc(as.POSIXct(Time_UTC, tz='UTC'), 'secs')),
             lat = gps_dm2dd(lat),
             lon = -gps_dm2dd(lon))
    left_join(lgr, gps, by='Time_UTC')
  })
  
  long_data <- reactive({
    invalidateLater(300000, session)
    '/projects/data/trx02/raw/lgrNo2/' %>%
      dir(full.names=T) %>%
      tail(2) %>%
      lapply(read_lines) %>%
      unlist() %>%
      breakstr() %>%
      rename(Time_UTC = V1,
             Concentration_ppb = V4,
             Pressure_torr = V6,
             Temperature_C = V7,
             Instrument_Temperature_C = V9,
             Valve_desc = V22) %>%
      filter(Valve_desc == 'Sample') %>%
      select(Time_UTC, Concentration_ppb, Pressure_torr, 
             Temperature_C, Instrument_Temperature_C) %>%
      mutate(Time_UTC = as.POSIXct(trunc(as.POSIXct(Time_UTC, tz='UTC'), 'secs'))) %>%
      mutate_each(funs(as.numeric(.)), -Time_UTC)
  })
  
  
  # Outputs --------------------------------------------------------------------
  output$leaf <- renderLeaflet({
    tab <- input$tab
    validate(need(!is.null(tab), ''))
    
    md <- na.omit(data()[ ,c('Time_UTC', 'lat', 'lon', tab)]) %>%
      filter(Time_UTC > Sys.time() - 3600 * 1.5)
    validate(need(nrow(md) > 1, 'No data found...'))
    
    # minmax <- switch(tab,
    #                  'co2' = c(400, 550),
    #                  'ch4' = c(1.9, 2.5),
    #                  'o3'  = c(0, 55),
    #                  'pm' = c(10, 90))
    minmax <- range(md[ ,tab])
    
    cpal <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                         seq(minmax[1], minmax[2], length.out=64))
    
    leaf <- leaflet() %>%
      addTiles(urlTemplate='http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png',
               attribution='UATAQ, Ben Fasoli. Basemap from CartoDB') %>%
      setView(lng=-111.9, lat=40.6338, zoom=10)
    
    if(nrow(md) > 300){
      temp <- md
      smoothby <- 10
      temp[ ,tab] <- ivis::runSmooth(temp[ ,tab], n=smoothby)
      temp <- temp[seq(1, nrow(temp), by=smoothby), ]
    } else temp <- md
    
    # Specify color index according to max and min
    dat <- na.omit(temp)
    dat$color <- dat[ ,tab]
    # dat$color <- scales::rescale(dat[ , tab], to=minmax)
    
    pop<- paste(sep='<br>',
                paste('Value:<b>', round(dat[[tab]], 2), '</b>'),
                paste('Last updated:  ', format(dat$Time_UTC, '%Y-%m-%d %H:%M %Z')))
    
    leaf <- leaf %>%
      addCircleMarkers(lng=dat$lon, lat=dat$lat, radius=2, popup=pop, stroke=T, weight=2,
                       fillColor=cpal(dat$color), color=cpal(dat$color),
                       opacity=0.3, fillOpacity=0.3) %>%
      addCircleMarkers(lng=tail(dat$lon, 1), lat=tail(dat$lat, 1), radius=5, popup=tail(pop, 1),
                       stroke=T, weight=2, fillColor=cpal(tail(dat$color, 1)), color='black',
                       opacity=0.8, fillOpacity=0.8)
    
    # Legend colorscale is computed with negative numbers, forcing the highest on top
    #  (opposite of default). The labels are then transformed back into their positive
    #  version upon legend creation.
    cpal_lgnd <- colorNumeric(
      rev(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red')),
      -seq(minmax[1], minmax[2], length.out=64))
    
    breaks_lgnd <- -seq(minmax[1], minmax[2], length.out=10)
    
    leaf %>%
      addLegend('bottomright', pal=cpal_lgnd, values=breaks_lgnd,
                labFormat=labelFormat(transform=function(x){-x}), opacity=0.7)
  })
  
  output$ts <- renderDygraph({
    # md <- data()
    # l <- opts$short[opts$long == input$left]
    # r <- opts$short[opts$long == input$right]
    
    # if (length(r) < 1) r <- NULL
    tab <- input$tab
    
    md <- long_data()[ ,c('Time_UTC', tab)]
    
    validate(need(nrow(md) > 0, ''))
    # temp <- temp[!apply(temp[-1], 1, function(x){all(is.na(x))}), ]
    
    # temp <- na.omit(temp)
    
    validate(need(nrow(md) > 1, 'No data found. Try a different dataset?'))
    
    d <- xts(md[-1], md$Time_UTC)
    
    dygraph(d , xlab='Time (Local)') %>%
      dyHighlight(highlightCircleSize = 3,
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = T) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),
                connectSeparatedPoints = T)
  })
}
