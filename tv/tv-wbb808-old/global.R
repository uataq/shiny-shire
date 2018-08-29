# Ben Fasoli

# Load dependencies ------------------------------------------------------------
lapply(c('dplyr', 'ggplot2', 'leaflet', 'shiny', 'shinydashboard', 'uataq'),
       library, character.only=T)

# User accessible fields -------------------------------------------------------
opts <- data_frame(
  long = c('Carbon Dioxide (CO2)', 'Methane (CH4)', 'Ozone (O3)', 
           'Particulate Matter (PM2.5)'),
  short = c('CO2d_ppm', 'CH4d_ppm', 'O3_ppbv', 'PM25_ugm3')
)

# Modular UI -------------------------------------------------------------------
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML(
      'CO2d_ppm-map.doubleClickZoom.disable()'
    )),
    leafletOutput(ns('map'), height=420),
    div(class='time-display',
        textOutput(ns('last_time'))),
    div(class='map-control',
        actionButton(ns('goto_all'), 'All Sites', icon=icon('expand'), width=150,
                     class='btn btn-primary'),
        div(br()),
        actionButton(ns('goto_slc'), 'Salt Lake City', icon=icon('search'), width=150,
                     class='btn btn-primary'),
        # div(br()),
        img(src='SCIF_logo.png', width=150, align='middle'))#,
  )
}

# Modular Server ---------------------------------------------------------------
mod_map_srv <- function(input, output, session, tab, rdsfile) {
  df <- reactiveFileReader(30000, session, 
                           rdsfile,
                           readRDS)
  
  observeEvent(input$goto_slc, {
    ns <- NS(id)
    leafletProxy(session$ns('map')) %>%
      setView(lng=-111.9, lat=40.64, zoom=10)})
  
  observeEvent(input$goto_all, {
    ns <- NS(id)
    leafletProxy(session$ns('map')) %>%
      setView(lng=-111.2, lat=41.0, zoom=7)})
  
  output$last_time <- renderText({
    data <- df()
    times <- c(data$fixed$Time_UTC, data$mobile$Time_UTC)
    format(max(times, na.rm=T), format='Last update: %Y-%m-%d %H:%M %Z')})
  
  output$map <- renderLeaflet({
    if (is.null(tab)) return()
    data <- df()
    
    # Split data into fixed (f) and mobile (m) data frames that will be mapped
    # differently due to the spatial density of points.
    f <- na.omit(data$fixed[ ,c('Time_UTC', 'lat', 'lon', 'site', tab)]) %>%
      filter(Time_UTC > Sys.time() - 5400)
    m <- na.omit(data$mobile[ ,c('Time_UTC', 'lat', 'lon', 'site', tab)]) %>%
      filter(Time_UTC > Sys.time() - 5400)
    
    # Define point colors
    minmax <- switch(tab,
                     'CO2d_ppm'  = c(400, 550),
                     'CH4d_ppm'  = c(1.9, 2.5),
                     'O3_ppbv'   = c(0, 55),
                     'PM25_ugm3' = c(10, 90))
    cpal <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                         seq(minmax[1], minmax[2], length.out=64))
    
    leaf <- leaflet() %>%
      addTiles(
        'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png',
        'UATAQ, Ben Fasoli. Basemap from CartoDB') %>%
      setView(lng=-111.9, lat=40.64, zoom=10)
    
    # Add mobile data points ---------------------------------------------------
    for (site in unique(m$site)) {
      mask <- m$site == site
      if (nrow(m[mask, ]) > 300){
        temp <- m[mask, ]
        smoothby <- 40
        temp[ ,tab] <- ivis::runSmooth(temp[ ,tab], n=smoothby)
        m <- temp[seq(1, nrow(temp), by=smoothby), ]
      } else m <- m[mask, ]
      
      m <- na.omit(m)
      if (nrow(m) > 0) {
        m$color <- m[[tab]]
        m$color[m$color < minmax[1]] <- minmax[1]
        m$color[m$color > minmax[2]] <- minmax[2]
        
        pop<- paste(sep='<br>',
                    paste0('<b>', opts$long[opts$short == tab], '</b>'),
                    paste('Value:<b>', round(m[[tab]], 2), '</b>'),
                    paste('Last updated:  ',
                          format(m$Time_UTC, '%Y-%m-%d %H:%M %Z')),
                    'See more at air.utah.edu')
        
        leaf <- leaf %>%
          addCircleMarkers(lng=m$lon, lat=m$lat, popup=pop, 
                           radius=2, stroke=T, weight=2, 
                           opacity=0.3, fillOpacity=0.3,
                           fillColor=cpal(m$color), color=cpal(m$color)) %>%
          addCircleMarkers(lng=tail(m$lon, 1), lat=tail(m$lat, 1), 
                           popup=tail(pop, 1),
                           radius=5, stroke=T, weight=2,
                           opacity=0.8, fillOpacity=0.8,
                           fillColor=cpal(tail(m$color, 1)), color='black')
      }
    }
    
    # Add fixed data points ----------------------------------------------------
    f <- na.omit(f)
    if (nrow(f) > 0) {
      f$color <- f[[tab]]
      f$color[f$color < minmax[1]] <- minmax[1]
      f$color[f$color > minmax[2]] <- minmax[2]
      
      pop <- paste(sep='<br>',
                   paste0('<b>', opts$long[opts$short == tab], '</b>'),
                   paste('Value:<b>', round(f[[tab]], 2), '</b>'),
                   paste('Last updated:  ',
                         format(f$Time_UTC, '%Y-%m-%d %H:%M %Z')),
                   'See more at air.utah.edu')
      
      leaf <- leaf %>%
        addCircleMarkers(lng=f$lon, lat=f$lat, popup=pop, 
                         radius=10, weight=2,
                         opacity=0.5, fillOpacity=0.5,
                         fillColor=cpal(f$color), color=cpal(f$color))
    }
    
    # Add legend ---------------------------------------------------------------
    # Legend colorscale is computed with negative numbers, forcing the highest
    # on top (opposite of leaflet default). The labels are then transformed back
    # into a positive number upon legend creation.
    cpal_lgnd <- colorNumeric(
      rev(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red')),
      -seq(minmax[1], minmax[2], length.out=64))
    
    breaks_lgnd <- -seq(minmax[1], minmax[2], length.out=10)
    
    leaf %>%
      addLegend('bottomright', pal=cpal_lgnd, values=breaks_lgnd,
                labFormat=labelFormat(transform=function(x){-x}), opacity=0.7)
  })
}
