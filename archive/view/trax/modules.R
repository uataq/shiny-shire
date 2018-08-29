# Load dependencies -----------------------------------------------------------
lapply(c('dygraphs', 'leaflet', 'shinydashboard', 'xts', 'dplyr'), 
       library, character.only=T)

# User accessible fields ------------------------------------------------------
opts <- data_frame(
  long = c('Carbon Dioxide (CO2)', 'Methane (CH4)', 'Ozone (O3)', 'Particulate Matter (PM2.5)', 
           'Temperature', 'Relative Humidity', 'Pressure'),
  short = c('CO2d_ppm', 'CH4d_ppm', 'O3_ppbv', 'PM25_ugm3', 
            'amb_T_C', 'amb_RH_pct', 'case_P_hPa')
)

# Modularized UI --------------------------------------------------------------
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns('leaf'), height=500),
    hr(),
    dygraphOutput(ns('ts'))
  )
}

# Modularized Server ----------------------------------------------------------
map <- function(input, output, session, data, tab) {
  output$leaf <- renderLeaflet({
    validate(need(!is.null(tab), ''))
    
    md <- na.omit(data[ ,c('Time_UTC', 'lat', 'lon', 'site', tab)])
    validate(need(nrow(md) > 1, 'No data found... Try http://meso1.chpc.utah.edu/mesotrax/ for historic data.'))
    
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
    
    for (site in unique(md$site)) {
      mask <- md$site == site
      if(nrow(md[mask, ]) > 300){
        temp <- md[mask, ]
        smoothby <- 30
        temp[ ,tab] <- uataq::run_smooth(temp[ ,tab], n=smoothby)
        temp <- temp[seq(1, nrow(temp), by=smoothby), ]
      } else temp <- md[mask, ]
      
      # Specify color index according to max and min
      dat <- na.omit(temp)
      dat$color <- dat[[tab]]
      # dat$color <- scales::rescale(dat[ , tab], to=minmax)
      
      pop<- paste(sep='<br>',
                  paste0('<b>', opts$long[opts$short == tab], '</b>'),
                  paste('Value:<b>', round(dat[[tab]], 2), '</b>'),
                  paste('Last updated:  ', format(dat$Time_UTC, '%Y-%m-%d %H:%M %Z')))
      
      leaf <- leaf %>%
        addCircleMarkers(lng=dat$lon, lat=dat$lat, radius=2, popup=pop, stroke=T, weight=2,
                         fillColor=cpal(dat$color), color=cpal(dat$color),
                         opacity=0.3, fillOpacity=0.3) %>%
        addCircleMarkers(lng=tail(dat$lon, 1), lat=tail(dat$lat, 1), radius=5, popup=tail(pop, 1),
                         stroke=T, weight=2, fillColor=cpal(tail(dat$color, 1)), color='black',
                         opacity=0.8, fillOpacity=0.8)
    }
    
    # Legend colorscale is computed with negative numbers, forcing the highest on top
    #  (opposite of default). The labels are then transformed back into their positive
    #  version upon legend creation.
    # cpal_lgnd <- colorNumeric(rev(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red')),
    #                           -seq(minmax[1], minmax[2], length.out=64))
    # breaks_lgnd <- rev(-seq(minmax[1], minmax[2], length.out=10))
    breaks_lgnd <- seq(minmax[1], minmax[2], length.out=10)
    
    leaf %>%
      leaflet::addLegend('bottomright', pal=cpal, values=breaks_lgnd, opacity=0.7)
    # conflict with addLegend xts
  })
  
  output$ts <- renderDygraph({
    # data <- md()
    # l <- opts$short[opts$long == input$left]
    # r <- opts$short[opts$long == input$right]
    
    # if (length(r) < 1) r <- NULL
    l <- tab
    r <- NULL
    
    md <- data[ ,c('Time_UTC', 'site', l, r)]
    
    temp <- NULL
    for (trx in c('trx01', 'trx02')) {
      mask <- md$site == trx
      mask[is.na(mask)] <- F
      if (length(which(mask)) > 5 && any(!is.na(md[mask,l]))) {
        make <- data_frame(Time_UTC = md$Time_UTC[mask])
        # make[ ,paste(trx, r, sep='_'))] <- md[ ,c(l, r)]
        # make[ ,(paste(trx, l, sep='_'))] <- md[ ,l]
        make[ ,trx] <- md[mask, l]
        temp <- bind_rows(temp, make)
      }
    }
    # temp <- temp[ ,colSums(is.na(temp)) < nrow(temp)]
    validate(need(nrow(temp) > 0, ''))
    temp <- temp[!apply(temp[-1], 1, function(x){all(is.na(x))}), ]
    
    # temp <- na.omit(temp)
    
    validate(need(nrow(temp) > 1, 'No data found. Try a different dataset?'))
    
    d <- xts(temp[-1], temp$Time_UTC)
    str(d)
    
    dygraph(d , xlab='Time (Local)') %>%
      dyHighlight(highlightCircleSize = 3,
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = T) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),
                connectSeparatedPoints = T)
  })
}