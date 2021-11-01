# Ben Fasoli
source('global.R')

rdsfile <- '/home/benfasoli/cron/air.utah.edu/_data/airmap.rds'
function(input, output, session) {
  
  # Fetch data_frame containing the last 10 days of WBB observations
  # Raw data for metone-es642 and teledyne-t400
  # Calibrated data for lgr-ugga
  getData <- function() {
    
    getInstData <- function(loc) {
      read_base <- function(., ...) read_csv(., locale = locale(tz = 'UTC'),
                                             progress = F, ...)
      fun <- switch(basename(loc),
                    'metone-es642' = function(.) read_base(., col_types = 'Td______',
                                                           col_names = c('Time', 'PM2.5')),
                    'teledyne-t400' = function(.) read_base(., col_types = 'T_____d___',
                                                            col_names = c('Time', 'O3')),
                    'lgr-ugga' = function(.) read_base(., col_types = 'Td______d_______',
                                                       col_names = c('Time', 'CO2', 'CH4')))
      funw <- function(.) suppressWarnings(fun(.))
      
      N <- switch(basename(loc),
                    'metone-es642' = 10,
                    'teledyne-t400' = 10,
                    'lgr-ugga' = 2)
      
      loc %>%
        dir(full.names = T) %>%
        head(1) %>%
        dir(full.names = T) %>%
        tail(N) %>%
        lapply(funw) %>%
        bind_rows() %>%
        filter(Time >= (Sys.time() - 10*86400))
    }
    
    file.path('/projects/data/wbb', c('lgr-ugga', 'metone-es642', 'teledyne-t400')) %>%
      lapply(getInstData) %>%
      bind_rows() %>%
      group_by(Time = trunc(Time, units = 'mins') %>% as.POSIXct()) %>%
      summarize_all(funs(mean(., na.rm = T))) %>%
      mutate(`PM2.5` = `PM2.5` * 1000)
  }
  
  data <- getData()
  currently <- data %>%
    select(-Time) %>%
    tail(5 * 3600) %>%
    summarize_all(funs(round(mean(., na.rm = T), 1)))
  
  # Get mapped data ------------------------------------------------------------
  df <- reactiveFileReader(30000, session, 
                           rdsfile,
                           readRDS)
  
  # Get Mesowest API weather data ----------------------------------------------
  api <- fromJSON('http://api.mesowest.net/v2/stations/latest?&token=cbcb914c5e3a4d14a24e455ac063371f&stids=WBB&vars=wind_speed,air_temp')
  currently$Temp <- api$STATION$OBSERVATIONS$air_temp_value_1$value %>%
    (function(.) . * 1.8 + 32) # F
  currently$Wspd <- api$STATION$OBSERVATIONS$wind_speed_value_1$value %>%
    (function(.) . * 2.23694) # mph
  
  # Value boxes ----------------------------------------------------------------
  output$wbb_temp <- renderValueBox({
    value <- currently$Temp
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'black'
    } else {
      if (value < 10) { color <- 'blue'
      } else if (value >= 10 && value < 30) { color <- 'cyan'
      } else if (value >= 30 && value < 50) { color <- 'green'
      } else if (value >= 50 && value < 70) { color <- 'yellow'
      } else if (value >= 70 && value < 90) { color <- 'orange'
      } else if (value >= 90) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(HTML(value, '<sup>o</sup>F'), HTML('Air Temperature'), 
             icon('thermometer'), color)
  })
  
  output$wbb_wspd <- renderValueBox({
    value <- currently$Wspd
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'black'
    } else {
      if (value < 4) { color <- 'green'
      } else if (value >= 4 && value < 8) { color <- 'yellow'
      } else if (value >= 8 && value < 16) { color <- 'orange'
      } else if (value >= 16) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(paste(value, 'mph'), HTML('Wind Speed'), 
             icon('tachometer'), color)
  })
  
  output$wbb_co2 <- renderValueBox({
    value <- currently$CO2
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'black'
    } else {
      if (value < 430) { color <- 'green'
      } else if (value >= 430 && value < 460) { color <- 'yellow'
      } else if (value >= 460 && value < 520) { color <- 'orange'
      } else if (value >= 520) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(paste(value, 'ppm'), HTML('CO<sub>2</sub> concentration'), 
             icon('cloud'), color)
  })
  
  output$wbb_ch4 <- renderValueBox({
    value <- currently$CH4
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'black'
    } else {
      if (value < 2.0) { color <- 'green'
      } else if (value >= 2.0 && value < 2.3) { color <- 'yellow'
      } else if (value >= 2.3 && value < 2.5) { color <- 'orange'
      } else if (value >= 2.5) { color <- 'red'
      }
      value <- round(value, 2)
    }
    valueBox(paste(value, 'ppm'), HTML('CH<sub>4</sub> concentration'), 
             icon('industry'), color)
  })
  
  output$wbb_o3 <- renderValueBox({
    value <- currently$O3
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'red'
    } else {
      if (value < 20) { color <- 'green'
      } else if (value >= 20 && value < 40) { color <- 'yellow'
      } else if (value >= 40 && value < 70) { color <- 'orange'
      } else if (value >= 70) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(paste(value, 'ppb'), HTML('O<sub>3</sub> concentration'), 
             icon('sun-o'), color)
  })
  
  output$wbb_pm25 <- renderValueBox({
    value <- currently$`PM2.5`
    if (length(value) < 1 || is.na(value)) {
      value <- '?'
      color <- 'red'
    } else {
      if (value < 15) { color <- 'green'
      } else if (value >= 15 && value < 30) { color <- 'yellow'
      } else if (value >= 30 && value < 45) { color <- 'orange'
      } else if (value >= 45) { color <- 'red'
      }
      value <- round(value)
    }
    valueBox(HTML(value, 'ug m<sup>3</sup>'), HTML('PM<sub>2.5</sub> concentration'), 
             icon('car'), color)
  })
  
  # Produce timeseries ---------------------------------------------------------
  output$ts <- renderPlot({
      d <- data
      attributes(d$Time)$tzone <- 'America/Denver'
      minmax <- c(400, 550)
      d$color <- d$CO2
      d$color[d$color < minmax[1]] <- minmax[1]
      d$color[d$color > minmax[2]] <- minmax[2]
      co2 <- ggplot(data=d,
                    aes(x=Time, y=CO2, color=color)) +
        geom_point(alpha=0.3) +
        scale_color_gradientn(colors=c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                              limits = minmax, guide=F) +
        scale_x_datetime(date_breaks='1 day', date_labels='%a') +
        xlab(NULL) +
        ylab(expression(CO[2] ~ ' (ppm)')) +
        theme_classic() +
        theme(legend.position='bottom', legend.title=element_blank())
      
      minmax <- c(1.9, 2.5)
      d$color <- d$CH4
      d$color[d$color < minmax[1]] <- minmax[1]
      d$color[d$color > minmax[2]] <- minmax[2]
      ch4 <- ggplot(data=d, aes(x=Time, y=CH4, color=color)) +
        geom_point(alpha=0.3) +
        scale_color_gradientn(colors=c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                              limits = minmax, guide=F) +
        scale_x_datetime(date_breaks='1 day', date_labels='%a') +
        xlab(NULL) +
        ylab(expression(CH[4] ~ ' (ppm)')) +
        theme_classic() +
        theme(legend.position='bottom', legend.title=element_blank())
      
      minmax <- c(10, 70)
      d$color <- d$O3
      d$color[d$color < minmax[1]] <- minmax[1]
      d$color[d$color > minmax[2]] <- minmax[2]
      o3 <- ggplot(data=d, aes(x=Time, y=O3, color=color)) +
        geom_point(alpha=0.3) +
        scale_color_gradientn(colors=c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                              limits = minmax, guide=F) +
        scale_x_datetime(date_breaks='1 day', date_labels='%a') +
        xlab(NULL) +
        ylab(expression(O[3] ~ ' (ppb)')) +
        theme_classic() +
        theme(legend.position='bottom', legend.title=element_blank())
      
      minmax <- c(0, 70)
      d$color <- d$`PM2.5`
      d$color[d$color < minmax[1]] <- minmax[1]
      d$color[d$color > minmax[2]] <- minmax[2]
      pm <- ggplot(data=d, aes(x=Time, y=`PM2.5`, color=color)) +
        geom_point(alpha=0.3) +
        scale_color_gradientn(colors=c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                              limits = minmax, guide=F) +
        scale_x_datetime(date_breaks='1 day', date_labels='%a') +
        xlab(NULL) +
        ylab(expression(PM[2.5] ~ '(' ~ mu ~ 'g' ~ m[-3] ~ ')')) +
        theme_classic() +
        theme(legend.position='bottom', legend.title=element_blank())
      cowplot::plot_grid(pm, o3, co2, ch4, ncol = 1, align = 'hv')
  })


  # Map tabs use shiny modules to define the map UI
  # for (tracer in opts$short)
  #   callModule(mod_map_srv, tracer, tab=input$tab, rdsfile = rdsfile)
  output$map <- renderLeaflet({
    tab <- 'CO2d_ppm'
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
      addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png',
               attribution = 'UATAQ, Ben Fasoli. Map tiles from CartoDB and Esri') %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}',
               options = tileOptions(opacity = 0.3)) %>%
      setView(lng=-111.88, lat=40.64, zoom=11)
    
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
                         radius=20, weight=1,
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
