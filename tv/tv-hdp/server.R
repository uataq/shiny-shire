# Ben Fasoli
source('global.R')

function(input, output, session) {
  
  hdpr <- reactive({'/projects/data/hdp/lgr-ugga/calibrated/' %>%
    dir(full.names = T) %>%
    tail(2) %>%
    lapply(read_csv, col_types = 'Td______d_______',
           locale = locale(tz = 'UTC')) %>%
    bind_rows() %>%
    mutate(site_id = 'Hidden Peak')})
  
  # Fetch met data ---------------------------------------------------------------
  met_fetch <- reactive({
    url <- 'http://api.mesowest.net/v2/stations/nearesttime?stid=amb&token=cbcb914c5e3a4d14a24e455ac063371f&within=1440'
    con <- curl(url)
    js <- fromJSON(read_lines(con))
    data_frame(
      rh_pct = js$STATION$OBSERVATIONS$relative_humidity_value_1$value,
      temp_c = js$STATION$OBSERVATIONS$air_temp_value_1$value,
      # wchil_c = js$STATION$OBSERVATIONS$wind_chill_value_1$value,
      wspd_ms = js$STATION$OBSERVATIONS$wind_speed_value_1$value,
      wspd_kmh = wspd_ms * 3.6,
      wchil_c = 13.12 + 0.6215*temp_c - 11.37*(wspd_kmh)^0.16 +
        0.3975*temp_c * (wspd_kmh)^0.16
    )
  })
  
  # Value Boxes ----------------------------------------------------------------
  scale_color1 <- function(rng, val) {
    cols <- c('teal', 'yellow', 'red')
    len <- length(cols)
    brk <- seq(rng[1], rng[2], length.out = len)
    if (val < rng[1]) return(cols[1])
    if (val > rng[2]) return(cols[3])
    return(cols[2])
  }
  scale_color2 <- function(rng, val) {
    cols <- c('green', 'yellow', 'red')
    len <- length(cols)
    brk <- seq(rng[1], rng[2], length.out = len)
    if (val < rng[1]) return(cols[1])
    if (val > rng[2]) return(cols[3])
    return(cols[2])
  }
  output$value_co2_ppm <- renderValueBox({
    val <- round(mean(tail(hdpr()$CO2d_ppm_cal, 6), na.rm = T), 1)
    valueBox(HTML(val, 'ppm'), 'Carbon Dioxide',
             color = scale_color2(c(420, 450), val),
             icon('leaf'), width = NULL)
  })
  output$value_ch4_ppb <- renderValueBox({
    val <- round(mean(tail(hdpr()$CH4d_ppm_cal, 6), na.rm = T) * 1000)
    valueBox(HTML(format(val, big.mark = ','), 'ppb'), 'Methane',
             color = scale_color2(c(2000, 2100), val),
             icon('home'), width = NULL)
  })
  output$value_temp_f <- renderValueBox({
    val <- round(met_fetch()$temp_c, 1) * 1.8 + 32
    valueBox(HTML(val, '<sup>o</sup>F'), 'Air Temperature',
             color = scale_color1(c(20, 50), val),
             icon('thermometer-quarter'), width = NULL)
  })
  output$value_wspd_ms <- renderValueBox({
    val <- round(met_fetch()$wspd_ms, 1)
    valueBox(HTML(val, 'm/s'), 'Wind Speed',
             color = scale_color2(c(5, 10), val),
             icon('arrow-circle-o-right'), width = NULL)
  })
  output$value_rh_pct <- renderValueBox({
    val <- round(met_fetch()$rh_pct, 1)
    valueBox(HTML(val, '%'), 'Relative Humidity',
             color = scale_color2(c(30, 70), val),
             icon('tint'), width = NULL)
  })
  output$value_wchil_c <- renderValueBox({
    val <- round(met_fetch()$wchil_c, 1)
    valueBox(HTML(val, '<sup>o</sup>C'), 'Wind Chill',
             color = scale_color1(c(20, 50), val),
             icon('snowflake-o'), width = NULL)
  })
  
  output$timeseries <- renderPlotly({
    # hdp <- '/projects/data/hpl/lgr-ugga/calibrated/' %>%
    #   dir(full.names = T) %>%
    #   tail(2) %>%
    #   lapply(read_csv, col_types = 'Td______d______c',
    #          locale = locale(tz = 'UTC')) %>%
    #   bind_rows() %>%
    #   mutate(site_id = 'Hidden Peak')
    
    hdp <- hdpr()
    
    imc <- '/projects/data/imc/licor-6262/calibrated/' %>%
      dir(full.names = T) %>%
      tail(2) %>%
      lapply(read_csv, col_types = 'Td_______',
             locale = locale(tz = 'UTC')) %>%
      bind_rows() %>%
      mutate(site_id = 'Murray')
    
    sug <- '/projects/data/sug/licor-6262/calibrated/' %>%
      dir(full.names = T) %>%
      tail(2) %>%
      lapply(read_csv, col_types = 'Td_______',
             locale = locale(tz = 'UTC')) %>%
      bind_rows() %>%
      mutate(site_id = 'Sugarhouse')
    
    wbb <- '/projects/data/wbb/lgr-ugga/calibrated/' %>%
      dir(full.names = T) %>%
      tail(2) %>%
      lapply(read_csv, col_types = 'Td______d_______',
             locale = locale(tz = 'UTC')) %>%
      bind_rows() %>%
      mutate(site_id = 'University of Utah')
    
    ag <- bind_rows(hdp, imc, sug, wbb) %>%
      filter(CO2d_ppm_cal > 390) %>%
      filter(Time_UTC > Sys.time() - 5 * 86400) %>%
      group_by(site_id,
               Time_UTC = trunc(Time_UTC, unit = 'hours') %>%
                 as.POSIXct(tz = 'UTC')) %>%
      summarize_each(funs(mean(., na.rm = T)), CO2d_ppm_cal, CH4d_ppm_cal)
    attributes(ag$Time_UTC)$tzone <- 'America/Denver'
    
    
    plot_ly(data = filter(ag, site_id != 'Hidden Peak'),
            x = ~Time_UTC, y = ~CO2d_ppm_cal, color = ~site_id,
            colors = c('#56cccd', '#c2a801', '#c20702')) %>%
      add_lines(opacity = 0.9) %>%
      add_lines(data = filter(ag, site_id == 'Hidden Peak'),
                fill = 'tozeroy', fillcolor = '#903293d3',
                line = list(color = '#e075b432', dash = 'solid', width = 5)) %>%
      layout(
        legend = list(x = 0, y = 1),
        xaxis = list(title = '',
                     showgrid = F),
        yaxis = list(title = 'Carbon Dioxide [ppm]',
                     range = range(ag$CO2d_ppm_cal, na.rm = T),
                     showgrid = F))
  })
  
  output$map <- renderLeaflet({
    recent <- readRDS('/home/benfasoli/cron/air.utah.edu/data/airmap.rds')
    f <- recent$fixed
    m <- recent$mobile %>%
      select(Time_UTC, lat, lon, CO2d_ppm) %>%
      na.omit()
    minmax <- c(400, 500)
    cpal <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                         seq(minmax[1], minmax[2], length.out=64))
    
    leaf <- leaflet() %>%
      addTiles(
        'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png',
        'UATAQ, Ben Fasoli. Basemap from CartoDB') %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}',
               options = tileOptions(opacity = 0.5)) %>%
      setView(lng=-111.75, lat=40.64, zoom=10)
    
    # Add mobile data points ---------------------------------------------------
    f$color <- f$CO2d_ppm
    f$color[f$color < minmax[1]] <- minmax[1]
    f$color[f$color > minmax[2]] <- minmax[2]
    
    m$color <- m$CO2d_ppm
    m$color[m$color < minmax[1]] <- minmax[1]
    m$color[m$color > minmax[2]] <- minmax[2]
    
    glong <- seq(-113, -110, by = 0.1)
    glati <- seq(39, 43, by = 0.1)
    m <- m %>%
      group_by(lon = lon[findInterval(lon, glong)],
               lat = lat[findInterval(lat, glati)]) %>%
      summarize(color = mean(color, na.rm = T)) %>%
      ungroup()
    
    leaf %>%
      addCircleMarkers(lng=f$lon, lat=f$lat, 
                       radius=10, weight=2,
                       opacity=0.5, fillOpacity=0.5,
                       fillColor=cpal(f$color), color=cpal(f$color)) %>%
      addCircleMarkers(lng=m$lon, lat=m$lat, radius = 5, weight = 0,
                       opacity = 0.1, fillOpacity = 0.1,
                       fillColor=cpal(m$color), color=cpal(m$color)) %>%
      addCircleMarkers(lng=tail(m$lon, 1), lat=tail(m$lat,1), radius = 5,
                       weight = 1, opacity = 0.5, fillOpacity = 0,
                       fillColor=cpal(m$color), color='#000000') %>%
      addLegend(pal = cpal, values = c(f$color, m$color), title = HTML('ppm CO<sub>2</sub>')) #%>%
      # addMarkers(lng = -111.61, lat = 40.53,
      #            icon = makeIcon(iconUrl = 'http://www.gmkfreelogos.com/logos/S/img/Snowbird-1.gif',
      #                            iconWidth = 40, iconHeight = 40))
  })
  
}