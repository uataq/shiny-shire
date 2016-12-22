# Ben Fasoli
source('global.R')

function(input, output, session) {
  
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
  scale_color <- function(rng, val) {
    cols <- c('teal', 'yellow', 'red')
    len <- length(cols)
    brk <- seq(rng[1], rng[2], length.out = len)
    # if (val < brk[1]) return(cols[1])
    # if (val < brk[2]) return(cols[2])
    # return(cols[3])
    if (val < rng[1]) return(cols[1])
    if (val > rng[2]) return(cols[3])
    return(cols[2])
  }
  output$value_temp_c <- renderValueBox({
    val <- round(met_fetch()$temp_c, 1)
    valueBox(HTML(val, '<sup>o</sup>C'), 'Air Temperature',
             color = scale_color(c(-3, 15), val),
             icon('thermometer-quarter'), width = NULL)
  })
  output$value_wspd_ms <- renderValueBox({
    val <- round(met_fetch()$wspd_ms, 1)
    valueBox(HTML(val, 'm/s'), 'Wind Speed',
             color = scale_color(c(3, 7), val),
             icon('arrow-circle-o-right'), width = NULL)
  })
  output$value_rh_pct <- renderValueBox({
    val <- round(met_fetch()$rh_pct, 1)
    valueBox(HTML(val, '%'), 'Relative Humidity',
             color = scale_color(c(20, 70), val),
             icon('tint'), width = NULL)
  })
  output$value_wchil_c <- renderValueBox({
    val <- round(met_fetch()$wchil_c, 1)
    valueBox(HTML(val, '<sup>o</sup>C'), 'Wind Chill',
             color = scale_color(c(-3, 15), val),
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
    
    hdp <- '/projects/data/dbk/licor-6262/calibrated/' %>%
      dir(full.names = T) %>%
      tail(2) %>%
      lapply(read_csv, col_types = 'Td______c',
             locale = locale(tz = 'UTC')) %>%
      bind_rows() %>%
      mutate(site_id = 'Hidden Peak')
    
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
    
    hexcolors <- c('#00A65A', # Green,
                   '#DD4B39', # Red
                   '#FF851B', # Yellow
                   '#0073B7')
    plot_ly(data = filter(ag, site_id != 'Hidden Peak'),
            x = ~Time_UTC, y = ~CO2d_ppm_cal, color = ~site_id,
            colors = hexcolors[c(1,2,4)], line = list(dash = 3)) %>%
      add_lines(opacity = 0.5) %>%#, fill = 'tozeroy') %>%
      add_lines(data = filter(ag, site_id == 'Hidden Peak'),
                fill = 'tozeroy', line = list(dash = 'solid')) %>%
      layout(
        legend = list(x = 0, y = 1),
        xaxis = list(title = '',
                     showgrid = F),
        yaxis = list(title = 'CO2 [ppm]',
                     range = range(ag$CO2d_ppm_cal, na.rm = T),
                     showgrid = F)
      )
  })
  
  output$map <- renderLeaflet({
    f <- readRDS('/home/benfasoli/cron/air.utah.edu/data/airmap.rds')$fixed
    minmax <- c(400, 500)
    cpal <- colorNumeric(c('green', 'yellow', 'orange', 'red'),
                         seq(minmax[1], minmax[2], length.out=64))
    
    leaf <- leaflet() %>%
      addTiles(
        'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png',
        'UATAQ, Ben Fasoli. Basemap from CartoDB') %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}',
               options = tileOptions(opacity = 0.5)) %>%
      setView(lng=-111.9, lat=40.64, zoom=10)
    
    # Add mobile data points ---------------------------------------------------
    f$color <- f$CO2d_ppm
    f$color[f$color < minmax[1]] <- minmax[1]
    f$color[f$color > minmax[2]] <- minmax[2]
    
    leaf <- leaf %>%
      addCircleMarkers(lng=f$lon, lat=f$lat, 
                       radius=10, weight=2,
                       opacity=0.5, fillOpacity=0.5,
                       fillColor=cpal(f$color), color=cpal(f$color)) %>%
      addLegend(pal = cpal, values = f$color,
                title = 'CO2')
  })
  
}