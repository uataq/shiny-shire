# Ben Fasoli
setwd('/srv/shiny-server/map/')

# - GLOBAL -
other_sites <- data_frame(site=c('haw', 'wnd'),
                          lat=c(40.734361, 39.9018),
                          lon=c(-111.872139, -113.7181))

pretty_site_names <- data_frame(csp='Castle Peak',
                                dbk='Daybreak',
                                fru='Fruitland',
                                hdp='Hidden Peak',
                                heb='Heber',
                                hpl='Horsepool',
                                lgn='Logan',
                                roo='Roosevelt',
                                rpk='Rose Park',
                                sug='Sugarhouse',
                                sun='Suncrest', 
                                trx01='TRAX 1',
                                trx02='TRAX 2',
                                wbb='University of Utah',
                                
                                haw='Hawthorne (DAQ)',
                                wnd='Wendover')

url_link <- data_frame(csp='http://air.utah.edu/s/view/gasview/?site=',
                       dbk=csp,
                       fru=csp,
                       hdp=csp,
                       heb=csp,
                       hpl=csp,
                       lgn=csp,
                       roo=csp,
                       rpk=csp,
                       sug=csp,
                       sun=csp,
                       trx01='http://air.utah.edu/s/view/trax/',
                       trx02=trx01,
                       wbb=csp,
                       
                       haw='http://air.utah.gov/currentconditions.php?id=slc',
                       wnd='http://www.eol.ucar.edu/homes/stephens/RACCOON/')

min_max <- data_frame('Carbon Dioxide (CO2)'=c(400, 550),
               'Methane (CH4)'=c(1.9, 2.5),
               'Particulate Matter (PM2.5)'=c(0, 55),
               'Ozone (O3)'=c(10, 90),
               'Water vapor (H2O)'=c(8000,20000))
short_species <- data_frame('Carbon Dioxide (CO2)'='CO2d_ppm',
                      'Methane (CH4)'='CH4d_ppm',
                      'Particulate Matter (PM2.5)'='PM25_ugm3',
                      'Ozone (O3)'='O3_ppbv')
units <- data_frame('Carbon Dioxide (CO2)'='ppm',
              'Methane (CH4)'='ppm',
              'Particulate Matter (PM2.5)'='ug/m^3',
              'Ozone (O3)'='ppb',
              'Water vapor (H2O)'='ppm')

# view_default <- function(map) setView(map, lng=-111.9, lat=40.6338, zoom=10)
view_default <- function(map) setView(map, lng=-111.2, lat=40.7425, zoom=8)


# Per user server -------------------------------------------------------------
function(input, output, session) {
  # Query URL -----------------------------------------------------------------
  observe({
    q <- parseQueryString(isolate(session$clientData$url_search))
    if('slc' %in% names(q) && q[['slc']]=='T') {
      leafletProxy('map') %>% setView(lng=-111.9, lat=40.6338, zoom=10)
    } else if('slc' %in% names(q) && q[['slc']]=='F') {
      leafletProxy('map') %>% setView(lng=-111.2, lat=40.7425, zoom=8)
    }
    if('tracer' %in% names(q) && q[['tracer']] %in% short_species) {
      updateSelectInput(session, 'tracer', 
                        selected=names(short_species)[short_species == q[['tracer']]])
    }
  })
  
  # Data is processed on CHPC. airmap.rds contains recent observations and map locations
  #  for all measurement sites. Automatically updates every 300 seconds (5 minutes).
  d <- reactive({
    invalidateLater(300000, session)
    d <- readRDS('/srv/shiny-server/map/shared/airmap.rds')
    d$fixed[d$fixed$Time_UTC < Sys.time() - 3600*1.5, -1:-4] <- NA
    d
  })
  
  output$last_time <- renderText({
    times <- d()$fixed$Time_UTC
    format(max(times, na.rm=T), format='Last update: %Y-%m-%d %H:%M %Z')
  })
  
  # Reactivity
  data <- reactive({
    if(!is.null(input$show_other) && input$show_other){
      bind_rows(d()$fixed, other_sites)
    } else d()$fixed
  })

  pops <- reactive({
    pup.times <- format(as.POSIXct(data()$Time_UTC, origin='1970-01-01'), '%Y-%m-%d %H:%M %Z')
    pop <- character()
    for (i in 1:nrow(data())) {
      conc <-  round(as.numeric(data()[i, short_species[[input$tracer]]]), 2)
      lastupd <- pup.times[i]
      
      if (is.na(conc)) conc <- 'Unknown'
      if (is.na(lastupd)) lastupd <- 'Unknown'
      
      pop[i] <- paste(sep='<br>',
                      paste0('<b>', pretty_site_names[[data()$site[i]]], '</b>'),
                      paste(input$tracer, ':<b>', conc, '</b>', units[[input$tracer]]),
                      paste('Last updated:  ', lastupd),
                      a(target='_blank', 
                        href=paste0(url_link[[data()$site[i]]], data()$site[i]),
                        tags$button(type='button',
                                    class='btn action-button btn-large btn-danger',
                                    HTML('<i class="fa fa-line-chart"></i>Go to the data!'))))
    }
    pop
  })
  
  # Create map base
  output$map <- renderLeaflet(
    leaflet() %>% 
      addTiles(urlTemplate='http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png',
               attribution='UATAQ, Ben Fasoli. Basemap from CartoDB') %>%
      # addProviderTiles("Acetate.terrain", options = providerTileOptions(opacity = 0.1)) %>%
      view_default)
  
  # Overlay points on map
  observe({
    dat <- data()[c('Time_UTC', 'lat', 'lon', 'site', short_species[[input$tracer]])]
    
    # Optionally remove NA (gray) points
    #     idx <- apply(d, 1, function(x){!any(is.na(x))})
    #     mask.t <- dat$Time > (Sys.time()-60*60*1.5)
    #     mask <- !(mask.t & is.na(dat[ , short_species()])) #& mask.t
    #     mask[is.na(mask)] <- TRUE
    #     dat<- dat[mask, ]
    #     pop <- pops()[mask]
    
    
    # Clear previous options on map refresh
    leaf <- leafletProxy('map') %>%
      clearShapes()   %>%
      clearControls() %>%
      clearMarkers()
    
    # Change color scale according to species
    cpal <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                         seq(min_max[[input$tracer]][1], min_max[[input$tracer]][2], length.out=64))
    
    if (nrow(dat) > 0) {
      pop <- pops()
      # Color is scaled between defined min/max
      color <- dat[[short_species[[input$tracer]]]]
      color[color < min_max[[input$tracer]][1]] <- min_max[[input$tracer]][1]
      color[color > min_max[[input$tracer]][2]] <- min_max[[input$tracer]][2]
      
      # Add to basemap
      leaf <- leaf  %>%
        addCircleMarkers(lng=dat$lon, lat=dat$lat, pop=pop, radius=10, weight=2,
                         fillColor=cpal(color), color=cpal(color), 
                         opacity=0.5, fillOpacity=0.5)

    }
    
    md <- d()$mobile
    if (nrow(md) > 1) {
      md <- md[ , c('Time_UTC', 'lat', 'lon', 'site', short_species[[input$tracer]])]
      md <- na.omit(md)

      for (site in unique(md$site)){
        mask <- md$site == site
        if(nrow(md[mask, ]) > 300){
          temp <- md[mask, ]
          smoothby <- 30
          temp[ ,short_species[[input$tracer]]] <- uataq::run_smooth(temp[ ,short_species[[input$tracer]]], n=smoothby)
          temp <- temp[seq(1, nrow(temp), by=smoothby), ]
        } else temp <- md[mask, ]

        mdat <- na.omit(temp)
        mcolor <- mdat[[short_species[[input$tracer]]]]
        mcolor[mcolor < min_max[[input$tracer]][1]] <- min_max[[input$tracer]][1]
        mcolor[mcolor > min_max[[input$tracer]][2]] <- min_max[[input$tracer]][2]

        mpop<- paste(sep='<br>',
                     paste0('<b>', pretty_site_names[mdat$site], '</b>'),
                     paste(input$tracer, ':<b>', round(mdat[[short_species[[input$tracer]]]], 2), '</b>', units[[input$tracer]]),
                     paste('Last updated:  ', format(mdat$Time_UTC, '%Y-%m-%d %H:%M %Z')),
                     a(target='_blank',
                       href=url_link[mdat$site],
                       tags$button(type='button',
                                   class='btn action-button btn-large btn-danger',
                                   HTML('<i class="fa fa-line-chart"></i>Go to the data!'))))

        leaf <- leaf %>%
          addCircleMarkers(lng=mdat$lon, lat=mdat$lat, radius=2, popup=mpop, stroke=T, weight=2,
                           fillColor=cpal(mcolor), color=cpal(mcolor),
                           opacity=0.3, fillOpacity=0.3) %>%
          addCircleMarkers(lng=tail(mdat$lon, 1), lat=tail(mdat$lat, 1), radius=5, popup=tail(mpop, 1),
                           stroke=T, weight=2, fillColor=cpal(tail(mcolor, 1)), color='black',
                           opacity=0.8, fillOpacity=0.8)
      }
    }
    
    # Legend colorscale is computed with negative numbers, forcing the highest on top
    #  (opposite of default). The labels are then transformed back into their positive
    #  version upon legend creation.
    cpal_lgnd <- colorNumeric(rev(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red')),
                              -seq(min_max[[input$tracer]][1], min_max[[input$tracer]][2], length.out=64))
    
    breaks_lgnd <- rev(-seq(min_max[[input$tracer]][1], min_max[[input$tracer]][2], length.out=10))
    leaf %>%
      addLegend('bottomright', pal=cpal_lgnd, values=breaks_lgnd, title=paste0('(', units[[input$tracer]], ')'),
                labFormat=labelFormat(transform=function(x){-x}), opacity=0.7)
  })
}




# DEBUG
# data <- function() {readRDS('/srv/shiny-server/map/shared/airmap.rds')$fixed}
# d <- function() {readRDS('/srv/shiny-server/map/shared/airmap.rds')}
# input <- list(tracer='Carbon Dioxide (CO2)')
