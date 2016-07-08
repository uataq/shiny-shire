library(dplyr)
library(leaflet)
library(uataq)
library(shiny)

ui <- fillPage(
  leafletOutput('leaf', height='100%', width='100%'),
  absolutePanel(top=70, left=10, width=100,
                selectInput('show', NULL, 
                            c('t_c', 'rh_pct', 'p_pa', 'co2_ppm', 'pm_ugm3'),
                            'p_pa'))
)

server <- function(input, output) {
  
  output$leaf <- renderLeaflet({
    show <- input$show
    df <- readRDS('/projects/archive/urop_airmet/dat1.rds') %>%
      .[ ,c(show, 'lat', 'lon')]
    
    interp <- as_data_frame(lapply(df, na_interp)) %>%
      na.omit() %>%
      .[c(1000:3600, 10000:44600), ]
    
    interp <- interp[interp$lon < -111.818 & interp$lon > -112, ]
    
    # Grid averaging ---------------------------------------------------------------
    grid_size <- 0.0002
    domain <- list()
    domain$lat   <- seq(40.47511, 40.80111, by=grid_size)
    domain$lon   <- seq(-112.1142, -111.7782, by=grid_size)
    
    interp$grlat <- find_neighbor(interp$lat, domain$lat)
    interp$grlon <- find_neighbor(interp$lon, domain$lon)
    
    obsav <- aggregate(interp[[show]], by=list(interp$grlat, interp$grlon), 
                       mean, na.rm=T)
    grd <- data_frame(tracer = obsav$x,
                      lat   = domain$lat[obsav$Group.1],
                      lon   = domain$lon[obsav$Group.2])
    
    
    
    # Create map -------------------------------------------------------------------
    cpal <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'),
                         seq(min(grd$tracer, na.rm=T), 
                             max(grd$tracer, na.rm=T), 
                             length.out=64))
    leaflet() %>%
      fitBounds(min(grd$lon, na.rm=T), min(grd$lat, na.rm=T),
                max(grd$lon, na.rm=T), max(grd$lat, na.rm=T)) %>%
      addTiles(urlTemplate='http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png',
               attribution='UATAQ, Ben Fasoli. Basemap from CartoDB') %>%
      addCircleMarkers(lng=grd$lon, lat=grd$lat, radius=5, weight=2,
                       fillColor=cpal(grd$tracer), color=cpal(grd$tracer),
                       opacity=0.3, fillOpacity=0.3) %>%
      addLegend(pal=cpal, values=grd$tracer, position='bottomleft')
  })
}

shinyApp(ui, server)
