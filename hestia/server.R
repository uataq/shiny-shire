# Ben Fasoli

source('global.R')

library(leaflet)
library(ncdf4)
library(raster)
library(shiny)

# zmin   <- 0
# zmax   <- 300000

# h <- readRDS('/projects/slcinterp/hestia_ex.rds')

ncpath <- 'data/gridded/'

function(input, output, session) {
  output$map <- renderLeaflet({
    # -----------------------------------------------------------------------
    # Get data
    city       <- input$city
    sectors    <- input$sectors
    day_range  <- input$day_range
    hour_range <- input$hour_range
    
    if (sectors==sector_opts)
      sectors <- 'Total'
    
    hoy    <- day_range * 24
    hoy[1] <- hoy[1] - 23
    
    raw <- NULL
    for (sector in sectors) {
      nc     <- nc_open(paste0(ncpath, 'SLC.', sector, '.2012.hourly.nc'))
      tmp    <- ncvar_get(nc, varid='Carbon Emission', 
                          start=c(1, 1, hoy[1]), count=c(-1, -1, hoy[2]-hoy[1]))
      nc_close(nc)
      raw <- tmp
    }
    
    nc     <- nc_open(paste0(ncpath, 'SLC.Total.2012.annual.nc'))
    lat    <- ncvar_get(nc, varid='Lat')
    lon    <- ncvar_get(nc, varid='Lon')
    nc_close(nc)
    
    emit <- apply(raw, c(1, 2), mean, na.rm=T)
    
    emit[emit < 0]    <- 0
    
    nx     <- length(lon)
    ny     <- length(lat)
    latm   <- matrix(lat, nrow=nx, ncol=ny, byrow=T)
    lonm   <- matrix(lon, nrow=nx, ncol=ny)
    d <- data.frame(lng = c(lonm),
                    lat = c(latm),
                    values = c(emit))
    
    # -----------------------------------------------------------------------
    # Return map
    
    zmin <- min(emit, na.rm=T)
    zmax <- max(emit, na.rm=T)
    
    r <- rasterFromXYZ(d)
    crs(r) <- CRS("+proj=longlat")
    
    cols  <- colorNumeric(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'), 
                          domain=c(zmin-1, zmax+1), na.color='#00000000')
    
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addRasterImage(r, colors=cols, opacity = 0.3) %>%
      addLegend(pal=cols, values=r@data@values, position='bottomright', title='KgC')
  })
}