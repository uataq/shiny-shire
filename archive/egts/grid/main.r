#!/usr/bin/env Rscript
# Ben Fasoli

library(leaflet)
library(ncdf4)
library(raster)

files <- dir('raw', full.names = T)
data <- 0

for (file in files) {
  message(file)
  nc <- nc_open(files[1])
  
  r_earth <- 6370000
  lon_0 <- ncatt_get(nc, varid = 0, attname = 'XCENT')$value
  lat_0 <- ncatt_get(nc, varid = 0, attname = 'YCENT')$value
  lat_1 <- ncatt_get(nc, varid = 0, attname = 'P_ALP')$value
  lat_2 <- ncatt_get(nc, varid = 0, attname = 'P_BET')$value
  
  nx <- ncatt_get(nc, varid = 0, attname = 'NCOLS')$value
  dx <- ncatt_get(nc, varid = 0, attname = 'XCELL')$value
  xmn <- ncatt_get(nc, varid = 0, attname = 'XORIG')$value
  xmx <- xmn + nx * dx
  
  ny <- ncatt_get(nc, varid = 0, attname = 'NROWS')$value
  dy <- ncatt_get(nc, varid = 0, attname = 'YCELL')$value
  ymn <- ncatt_get(nc, varid = 0, attname = 'YORIG')$value
  ymx <- ymn + ny * dy
  
  proj <- as.character(glue::glue('+proj=lcc ',
                                  '+lon_0={lon_0} ',
                                  '+lat_0={lat_0} ',
                                  '+lat_1={lat_1} ',
                                  '+lat_2={lat_2} ',
                                  '+a={r_earth} ',
                                  '+b={r_earth} '))
  rasters <- list()
  for (v in nc$var) {
    name <- v$name
    if (!v$dim[[1]]$name == 'COL') next
    
    values <- ncvar_get(nc, name)
    arr <- apply(values, c(1, 2), mean, na.rm = T)
    arr <- t(arr)
    r <- raster(arr,
                xmn = xmn,
                xmx = xmx,
                ymn = ymn,
                ymx = ymx,
                crs = proj)
    rasters[name] <- list(flip(r, 2))
  }
  data <- data + stack(rasters)
}

data <- data / length(files)
saveRDS(data, 'average_stacks.rds')


library(leaflet)
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addRasterImage(data[['CO']], opacity = 0.5)
