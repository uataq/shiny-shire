#!/usr/bin/env Rscript
# Ben Fasoli

library(proj4)
library(tidyverse)

rasters <- readRDS('../grid/average_stacks.rds')

files <- dir('raw', full.names = T)

find_block_rows <- function(block, strings, skip = 0) {
  ends <- which(strings == 'END')
  line_start <- which(strings == block)[1] + 1 + skip
  line_end <- ends[ends > line_start][1] - 1
  line_start:line_end
}

# Locations remain the same across all text files
strings <- readLines(files[1])
strings_locations <- strings[find_block_rows('POINT SOURCES', strings)]
strings_locations <- grep('STD', strings_locations, value = T)
locations <- data.frame(
  idx = as.integer(substr(strings_locations, 1, 10)),
  x = as.numeric(substr(strings_locations, 21, 30)),
  y = as.numeric(substr(strings_locations, 31, 40))
)


values <- bind_rows(lapply(files, function(file) {
  strings <- readLines(files[1])
  strings_values <- strings[find_block_rows('EMISSIONS VALUES', strings, skip = 1)]
  data.frame(
    idx = as.integer(substr(strings_values, 1, 10)),
    variable = trimws(substr(strings_values, 11, 20)),
    value = as.numeric(substr(strings_values, 21, 30)),
    stringsAsFactors = F
  )
}))

average_values <- values %>%
  group_by(idx, variable) %>%
  summarize(value = mean(value, na.rm = T))

df <- full_join(locations, average_values, by = 'idx')
xy_lcc <- df[c('x', 'y')]
xy_latlon <- ptransform(data = xy_lcc, 
                        src.proj = rasters@crs@projargs,
                        dst.proj = '+proj=longlat')
df$longitude <- xy_latlon$x * 180/pi
df$latitude <- xy_latlon$y * 180/pi
df[c('x', 'y')] <- NULL

saveRDS(df, 'geolocated.rds')
