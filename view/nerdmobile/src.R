# Nerdmobile spatial data aggregation
# Ben Fasoli, benfasoli@gmail.com
# -----------------------------------------------------------------------------
# Land Atmosphere Interactions Research (LAIR, http://lair.utah.edu)
# Utah Atmospheric Trace gas & Air Quality Lab (UATAQ, http://air.utah.edu)
# -----------------------------------------------------------------------------

library(dplyr)
library(uataq)

# Data Calibration ------------------------------------------------------------
# calibrate() function from uataq R package. See:
# https://github.com/benfasoli/uataq

# Process Data ----------------------------------------------------------------
proc <- function(zipfile, reader, calibrate)
{
  # Establish Directory Structure ---------------------------------------------
  if (dir.exists('UATAQ_Nerdmobile'))
    unlink('UATAQ_Nerdmobile', recursive=T)
  Sys.umask('002')
  dir.create('UATAQ_Nerdmobile/raw', showWarnings=F, recursive=T)
  dir.create('UATAQ_Nerdmobile/maps/dependencies', showWarnings=F, recursive=T)
  dir.create('UATAQ_Nerdmobile/plots/dependencies', showWarnings=F, recursive=T)
  z <- unzip(zipfile, exdir='UATAQ_Nerdmobile/raw/')
  Sys.umask('022')
  if(length(z) == 0) return(NULL)
  
  # Raw File Import -----------------------------------------------------------
  # Import raw data into a list of dataframes, named using the same
  # naming convention as the reader functions.
  raw <- lapply(names(reader), function(x){ reader[[x]]() })
  names(raw) <- names(reader)
  
  # Optional Data Calibrations ------------------------------------------------
  # Remove cal periods from LGR and Picarro data. Calibrations will be
  # implemented here down the road.
  d <- raw
  if ('calflag' %in% names(d$lgr)) {
    d$lgr        <- subset(d$lgr,        calflag == 1)
  }
  if ('calflag' %in% names(d$picarro_co)) {
    d$picarro_co <- subset(d$picarro_co, calflag == 1)
  }
  
  # Aggregate Datasets --------------------------------------------------------
  # Extract desired columns from each dataset and merge into a single
  # dataframe with a shared timestamp 'Time_common'. Measurements are then
  # linearly interpolated over time to fill in the dataframe.
  geo <- bind_rows(
    data_frame(Time_common=NA, lat=NA, lon=NA, CO2d_ppm=NA, 
               CH4d_ppm=NA, CO_ppm=NA, NOx=NA, O3_ppbv=NA,
               pm25=NA),
    d$garmin[c('Time_common', 'lat', 'lon')],
    d$lgr[c('Time_common', 'CO2d_ppm', 'CH4d_ppm')],
    d$picarro_co[c('Time_common', 'CO_ppm')],
    d$nox2b[c('Time_common', 'NOx')],
    d$o32b[c('Time_common', 'O3_ppbv')],
    d$grimm[c('Time_common', 'pm25')])
  
  geo_s <- geo %>%
    arrange(Time_common) %>%
    group_by(Time_common = as.POSIXct(trunc(Time_common))) %>%
    summarize(lat = mean(lat, na.rm=T),
              lon = mean(lon, na.rm=T),
              CO2d_ppm = mean(CO2d_ppm, na.rm=T),
              CH4d_ppm = mean(CH4d_ppm, na.rm=T),
              CO_ppm = mean(CO_ppm, na.rm=T),
              pm25 = mean(pm25, na.rm=T),
              NOx = mean(NOx, na.rm=T),
              O3_ppbv = mean(O3_ppbv, na.rm=T))
  
  geo.interp <- cbind(Time_common = geo_s$Time_common,
                      as_data_frame(
                        lapply(geo_s[-1], na_interp, x=geo_s$Time_common)))
  return(geo.interp)
}


# Reader Functions ------------------------------------------------------------
reader <- list()
reader$lgr <- function(path='UATAQ_Nerdmobile/raw/lgr.dat')
{
  # Los Gatos Research Ultraportable OAICOS GGA
  header <- c('Time_common', 'calflag', 'Time_inst', 'CH4_ppm', 'CH4_ppm_sd', 'H2O_ppm', 'H2O_ppm_sd',
              'CO2_ppm', 'CO2_ppm_sd', 'CH4d_ppm', 'CH4d_ppm_sd', 'CO2d_ppm', 'CO2d_ppm_sd',
              'GasP_torr', 'GasP_torr_sd', 'GasT_C', 'GasT_C_sd', 'AmbT_C', 'AmbT_C_sd',
              'RD0_us', 'RD0_us_sd', 'RD1_us', 'RD1_us_sd', 'Fit_Flag', 'MIU_v', 'MIU')
  
  if(!file.exists(path)) return(NULL)
  raw <- scan(path, what=character(), sep='\n', skipNul=T)
  ndelim <- as.integer(lapply(raw, function(x){length(gregexpr(',', x)[[1]])}))
  raw  <- raw[ndelim == 25]
  data <- read.table(textConnection(raw), sep=',', skipNul=T, 
                     stringsAsFactors=F, col.names=header)
  mask <- substr(data[ ,1], 1, 2) == '20'
  data <- data[mask, ]
  if(nrow(data) < 1) return(NULL)
  
  data$Time_common <- as.POSIXct(data$Time_common, tz='UTC')
  return(data)
}
reader$nox2b <- function(path='UATAQ_Nerdmobile/raw/nox.dat')
{
  # 2B Technologies model 410
  header <- c('Time_common', 'NOx', 'Temp', 'Press', 'SampleFlow', 'TotalFlow',
              'O3Flow', 'ScrubberTemp', 'O3', 'Date', 'Time', 'Status')
  
  if(!file.exists(path)) return(NULL)
  raw <- scan(path, what=character(), sep='\n', skipNul=T)
  ndelim <- as.integer(lapply(raw, function(x){length(gregexpr(',', x)[[1]])}))
  raw  <- raw[ndelim == 11]
  raw  <- grep('Temp', raw, invert=T, fixed=T, value=T)
  data <- read.table(textConnection(raw), sep=',', skipNul=T, 
                     stringsAsFactors=F, col.names=header)
  str(data)
  mask <- substr(data[ ,1], 1, 2) == '20'
  data <- data[mask, ]
  if(nrow(data) < 1) return(NULL)
  
  data$Time_common <- as.POSIXct(data$Time_common, tz='UTC')
  return(data)
}
reader$o32b <- function(path='UATAQ_Nerdmobile/raw/o3.dat')
{
  # 2B Technologies model 205
  header <- c('Time_common', 'O3_ppbv', 'CellT_C', 'CellP_hPa', 
              'Flow_ccmin', 'Date', 'Time')
  
  if(!file.exists(path)) return(NULL)
  raw <- scan(path, what=character(), sep='\n', skipNul=T)
  ndelim <- as.integer(lapply(raw, function(x){length(gregexpr(',', x)[[1]])}))
  raw  <- raw[ndelim == 6]
  data <- read.table(textConnection(raw), sep=',', skipNul=T, 
                     stringsAsFactors=F, col.names=header)
  mask <- substr(data[ ,1], 1, 2) == '20'
  data <- data[mask, ]
  if(nrow(data) < 1) return(NULL)
  
  data$Time_common <- as.POSIXct(data$Time_common, tz='UTC')
  return(data)
}
reader$picarro_co <- function(path='UATAQ_Nerdmobile/raw/picarro_co.dat')
{
  # Picarro CRDS 1st generation CO/CO2
  header <- c('Time_common', 'calflag', 'CavT_C', 'CavP_torr', 'CO2_ppm', 'CO2d_ppm',
              'CO_ppm', 'H2O_percent')
  
  if(!file.exists(path)) return(NULL)
  raw <- scan(path, what=character(), sep='\n', skipNul=T)
  ndelim <- as.integer(lapply(raw, function(x){length(gregexpr(';', x)[[1]])}))
  raw  <- raw[ndelim == 7]
  data <- read.table(textConnection(raw), sep=';', skipNul=T, 
                     stringsAsFactors=F, col.names=header)
  mask <- substr(data[ ,1], 1, 2) == '20'
  data <- data[mask, ]
  if(nrow(data) < 1) return(NULL)
  
  data$Time_common <- as.POSIXct(data$Time_common, tz='UTC')
  return(data)
}
reader$airmar <- function(path='UATAQ_Nerdmobile/raw/airmar.dat')
{
  # Airmar WX
  if(!file.exists(path)) return(NULL)
  data <- scan(path, what=character(), sep='\n', skipNul=T)
  return(data)
}
reader$garmin <- function(path='UATAQ_Nerdmobile/raw/garmin.dat')
{
  # Garmin GPS
  if(!file.exists(path)) return(NULL)
  raw <- scan(path, what=character(), sep='\n', skipNul=T)
  gpgga <- grep('$GPGGA', raw, fixed=T, value=T) %>%
    iconv('latin1', 'ASCII', sub='')
  ndelim <- as.integer(lapply(gpgga, function(x){length(gregexpr(',', x)[[1]])}))
  gpgga <- gpgga[ndelim == 15]
  gpgga <- matrix(data=unlist(strsplit(gpgga, ',', fixed=T)), ncol=16, byrow=T)
  data <- data.frame(Time_common = as.POSIXct(gpgga[ ,1], tz='UTC'),
                     lat = as.numeric(gpgga[ ,4]),
                     lon = as.numeric(gpgga[ ,6]))
  # Convert NMEA style lat/lon (ddmm.mmmm) to dd.dddd
  data$lat <- with(data, floor(lat/100)+(lat-floor(lat/100)*100)/60)
  data$lon <- with(data, -(floor(lon/100)+(lon-floor(lon/100)*100)/60))
  data$whichnmea <- 1
  return(data)
}
reader$grimm <- function(path='UATAQ_Nerdmobile/raw/grimm.dat')
{
  # GRIMM 1109
  require(dplyr)
  header <- c('Time_common', paste0('C', 1:32))
  
  if(!file.exists(path)) return(NULL)
  raw <- scan(path, what=character(), sep='\n', skipNul=T)
  ndelim <- as.integer(lapply(raw, function(x){length(gregexpr(',', x)[[1]])}))
  raw  <- raw[ndelim == 32]
  data <- read.table(textConnection(raw), sep=',', skipNul=T, 
                     stringsAsFactors=F, col.names=header)
  mask <- substr(data[ ,1], 1, 2) == '20'
  data <- data[mask, ]
  if(nrow(data) < 1) return(NULL)
  
  data$Time_common <- as.POSIXct(data$Time_common, tz='UTC')
  
  # Mass concentration calculation --------------------------------------------
  calc_mc <- function(count, radius, rho=3.65)
  {
    # count  - m x n binned particle counts
    # radius - n length vector of particle radii [m]
    # rho    - assumed density [g cm-3]
    rho <- rho * 10^12                                          # ug m-3
    vol <- 4/3 * pi * radius^3                                  # m3
    wt  <- vol * rho                                            # ug particle-1
    
    bin_mc <- wt * count                                        # ug 100mL-1
    bin_mc <- bin_mc / 10^-4                                    # ug m-3
    return(rowSums(bin_mc, na.rm=T) /100 + 2.3)
  }
  
  # data$pm25 <- rowSums(data[ ,2:ncol(data)], na.rm=T)
  r <- c(1.125e-07, 1.325e-07, 1.450e-07, 1.625e-07, 1.875e-07, 2.125e-07, 
         2.375e-07, 2.700e-07, 3.075e-07, 3.375e-07, 3.750e-07, 4.500e-07, 
         5.750e-07, 7.250e-07, 9.000e-07, 1.125e-06)
  data$pm25 <- data[-1] %>% select(1:17) %>%
    calc_mc(radius = r)
  
  return(data)
}
reader$user_log <- function(path='UATAQ_Nerdmobile/raw/user_log.txt')
{
  data <- scan(path, what=character(), sep='\n', skipNul=T)
  return(data)
}