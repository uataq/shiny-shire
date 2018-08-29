# UTILITY FUNCTIONS
# Required for tech shiny app
# Ben Fasoli
# 
# Functions used in utility calculations.

# -------------------------------------------------------------------------------
dewtoppm <- function(Td, P, Td_unit='C', P_unit='hPa') {
  # Apply the August-Roche-Magnus approximation of the Clausius-Clapeyron
  # equation to estimate the mole fraction of water vapor using dewpoint 
  # temperature and ambient atmospheric pressure.
  Td_factor <- switch(Td_unit, 'C'=1, 'F'=1.8)
  Td_offset <- switch(Td_unit, 'C'=0, 'F'=-32)
  P_factor <- switch(P_unit, 'hPa'=1, 'kPa'=10)
  Td <- (Td + Td_offset) * Td_factor
  P <- P * P_factor
  
  e <- 0.61094 * exp(17.625 * Td / (Td + 243.04))  # H2O partial pressure, kPa
  ppm <- e / (P / 10) * 10^6                       # mole fraction, ppm
  ppt <- ppm / 1000                                # mole fraction, mm/m, ppt
  return(data.frame(ppm, ppt))
}

# -------------------------------------------------------------------------------
cylvol <- function(l, d, l_unit='inches', d_unit='inches') {
  # Calculate the volume of a cylinder given the length and diameter.
  l_factor <- switch(l_unit, 'feet'=0.3048, 'inches'=0.0254, 'meters'=1)
  d_factor <- switch(d_unit, 'feet'=0.3048, 'inches'=0.0254, 'mm'=0.001)
  l_m <- l * l_factor
  d_m <- d * d_factor
  
  area <- (d_m / 2)^2 * pi
  Volume_m3 <- area * l_m
  Volume_L <- Volume_m3 * 1000
  return(Volume_L)
}

# -------------------------------------------------------------------------------
tankdilution <- function(sample, sample_flow, dilution_flow) {
  flow_ratio <- sample_flow / (sample_flow + dilution_flow)
  ppm <- sample * flow_ratio
  return(data.frame(ppm))
}

# -------------------------------------------------------------------------------
tankspike <- function(pressure=2200, ambient, spike, target) {
  calc <- (pressure * (target - ambient)) / (spike - ambient)
  return(calc)
}

# -------------------------------------------------------------------------------
minivol <- function(Tavg, P, Tavg_unit, P_unit) {
  Tavg_factor <- switch(Tavg_unit, 'C'=1, 'F'=1.8)
  Tavg_offset <- switch(Tavg_unit, 'C'=0, 'F'=-32)
  P_factor    <- switch(P_unit, 'hPa'=1, 'inHg'=33.8639, 'mmHg'=1.33322) # to hPa
  Tavg <- (Tavg + Tavg_offset) * Tavg_factor
  P    <- P * P_factor / 1.33322 # to mmHg... Stupid
  
  samplers <- list(id = c(4760, 4761, 4762, 4763, 4764, 4765),
                   m  = c(1.0836, 1.0577, 1.0061, 1.0918, 1.0292, 0.9718),
                   b  = c(-0.1842, -0.1204, 0.0581, -0.2603, 0.0776, 0.3103))
  calc <- (5 * sqrt((298 / (273 + Tavg)) * (P / 760)) - samplers$b) / samplers$m
  return(list(id=samplers$id, P_mmHg=P, calc=calc))
}
