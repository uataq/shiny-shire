# ---------------------------------------------------------------------------------
# WBB FILL IN SPIKE CALCULATOR
output$tankspike_ambient_wbb <- renderUI({
  wbb_file <- '/srv/shiny-server/air/data/airmap.rds'
  wbb <- tail(subset(readRDS(wbb_file)$data, site=='wbb'), 1)
  if((Sys.time() - wbb$Time) < 60 * 60){
    textInput('tankspike_ambient', h5(format(wbb$Time, format='Ambient - %Y-%m-%d %H:%M %Z')), 
              value=as.character(wbb$co2))
  } else NULL
})

# ---------------------------------------------------------------------------------
# SPIKE CALCULATION
output$tankspike_calculated <- renderText({
  needs <- c(input$tankspike_pressure,
             input$tankspike_ambient,
             input$tankspike_spike,
             input$tankspike_target)
  if (any(nchar(needs) < 1)) return(' ')
  pressure <- as.numeric(input$tankspike_pressure)
  ambient  <- as.numeric(input$tankspike_ambient)
  spike  <- as.numeric(input$tankspike_spike)
  target  <- as.numeric(input$tankspike_target)
  fnout <- tankspike(pressure, ambient, spike, target)
  paste0('psi : ', round(fnout[1], 3))
})

# ---------------------------------------------------------------------------------
# DEWPOINT TO PPM CALCULATION
output$dewtoppm_calculated <- renderText({
  needs <- c(input$dewtoppm_Td, input$dewtoppm_P)
  if (any(nchar(needs) < 1)) return(' \n ')
  Td <- as.numeric(input$dewtoppm_Td)
  P  <- as.numeric(input$dewtoppm_P)
  fnout <- dewtoppm(Td, P,input$dewtoppm_Td_unit, input$dewtoppm_P_unit)
  paste0('ppm : ', round(fnout[1], 3), '\nppt : ', round(fnout[2], 3))
})

# ---------------------------------------------------------------------------------
# CYLINDRICAL VOLUME CALCULATION
output$cylvol_calculated <- renderText({
  needs <- c(input$cylvol_d, input$cylvol_l)
  if (any(nchar(needs) < 1)) return(' ')
  validate(need(all(nchar(c(input$cylvol_d, input$cylvol_l)) > 0), ' '))
  l <- as.numeric(input$cylvol_l)
  d <- as.numeric(input$cylvol_d)
  fnout <- cylvol(l=l, d=d, l_unit=input$cylvol_l_unit, d_unit=input$cylvol_d_unit)
  paste0('Volume (L) : ', round(fnout, 4))
})

# ---------------------------------------------------------------------------------
# DILUTION CALCULATION
output$tankdilution_calculated <- renderText({
  needs <- c(input$dilution_sample, input$dilution_sample_flow, input$dilution_dilution_flow)
  if (any(nchar(needs) < 1)) return(' ')
  sample <- as.numeric(input$dilution_sample)
  sample_flow <- as.numeric(input$dilution_sample_flow)
  dilution_flow <- as.numeric(input$dilution_dilution_flow)
  fnout <- tankdilution(sample, sample_flow, dilution_flow)
  paste0('Mole fraction (ppm) : ', round(fnout, 4))
})

# ---------------------------------------------------------------------------------
# MINIVOL CALCULATION
output$minivol_calculated <- renderText({
  needs <- c(input$minivol_Tavg, input$minivol_P)
  if (any(nchar(needs) < 1)) return(' \n ')
  Tavg <- as.numeric(input$minivol_Tavg)
  P    <- as.numeric(input$minivol_P)
  # paste(c(Tavg, P, input$minivol_Tavg_unit, input$minivol_P_unit))
  fnout <- minivol(Tavg, P, input$minivol_Tavg_unit, input$minivol_P_unit)
  paste(c('ID\tmmHg\tSetPoint', 
          paste0(fnout$id, '\t', round(fnout$P_mmHg, 1), '\t', round(fnout$calc, 2))), 
        collapse='\n')
})