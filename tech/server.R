# Ben Fasoli

library(DT)
library(ggplot2)
library(plyr)
library(R.matlab)
library(sendmailR)
library(shinydashboard)
source('util.R')

# ---------------------------------------------------------------------------------
# SETTINGS
wbb_path <- '/srv/shiny-server/air/data/uinta/wbb'
simple_cols <- c('ID', 'Time', 'Pressure', 'Location', 'Network', 'CO2', 
                 'CH4', 'CO', 'Note', 'Name')

# ---------------------------------------------------------------------------------
# FUNCTIONS
resetFields <- function(session) {
  textfields <- c('tank_id', 'tank_psi', 'tank_location', 'tank_network', 'tank_co2', 'tank_ch4', 'tank_co', 
              'tank_note', 'tank_serial', 'tank_relative', 'tank_co2_stdev', 'tank_co2_sterr', 
              'tank_co2_n', 'tank_ch4_stdev', 'tank_ch4_sterr', 'tank_ch4_n', 'tank_co_stdev', 
              'tank_co_sterr', 'tank_co_n', 'tank_d13C', 'tank_d18O', 'tank_d13C_stdev', 'tank_d13C_sterr',
              'tank_d13C_n', 'tank_d18O_stdev','tank_d18O_sterr','tank_d18O_n')
  selfields <- c('tank_num', 'tank_num_view')
  for (field in textfields) updateTextInput(session, field, value='')
  for (field in selfields)  updateSelectInput(session, field, selected='')
}


function(input, output, session) {
  # Modular pages found in apps directory
  for(app in dir('apps', pattern='.*.{1}R', full.names=T)){
    source(app, local=T)
  }
}