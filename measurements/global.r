# Ben Fasoli

library(data.table)
library(fasttime)
library(future)
library(plotly)
library(promises)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(tidyverse)

Sys.setenv(TZ = 'UTC')
options(shiny.sanitize.errors = F)

enableBookmarking(store = 'url')
plan(strategy = multisession)

site_config <- fread('https://raw.githubusercontent.com/uataq/data-pipeline/main/config/site_config.csv',
                     showProgress = F)

stids <- site_config$stid
names(stids) <- paste(site_config$stid, site_config$name, sep = ' - ')

instruments <- c('lgr_ugga', 'licor_6262', 'licor_7000')
instrument_regex <- paste0('\\b(', paste(instruments, collapse = '|'), ')\\b')
# instrument_regex <- '\\b(licor_6262|licor_7000|lgr_ugga)\\b'
