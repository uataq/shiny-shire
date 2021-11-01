# Ben Fasoli

library(data.table)
library(fasttime)
library(future)
library(plotly)
library(promises)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

Sys.setenv(TZ = 'UTC')
options(shiny.sanitize.errors = F)

enableBookmarking(store = 'url')
plan(strategy = multisession)

site_config <- read.csv('https://raw.githubusercontent.com/uataq/data-pipeline/main/config/site_config.csv',
                        stringsAsFactors = F)

stids <- site_config$stid
names(stids) <- paste(site_config$stid, site_config$name, sep = ' - ')
