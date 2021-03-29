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

site_config <- fread('/srv/git-mirror/data-pipeline/config/site_config.csv',
                     showProgress = F)

stids <- site_config$stid
names(stids) <- paste(site_config$stid, site_config$name, sep = ' - ')
