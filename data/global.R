library(digest)
library(dplyr)
library(DT)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(readr)


data_opts <- dir('/projects/backups/smaug/measurements/data/')
loc_opts <- c('asb', 'cpk', 'dbk', 'fru', 'hdp', 'heb', 'hpl', 'lgn', 'mur',
              'nrd', 'roo', 'rpk', 'spl', 'sug', 'sun', 'trx01', 'trx02',
              'wbb', 'other')
n2_sites <- c('dbk', 'heb', 'lgn', 'mur', 'rpk', 'sug', 'sun')
