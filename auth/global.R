# Ben Fasoli
library(dplyr)
library(DT)
library(digest)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyBS)

loc_opts <- c('asb279', 'asb286', 'cpk', 'dbk', 'fru', 'hdp', 'heb', 'hpl',
              'imc', 'lgn', 'lg2', 'nrd', 'roo', 'rpk', 'spl', 
              'sug', 'sun', 'trx01', 'trx02', 'wbb', 'other')
n2_sites <- c('dbk', 'heb', 'imc', 'lgn', 'rpk', 'sug', 'sun')
