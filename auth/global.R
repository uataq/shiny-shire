# Ben Fasoli
library(dplyr)
library(DT)
library(digest)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyBS)

loc_opts <- c('asb', 'cpk', 'dbk', 'fru', 'hdp', 'heb', 'hpl', 'lgn', 'mur', 'nrd', 'roo', 'rpk', 'spl', 
              'sug', 'sun', 'trx01', 'trx02', 'wbb', 'other')
n2_sites <- c('dbk', 'heb', 'lgn', 'mur', 'rpk', 'sug', 'sun')