# Ben Fasoli

enableBookmarking(store = 'url')

hexcolors <- c('#DD4B39', # Red
               '#00A65A', # Green
               '#FF851B', # Yellow
               '#0073B7') # Blue

# Dependencies -----------------------------------------------------------------
libs <- c(
  'ggthemes',
  'plotly',
  'shiny',
  'shinycssloaders',
  'shinydashboard',
  'shinyjs',
  'tidyverse',
  'uataq'
)

lapply(libs, library, character.only = T)

