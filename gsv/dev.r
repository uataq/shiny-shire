library(googlesheets4)

URL <- list(
  lone = 'https://docs.google.com/spreadsheets/d/1JIR0ml6SSWndnPDHdQroqWjO71kTfkz3Db-sDX3zUS4'
)
read_sheet(URL$lone, 'gps')

