rm(list=ls())

# Batch tank data type change --------------------------------------------------
# tanks <- readRDS(tail(dir('src/tanks', full.names=T), 1))
# temp <- lapply(tanks, function(x) {
#   proper <- 'ccTnccnnnnnnnnnnnnnnnnnnnnccc'
#   for (i in 1:ncol(x)) {
#     type <- substr(proper, i, i)
#     fun <- switch(type,
#                   'c' = as.character,
#                   'n' = as.numeric,
#                   'T' = NULL)
#     if (!is.null(fun)) {
#       x[ ,i] <- fun(x[ ,i])
#     }
#   }
#   x
# })
# saveRDS(temp, 'src/tanks/160418_0953_tankdata.rds')

# Site naming convention change ------------------------------------------------
readRDS(tail(dir('src/tanks', full.names=T), 1)) %>%
  lapply(function(tank) {
    long <- c('Horsepool', 'Murray', 'Rose Park', 'Heber', 'Sugarhouse', 'Daybreak', 
              'Fruitland', 'Roosevelt', 'WBB', 'Draper', 'Logan', 'Trax', 'csp', 'WBB808')
    short <- c('hpl', 'mur', 'rpk', 'heb', 'sug', 'dbk',
               'fru', 'roo', 'wbb', 'sun', 'lgn', 'trx01', 'cpk', 'wbb')
    for (i in 1:length(long)) {
      loc <- tail(tank$Location, 1)
      if (!is.na(loc) && loc == long[i]) {
        tank$Location[tank$Location == long[i]] <- short[i]
      }
    }
    tank
  }) %>%
  saveRDS(format(Sys.time(), 'src/tanks/%y%m%d_%H%M_tankdata.rds'))