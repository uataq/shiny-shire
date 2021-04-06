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
# readRDS(tail(dir('src/tanks', full.names=T), 1)) %>%
#   lapply(function(tank) {
#     long <- c('Horsepool', 'Murray', 'Rose Park', 'Heber', 'Sugarhouse', 'Daybreak', 
#               'Fruitland', 'Roosevelt', 'WBB', 'Draper', 'Logan', 'Trax', 'csp', 'WBB808')
#     short <- c('hpl', 'mur', 'rpk', 'heb', 'sug', 'dbk',
#                'fru', 'roo', 'wbb', 'sun', 'lgn', 'trx01', 'cpk', 'wbb')
#     for (i in 1:length(long)) {
#       loc <- tail(tank$Location, 1)
#       if (!is.na(loc) && loc == long[i]) {
#         tank$Location[tank$Location == long[i]] <- short[i]
#       }
#     }
#     tank
#   }) %>%
#   saveRDS(format(Sys.time(), 'src/tanks/%y%m%d_%H%M_tankdata.rds'))


# Fix cpk -> csp naming convention ------------------------------------------------
# notes <- readRDS(tail(dir('src/notes', full.names=T), 1))
# site_ids <- names(notes)
# site_ids <- ifelse(site_ids == 'cpk', 'csp', site_ids)
# names(notes) <- site_ids
# saveRDS(notes, format(Sys.time(), 'src/notes/%y%m%d_%H%M_notearchive.rds'))
# 
# tanks <- readRDS(tail(dir('src/tanks', full.names=T), 1)) 
# tanks <- lapply(tanks, function(tank) {
#   tank$Location <- ifelse(tank$Location == 'cpk', 'csp', tank$Location)
#   tank
# })
# saveRDS(tanks, format(Sys.time(), 'src/tanks/%y%m%d_%H%M_tankdata.rds'))


# Fix Ryan wrong in/out time ---------------------------------------------------
# notes <- readRDS(tail(dir('src/notes', full.names=T), 1))
# lg2 <- notes[['lg2']]
# mask <- lg2$time_in == as.POSIXct('2021-03-31 19:41:00')
# lg2$time_in[mask] <- lg2$time_in[mask] + 86400
# lg2$time_out[mask] <- lg2$time_out[mask] + 86400
# notes[['lg2']] <- lg2
# saveRDS(notes, format(Sys.time(), 'src/notes/%y%m%d_%H%M_notearchive.rds'))





