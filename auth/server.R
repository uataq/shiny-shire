# Ben Fasoli

# Server initialization --------------------------------------------------------
server <- function(input, output, session) {
  for (module in dir('src/modules', full.names=T))
    source(module, local=T)
}

server