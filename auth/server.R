# Ben Fasoli

# Server initialization --------------------------------------------------------
function(input, output, session) {
  for (module in dir('src/modules', full.names=T))
    source(module, local=T)
}
