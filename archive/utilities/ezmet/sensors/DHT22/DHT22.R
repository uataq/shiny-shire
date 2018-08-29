about <- list(
  type = 'D'
)

new_sensor <- function(dpin) {
  # -------------------------------------------------------
  # INFO
  header = paste(sep='\n',
                 'DHT22',
                 'Wiring Info (pins left to right):',
                 '10k pullup resistor between 5V and data',
                 '1 (VCC)  : 5V',
                 paste0('2 (Data) : ', dpin),
                 '3 : No connection',
                 '4 (GND)  : GND')
  
  colhdr = 'dht_temp_c, dht_rh_pct'
  
  # -------------------------------------------------------
  # C CODE
  lib = '#include "dht.h"'
  init = paste(sep='\n',
               'dht DHT;',
               paste0('const int dhtPin = ', dpin, ';'))
  setup = NULL
  loop  = paste(sep='\n',
                'int dht_chk  = DHT.read22(dhtPin);',
                'if (dht_chk == 0) {',
                'float dht_temp_c = DHT.temperature;',
                'float dht_rh_pct = DHT.humidity;',
                'Serial.print(dht_temp_c); Serial.print(",");',
                'Serial.print(dht_rh_pct); Serial.print(",");',
                'file.print(dht_temp_c); file.print(",");',
                'file.print(dht_rh_pct); file.print(",");',
                '} else {',
                'Serial.print(-9999.0); Serial.print(",");',
                'Serial.print(-9999.0); Serial.print(",");',
                'file.print(-9999.0); file.print(",");',
                'file.print(-9999.0); file.print(",");',
                '}')
  
  output = list(header = header,
                colhdr = colhdr,
                lib = lib,
                init = init,
                setup = setup,
                loop = loop)
  return(output)
}