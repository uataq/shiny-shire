about <- list(
  type = 'D'
)

new_sensor <- function(dpin) {
  # -------------------------------------------------------
  # INFO
  header = paste(sep='\n',
                 'DS18B20',
                 'Wiring Info:',
                 '10k resistor between 5V and data',
                 'Red : 5V',
                 'Blk : GND',
                 paste0('Ylw : ', dpin, ')'))
  
  colhdr = 'ds_temp_c'
  
  # -------------------------------------------------------
  # C CODE
  lib = paste(sep='\n',
              '#include "DallasTemperature.h"',
              '#include "OneWire.h"')
  init = paste(sep='\n',
               paste0('#define ONE_WIRE_BUS ', dpin),
               'OneWire oneWire(ONE_WIRE_BUS);',
               'DallasTemperature ds(&oneWire);')
  setup = 'ds.begin();'
  loop  = paste(sep='\n',
                'ds.requestTemperatures();',
                'float ds_temp_c  = ds.getTempCByIndex(0);',
                'if (ds_temp_c < -30.0 | ds_temp_c > 100.0 | isnan(ds_temp_c)) {',
                'ds_temp_c = -9999.0;',
                '}',
                'Serial.print(ds_temp_c); Serial.print(",");',
                'file.print(am_temp_c); file.print(",");')
  
  output = list(header = header,
                colhdr = colhdr,
                lib = lib,
                init = init,
                setup = setup,
                loop = loop)
  return(output)
}