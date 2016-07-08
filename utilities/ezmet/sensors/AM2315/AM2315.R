about <- list(
  type = 'I2C'
)

new_sensor <- function(dpin) {
  # -------------------------------------------------------
  # INFO
  header = paste(sep='\n',
                 'AM2315',
                 'Wiring Info (I2C):',
                 '10k resistor between 5V and SDA/SCL',
                 'Red : 5V',
                 'Blk : GND',
                 'Ylw : A4',
                 'Wht : A5')
  
  colhdr = 'am_temp_c, am_rh_pct'
  
  # -------------------------------------------------------
  # C CODE
  lib = '#include "Adafruit_AM2315.h"'
  init = 'Adafruit_AM2315 am;'
  setup = 'am.begin();'
  loop  = paste(sep='\n',
                'float am_temp_c  = am.readTemperature();',
                'float am_rh_pct  = am.readHumidity();',
                'if (am_temp_c < -30.0 | am_temp_c > 100.0 | isnan(am_temp_c)) {',
                'am_temp_c = -9999.0;',
                'am_rh_pct  = -9999.0;',
                '}',
                'Serial.print(am_temp_c); Serial.print(",");',
                'Serial.print(am_rh_pct); Serial.print(",");',
                'file.print(am_temp_c); file.print(",");',
                'file.print(am_rh_pct); file.print(",");')
  
  output = list(header = header,
                colhdr = colhdr,
                lib = lib,
                init = init,
                setup = setup,
                loop = loop)
  return(output)
}