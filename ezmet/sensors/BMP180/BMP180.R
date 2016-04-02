about <- list(
  type = 'I2C'
)

new_sensor <- function(dpin) {
  # -------------------------------------------------------
  # INFO
  header = paste(sep='\n',
                 'BMP180',
                 'Wiring Info (I2C):',
                 'VCC : 5V',
                 'GND : GND',
                 'SDA : A4',
                 'SCL : A5')
  
  colhdr = 'bmp_pres_hpa, bmp_temp_c'
  
  # -------------------------------------------------------
  # C CODE
  lib = '#include "Adafruit_BMP085.h"'
  init = 'Adafruit_BMP085 bmp;'
  setup = 'bmp.begin();'
  loop  = paste(sep='\n',
                'float bmp_pres_hpa = bmp.readPressure() / 100.0;',
                'float bmp_temp_c  = bmp.readTemperature();',
                'if (bmp_pres_hpa < 300.0 | bmp_pres_hpa > 1100.0) {',
                'bmp_pres_hpa = -9999.0;',
                'bmp_temp_c  = -9999.0;',
                '}',
                'Serial.print(bmp_pres_hpa); Serial.print(",");',
                'Serial.print(bmp_temp_c); Serial.print(",");',
                'file.print(bmp_pres_hpa); file.print(",");',
                'file.print(bmp_temp_c); file.print(",");')
  
  output = list(header = header,
                colhdr = colhdr,
                lib = lib,
                init = init,
                setup = setup,
                loop = loop)
  return(output)
}