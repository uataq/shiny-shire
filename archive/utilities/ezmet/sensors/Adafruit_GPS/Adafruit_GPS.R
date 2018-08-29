about <- list(
  type = 'Dx2'
)

new_sensor <- function(dpin) {
  # -------------------------------------------------------
  # INFO
  header = paste(sep='\n',
                 'Adafruit GPS Breakout',
                 'Wiring Info (SPI):',
                 'VIN : 5V',
                 'GND : GND',
                 paste0('RX  : ', dpin[1]),
                 paste0('TX  : ', dpin[2]))
  
  colhdr = 'dht_temp_c, dht_rh_pct'
  
  # -------------------------------------------------------
  # C CODE
  lib = paste(sep='\n',
              '#include "Adafruit_GPS.h"',
              '#include "SoftwareSerial.h"')
  init = paste(sep='\n',
               paste0('SoftwareSerial gpsSerial(', dpin[2], ', ', dpin[1], ');'),
               'Adafruit_GPS GPS(&gpsSerial);')
  setup = paste(sep='\n',
                'GPS.begin(9600);',
                'GPS.sendCommand(PMTK_SET_NMEA_OUTPUT_RMCGGA);',
                'GPS.sendCommand(PMTK_SET_NMEA_UPDATE_1HZ);')
  loop  = paste(sep='\n',
                'if (GPS.fix) {',
                'Serial.print(GPS.year); Serial.print("-");',
                'Serial.print(GPS.month); Serial.print("-");',
                'Serial.print(GPS.day); Serial.print(" ");',
                'Serial.print(GPS.hour); Serial.print(":");',
                'Serial.print(GPS.minute); Serial.print(":");',
                'Serial.print(GPS.seconds); Serial.print(".");',
                'Serial.print(GPS.milliseconds); Serial.print(",");',
                'Serial.print(GPS.latitude); Serial.print(",");',
                'Serial.print(GPS.longitude); Serial.print(",");',
                'Serial.print(GPS.altitude); Serial.print(",");',
                'Serial.print(GPS.satellites); Serial.print(",");',
                'file.print(GPS.year); file.print("-");',
                'file.print(GPS.month); file.print("-");',
                'file.print(GPS.day); file.print(" ");',
                'file.print(GPS.hour); file.print(":");',
                'file.print(GPS.minute); file.print(":");',
                'file.print(GPS.seconds); file.print(".");',
                'file.print(GPS.milliseconds); file.print(",");',
                'file.print(GPS.latitude); file.print(",");',
                'file.print(GPS.longitude); file.print(",");',
                'file.print(GPS.altitude); file.print(",");',
                'file.print(GPS.satellites); file.print(",");',
                '} else {',
                'Serial.print(-9999.0); Serial.print(",");',
                'Serial.print(-9999.0); Serial.print(",");',
                'Serial.print(-9999.0); Serial.print(",");',
                'Serial.print(-9999.0); Serial.print(",");',
                'Serial.print(-9999.0); Serial.print(",");',
                'file.print(-9999.0); file.print(",");',
                'file.print(-9999.0); file.print(",");',
                'file.print(-9999.0); file.print(",");',
                'file.print(-9999.0); file.print(",");',
                'file.print(-9999.0); file.print(",");',
                '}'
  )
  
  output = list(header = header,
                colhdr = colhdr,
                lib = lib,
                init = init,
                setup = setup,
                loop = loop)
  return(output)
}