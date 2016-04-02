/*************************************
Generated by EZMet
http://air.utah.edu/s/ezmet/
Ben Fasoli

DHT22
Wiring Info (pins left to right):
10k pullup resistor between 5V and data
1 (VCC)  : 5V
2 (Data) : 2
3 : No connection
4 (GND)  : GND

Column Names:
dht_temp_c, dht_rh_pct
*************************************/

// Libraries
#include "SD.h"
#include "dht.h"

// Constants
int interval = 1000;
dht DHT;
const int dhtPin = 2;

// Program Setup
void setup() {
Serial.begin(9600);
Serial.println("Generated by EZMet");
Serial.println("http://air.utah.edu/s/ezmet/");
Serial.println("Ben Fasoli");

Serial.println();
SD.begin(4);

}

// Program Loop
void loop() {
File file = SD.open("raw.dat", FILE_WRITE);
if (file) {
int dht_chk  = DHT.read22(dhtPin);
if (dht_chk == 0) {
float dht_temp_c = DHT.temperature;
float dht_rh_pct = DHT.humidity;
Serial.print(dht_temp_c); Serial.print(",");
Serial.print(dht_rh_pct); Serial.print(",");
file.print(dht_temp_c); file.print(",");
file.print(dht_rh_pct); file.print(",");
} else {
Serial.print(-9999.0); Serial.print(",");
Serial.print(-9999.0); Serial.print(",");
file.print(-9999.0); file.print(",");
file.print(-9999.0); file.print(",");
}
file.println();
Serial.println();
} else {
Serial.println("ERROR: Could not open SD card file.");
}
delay(interval);
}
