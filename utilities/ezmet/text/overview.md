EZMet is a flexible way to build Arduino based meteorological data logging packages.

#### Requirements
Several components are required:

- Arduino IDE, open source and available at [arduino.cc](https://www.arduino.cc/en/Main/Software)
- Arduino UNO R3 board w/ USB cable
- SD card logging shield compatible with the base SD library (most are)
- Sensors to interface with

#### How To
Through the **Setup** page, provide information on:

1. Project name - a name for your project compatible with arduino's IDE (keep it simple, no special characters).
2. Sampling interval - set the delay between data scans.
3. Desired sensors - list of pre-programmed sensors. Most are available at low-cost on Amazon.
4. Custom analog sensors - allows interfacing with instruments capable of outputting a 0-5V analog signal.

#### Output
This utility will generate a zip file containing a single directory. This directory houses a .ino file to be used with the IDE as well as any necessary libraries to interface with sensors. The source code found in the .ino file contains information about wiring your sensors to the arduino board - be sure to read the information stored in the header. The arduino IDE can be used to compile and send the program to the arduino board.

By default, the program will write the data to the SD card as well as the serial port. To test if the system is working, open the serial monitor (default 9600 baud) to see if data is streaming properly.