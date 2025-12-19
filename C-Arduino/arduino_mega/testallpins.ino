/*
 * Arduino Mega 2560: Set all pins to HIGH
 * Total pins covered: 70 (0-53 digital, 54-69 analog)
 * https://www.build-electronic-circuits.com/arduino-blink-led/
 */

void setup() {
  // Loop through every pin from 0 to 69
  for (int i = 0; i <= 69; i++) {
    pinMode(i, OUTPUT);      // Configure pin as an output
    digitalWrite(i, HIGH);   // Set the voltage to HIGH (5V)
  }
}

void loop() {
  // No code needed here. 
  // Once pins are set HIGH in setup(), they stay HIGH until power is lost or reset.
}
