/*
 * Arduino Mega 2560: Print Hello TianSemi to LCD I2C display
 * My I2C is : 0x27, please yours with code from
 * https://www.geeksforgeeks.org/electronics-engineering/how-to-interface-i2c-lcd-display-with-arduino/
 */
#include <Wire.h>  
#include <LiquidCrystal_I2C.h>

LiquidCrystal_I2C lcd(0x27, 20, 4); // please put correct I2C address
int ledR = 22;

void setup()
{
    // 1. Start Serial communication at 9600 baud
    Serial.begin(9600);
    Serial.println("--- System Initializing ---");
    delay(100);
    // Initialize the LCD
    lcd.init();
    lcd.backlight();
    Serial.println("LCD Initialized.");
    delay(100);
    pinMode(ledR, OUTPUT);
}

void loop()
{
    Serial.println("Updating Display...");
    delay(100);    
    lcd.setCursor(0, 0);
    lcd.print("Hello"); 
    lcd.setCursor(0, 1);
    lcd.print("TianSemi");
    delay(100);
    digitalWrite(ledR, HIGH);
    Serial.println("LED Pin 22 set to HIGH.");
    delay(10000); // Increased delay to make serial output readable
    digitalWrite(ledR, LOW);
    Serial.println("LED turned OFF.");
    lcd.clear();
    delay(500);
}
