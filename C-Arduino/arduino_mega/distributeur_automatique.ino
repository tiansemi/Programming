/*
 * Arduino Mega 2560 / Step motor 28BYJ-48 and ULN2003 driver board / Red led
 * StepMotor 1 : IN1=D53/IN2=D51/IN3=D49/IN4=D47
 * StepMotor 2 : IN1=D50/IN2=D46/IN3=D48/IN4=D52
 */
 
#include <Stepper.h>

const int stepsPerRevolution = 2048;
const int rpm = 12;
Stepper stepper1(stepsPerRevolution, 53, 49, 51, 47);
Stepper stepper2(stepsPerRevolution, 50, 48, 46, 52);

int ledR = 22;

void setup() {
    Serial.begin(9600);
    stepper1.setSpeed(rpm);
    stepper2.setSpeed(rpm);
    delay(100);
    pinMode(ledR, OUTPUT);
    Serial.println("Initialization complete.\n");
}
void loop() {
    digitalWrite(ledR, HIGH);
    delay(100);
    Serial.println("LED Pin 22 set to HIGH.");
    Serial.println("[MOTOR 1] Forward rotation START");
    delay(100);
    stepper1.step(stepsPerRevolution);
    delay(100);
    Serial.println("[MOTOR 1] Forward rotation DONE");
    delay(100);
    digitalWrite(ledR, LOW);
    delay(100);
    Serial.println("LED turned OFF.");
    

    Serial.println("[MOTOR 1] Reverse rotation START");
    stepper1.step(-stepsPerRevolution);
    Serial.println("[MOTOR 1] Reverse rotation DONE");
    delay(100);

    digitalWrite(ledR, HIGH);
    delay(100);
    Serial.println("LED Pin 22 set to HIGH.");
    Serial.println("[MOTOR 2] Forward rotation START");
    delay(100);
    stepper2.step(stepsPerRevolution);
    Serial.println("[MOTOR 2] Forward rotation DONE");
    delay(100);
    digitalWrite(ledR, LOW);
    delay(100);
    Serial.println("LED turned OFF.");

    Serial.println("[MOTOR 2] Reverse rotation START");
    delay(100);
    stepper2.step(-stepsPerRevolution);
    Serial.println("[MOTOR 2] Reverse rotation DONE");

    Serial.println("=== Cycle complete ===\n");
    delay(500);
}
