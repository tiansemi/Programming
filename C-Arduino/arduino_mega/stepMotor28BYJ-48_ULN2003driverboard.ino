/*
 * Arduino Mega 2560: Step motor 28BYJ-48 and ULN2003 driver board
 * StepMotor 1 : IN1=D53/IN2=D51/IN3=D49/IN4=D47
 * StepMotor 2 : IN1=D50/IN2=D46/IN3=D48/IN4=D52
 * https://www.codrey.com/electronics/arduino-stepper-motor-a-quick-revision/
 */
 
#include <Stepper.h>

const int stepsPerRevolution = 2048;
const int rpm = 12;
Stepper stepper1(stepsPerRevolution, 53, 49, 51, 47);
Stepper stepper2(stepsPerRevolution, 50, 48, 46, 52);

void setup() {
    Serial.begin(9600);
    stepper1.setSpeed(rpm);
    stepper2.setSpeed(rpm);
    Serial.println("Initialization complete.\n");
}
void loop() {
    Serial.println("[MOTOR 1] Forward rotation START");
    stepper1.step(stepsPerRevolution);
    Serial.println("[MOTOR 1] Forward rotation DONE");
    delay(100);

    Serial.println("[MOTOR 1] Reverse rotation START");
    stepper1.step(-stepsPerRevolution);
    Serial.println("[MOTOR 1] Reverse rotation DONE");
    delay(100);

    Serial.println("[MOTOR 2] Forward rotation START");
    stepper2.step(stepsPerRevolution);
    Serial.println("[MOTOR 2] Forward rotation DONE");
    delay(100);

    Serial.println("[MOTOR 2] Reverse rotation START");
    stepper2.step(-stepsPerRevolution);
    Serial.println("[MOTOR 2] Reverse rotation DONE");

    Serial.println("=== Cycle complete ===\n");
    delay(500);
}
