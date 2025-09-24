const int ledRedPin   = 14;
const int ledGreenPin = 23;
const int bpPin       = 35;

bool ledState = false;  // false = rouge, true = vert

void setup() {
  Serial.begin(115200);

  pinMode(ledRedPin, OUTPUT);
  pinMode(ledGreenPin, OUTPUT);
  pinMode(bpPin, INPUT_PULLDOWN);

  // État initial : LED rouge allumée
  digitalWrite(ledRedPin, HIGH);
  digitalWrite(ledGreenPin, LOW);
}

void loop() {
  static bool lastButtonState = LOW;
  bool buttonState = digitalRead(bpPin);

  if (buttonState == HIGH && lastButtonState == LOW) {
    // Détection d'un front montant (pression)
    ledState = !ledState;  // inversion de l'état
    
    if (ledState) {
      // Allumer vert, éteindre rouge
      digitalWrite(ledRedPin, LOW);
      digitalWrite(ledGreenPin, HIGH);
      Serial.println("LED verte allumée");
    } else {
      // Allumer rouge, éteindre vert
      digitalWrite(ledRedPin, HIGH);
      digitalWrite(ledGreenPin, LOW);
      Serial.println("LED rouge allumée");
    }

    delay(200); // anti-rebond
  }

  lastButtonState = buttonState;
}
