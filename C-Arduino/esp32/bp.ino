const int ledRedPin = 14;
const int ledGreenPin = 23;
const int ledBluePin = 26;
const int bpPin = 35;
unsigned int toggle = 0;

void setup() {
  Serial.begin(115200);
  pinMode(ledRedPin, OUTPUT);
  pinMode(ledGreenPin, OUTPUT);
  pinMode(ledBluePin, OUTPUT);
  pinMode(bpPin, INPUT_PULLDOWN);

  Serial.println("\n✅ prêt !");
  digitalWrite(ledRedPin, !toggle);
  digitalWrite(ledGreenPin, toggle);
}

void loop() {
  if (digitalRead(bpPin) == HIGH) {
    Serial.println("🔘 Bouton pressé");
    // Un petit délai pour éviter les rebonds et pour ne pas afficher le message plusieurs fois par pression
    delay(200);
    // Attendre que le bouton soit relâché avant de continuer
    while(digitalRead(bpPin) == HIGH){ // On attend que la broche revienne à LOW (bouton relâché)
      delay(10);
    }
  }
}
