#include <WiFi.h>
#include <HTTPClient.h>

// Define the pin number for the LED
const int ledPin = 23; 
const int bpPin = 22;
const char* ssid = "TON_SSID";
const char* password = "TON_MOT_DE_PASSE";

// Remplace par l'URL de ton API Gateway AWS
const char* serverName = "https://TON_API_ID.execute-api.REGION.amazonaws.com/chemin";

unsigned long lastTime = 0;
unsigned long timerDelay = 10000; // envoi toutes les 10 secondes

volatile int pulseCount = 0;

void IRAM_ATTR handlePulse() {
  pulseCount++;
}

void setup() {
  Serial.begin(115200);
  // initialize the ledPin as an output.
  pinMode(ledPin, OUTPUT);
  pinMode(bpPin, INPUT);
  attachInterrupt(bpPin, handlePulse, RISING);

  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("WiFi connecté");
}

// the loop function runs over and over again forever
void loop() {
  if(digitalRead(bpPin)==HIGH){
    digitalWrite(ledPin,HIGH);
  }else {
    digitalWrite(ledPin,LOW);
   }
  if ((millis() - lastTime) > timerDelay) {
    int freq = pulseCount / (timerDelay / 1000); // fréquence en Hz
    pulseCount = 0;
    lastTime = millis();

    if (WiFi.status() == WL_CONNECTED) {
      HTTPClient http;
      http.begin(serverName);
      http.addHeader("Content-Type", "application/json");
      String json = "{\"timestamp\": " + String(lastTime) + ", \"frequency\": " + String(freq) + "}";
      int httpResponseCode = http.POST(json);
      Serial.println(httpResponseCode);
      http.end();
    }
  }
}
