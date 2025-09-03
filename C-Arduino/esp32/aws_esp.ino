#include <WiFi.h>
#include <HTTPClient.h>

// Définir les pins
const int ledPin = 26;
const int bpPin = 27;

// Infos WiFi
const char* ssid = "WIFI ODC";
const char* password = "Digital1";

// Endpoint API
const char* serverName = "https://0enopjgto0.execute-api.us-east-2.amazonaws.com/";

// Variables
unsigned long lastTime = 0;

void setup() {
  Serial.begin(115200);
  pinMode(ledPin, OUTPUT);
  pinMode(bpPin, INPUT);

  // Connexion WiFi
  WiFi.begin(ssid, password);
  Serial.print("Connexion WiFi");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("\n✅ WiFi connecté !");
}

void loop() {
  // Si bouton pressé
  if (digitalRead(bpPin) == HIGH) {
    Serial.println("🔘 Bouton pressé");
    digitalWrite(ledPin, HIGH);
    lastTime = millis();

    if (WiFi.status() == WL_CONNECTED) {
      HTTPClient http;
      http.begin(serverName);
      http.addHeader("Content-Type", "application/json");

      // JSON statique à envoyer
      String json = R"rawliteral(
        {
          "Température": "33",
          "Vitesse_roue": "212",
          "Nombre_tractions": "21",
          "Tension": "230",
          "Intensité": "10"
        }
      )rawliteral";

      // Supprime espaces / retours à la ligne
      json.replace("\n", "");
      json.replace("  ", "");

      int httpResponseCode = http.POST(json);

      Serial.print("📡 Code réponse HTTP : ");
      Serial.println(httpResponseCode);
      Serial.print("📤 JSON envoyé : ");
      Serial.println(json);

      http.end();
    } else {
      Serial.println("⚠️ WiFi non connecté");
    }

    delay(1000);  // Anti-rebond simple
  } else {
    digitalWrite(ledPin, LOW);
  }
}
