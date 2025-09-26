#include <WiFi.h>
#include <HTTPClient.h>
#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <DHT.h>

// ---------------- Configuration LCD ----------------
LiquidCrystal_I2C lcd(0x27, 20, 4);

// ---------------- Configuration DHT11 ----------------
#define DHT_PIN 32
#define DHT_TYPE DHT11
DHT dht(DHT_PIN, DHT_TYPE);

// ---------------- Broches ESP32 ----------------
#define VOLTAGE_SENSOR_PIN 34
#define CURRENT_SENSOR_PIN 35
#define LED_GREEN 25
#define LED_RED 26
#define BUZZER 27
#define RELAIS 23
#define BOUTON 33

// ---------------- Configuration Cloud AWS ----------------
const char* ssid = "WIFI ODC";  // Votre WiFi
const char* password = "Digital1";
const char* awsEndpoint = "https://7a8i1xrmm0.execute-api.eu-north-1.amazonaws.com/";

// ---------------- Calibration ----------------
const float R1 = 30000.0;
const float R2 = 7500.0;
const float VREF = 3.3;
const float ADC_RESOLUTION = 4095.0;
const float ACS_SENSITIVITY = 0.100;

// ---------------- Seuils d'alerte ----------------
const float VOLTAGE_NORMAL_MAX = 5.0;
const float TEMP_ALERTE = 30.0;
const float COURANT_MAX = 2.0;

// ---------------- Variables globales ----------------
float currentOffset = 1.65;
float tension = 0;
float temperature = 0;
float humidity = 0;
float courant = 0;
bool moteurActif = false;
bool calibrationDone = false;

// ---------------- Variables pour envoi cloud ----------------
unsigned long dernierEnvoiCloud = 0;
const unsigned long INTERVALLE_CLOUD = 60000; // Envoi toutes les 10 secondes

// ---------------- Variables pour buzzer ----------------
bool buzzerState = false;
unsigned long lastBuzzerTime = 0;
const unsigned long BUZZER_INTERVAL = 500;

// ---------------- Variables pour anti-rebond ----------------
unsigned long dernierAppui = 0;
const unsigned long DELAI_ANTI_REBOND = 100;

void setup() {
  Serial.begin(115200);
  
  // Initialisation des broches
  pinMode(LED_GREEN, OUTPUT);
  pinMode(LED_RED, OUTPUT);
  pinMode(BUZZER, OUTPUT);
  pinMode(RELAIS, OUTPUT);
  pinMode(BOUTON, INPUT_PULLUP);
  
  // États initiaux
  digitalWrite(LED_GREEN, LOW);
  digitalWrite(LED_RED, LOW);
  digitalWrite(BUZZER, LOW);
  digitalWrite(RELAIS, LOW);

  // Initialisation LCD
  Wire.begin(21, 22);
  lcd.init();
  lcd.backlight();
  
  // Initialisation DHT11
  dht.begin();
  
  // Message de démarrage
  lcd.setCursor(0, 0);
  lcd.print("Systeme Energie  ");
  lcd.setCursor(0, 1);
  lcd.print("Connexion WiFi... ");
  
  // Connexion WiFi
  WiFi.begin(ssid, password);
  int tentatives = 0;
  while (WiFi.status() != WL_CONNECTED && tentatives < 30) {
    delay(500);
    Serial.print(".");
    tentatives++;
    
    // Animation LCD
    lcd.setCursor(0, 2);
    lcd.print("Tentative ");
    lcd.print(tentatives);
    lcd.print("/30   ");
  }
  
  if (WiFi.status() == WL_CONNECTED) {
    lcd.clear();
    lcd.setCursor(0, 0);
    lcd.print("WiFi CONNECTE    ");
    lcd.setCursor(0, 1);
    lcd.print("IP: ");
    lcd.print(WiFi.localIP());
    Serial.println("\n✅ WiFi connecté !");
    delay(2000);
  } else {
    lcd.clear();
    lcd.print("Erreur WiFi!    ");
    Serial.println("❌ Erreur connexion WiFi");
    delay(2000);
  }
  
  // Calibration du capteur de courant
  calibrerCapteurCourant();
  lcd.clear();
}

// CALIBRATION AUTOMATIQUE du point zéro
void calibrerCapteurCourant() {
  lcd.setCursor(0, 0);
  lcd.print("CALIBRATION EN COURS");
  lcd.setCursor(0, 1);
  lcd.print("Ne rien brancher!  ");
  
  float sommeTensions = 0;
  int nombreMesures = 300;
  
  for (int i = 0; i < nombreMesures; i++) {
    int valeurADC = analogRead(CURRENT_SENSOR_PIN);
    float tensionMesuree = (valeurADC / ADC_RESOLUTION) * VREF;
    sommeTensions += tensionMesuree;
    
    if (i % 50 == 0) {
      lcd.setCursor(0, 2);
      lcd.print("Progression: ");
      lcd.print((i * 100) / nombreMesures);
      lcd.print("%   ");
    }
    delay(10);
  }
  
  currentOffset = sommeTensions / nombreMesures;
  calibrationDone = true;
  
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("Calibration OK!   ");
  lcd.setCursor(0, 1);
  lcd.print("Offset: ");
  lcd.print(currentOffset, 3);
  lcd.print("V    ");
  delay(2000);
}

// Fonction pour envoyer les données vers AWS
void envoyerDonneesCloud() {
  if (WiFi.status() != WL_CONNECTED) {
    Serial.println("❌ WiFi déconnecté - Impossible d'envoyer vers cloud");
    return;
  }
  
  HTTPClient http;
  http.begin(awsEndpoint);
  http.addHeader("Content-Type", "application/json");
  
  // Création du JSON selon le format de votre API AWS
  String json = "{";
  json += "\"Température\":\"" + String(temperature, 1) + "\",";
  json += "\"Vitesse_roue\":\"0\",";  // À adapter si vous avez un capteur de vitesse
  json += "\"Nombre_tractions\":\"0\","; // À adapter selon votre application
  json += "\"Tension\":\"" + String(tension, 1) + "\",";
  json += "\"Intensité\":\"" + String(courant, 3) + "\",";
  json += "\"Puissance\":\"" + String(tension * courant, 1) + "\",";
  json += "\"Humidité\":\"" + String(humidity, 1) + "\",";
  json += "\"Moteur\":\"" + String(moteurActif ? "ON" : "OFF") + "\",";
  json += "\"Timestamp\":\"" + String(millis() / 1000) + "\"";
  json += "}";
  
  int httpResponseCode = http.POST(json);
  
  if (httpResponseCode > 0) {
    Serial.print("✅ Données envoyées vers AWS. Code: ");
    Serial.println(httpResponseCode);
    
    // Feedback visuel d'envoi réussi
    lcd.setCursor(19, 0); // Coin supérieur droit
    lcd.print("☁"); // Icône cloud
  } else {
    Serial.print("❌ Erreur envoi AWS. Code: ");
    Serial.println(httpResponseCode);
    
    lcd.setCursor(19, 0);
    lcd.print("❌"); // Icône erreur
  }
  
  Serial.print("📤 JSON envoyé: ");
  Serial.println(json);
  
  http.end();
}

// Fonction pour buzzer intermittent
void handleBuzzer(bool alertActive) {
  if (alertActive) {
    unsigned long currentTime = millis();
    if (currentTime - lastBuzzerTime >= BUZZER_INTERVAL) {
      buzzerState = !buzzerState;
      digitalWrite(BUZZER, buzzerState);
      lastBuzzerTime = currentTime;
    }
  } else {
    digitalWrite(BUZZER, LOW);
    buzzerState = false;
  }
}

// Lecture du DHT11
void lireDHT() {
  static unsigned long dernierLecture = 0;
  if (millis() - dernierLecture >= 2000) {
    float nouvelleTemp = dht.readTemperature();
    float nouvelleHum = dht.readHumidity();
    
    if (!isnan(nouvelleTemp)) temperature = nouvelleTemp;
    if (!isnan(nouvelleHum)) humidity = nouvelleHum;
    
    dernierLecture = millis();
  }
}

// Fonction pour lire le courant avec filtrage
float lireCourant() {
  int adcCurrent = analogRead(CURRENT_SENSOR_PIN);
  float tensionMesuree = (adcCurrent / ADC_RESOLUTION) * VREF;
  
  float tensionCorrigee = tensionMesuree - currentOffset;
  float courantMesure = tensionCorrigee / ACS_SENSITIVITY;
  
  if (abs(courantMesure) < 0.02) {
    courantMesure = 0.0;
  }
  
  return courantMesure;
}

// Gestion du bouton poussoir
void gererBouton() {
  int etatBouton = digitalRead(BOUTON);
  unsigned long tempsActuel = millis();
  
  if (etatBouton == LOW && (tempsActuel - dernierAppui) > DELAI_ANTI_REBOND) {
    moteurActif = !moteurActif;
    digitalWrite(RELAIS, moteurActif ? HIGH : LOW);
    
    // Feedback sonore
    digitalWrite(BUZZER, HIGH);
    delay(100);
    digitalWrite(BUZZER, LOW);
    
    dernierAppui = tempsActuel;
    
    Serial.print("🔘 Bouton appuyé - Moteur: ");
    Serial.println(moteurActif ? "ON" : "OFF");
  }
}

// Gestion des LEDs selon état et alertes
void gererLEDs() {
  bool alerteSurtension = (tension > VOLTAGE_NORMAL_MAX);
  bool alerteTemperature = (temperature > TEMP_ALERTE);
  bool alerteCourant = (courant > COURANT_MAX);
  bool alerteActive = alerteSurtension || alerteTemperature || alerteCourant;
  
  if (alerteActive) {
    digitalWrite(LED_RED, HIGH);
    digitalWrite(LED_GREEN, LOW);
    handleBuzzer(true);
  } else {
    digitalWrite(LED_RED, LOW);
    handleBuzzer(false);
    digitalWrite(LED_GREEN, moteurActif ? HIGH : LOW);
  }
}

// Affichage sur LCD
void afficherLCD() {
  // Ligne 0: Tension et Courant
  lcd.setCursor(0, 0);
  lcd.print("V:");
  lcd.print(tension, 1);
  lcd.print(" I:");
  lcd.print(courant, 3);
  lcd.print("  ");
  
  // Ligne 1: Puissance et Température
  lcd.setCursor(0, 1);
  lcd.print("P:");
  lcd.print(tension * courant, 1);
  lcd.print("W T:");
  lcd.print(temperature, 1);
  lcd.print("C ");
  
  // Ligne 2: Humidité et état WiFi
  lcd.setCursor(0, 2);
  lcd.print("H:");
  lcd.print(humidity, 0);
  lcd.print("% WiFi:");
  lcd.print(WiFi.status() == WL_CONNECTED ? "OK" : "KO");
  
  // Ligne 3: État moteur et alertes
  lcd.setCursor(0, 3);
  if (tension > VOLTAGE_NORMAL_MAX) {
    lcd.print("ALERTE: SURTENSION!");
  } else if (temperature > TEMP_ALERTE) {
    lcd.print("ALERTE: TEMP HAUTE!");
  } else if (courant > COURANT_MAX) {
    lcd.print("ALERTE: COURANT MAX!");
  } else {
    lcd.print("Moteur:");
    lcd.print(moteurActif ? "ON " : "OFF");
    lcd.print(" Cloud:OK");
  }
}

void loop() {
  // Gestion des entrées
  gererBouton();
  
  // Lecture des capteurs
  lireDHT();
  int adcVoltage = analogRead(VOLTAGE_SENSOR_PIN);
  float voltageOutput = (adcVoltage / ADC_RESOLUTION) * VREF;
  tension = voltageOutput * ((R1 + R2) / R2);
  courant = lireCourant();
  
  // Gestion des sorties
  gererLEDs();
  afficherLCD();
  
  // Envoi périodique vers AWS
  if (millis() - dernierEnvoiCloud >= INTERVALLE_CLOUD) {
    if (WiFi.status() == WL_CONNECTED) {
      envoyerDonneesCloud();
    } else {
      Serial.println("⚠️ Tentative de reconnexion WiFi...");
      WiFi.reconnect();
    }
    dernierEnvoiCloud = millis();
  }
  
  // Debug série
  static unsigned long dernierDebug = 0;
  if (millis() - dernierDebug >= 2000) {
    Serial.print("📊 V:");
    Serial.print(tension, 1);
    Serial.print("V I:");
    Serial.print(courant, 3);
    Serial.print("A T:");
    Serial.print(temperature, 1);
    Serial.print("C Moteur:");
    Serial.print(moteurActif ? "ON" : "OFF");
    Serial.print(" WiFi:");
    Serial.println(WiFi.status() == WL_CONNECTED ? "OK" : "KO");
    dernierDebug = millis();
  }
  
  delay(100);
}
