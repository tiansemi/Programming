#include <ESP8266WiFi.h>
#include <ESP8266WebServer.h>
const char* ssid = "WIFI INP" , *password = "";

IPAddress local_ip(10,55,9,233);
IPAddress gateway(10,55,0,1);
IPAddress subnet(255,255,128,0);

ESP8266WebServer server(80);

uint8_t LED1pin = D0;
bool LED1status = LOW;
#define PIR D5

//   digitalWrite(LED1pin, LED1status);
void setup() {
  Serial.begin(115200);
  pinMode(LED1pin, OUTPUT);
  pinMode(PIR,INPUT);

  WiFi.softAP(ssid, password);
  WiFi.softAPConfig(local_ip, gateway, subnet);
  delay(80);
  
  server.on("/", handle_OnConnect);
  server.on("/toggle", handle_Toggle);
  server.onNotFound(handle_NotFound);
  
  server.begin();
  Serial.println("HTTP server started");
  Serial.println("PIR Motion OK");
}

unsigned long currentTime ;
unsigned long previousMotionCheckTime = 0;
const unsigned long motionCheckInterval = 150UL ;
const unsigned int DELAY = 15000;
void loop() {
  server.handleClient();
  if(LED1status)
  {digitalWrite(LED1pin, HIGH);}
  else
  {digitalWrite(LED1pin, LOW);}
  currentTime = millis();
  if (currentTime - previousMotionCheckTime >= motionCheckInterval) {
    int isMotionDetected = digitalRead(PIR);
    if (isMotionDetected == 0) {
      Serial.println("Motion ended!");
      LED1status = LED1status;
    } else {
      Serial.println("Motion detected!");
        digitalWrite(LED1pin, HIGH);
        delay(DELAY);
        digitalWrite(LED1pin, LOW);
        LED1status = LOW;
    }
    previousMotionCheckTime = currentTime;
  }
}

void handle_OnConnect() {
  Serial.println("GPIO7 Status: OFF | GPIO6 Status: OFF");
  server.send(200, "text/html", SendHTML(LED1status));
}

void handle_Toggle() {
  LED1status = !LED1status;
  server.send(200, "text/plain", LED1status ? "ON" : "OFF");
}

void handle_NotFound(){
  server.send(404, "text/plain", "Not found");
}

String SendHTML(uint8_t led1stat){
  String ptr = "<!DOCTYPE html> <html>\n";
  ptr +="<head><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=no\">\n";
  ptr +="<title>LED Control</title>\n";
  ptr +="<style>html { font-family: Helvetica; display: inline-block; margin: 0px auto; text-align: center;}\n";
  ptr +="body{margin-top: 50px; background-color: #3d3d3d;} h1 {color: #fff;margin: 50px auto 30px;} h3 {color: #fff;margin-bottom: 50px;}\n";
  ptr +=".button {display: block;width: 153px;background-color: #1abc9c;border: none;color: white;padding: 13px 30px;text-decoration: none;font-size: 25px;margin: 0px auto 35px;cursor: pointer;border-radius: 41px;}\n";
  ptr +=".button-on {background-color: #1abc9c;}\n";
  ptr +=".button-on:active {background-color: #16a085;}\n";
  ptr +=".button-off {background-color: #dd305a;}\n";
  ptr +=".button-off:active {background-color: #2c3e50;}\n";
  ptr +="p {font-size: 14px;color: #fff;margin-bottom: 10px; font-weight: bold;}\n";
  ptr +="</style>\n";
  ptr +="<script>\n";
  ptr +="function toggleLED() {\n";
  ptr +="  var xhttp = new XMLHttpRequest();\n";
  ptr +="  xhttp.onreadystatechange = function() {\n";
  ptr +="    if (this.readyState == 4 && this.status == 200) {\n";
  ptr +="      document.getElementById('led').innerText = 'LED1 Status: ' + this.responseText;\n";
  ptr +="      document.getElementById('tog').innerText = this.responseText == 'ON' ? 'OFF' : 'ON';\n";
  ptr +="      document.getElementById('tog').className = 'button button-' + this.responseText.toLowerCase();\n";
  ptr +="    }\n";
  ptr +="  };\n";
  ptr +="  xhttp.open('GET', '/toggle', true);\n";
  ptr +="  xhttp.send();\n";
  ptr +="}\n";
  ptr +="</script>\n";
  ptr +="</head>\n";
  ptr +="<body>\n";
  ptr +="<marquee behavior=\"scroll\" direction=\"left\"><h1>ESP8266 Web Server</h1></marquee>\n";
  ptr +="<h3>Using Access Point(AP) Mode</h3>\n";
  ptr +="<p id='led'>LED1 Status: " + String(led1stat ? "ON" : "OFF") + "</p>\n";
  ptr +="<button id='tog' class='button button-" + String(led1stat ? "off" : "on") + "' onclick='toggleLED()'>" + String(led1stat ? "OFF" : "ON") + "</button>\n";
  ptr +="</body>\n";
  ptr +="</html>\n";
  return ptr;
}
