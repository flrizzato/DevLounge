#include <WiFi.h>
#include <ESPAsyncWebServer.h>
#include <DHT.h>
#include <NewPing.h>

//Access Wi-Fi 
//const char* ssid = "Techtrends Corp";
//const char* password = "T3chtrend$@2024";
const char * ssid = "Santhiago";
const char * password = "sam07145";

// DHT22
#define DHTPIN 4
#define DHTTYPE DHT11
DHT dht(DHTPIN, DHTTYPE);

// LED
#define LED_PIN 42

// Buzzer
#define BUZZER_PIN 6

// Relés
#define RELE1 45
#define RELE2 48
#define RELE3 47

// Sensor de Proximidade (HC-SR04)
#define TRIGGER_PIN 16
#define ECHO_PIN 4
NewPing sonar(TRIGGER_PIN, ECHO_PIN, 200); // max 200cm

//Potenciometro
#define POT_PIN 9 

//Sensor Presença
#define PIR_PIN 46  

// Servidor Web
AsyncWebServer server(80);

void setup() {
  Serial.begin(115200);

  // Inicia sensores
  dht.begin();
  pinMode(LED_PIN, OUTPUT);
  pinMode(BUZZER_PIN, OUTPUT);
  pinMode(PIR_PIN, INPUT);
  pinMode(RELE1, OUTPUT);
  pinMode(RELE2, OUTPUT);
  pinMode(RELE3, OUTPUT);

  // Desligar todos no início
  digitalWrite(RELE1, LOW);
  digitalWrite(RELE2, LOW);
  digitalWrite(RELE3, LOW);


  // Wi-Fi
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) delay(500);
  Serial.println(WiFi.localIP());
 

  // Rotas
  
//Ip associado ao ESP32
server.on("/ip", HTTP_GET, [](AsyncWebServerRequest *request){
  String ip = WiFi.localIP().toString();
  request->send(200, "text/plain", ip);
   //## Obter IP do Esp32 na REDE ###
  //http://192.168.15.63/ip
});

  // Obter Temperatura
  //### Temperatura C ###
  //http://192.168.15.63/temperatura
  server.on("/temperatura", HTTP_GET, [](AsyncWebServerRequest *request){
    float t = dht.readTemperature();
          if (isnan(t)) {
    AsyncWebServerResponse *response = request->beginResponse(500, "text/plain", "Erro");
    response->addHeader("Access-Control-Allow-Origin", "*");   //Tratamento de CORS
    request->send(response);
  } else {
    AsyncWebServerResponse *response = request->beginResponse(200, "text/plain", String((int)t));
    response->addHeader("Access-Control-Allow-Origin", "*");    //Tratamento de CORS
    request->send(response);
  }
  });

  // Ligar e desligar LEDS
  // Ligar LED: http://192.168.15.63/led?estado=on
  // Desligar LED: http://192.168.15.63/led?estado=off
  server.on("/led", HTTP_GET, [](AsyncWebServerRequest *request){
    if (request->hasParam("estado")) {
      String estado = request->getParam("estado")->value();
      digitalWrite(LED_PIN, estado == "on" ? HIGH : LOW);
      request->send(200, "text/plain", "LED " + estado);
    } else {
      request->send(400, "text/plain", "Faltando parâmetro");
    }
   });

   // Ligar e desligar RELES
   // Ligar Rele 1: http://192.168.15.63/rele1?estado=on
   // Desligar Rele 1: http://192.168.15.63/rele1?estado=off
  server.on("/rele1", HTTP_GET, [](AsyncWebServerRequest *request){
  String estado = request->getParam("estado")->value();
  digitalWrite(RELE1, estado == "on" ? HIGH : LOW);
  request->send(200, "text/plain", "Relé 1 " + estado);
});
  
  server.on("/rele2", HTTP_GET, [](AsyncWebServerRequest *request){
  String estado = request->getParam("estado")->value();
  digitalWrite(RELE2, estado == "on" ? HIGH : LOW);
  request->send(200, "text/plain", "Relé 2 " + estado);
});
  server.on("/rele3", HTTP_GET, [](AsyncWebServerRequest *request){
  String estado = request->getParam("estado")->value();
  digitalWrite(RELE3, estado == "on" ? HIGH : LOW);
  request->send(200, "text/plain", "Relé 3 " + estado);
  });

  // Ligar Buzzer
  // http://192.168.15.63/buzzer
  server.on("/buzzer", HTTP_GET, [](AsyncWebServerRequest *request){
  tone(BUZZER_PIN, 1000, 300); // 1000 Hz por 300 ms
  request->send(200, "text/plain", "Buzzer acionado");
  });

  // Obter Proximidade
  // http://192.168.15.63/proximidade
  server.on("/proximidade", HTTP_GET, [](AsyncWebServerRequest *request){
    int distancia = sonar.ping_cm();
    request->send(200, "text/plain", String(distancia));
  });

  // Dados Potenciometro
  // http://192.168.15.63/potenciometro
  server.on("/potenciometro", HTTP_GET, [](AsyncWebServerRequest *request){
  int valor = analogRead(POT_PIN);
  AsyncWebServerResponse *response = request->beginResponse(200, "text/plain", String(valor));
  response->addHeader("Access-Control-Allow-Origin", "*");  // <- habilita CORS
  request->send(response);
});
  // Dados de Presença
  // http://192.168.15.63/presenca
  server.on("/presenca", HTTP_GET, [](AsyncWebServerRequest *request){
  int estado = digitalRead(PIR_PIN);
  String resposta = estado == HIGH ? "Presença Detectada" : "Sem Movimento";
  AsyncWebServerResponse *response = request->beginResponse(200, "text/plain", resposta);
  response->addHeader("Access-Control-Allow-Origin", "*");
  request->send(response);
});

  server.begin();
}

void loop(){
  //TESTE TEMPERATURA
  //float t = dht.readTemperature();
  //Serial.print("%  Temperature: ");
  //Serial.print(t);
  //Serial.print("°C, ");

  //TESTE POTENCIOMETRO
  //int valor = analogRead(POT_PIN);
  //Serial.println(valor);
  //delay(500);
  
  //TESTE SENSOR PRESENÇA
  pinMode(PIR_PIN, INPUT);
 

}


