🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.6 Consommation d'API publiques

## Introduction

Maintenant que vous savez utiliser TFPHttpClient, il est temps de mettre vos connaissances en pratique en consommant de vraies API publiques ! Il existe des milliers d'API gratuites qui vous permettent d'accéder à des données variées : météo, géolocalisation, informations financières, réseaux sociaux, et bien plus encore.

## Qu'est-ce qu'une API Publique ?

Une **API publique** est une interface de programmation accessible à tout développeur, généralement via Internet. Ces API permettent d'accéder à des services et des données sans avoir à les créer soi-même.

### Types d'API Publiques

**1. API Gratuites sans Authentification**
- Accès libre, sans inscription
- Idéales pour débuter et tester
- Exemples : JSONPlaceholder, REST Countries

**2. API Gratuites avec Clé d'API**
- Inscription requise pour obtenir une clé
- Limites de requêtes (ex: 1000 requêtes/jour)
- Exemples : OpenWeatherMap, NewsAPI

**3. API Freemium**
- Version gratuite avec limitations
- Version payante pour plus de fonctionnalités
- Exemples : Google Maps API, Stripe

**4. API Payantes**
- Nécessitent un abonnement
- Pour des services professionnels
- Exemples : certaines API de données financières

## Trouver des API Publiques

### Ressources Utiles

**Sites de référencement d'API :**
- **RapidAPI** (rapidapi.com) : marketplace d'API avec des milliers d'options
- **Public APIs** (github.com/public-apis/public-apis) : liste GitHub d'API gratuites
- **API List** (apilist.fun) : répertoire d'API avec filtres
- **ProgrammableWeb** : annuaire historique d'API

### Catégories Populaires

```
Météo              OpenWeatherMap, WeatherAPI  
Géolocalisation    OpenStreetMap, ipapi.co  
Finance            Alpha Vantage, CoinGecko  
Actualités         NewsAPI, Guardian API  
Réseaux sociaux    Twitter API, Reddit API  
Données publiques  Data.gouv.fr, Open Data  
Divertissement     TMDB (films), Spotify, PokeAPI  
Outils             QR Code Generator, PDF APIs  
```

## Lire la Documentation d'une API

Avant d'utiliser une API, il est **essentiel** de lire sa documentation.

### Éléments à Rechercher

**1. URL de Base (Base URL)**
```
https://api.example.com/v1
```

**2. Points de Terminaison (Endpoints)**
```
GET  /users           → Liste des utilisateurs  
GET  /users/{id}      → Un utilisateur spécifique  
POST /users           → Créer un utilisateur  
```

**3. Authentification**
```
X-API-Key: votre_cle_secrete  
Authorization: Bearer token  
```

**4. Paramètres de Requête**
```
?page=1&limit=10&sort=name
```

**5. Format de Réponse**
```json
{
  "status": "success",
  "data": { ... }
}
```

**6. Codes d'Erreur**
```
200 OK
400 Bad Request
401 Unauthorized
429 Too Many Requests
```

**7. Limites de Taux (Rate Limiting)**
```
1000 requêtes / jour
10 requêtes / seconde
```

## Exemple 1 : JSONPlaceholder (API de Test)

JSONPlaceholder est une API gratuite sans authentification, parfaite pour débuter.

### Informations

- **URL** : https://jsonplaceholder.typicode.com
- **Authentification** : Aucune
- **Limites** : Aucune
- **Documentation** : jsonplaceholder.typicode.com

### Endpoints Disponibles

```
/posts       → 100 articles de blog
/comments    → 500 commentaires
/albums      → 100 albums
/photos      → 5000 photos
/todos       → 200 tâches
/users       → 10 utilisateurs
```

### Exemple de Code : Lister les Utilisateurs

```pascal
program JsonPlaceholderExample;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

procedure ListUsers;  
var  
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      WriteLn('Récupération des utilisateurs...');
      Response := Client.Get('https://jsonplaceholder.typicode.com/users');

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('=== Utilisateurs (JSON brut) ===');
        WriteLn(Response);
      end
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

begin
  ListUsers;
  ReadLn;
end.
```

### Obtenir un Utilisateur Spécifique

```pascal
procedure GetUser(UserID: Integer);  
var  
  Client: TFPHttpClient;
  Response: String;
  URL: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      URL := 'https://jsonplaceholder.typicode.com/users/' + IntToStr(UserID);
      WriteLn('Récupération de l''utilisateur ', UserID, '...');

      Response := Client.Get(URL);

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('=== Utilisateur #', UserID, ' ===');
        WriteLn(Response);
      end
      else if Client.ResponseStatusCode = 404 then
        WriteLn('Utilisateur introuvable')
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
begin
  GetUser(1);
  GetUser(999);  // N'existe pas → 404
end;
```

### Créer un Nouvel Article

```pascal
procedure CreatePost(const Title, Body: String; UserID: Integer);  
var  
  Client: TFPHttpClient;
  JsonData, Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      // Construire le JSON
      JsonData := Format('{"title":"%s","body":"%s","userId":%d}',
                        [Title, Body, UserID]);

      // Configurer les headers
      Client.AddHeader('Content-Type', 'application/json');
      Client.RequestBody := TRawByteStringStream.Create(JsonData);

      try
        WriteLn('Création d''un article...');
        Response := Client.Post('https://jsonplaceholder.typicode.com/posts');

        if Client.ResponseStatusCode = 201 then
        begin
          WriteLn('Article créé avec succès !');
          WriteLn(Response);
        end
        else
          WriteLn('Erreur : Code ', Client.ResponseStatusCode);

      finally
        Client.RequestBody.Free;
      end;

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
begin
  CreatePost('Mon Premier Article',
             'Ceci est le contenu de mon article.',
             1);
end;
```

## Exemple 2 : OpenWeatherMap (Météo)

OpenWeatherMap fournit des données météorologiques en temps réel.

### Informations

- **URL** : https://api.openweathermap.org/data/2.5
- **Authentification** : Clé API (gratuite avec inscription)
- **Limites Gratuites** : 60 requêtes/minute, 1 000 000/mois
- **Documentation** : openweathermap.org/api

### Obtenir une Clé API

1. Créer un compte sur openweathermap.org
2. Aller dans "API keys"
3. Copier votre clé (ex: `abc123def456`)

### Exemple : Météo Actuelle

```pascal
program WeatherApp;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

const
  API_KEY = 'VOTRE_CLE_API_ICI';  // Remplacer par votre vraie clé
  BASE_URL = 'https://api.openweathermap.org/data/2.5';

procedure GetCurrentWeather(const City: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  URL: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      // Construire l'URL avec les paramètres
      URL := Format('%s/weather?q=%s&appid=%s&units=metric&lang=fr',
                    [BASE_URL, City, API_KEY]);

      WriteLn('Récupération de la météo pour ', City, '...');
      Response := Client.Get(URL);

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('=== Météo à ', City, ' ===');
        WriteLn(Response);
        WriteLn;
        WriteLn('Note : Les données sont en JSON. ');
        WriteLn('Dans la prochaine section, nous apprendrons à parser ce JSON !');
      end
      else if Client.ResponseStatusCode = 404 then
        WriteLn('Ville introuvable')
      else if Client.ResponseStatusCode = 401 then
        WriteLn('Clé API invalide')
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

begin
  GetCurrentWeather('Paris');
  WriteLn;
  GetCurrentWeather('London');
  WriteLn;
  GetCurrentWeather('Tokyo');
  ReadLn;
end.
```

### Paramètres Utiles

```
q         → Nom de la ville (ex: Paris, London)  
units     → metric (Celsius), imperial (Fahrenheit), standard (Kelvin)  
lang      → Langue (fr, en, es, de, etc.)  
appid     → Votre clé API (obligatoire)  
```

### Exemple de Réponse JSON

```json
{
  "coord": {"lon": 2.3488, "lat": 48.8534},
  "weather": [
    {
      "id": 800,
      "main": "Clear",
      "description": "ciel dégagé",
      "icon": "01d"
    }
  ],
  "main": {
    "temp": 18.5,
    "feels_like": 17.8,
    "temp_min": 16.2,
    "temp_max": 20.1,
    "pressure": 1013,
    "humidity": 65
  },
  "wind": {
    "speed": 3.5,
    "deg": 220
  },
  "name": "Paris"
}
```

## Exemple 3 : REST Countries (Informations sur les Pays)

API gratuite fournissant des informations détaillées sur tous les pays du monde.

### Informations

- **URL** : https://restcountries.com/v3.1
- **Authentification** : Aucune
- **Limites** : Aucune officielle
- **Documentation** : restcountries.com

### Endpoints

```
/all                    → Tous les pays
/name/{name}            → Recherche par nom
/alpha/{code}           → Par code ISO (FR, US, JP)
/region/{region}        → Par région (Europe, Asia, etc.)
```

### Exemple : Informations sur un Pays

```pascal
procedure GetCountryInfo(const CountryName: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  URL: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      URL := 'https://restcountries.com/v3.1/name/' + CountryName;
      WriteLn('Recherche d''informations sur : ', CountryName);

      Response := Client.Get(URL);

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('=== Informations trouvées ===');
        WriteLn(Response);
      end
      else if Client.ResponseStatusCode = 404 then
        WriteLn('Pays introuvable')
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
begin
  GetCountryInfo('France');
  WriteLn;
  GetCountryInfo('Japan');
end;
```

### Pays d'une Région

```pascal
procedure GetCountriesByRegion(const Region: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  URL: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      URL := 'https://restcountries.com/v3.1/region/' + Region;
      WriteLn('Pays de la région : ', Region);

      Response := Client.Get(URL);

      if Client.ResponseStatusCode = 200 then
        WriteLn('Nombre de caractères reçus : ', Length(Response))
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

// Régions disponibles : Africa, Americas, Asia, Europe, Oceania
begin
  GetCountriesByRegion('Europe');
end;
```

## Exemple 4 : GitHub API (Informations sur les Dépôts)

L'API GitHub permet d'accéder aux informations publiques des dépôts et utilisateurs.

### Informations

- **URL** : https://api.github.com
- **Authentification** : Optionnelle (mais recommandée pour plus de requêtes)
- **Limites** : 60 requêtes/heure sans auth, 5000 avec token
- **Documentation** : docs.github.com/rest

### Exemple : Informations sur un Dépôt

```pascal
procedure GetRepoInfo(const Owner, RepoName: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  URL: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      // GitHub exige un User-Agent
      Client.AddHeader('User-Agent', 'FreePascal-App/1.0');

      URL := Format('https://api.github.com/repos/%s/%s', [Owner, RepoName]);
      WriteLn('Récupération des infos du dépôt : ', Owner, '/', RepoName);

      Response := Client.Get(URL);

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('=== Informations du dépôt ===');
        WriteLn(Copy(Response, 1, 500), '...');  // Extrait
        WriteLn;
        WriteLn('Limite de requêtes restantes : ',
                Client.GetHeader(Client.ResponseHeaders, 'X-RateLimit-Remaining'));
      end
      else if Client.ResponseStatusCode = 404 then
        WriteLn('Dépôt introuvable')
      else
        WriteLn('Erreur : Code ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
begin
  GetRepoInfo('fpc', 'FPCSource');  // Le code source de FreePascal
  WriteLn;
  GetRepoInfo('lazarus-ide', 'lazarus');  // Lazarus IDE
end;
```

## Gestion de l'Authentification

### API Key dans l'URL (Query Parameter)

```pascal
// Exemple avec OpenWeatherMap
URL := Format('https://api.example.com/data?apikey=%s', [API_KEY]);  
Response := Client.Get(URL);  
```

### API Key dans les Headers

```pascal
// Exemple classique
Client.AddHeader('X-API-Key', API_KEY);  
Response := Client.Get('https://api.example.com/data');  
```

### Bearer Token (OAuth, JWT)

```pascal
// Authentification par token (ex: GitHub)
Client.AddHeader('Authorization', 'Bearer ' + TOKEN);  
Response := Client.Get('https://api.example.com/data');  
```

### Basic Authentication

```pascal
uses Base64;

var
  Credentials: String;
begin
  // Format : username:password encodé en Base64
  Credentials := EncodeStringBase64('username:password');
  Client.AddHeader('Authorization', 'Basic ' + Credentials);
  Response := Client.Get('https://api.example.com/data');
end;
```

## Gestion du Rate Limiting

La plupart des API ont des **limites de taux** pour éviter les abus.

### Vérifier les Limites

```pascal
procedure CheckRateLimits(Client: TFPHttpClient);  
var  
  Limit, Remaining, Reset: String;
begin
  // Lire les headers de limitation
  Limit := Client.GetHeader(Client.ResponseHeaders, 'X-RateLimit-Limit');
  Remaining := Client.GetHeader(Client.ResponseHeaders, 'X-RateLimit-Remaining');
  Reset := Client.GetHeader(Client.ResponseHeaders, 'X-RateLimit-Reset');

  WriteLn('=== Rate Limiting ===');
  WriteLn('Limite : ', Limit);
  WriteLn('Restantes : ', Remaining);
  WriteLn('Réinitialisation : ', Reset);
end;
```

### Gérer l'Erreur 429 (Too Many Requests)

```pascal
procedure GetDataWithRetry(const URL: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  RetryAfter: String;
  WaitSeconds: Integer;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      Response := Client.Get(URL);

      if Client.ResponseStatusCode = 429 then
      begin
        // Récupérer le délai d'attente
        RetryAfter := Client.GetHeader(Client.ResponseHeaders, 'Retry-After');
        WaitSeconds := StrToIntDef(RetryAfter, 60);

        WriteLn('Limite de taux atteinte !');
        WriteLn('Veuillez attendre ', WaitSeconds, ' secondes.');
      end
      else if Client.ResponseStatusCode = 200 then
        WriteLn('Données récupérées avec succès');

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end;
```

## Classe Helper Réutilisable

Créons une classe helper pour faciliter l'utilisation d'API.

```pascal
unit APIHelper;

{$mode objfpc}{$H+}

interface

uses
  fphttpclient, opensslsockets, Classes, SysUtils;

type
  TAPIHelper = class
  private
    FClient: TFPHttpClient;
    FBaseURL: String;
    FAPIKey: String;
  public
    constructor Create(const BaseURL: String; const APIKey: String = '');
    destructor Destroy; override;

    function Get(const Endpoint: String): String;
    function Post(const Endpoint: String; const JsonData: String): String;
    function Put(const Endpoint: String; const JsonData: String): String;
    function Delete(const Endpoint: String): Boolean;

    function GetStatusCode: Integer;
    function GetHeader(const HeaderName: String): String;

    procedure AddHeader(const Name, Value: String);
    procedure SetTimeout(Milliseconds: Integer);
  end;

implementation

constructor TAPIHelper.Create(const BaseURL: String; const APIKey: String = '');  
begin  
  inherited Create;
  FBaseURL := BaseURL;
  FAPIKey := APIKey;

  FClient := TFPHttpClient.Create(nil);
  FClient.AddHeader('Accept', 'application/json');
  FClient.AddHeader('User-Agent', 'FreePascal-App/1.0');

  if APIKey <> '' then
    FClient.AddHeader('X-API-Key', APIKey);
end;

destructor TAPIHelper.Destroy;  
begin  
  FClient.Free;
  inherited Destroy;
end;

function TAPIHelper.Get(const Endpoint: String): String;  
var  
  URL: String;
begin
  Result := '';
  try
    URL := FBaseURL + Endpoint;
    Result := FClient.Get(URL);
  except
    on E: Exception do
      WriteLn('Erreur GET : ', E.Message);
  end;
end;

function TAPIHelper.Post(const Endpoint: String; const JsonData: String): String;  
var  
  URL: String;
begin
  Result := '';
  try
    URL := FBaseURL + Endpoint;
    FClient.AddHeader('Content-Type', 'application/json');
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result := FClient.Post(URL);
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur POST : ', E.Message);
  end;
end;

function TAPIHelper.Put(const Endpoint: String; const JsonData: String): String;  
var  
  URL: String;
begin
  Result := '';
  try
    URL := FBaseURL + Endpoint;
    FClient.AddHeader('Content-Type', 'application/json');
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result := FClient.Put(URL);
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur PUT : ', E.Message);
  end;
end;

function TAPIHelper.Delete(const Endpoint: String): Boolean;  
var  
  URL: String;
begin
  Result := False;
  try
    URL := FBaseURL + Endpoint;
    FClient.Delete(URL);
    Result := (FClient.ResponseStatusCode = 200) or
              (FClient.ResponseStatusCode = 204);
  except
    on E: Exception do
      WriteLn('Erreur DELETE : ', E.Message);
  end;
end;

function TAPIHelper.GetStatusCode: Integer;  
begin  
  Result := FClient.ResponseStatusCode;
end;

function TAPIHelper.GetHeader(const HeaderName: String): String;  
begin  
  Result := FClient.GetHeader(FClient.ResponseHeaders, HeaderName);
end;

procedure TAPIHelper.AddHeader(const Name, Value: String);  
begin  
  FClient.AddHeader(Name, Value);
end;

procedure TAPIHelper.SetTimeout(Milliseconds: Integer);  
begin  
  FClient.ConnectTimeout := Milliseconds;
end;

end.
```

### Utilisation de l'Helper

```pascal
program UseAPIHelper;

uses
  APIHelper, SysUtils;

var
  API: TAPIHelper;
  Response: String;
begin
  // Utilisation avec JSONPlaceholder
  API := TAPIHelper.Create('https://jsonplaceholder.typicode.com');
  try
    // GET
    Response := API.Get('/users/1');
    if API.GetStatusCode = 200 then
      WriteLn(Response);

    // POST
    Response := API.Post('/posts', '{"title":"Test","body":"Contenu","userId":1}');
    if API.GetStatusCode = 201 then
      WriteLn('Créé : ', Response);

  finally
    API.Free;
  end;

  WriteLn;

  // Utilisation avec une API nécessitant une clé
  API := TAPIHelper.Create('https://api.example.com', 'ma_cle_api');
  try
    API.SetTimeout(10000);  // 10 secondes
    Response := API.Get('/data');
    WriteLn('Statut : ', API.GetStatusCode);
  finally
    API.Free;
  end;

  ReadLn;
end.
```

## Bonnes Pratiques

### 1. Respecter les Limites de Taux

```pascal
✅ FAIRE
- Lire la documentation sur les limites
- Espacer vos requêtes
- Vérifier les headers X-RateLimit-*
- Gérer l'erreur 429 gracieusement

❌ ÉVITER
- Faire des boucles serrées de requêtes
- Ignorer les erreurs 429
- Dépasser les limites sans raison
```

### 2. Sécuriser les Clés API

```pascal
❌ DANGEREUX
const API_KEY = 'abc123';  // En dur dans le code

✅ MIEUX
// Lire depuis un fichier de configuration
function LoadAPIKey: String;  
var  
  F: TextFile;
begin
  AssignFile(F, 'config.txt');
  Reset(F);
  ReadLn(F, Result);
  CloseFile(F);
end;

✅ ENCORE MIEUX
// Utiliser des variables d'environnement
Result := GetEnvironmentVariable('API_KEY');
```

### 3. Gérer les Erreurs Réseau

```pascal
✅ TOUJOURS
try
  Response := Client.Get(URL);
except
  on E: EHTTPClient do
    // Erreur HTTP spécifique
  on E: Exception do
    // Erreur générale
end;
```

### 4. Logger les Requêtes (pour le débogage)

```pascal
procedure LogRequest(const Method, URL: String; StatusCode: Integer);  
begin  
  WriteLn(Format('[%s] %s %s -> %d',
         [FormatDateTime('hh:nn:ss', Now), Method, URL, StatusCode]));
end;
```

### 5. Valider les Réponses

```pascal
✅ VÉRIFIER
- Le code de statut (200, 201, etc.)
- Le format de la réponse (JSON valide ?)
- La présence des données attendues

if (StatusCode = 200) and (Length(Response) > 0) then
  // Traiter les données
```

### 6. Utiliser HTTPS

```pascal
✅ TOUJOURS
https://api.example.com

❌ ÉVITER (pour données sensibles)
http://api.example.com
```

## Dépannage des Problèmes Courants

### Problème : Erreur SSL/TLS

**Symptôme :** Exception lors de requêtes HTTPS

**Solution :**
```pascal
1. Vérifier que opensslsockets est inclus
2. S'assurer que les DLL OpenSSL sont disponibles
3. Sur Windows : libcrypto-1_1.dll et libssl-1_1.dll
4. Sur Linux : installer openssl (apt install openssl)
```

### Problème : Timeout

**Symptôme :** La requête prend trop de temps

**Solution :**
```pascal
Client.ConnectTimeout := 10000;  // 10 secondes
```

### Problème : 401 Unauthorized

**Symptôme :** Erreur d'authentification

**Solution :**
```pascal
1. Vérifier que la clé API est correcte
2. Vérifier le format d'authentification (header, query param)
3. Vérifier que la clé n'a pas expiré
```

### Problème : 429 Too Many Requests

**Symptôme :** Trop de requêtes

**Solution :**
```pascal
1. Espacer les requêtes (Sleep entre les appels)
2. Implémenter un système de cache
3. Utiliser une version payante de l'API
```

### Problème : Réponse vide

**Symptôme :** Response = ''

**Solution :**
```pascal
1. Vérifier le code de statut
2. Lire ResponseHeaders pour plus d'infos
3. Vérifier que l'endpoint existe
4. Activer le logging pour voir ce qui se passe
```

## Liste d'API Publiques Recommandées

### Pour Débuter (Sans Clé)

```
JSONPlaceholder      → Test d'API REST  
httpbin.org          → Tester requêtes HTTP  
Random User          → Générer des utilisateurs fictifs  
REST Countries       → Infos sur les pays  
PokeAPI              → Données Pokémon  
```

### Avec Clé Gratuite

```
OpenWeatherMap       → Météo  
NewsAPI              → Actualités  
ExchangeRate-API     → Taux de change  
IPGeolocation        → Géolocalisation par IP  
TheMovieDB (TMDB)    → Informations sur les films  
```

### Données Publiques

```
Data.gouv.fr         → Données publiques françaises  
NASA API             → Données spatiales  
Open Library         → Base de données de livres  
```

## Ressources et Documentation

### Documentation Officielle FreePascal

- **FCL (Free Component Library)** : wiki.freepascal.org
- **TFPHttpClient** : freepascal.org/docs-html/fcl/fphttpclient

### Outils de Test d'API

- **Postman** : Tester les API avant de coder
- **Insomnia** : Alternative à Postman
- **curl** : Ligne de commande
- **HTTPie** : curl moderne et coloré

### Apprentissage

- **MDN Web Docs** : Documentation HTTP
- **REST API Tutorial** : restapitutorial.com
- **GitHub Public APIs** : Liste d'API avec exemples

## Résumé des Points Clés

1. **API publiques** = accès à des services et données externes
2. **Documentation** = essentielle avant d'utiliser une API
3. **Authentification** = API key, Bearer token, Basic auth
4. **Rate limiting** = respecter les limites de requêtes
5. **Codes de statut** = toujours vérifier (200, 401, 429, etc.)
6. **Gestion d'erreurs** = try-except obligatoire
7. **HTTPS** = préférer pour la sécurité
8. **Clés API** = ne jamais les exposer publiquement
9. **Helper classes** = créer des abstractions réutilisables
10. **Testing** = tester avec Postman avant de coder

## Ce Qu'il Faut Retenir pour la Suite

Maintenant que vous savez consommer des API publiques, vous êtes prêt à :
- **Parser le JSON** reçu des API avec fpjson
- Extraire les données qui vous intéressent
- Créer des applications qui agrègent des données de plusieurs API
- Construire des dashboards, applications météo, etc.

Dans la prochaine section, nous allons enfin apprendre à **parser et manipuler le JSON** avec la bibliothèque fpjson de FreePascal pour extraire les informations utiles !

⏭️ [Parsing JSON avec fpjson](/17-communications-reseau-api-rest/07-parsing-json-fpjson.md)
