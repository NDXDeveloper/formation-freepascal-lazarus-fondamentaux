🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.9 Headers et authentification basique

## Introduction

Les **headers HTTP** (en-têtes) sont des informations supplémentaires envoyées avec chaque requête et réponse HTTP. Ils permettent au client et au serveur de communiquer des métadonnées importantes : type de contenu, authentification, préférences linguistiques, etc. Comprendre et maîtriser les headers est essentiel pour travailler efficacement avec les API.

## Qu'est-ce qu'un Header HTTP ?

### Analogie

Imaginez que vous envoyez une lettre par la poste :
- **Le contenu de la lettre** = le corps (body) de la requête HTTP
- **L'enveloppe avec l'adresse, le timbre, la mention "Urgent"** = les headers HTTP

Les headers donnent des instructions sur **comment traiter** le message, sans faire partie du message lui-même.

### Structure d'un Header

Un header est une paire `Nom: Valeur` :

```
User-Agent: Mozilla/5.0
Content-Type: application/json
Authorization: Bearer abc123token
Accept-Language: fr-FR,fr
```

**Format :** `Nom-Du-Header: valeur`

### Headers de Requête vs Réponse

**Headers de requête** (envoyés par le client) :
```
GET /api/users HTTP/1.1
Host: api.example.com
User-Agent: FreePascal-App/1.0
Accept: application/json
Authorization: Bearer token123
```

**Headers de réponse** (renvoyés par le serveur) :
```
HTTP/1.1 200 OK
Content-Type: application/json
Content-Length: 1234
Date: Wed, 15 Oct 2025 10:30:00 GMT
X-RateLimit-Remaining: 450
```

## Headers Courants de Requête

### 1. Host

Indique le nom de domaine du serveur cible.

```
Host: api.example.com
```

**Important :** Obligatoire en HTTP/1.1. TFPHttpClient l'ajoute automatiquement.

### 2. User-Agent

Identifie le client qui fait la requête (navigateur, application).

```pascal
Client.AddHeader('User-Agent', 'MonAppFreePascal/1.0');
```

**Exemples réels :**
```
User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64)
User-Agent: curl/7.68.0
User-Agent: PostmanRuntime/7.26.8
User-Agent: FreePascal-App/2.0 (Windows; FreePascal 3.2.2)
```

**Pourquoi c'est important :**
- Certaines API **exigent** un User-Agent (ex: GitHub API)
- Permet au serveur de savoir qui consomme son API
- Utile pour les statistiques

### 3. Accept

Indique au serveur quel type de contenu le client accepte en réponse.

```pascal
Client.AddHeader('Accept', 'application/json');
```

**Valeurs courantes :**
```
Accept: application/json          → Je veux du JSON
Accept: text/html                 → Je veux du HTML
Accept: application/xml           → Je veux du XML
Accept: image/png                 → Je veux une image PNG
Accept: */*                       → J'accepte tout
```

**Format de qualité :**
```
Accept: application/json, text/html;q=0.9, */*;q=0.8
```
Le paramètre `q` (quality) indique la préférence (0 à 1).

### 4. Content-Type

Indique le type de contenu envoyé dans le corps de la requête (pour POST, PUT).

```pascal
Client.AddHeader('Content-Type', 'application/json');
```

**Valeurs courantes :**
```
Content-Type: application/json                      → JSON
Content-Type: application/x-www-form-urlencoded     → Formulaire
Content-Type: multipart/form-data                   → Upload de fichier
Content-Type: text/plain                            → Texte brut
Content-Type: application/xml                       → XML
```

### 5. Accept-Language

Indique les langues préférées pour la réponse.

```pascal
Client.AddHeader('Accept-Language', 'fr-FR,fr;q=0.9,en;q=0.8');
```

**Exemples :**
```
Accept-Language: fr-FR           → Français (France) uniquement
Accept-Language: en-US,en        → Anglais américain, puis anglais
Accept-Language: fr,en;q=0.5     → Français prioritaire, anglais secondaire
```

### 6. Accept-Encoding

Indique les algorithmes de compression acceptés.

```
Accept-Encoding: gzip, deflate, br
```

**Note :** TFPHttpClient peut gérer automatiquement la décompression.

### 7. Referer

Indique l'URL de la page qui a déclenché la requête.

```pascal
Client.AddHeader('Referer', 'https://example.com/page');
```

### 8. Cache-Control

Contrôle la mise en cache.

```pascal
Client.AddHeader('Cache-Control', 'no-cache');
```

**Valeurs courantes :**
```
Cache-Control: no-cache          → Ne pas utiliser le cache
Cache-Control: no-store          → Ne pas stocker en cache
Cache-Control: max-age=3600      → Cache valide 1 heure
```

## Headers Courants de Réponse

### 1. Content-Type

Indique le type de contenu renvoyé par le serveur.

```pascal
ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
WriteLn('Type de contenu : ', ContentType);
```

### 2. Content-Length

Indique la taille du corps de la réponse en octets.

```pascal
ContentLength := Client.GetHeader(Client.ResponseHeaders, 'Content-Length');
WriteLn('Taille : ', ContentLength, ' octets');
```

### 3. Date

Date et heure de la réponse du serveur.

```
Date: Wed, 15 Oct 2025 10:30:00 GMT
```

### 4. Server

Identifie le logiciel serveur.

```
Server: nginx/1.18.0
Server: Apache/2.4.41 (Ubuntu)
Server: cloudflare
```

### 5. Set-Cookie

Demande au client de stocker un cookie.

```
Set-Cookie: session_id=abc123; Path=/; HttpOnly; Secure
```

### 6. Location

Utilisé avec les redirections (codes 3xx) pour indiquer la nouvelle URL.

```
Location: https://api.example.com/v2/users
```

### 7. Headers de Rate Limiting

Informent sur les limites de requêtes.

```
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 573
X-RateLimit-Reset: 1697368200
```

## Manipuler les Headers avec TFPHttpClient

### Ajouter un Header de Requête

```pascal
program AddHeaders;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Ajouter des headers personnalisés
    Client.AddHeader('User-Agent', 'MonApp/1.0');
    Client.AddHeader('Accept', 'application/json');
    Client.AddHeader('Accept-Language', 'fr-FR');
    Client.AddHeader('X-Custom-Header', 'MaValeurPersonnalisée');

    Response := Client.Get('https://api.github.com');
    WriteLn('Réponse reçue');
  finally
    Client.Free;
  end;

  ReadLn;
end.
```

### Lire les Headers de Réponse

```pascal
var
  Client: TFPHttpClient;
  Response: String;
  ContentType, Server, RateLimit: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    Response := Client.Get('https://api.github.com');

    // Lire des headers spécifiques
    ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
    Server := Client.GetHeader(Client.ResponseHeaders, 'Server');
    RateLimit := Client.GetHeader(Client.ResponseHeaders, 'X-RateLimit-Remaining');

    WriteLn('Type de contenu : ', ContentType);
    WriteLn('Serveur : ', Server);
    WriteLn('Requêtes restantes : ', RateLimit);
  finally
    Client.Free;
  end;
end;
```

### Afficher Tous les Headers de Réponse

```pascal
var
  Client: TFPHttpClient;
  Response: String;
  i: Integer;
begin
  Client := TFPHttpClient.Create(nil);
  try
    Response := Client.Get('https://api.github.com');

    WriteLn('=== Headers de réponse ===');
    for i := 0 to Client.ResponseHeaders.Count - 1 do
      WriteLn(Client.ResponseHeaders[i]);

  finally
    Client.Free;
  end;
end;
```

### Vérifier la Présence d'un Header

```pascal
function HasHeader(Client: TFPHttpClient; const HeaderName: String): Boolean;
var
  HeaderValue: String;
begin
  HeaderValue := Client.GetHeader(Client.ResponseHeaders, HeaderName);
  Result := HeaderValue <> '';
end;

// Utilisation
if HasHeader(Client, 'X-RateLimit-Remaining') then
  WriteLn('L''API utilise le rate limiting');
```

## Authentification HTTP

L'authentification permet de prouver votre identité au serveur pour accéder à des ressources protégées.

### Types d'Authentification Courants

```
1. Basic Authentication     → Username + Password (Base64)
2. API Key                  → Clé secrète unique
3. Bearer Token (OAuth)     → Token d'accès (JWT)
4. Digest Authentication    → Hash du mot de passe
5. OAuth 2.0                → Protocole complet d'autorisation
```

## Authentification Basique (Basic Auth)

### Principe

L'authentification basique encode les identifiants `username:password` en **Base64** et les envoie dans le header `Authorization`.

**Format :**
```
Authorization: Basic <credentials_en_base64>
```

### Exemple Manuel

```pascal
program BasicAuthManual;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, Base64,  // Base64 : encodage pour Basic Auth
  SysUtils;

var
  Client: TFPHttpClient;
  Response: String;
  Username, Password: String;
  Credentials, EncodedCredentials: String;
begin
  Username := 'jean.dupont';
  Password := 'monMotDePasse123';

  // Créer la chaîne "username:password"
  Credentials := Username + ':' + Password;

  // Encoder en Base64
  EncodedCredentials := EncodeStringBase64(Credentials);

  WriteLn('Credentials encodées : ', EncodedCredentials);

  // Utiliser dans une requête
  Client := TFPHttpClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Basic ' + EncodedCredentials);

    try
      Response := Client.Get('https://api.example.com/protected');

      if Client.ResponseStatusCode = 200 then
        WriteLn('Authentification réussie !')
      else if Client.ResponseStatusCode = 401 then
        WriteLn('Échec d''authentification')
      else
        WriteLn('Code : ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;

  ReadLn;
end.
```

### Fonction Réutilisable

```pascal
function CreateBasicAuthHeader(const Username, Password: String): String;
var
  Credentials: String;
begin
  Credentials := Username + ':' + Password;
  Result := 'Basic ' + EncodeStringBase64(Credentials);
end;

// Utilisation
Client.AddHeader('Authorization', CreateBasicAuthHeader('user', 'pass'));
```

### Méthode Simplifiée avec TFPHttpClient

TFPHttpClient peut gérer automatiquement l'authentification basique :

```pascal
var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Définir username et password
    Client.UserName := 'jean.dupont';
    Client.Password := 'monMotDePasse123';

    // TFPHttpClient ajoute automatiquement le header Authorization
    Response := Client.Get('https://api.example.com/protected');

    WriteLn(Response);
  finally
    Client.Free;
  end;
end;
```

### Sécurité de l'Authentification Basique

**⚠️ Important :**
- Base64 **n'est PAS du chiffrement** ! C'est simplement un encodage.
- Les identifiants sont facilement décodables.
- **Toujours utiliser HTTPS** (pas HTTP) avec Basic Auth.

```pascal
❌ DANGEREUX
http://api.example.com  + Basic Auth
→ Les identifiants peuvent être interceptés en clair !

✅ SÉCURISÉ
https://api.example.com + Basic Auth
→ La connexion est chiffrée, les identifiants sont protégés
```

## Authentification par API Key

Beaucoup d'API utilisent des **clés d'API** plutôt que des mots de passe.

### API Key dans un Header Personnalisé

```pascal
const
  API_KEY = 'sk_live_abc123def456ghi789';  // Votre clé d'API

var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Ajouter la clé dans un header personnalisé
    Client.AddHeader('X-API-Key', API_KEY);

    Response := Client.Get('https://api.example.com/data');
    WriteLn(Response);
  finally
    Client.Free;
  end;
end;
```

**Noms de headers courants pour les API keys :**
```
X-API-Key
X-Api-Key
api-key
apikey
Authorization: ApiKey <key>
```

### API Key dans l'URL (Query Parameter)

Certaines API acceptent la clé dans l'URL :

```pascal
const
  API_KEY = 'abc123def456';
  BASE_URL = 'https://api.openweathermap.org/data/2.5';

var
  URL: String;
begin
  // Construire l'URL avec la clé
  URL := Format('%s/weather?q=Paris&appid=%s', [BASE_URL, API_KEY]);

  Response := Client.Get(URL);
end;
```

**⚠️ Attention :** L'API key est visible dans l'URL (logs, historique navigateur). Moins sécurisé que dans un header.

### Stocker les API Keys en Sécurité

```pascal
❌ À ÉVITER
const API_KEY = 'ma_cle_secrete';  // En dur dans le code

✅ MIEUX
// Lire depuis un fichier de configuration
function LoadAPIKey: String;
var
  ConfigFile: TextFile;
begin
  AssignFile(ConfigFile, 'config.txt');
  Reset(ConfigFile);
  ReadLn(ConfigFile, Result);
  CloseFile(ConfigFile);
end;

✅ ENCORE MIEUX
// Utiliser des variables d'environnement
Result := GetEnvironmentVariable('MY_API_KEY');
```

## Authentification Bearer Token (OAuth/JWT)

Les tokens **Bearer** sont très utilisés avec OAuth 2.0 et JWT (JSON Web Tokens).

### Principe

Un **token** est une chaîne générée par le serveur après authentification. Il permet d'accéder aux ressources sans renvoyer le mot de passe à chaque fois.

**Format :**
```
Authorization: Bearer <token>
```

### Exemple avec Bearer Token

```pascal
program BearerTokenAuth;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

const
  ACCESS_TOKEN = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...';

var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Ajouter le token Bearer
    Client.AddHeader('Authorization', 'Bearer ' + ACCESS_TOKEN);
    Client.AddHeader('Accept', 'application/json');

    try
      Response := Client.Get('https://api.example.com/user/profile');

      if Client.ResponseStatusCode = 200 then
      begin
        WriteLn('Accès autorisé !');
        WriteLn(Response);
      end
      else if Client.ResponseStatusCode = 401 then
        WriteLn('Token invalide ou expiré')
      else
        WriteLn('Erreur : ', Client.ResponseStatusCode);

    except
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;
  finally
    Client.Free;
  end;

  ReadLn;
end.
```

### Obtenir un Token (Flux OAuth Simplifié)

Voici un exemple simplifié d'obtention de token :

```pascal
function GetAccessToken(const Username, Password: String): String;
var
  Client: TFPHttpClient;
  JsonData, Response: String;
  JsonResponse: TJSONData;
  JsonObj: TJSONObject;
begin
  Result := '';
  Client := TFPHttpClient.Create(nil);
  try
    // Préparer les données de connexion (JSON)
    JsonData := Format('{"username":"%s","password":"%s"}', [Username, Password]);

    Client.AddHeader('Content-Type', 'application/json');
    Client.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      // Envoyer la requête d'authentification
      Response := Client.Post('https://api.example.com/auth/login');

      if Client.ResponseStatusCode = 200 then
      begin
        // Parser la réponse JSON pour extraire le token
        JsonResponse := GetJSON(Response);
        try
          JsonObj := TJSONObject(JsonResponse);
          Result := JsonObj.Get('access_token', '');
          WriteLn('Token obtenu avec succès');
        finally
          JsonResponse.Free;
        end;
      end
      else
        WriteLn('Échec d''authentification : ', Client.ResponseStatusCode);

    finally
      Client.RequestBody.Free;
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
var
  Token: String;
begin
  Token := GetAccessToken('jean.dupont', 'motdepasse123');
  if Token <> '' then
  begin
    WriteLn('Token : ', Token);
    // Utiliser le token pour les requêtes suivantes
  end;
end;
```

### Gérer l'Expiration du Token

Les tokens ont généralement une **durée de vie limitée**.

```pascal
type
  TTokenInfo = record
    AccessToken: String;
    ExpiresAt: TDateTime;
    RefreshToken: String;
  end;

var
  CurrentToken: TTokenInfo;

function IsTokenValid: Boolean;
begin
  Result := (CurrentToken.AccessToken <> '') and
            (Now < CurrentToken.ExpiresAt);
end;

function GetValidToken: String;
begin
  if not IsTokenValid then
  begin
    WriteLn('Token expiré, renouvellement...');
    // Obtenir un nouveau token ou utiliser le refresh token
    CurrentToken.AccessToken := GetAccessToken('user', 'pass');
    CurrentToken.ExpiresAt := Now + (1 / 24);  // Expire dans 1 heure
  end;

  Result := CurrentToken.AccessToken;
end;

// Utilisation
Client.AddHeader('Authorization', 'Bearer ' + GetValidToken);
```

## Exemple Complet : Classe d'Authentification

Créons une classe réutilisable pour gérer différents types d'authentification.

```pascal
unit AuthManager;

{$mode objfpc}{$H+}

interface

uses
  fphttpclient, Base64, SysUtils;

type
  TAuthType = (atNone, atBasic, atAPIKey, atBearer);

  TAuthManager = class
  private
    FAuthType: TAuthType;
    FUsername: String;
    FPassword: String;
    FAPIKey: String;
    FAPIKeyHeader: String;
    FBearerToken: String;
  public
    constructor Create;

    // Configuration
    procedure SetBasicAuth(const Username, Password: String);
    procedure SetAPIKeyAuth(const APIKey: String; const HeaderName: String = 'X-API-Key');
    procedure SetBearerAuth(const Token: String);
    procedure ClearAuth;

    // Application de l'authentification
    procedure ApplyAuth(Client: TFPHttpClient);

    // Propriétés
    property AuthType: TAuthType read FAuthType;
  end;

implementation

constructor TAuthManager.Create;
begin
  inherited Create;
  FAuthType := atNone;
  FAPIKeyHeader := 'X-API-Key';
end;

procedure TAuthManager.SetBasicAuth(const Username, Password: String);
begin
  FAuthType := atBasic;
  FUsername := Username;
  FPassword := Password;
end;

procedure TAuthManager.SetAPIKeyAuth(const APIKey: String; const HeaderName: String);
begin
  FAuthType := atAPIKey;
  FAPIKey := APIKey;
  FAPIKeyHeader := HeaderName;
end;

procedure TAuthManager.SetBearerAuth(const Token: String);
begin
  FAuthType := atBearer;
  FBearerToken := Token;
end;

procedure TAuthManager.ClearAuth;
begin
  FAuthType := atNone;
  FUsername := '';
  FPassword := '';
  FAPIKey := '';
  FBearerToken := '';
end;

procedure TAuthManager.ApplyAuth(Client: TFPHttpClient);
var
  Credentials: String;
begin
  case FAuthType of
    atBasic:
    begin
      Credentials := FUsername + ':' + FPassword;
      Client.AddHeader('Authorization', 'Basic ' + EncodeStringBase64(Credentials));
    end;

    atAPIKey:
    begin
      Client.AddHeader(FAPIKeyHeader, FAPIKey);
    end;

    atBearer:
    begin
      Client.AddHeader('Authorization', 'Bearer ' + FBearerToken);
    end;

    // atNone : rien à faire
  end;
end;

end.
```

### Utilisation de la Classe

```pascal
program UseAuthManager;

uses
  AuthManager, fphttpclient, opensslsockets, SysUtils;

var
  Auth: TAuthManager;
  Client: TFPHttpClient;
  Response: String;
begin
  Auth := TAuthManager.Create;
  Client := TFPHttpClient.Create(nil);
  try
    // === Exemple 1 : Basic Auth ===
    WriteLn('=== Test Basic Auth ===');
    Auth.SetBasicAuth('user', 'password');
    Auth.ApplyAuth(Client);

    try
      Response := Client.Get('https://httpbin.org/basic-auth/user/password');
      WriteLn('Succès Basic Auth');
    except
      WriteLn('Échec Basic Auth');
    end;

    WriteLn;

    // === Exemple 2 : API Key ===
    WriteLn('=== Test API Key ===');
    Auth.SetAPIKeyAuth('abc123def456');
    Auth.ApplyAuth(Client);

    // Requête avec API key...

    WriteLn;

    // === Exemple 3 : Bearer Token ===
    WriteLn('=== Test Bearer Token ===');
    Auth.SetBearerAuth('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...');
    Auth.ApplyAuth(Client);

    // Requête avec Bearer token...

  finally
    Client.Free;
    Auth.Free;
  end;

  ReadLn;
end.
```

## Headers Personnalisés (Custom Headers)

Certaines API utilisent des headers personnalisés pour diverses fonctions.

### Exemples Courants

```pascal
// Versioning d'API
Client.AddHeader('X-API-Version', '2.0');

// ID de requête pour le débogage
Client.AddHeader('X-Request-ID', GenerateGUID);

// Préférences
Client.AddHeader('X-Timezone', 'Europe/Paris');
Client.AddHeader('X-Currency', 'EUR');

// Tracking
Client.AddHeader('X-Client-ID', 'myapp-12345');
Client.AddHeader('X-Session-ID', 'sess-abc123');
```

### GitHub API - Headers Spéciaux

```pascal
Client.AddHeader('User-Agent', 'MyApp');  // OBLIGATOIRE pour GitHub
Client.AddHeader('Accept', 'application/vnd.github.v3+json');  // Version de l'API
```

### Stripe API

```pascal
Client.AddHeader('Authorization', 'Bearer ' + API_KEY);
Client.AddHeader('Stripe-Version', '2020-08-27');  // Version de l'API
```

## Bonnes Pratiques

### 1. Toujours Utiliser HTTPS avec Authentification

```pascal
✅ SÉCURISÉ
https://api.example.com + Authorization header

❌ DANGEREUX
http://api.example.com + Authorization header
→ Les credentials peuvent être interceptés !
```

### 2. Ne Jamais Exposer les Credentials

```pascal
❌ À ÉVITER
const API_KEY = 'ma_cle_secrete';  // En dur dans le code source
// Si le code est public (GitHub), la clé est compromise !

✅ BON
// Fichier de configuration non versionné
API_KEY := LoadFromConfig('api_key');

✅ MIEUX
// Variable d'environnement
API_KEY := GetEnvironmentVariable('API_KEY');
```

### 3. Gérer l'Expiration des Tokens

```pascal
✅ TOUJOURS
- Vérifier si le token est expiré
- Renouveler le token automatiquement
- Gérer l'erreur 401 (Unauthorized)
```

### 4. Headers User-Agent Descriptifs

```pascal
✅ BON
Client.AddHeader('User-Agent', 'MonApp/2.0 (contact@example.com)');

❌ GÉNÉRIQUE
Client.AddHeader('User-Agent', 'Mozilla/5.0');
```

### 5. Respecter les Conventions

```pascal
✅ STANDARD
Authorization: Bearer token
X-API-Key: key

❌ NON STANDARD (éviter)
Auth: Bearer token
ApiKey: key
```

### 6. Logger Sans Exposer les Secrets

```pascal
✅ SÉCURISÉ
LogRequest('GET /users with auth');  // Ne pas logger le token

❌ DANGEREUX
LogRequest('GET /users with token: abc123...');  // Token visible dans les logs !
```

## Tester l'Authentification

### Service de Test : httpbin.org

httpbin.org fournit des endpoints pour tester différents types d'authentification.

```pascal
// Test Basic Auth
Response := Client.Get('https://httpbin.org/basic-auth/user/pass');

// Test Bearer
Client.AddHeader('Authorization', 'Bearer mytoken');
Response := Client.Get('https://httpbin.org/bearer');

// Afficher les headers reçus
Response := Client.Get('https://httpbin.org/headers');
WriteLn(Response);  // Montre tous les headers envoyés
```

## Résumé des Points Clés

1. **Headers HTTP** = métadonnées envoyées avec requêtes/réponses
2. **AddHeader** = ajouter un header de requête
3. **GetHeader** = lire un header de réponse
4. **User-Agent** = identifier votre application
5. **Content-Type** = type de contenu (JSON, XML, etc.)
6. **Accept** = types de contenu acceptés en réponse
7. **Basic Auth** = username:password encodé en Base64
8. **API Key** = clé secrète dans header ou URL
9. **Bearer Token** = token d'accès (OAuth/JWT)
10. **HTTPS obligatoire** avec authentification
11. **Ne jamais exposer** les credentials dans le code
12. **Gérer l'expiration** des tokens

## Ce Qu'il Faut Retenir pour la Suite

Vous savez maintenant :
- Comprendre et manipuler les headers HTTP
- Implémenter différents types d'authentification
- Sécuriser vos credentials (API keys, tokens)
- Créer des classes réutilisables pour gérer l'authentification
- Respecter les bonnes pratiques de sécurité

Ces compétences sont essentielles pour travailler avec des API sécurisées et construire des applications professionnelles qui protègent les données sensibles !

⏭️ [Introduction Pratique au Multi-threading](/18-introduction-pratique-multi-threading/README.md)
