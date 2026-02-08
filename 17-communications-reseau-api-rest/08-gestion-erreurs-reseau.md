🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.8 Gestion des erreurs réseau

## Introduction

Lorsque vous travaillez avec des API et des connexions réseau, les choses ne se passent pas toujours comme prévu. Le serveur peut être indisponible, la connexion Internet peut être coupée, l'API peut renvoyer des données inattendues... Une application robuste doit **anticiper et gérer** toutes ces situations d'erreur.

## Pourquoi Gérer les Erreurs Réseau ?

### Sans Gestion d'Erreurs

```pascal
❌ CODE FRAGILE
var
  Response: String;
begin
  Response := Client.Get('https://api.example.com/data');
  WriteLn(Response);  // Crash si erreur réseau !
end;
```

**Problèmes :**
- L'application plante si le réseau est coupé
- L'utilisateur ne sait pas ce qui s'est passé
- Impossible de récupérer de l'erreur

### Avec Gestion d'Erreurs

```pascal
✅ CODE ROBUSTE
var
  Response: String;
begin
  try
    Response := Client.Get('https://api.example.com/data');
    if Client.ResponseStatusCode = 200 then
      WriteLn(Response)
    else
      WriteLn('Erreur serveur : Code ', Client.ResponseStatusCode);
  except
    on E: Exception do
      WriteLn('Erreur de connexion : ', E.Message);
  end;
end;
```

**Avantages :**
- L'application ne plante pas
- L'utilisateur est informé du problème
- Possibilité de réessayer ou de proposer une alternative

## Types d'Erreurs Réseau

### 1. Erreurs de Connexion

**Causes courantes :**
- Pas de connexion Internet
- Serveur injoignable
- Nom de domaine invalide
- Timeout de connexion

**Symptôme :** Exception levée par TFPHttpClient

```pascal
try
  Response := Client.Get('https://serveur-inexistant.com/api');
except
  on E: EHTTPClient do
    WriteLn('Impossible de se connecter : ', E.Message);
  on E: Exception do
    WriteLn('Erreur générale : ', E.Message);
end;
```

### 2. Erreurs HTTP (Codes de Statut)

**Causes courantes :**
- 400 Bad Request : requête mal formée
- 401 Unauthorized : authentification requise
- 403 Forbidden : accès interdit
- 404 Not Found : ressource introuvable
- 429 Too Many Requests : trop de requêtes
- 500 Internal Server Error : erreur serveur
- 503 Service Unavailable : service indisponible

**Symptôme :** ResponseStatusCode différent de 2xx

```pascal
Response := Client.Get('https://api.example.com/users/999');

case Client.ResponseStatusCode of
  200: WriteLn('Succès !');
  404: WriteLn('Utilisateur introuvable');
  500: WriteLn('Erreur interne du serveur');
  503: WriteLn('Service temporairement indisponible');
else
  WriteLn('Erreur inattendue : ', Client.ResponseStatusCode);
end;
```

### 3. Erreurs de Timeout

**Causes :**
- Connexion trop lente
- Serveur qui ne répond pas
- Réponse très volumineuse

**Solution :** Définir des timeouts appropriés

```pascal
Client.ConnectTimeout := 10000;  // 10 secondes max pour se connecter
```

### 4. Erreurs de Parsing JSON

**Causes :**
- JSON invalide reçu
- Structure JSON différente de celle attendue
- Champs manquants

**Solution :** Valider et gérer les exceptions de parsing

```pascal
try
  JsonData := GetJSON(Response);
  try
    // Utilisation du JSON
  finally
    JsonData.Free;
  end;
except
  on E: Exception do
    WriteLn('JSON invalide : ', E.Message);
end;
```

### 5. Erreurs SSL/TLS

**Causes :**
- Certificat expiré
- Certificat non reconnu
- Bibliothèques OpenSSL manquantes

**Solution :** Vérifier les dépendances SSL

## Gestion Complète des Erreurs

### Structure de Base

```pascal
program ErrorHandlingBasic;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, SysUtils;

function GetDataSafely(const URL: String): String;
var
  Client: TFPHttpClient;
begin
  Result := '';
  Client := TFPHttpClient.Create(nil);
  try
    try
      // Configuration
      Client.ConnectTimeout := 10000;  // 10 secondes
      Client.AddHeader('User-Agent', 'MonApp/1.0');

      // Effectuer la requête
      Result := Client.Get(URL);

      // Vérifier le code de statut
      if (Client.ResponseStatusCode < 200) or
         (Client.ResponseStatusCode >= 300) then
      begin
        WriteLn('Erreur HTTP ', Client.ResponseStatusCode);
        Result := '';
      end;

    except
      // Erreurs HTTP spécifiques
      on E: EHTTPClient do
      begin
        WriteLn('Erreur HTTP : ', E.Message);
        Result := '';
      end;

      // Erreurs de socket (connexion)
      on E: ESocketError do
      begin
        WriteLn('Erreur de connexion : ', E.Message);
        Result := '';
      end;

      // Toutes les autres erreurs
      on E: Exception do
      begin
        WriteLn('Erreur inattendue : ', E.Message);
        Result := '';
      end;
    end;
  finally
    Client.Free;
  end;
end;

begin
  WriteLn(GetDataSafely('https://api.github.com'));
  ReadLn;
end.
```

### Fonction Avancée avec Détails d'Erreur

```pascal
type
  TRequestResult = record
    Success: Boolean;
    StatusCode: Integer;
    ErrorMessage: String;
    Data: String;
  end;

function ExecuteRequest(const URL: String): TRequestResult;
var
  Client: TFPHttpClient;
begin
  // Initialiser le résultat
  Result.Success := False;
  Result.StatusCode := 0;
  Result.ErrorMessage := '';
  Result.Data := '';

  Client := TFPHttpClient.Create(nil);
  try
    try
      Client.ConnectTimeout := 10000;
      Result.Data := Client.Get(URL);
      Result.StatusCode := Client.ResponseStatusCode;

      // Vérifier le succès (codes 2xx)
      if (Result.StatusCode >= 200) and (Result.StatusCode < 300) then
        Result.Success := True
      else
        Result.ErrorMessage := Format('Erreur HTTP %d', [Result.StatusCode]);

    except
      on E: EHTTPClient do
      begin
        Result.ErrorMessage := 'Erreur HTTP : ' + E.Message;
        Result.StatusCode := Client.ResponseStatusCode;
      end;

      on E: ESocketError do
        Result.ErrorMessage := 'Erreur de connexion : ' + E.Message;

      on E: Exception do
        Result.ErrorMessage := 'Erreur : ' + E.Message;
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
var
  Result: TRequestResult;
begin
  Result := ExecuteRequest('https://api.example.com/data');

  if Result.Success then
    WriteLn('Données : ', Result.Data)
  else
    WriteLn('Échec : ', Result.ErrorMessage);
end;
```

## Gestion des Codes de Statut HTTP

### Fonction Helper pour les Codes de Statut

```pascal
function GetStatusDescription(StatusCode: Integer): String;
begin
  case StatusCode of
    // 2xx - Succès
    200: Result := 'OK - Requête réussie';
    201: Result := 'Created - Ressource créée';
    204: Result := 'No Content - Succès sans contenu';

    // 3xx - Redirection
    301: Result := 'Moved Permanently - Ressource déplacée';
    304: Result := 'Not Modified - Ressource non modifiée';

    // 4xx - Erreurs client
    400: Result := 'Bad Request - Requête invalide';
    401: Result := 'Unauthorized - Authentification requise';
    403: Result := 'Forbidden - Accès interdit';
    404: Result := 'Not Found - Ressource introuvable';
    405: Result := 'Method Not Allowed - Méthode non autorisée';
    408: Result := 'Request Timeout - Timeout de la requête';
    409: Result := 'Conflict - Conflit (ex: ressource existante)';
    422: Result := 'Unprocessable Entity - Données invalides';
    429: Result := 'Too Many Requests - Trop de requêtes';

    // 5xx - Erreurs serveur
    500: Result := 'Internal Server Error - Erreur serveur';
    502: Result := 'Bad Gateway - Erreur de passerelle';
    503: Result := 'Service Unavailable - Service indisponible';
    504: Result := 'Gateway Timeout - Timeout de passerelle';

  else
    Result := Format('Code %d - Non documenté', [StatusCode]);
  end;
end;

// Utilisation
WriteLn(GetStatusDescription(404));
// Affiche : "Not Found - Ressource introuvable"
```

### Gérer les Erreurs par Catégorie

```pascal
procedure HandleHTTPError(StatusCode: Integer; const URL: String);
begin
  case StatusCode div 100 of
    2: WriteLn('Succès !');

    3: WriteLn('Redirection - L''URL a peut-être changé');

    4: begin
      WriteLn('Erreur client (4xx)');
      case StatusCode of
        401: WriteLn('Vérifiez vos identifiants');
        403: WriteLn('Vous n''avez pas accès à cette ressource');
        404: WriteLn('Ressource introuvable : ', URL);
        429: WriteLn('Trop de requêtes, attendez avant de réessayer');
      else
        WriteLn('Vérifiez votre requête');
      end;
    end;

    5: begin
      WriteLn('Erreur serveur (5xx)');
      WriteLn('Le problème vient du serveur, réessayez plus tard');
    end;

  else
    WriteLn('Code de statut inattendu : ', StatusCode);
  end;
end;
```

## Logique de Réessai (Retry Logic)

Parfois, une erreur temporaire peut être résolue en réessayant la requête.

### Réessai Simple

```pascal
function GetWithRetry(const URL: String; MaxAttempts: Integer): String;
var
  Client: TFPHttpClient;
  Attempt: Integer;
  Success: Boolean;
begin
  Result := '';
  Success := False;

  for Attempt := 1 to MaxAttempts do
  begin
    WriteLn('Tentative ', Attempt, '/', MaxAttempts);

    Client := TFPHttpClient.Create(nil);
    try
      try
        Result := Client.Get(URL);

        if Client.ResponseStatusCode = 200 then
        begin
          Success := True;
          WriteLn('Succès !');
          Break;  // Sortir de la boucle
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

    // Attendre avant de réessayer (sauf à la dernière tentative)
    if (not Success) and (Attempt < MaxAttempts) then
    begin
      WriteLn('Attente de 2 secondes...');
      Sleep(2000);  // 2 secondes
    end;
  end;

  if not Success then
  begin
    WriteLn('Échec après ', MaxAttempts, ' tentatives');
    Result := '';
  end;
end;

// Utilisation
var
  Data: String;
begin
  Data := GetWithRetry('https://api.example.com/data', 3);
  if Data <> '' then
    WriteLn('Données récupérées')
  else
    WriteLn('Impossible de récupérer les données');
end;
```

### Réessai avec Backoff Exponentiel

Pour éviter de surcharger le serveur, on augmente progressivement le délai d'attente.

```pascal
function GetWithExponentialBackoff(const URL: String; MaxAttempts: Integer): String;
var
  Client: TFPHttpClient;
  Attempt: Integer;
  WaitTime: Integer;
  Success: Boolean;
begin
  Result := '';
  Success := False;

  for Attempt := 1 to MaxAttempts do
  begin
    WriteLn('Tentative ', Attempt, '/', MaxAttempts);

    Client := TFPHttpClient.Create(nil);
    try
      try
        Result := Client.Get(URL);

        if Client.ResponseStatusCode = 200 then
        begin
          Success := True;
          Break;
        end;

      except
        on E: Exception do
          WriteLn('Erreur : ', E.Message);
      end;
    finally
      Client.Free;
    end;

    if (not Success) and (Attempt < MaxAttempts) then
    begin
      // Backoff exponentiel : 1s, 2s, 4s, 8s, etc.
      WaitTime := Round(Power(2, Attempt - 1)) * 1000;
      WriteLn('Attente de ', WaitTime div 1000, ' seconde(s)...');
      Sleep(WaitTime);
    end;
  end;

  if not Success then
    Result := '';
end;
```

### Quand Réessayer ?

```pascal
✅ RÉESSAYER dans ces cas :
- Erreurs réseau temporaires (timeout, connexion)
- 500 Internal Server Error (erreur serveur temporaire)
- 503 Service Unavailable (serveur surchargé)
- 504 Gateway Timeout

❌ NE PAS RÉESSAYER pour :
- 400 Bad Request (votre requête est incorrecte)
- 401 Unauthorized (problème d'authentification)
- 403 Forbidden (pas les droits d'accès)
- 404 Not Found (la ressource n'existe pas)
- 422 Unprocessable Entity (données invalides)
```

### Fonction Intelligente de Réessai

```pascal
function ShouldRetry(StatusCode: Integer): Boolean;
begin
  Result := False;

  // Réessayer pour les erreurs serveur temporaires
  case StatusCode of
    0:     Result := True;  // Pas de réponse (erreur réseau)
    500:   Result := True;  // Internal Server Error
    502:   Result := True;  // Bad Gateway
    503:   Result := True;  // Service Unavailable
    504:   Result := True;  // Gateway Timeout
  end;
end;

function SmartGetWithRetry(const URL: String; MaxAttempts: Integer): String;
var
  Client: TFPHttpClient;
  Attempt: Integer;
  StatusCode: Integer;
begin
  Result := '';

  for Attempt := 1 to MaxAttempts do
  begin
    Client := TFPHttpClient.Create(nil);
    try
      try
        Result := Client.Get(URL);
        StatusCode := Client.ResponseStatusCode;

        if StatusCode = 200 then
          Break  // Succès, on sort
        else if not ShouldRetry(StatusCode) then
        begin
          WriteLn('Erreur non temporaire : ', StatusCode);
          Break;  // Pas la peine de réessayer
        end;

      except
        StatusCode := 0;  // Erreur réseau
      end;
    finally
      Client.Free;
    end;

    if (Attempt < MaxAttempts) and ShouldRetry(StatusCode) then
      Sleep(2000);
  end;
end;
```

## Gestion des Erreurs de Parsing JSON

### Validation avant Parsing

```pascal
function ParseJSONSafely(const JsonString: String): TJSONData;
begin
  Result := nil;

  // Vérifier que la chaîne n'est pas vide
  if Trim(JsonString) = '' then
  begin
    WriteLn('JSON vide');
    Exit;
  end;

  // Tenter de parser
  try
    Result := GetJSON(JsonString);
  except
    on E: Exception do
    begin
      WriteLn('Erreur de parsing JSON : ', E.Message);
      WriteLn('JSON reçu : ', Copy(JsonString, 1, 200));
      Result := nil;
    end;
  end;
end;
```

### Extraction Sécurisée des Données

```pascal
function SafeGetString(JsonObj: TJSONObject; const Key: String; const Default: String): String;
begin
  try
    if Assigned(JsonObj) and (JsonObj.IndexOfName(Key) >= 0) then
    begin
      if JsonObj.Find(Key).JSONType <> jtNull then
        Result := JsonObj.Get(Key, Default)
      else
        Result := Default;
    end
    else
      Result := Default;
  except
    Result := Default;
  end;
end;

// Utilisation
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  UserName: String;
begin
  JsonData := ParseJSONSafely(Response);
  if Assigned(JsonData) then
  try
    if JsonData.JSONType = jtObject then
    begin
      JsonObj := TJSONObject(JsonData);
      UserName := SafeGetString(JsonObj, 'name', 'Inconnu');
      WriteLn('Utilisateur : ', UserName);
    end;
  finally
    JsonData.Free;
  end;
end;
```

## Logging des Erreurs

Le logging (journalisation) permet de garder une trace des erreurs pour faciliter le débogage.

### Logger Simple

```pascal
procedure LogError(const Message: String);
var
  F: TextFile;
  FileName: String;
begin
  FileName := 'errors.log';

  try
    AssignFile(F, FileName);
    if FileExists(FileName) then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, '[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ', Message);
    CloseFile(F);
  except
    // Ne pas bloquer l'application si le logging échoue
    WriteLn('Impossible d''écrire dans le log');
  end;
end;

// Utilisation
try
  Response := Client.Get(URL);
except
  on E: Exception do
  begin
    LogError('Erreur GET ' + URL + ' : ' + E.Message);
    WriteLn('Erreur : ', E.Message);
  end;
end;
```

### Logger Structuré

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

procedure Log(Level: TLogLevel; const Message: String);
var
  F: TextFile;
  LevelStr: String;
begin
  case Level of
    llDebug:   LevelStr := 'DEBUG';
    llInfo:    LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError:   LevelStr := 'ERROR';
  end;

  try
    AssignFile(F, 'app.log');
    if FileExists('app.log') then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, Format('[%s] [%s] %s',
            [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), LevelStr, Message]));
    CloseFile(F);
  except
    // Silencieux si échec
  end;
end;

// Utilisation
Log(llInfo, 'Démarrage de l''application');
Log(llDebug, 'Connexion à l''API : ' + URL);

try
  Response := Client.Get(URL);
  Log(llInfo, 'Données récupérées avec succès');
except
  on E: Exception do
  begin
    Log(llError, 'Erreur GET : ' + E.Message);
    raise;  // Propager l'exception
  end;
end;
```

## Feedback Utilisateur

Dans une application graphique, il est important d'informer l'utilisateur des problèmes.

### Messages Clairs

```pascal
✅ BON
'Impossible de se connecter au serveur. Vérifiez votre connexion Internet.'

❌ MAUVAIS
'Error: ESocketError with message ''Host not found'' in...'
```

### Fonction pour Messages Conviviaux

```pascal
function GetUserFriendlyError(const TechnicalError: String; StatusCode: Integer): String;
begin
  if StatusCode = 0 then
  begin
    // Erreur réseau
    if Pos('timeout', LowerCase(TechnicalError)) > 0 then
      Result := 'La connexion a pris trop de temps. Veuillez réessayer.'
    else if Pos('host', LowerCase(TechnicalError)) > 0 then
      Result := 'Impossible de joindre le serveur. Vérifiez votre connexion Internet.'
    else
      Result := 'Erreur de connexion. Vérifiez votre connexion Internet.';
  end
  else
  begin
    // Erreur HTTP
    case StatusCode of
      400: Result := 'Requête invalide. Veuillez vérifier vos données.';
      401: Result := 'Authentification requise. Veuillez vous connecter.';
      403: Result := 'Accès refusé. Vous n''avez pas les droits nécessaires.';
      404: Result := 'Ressource introuvable. Elle a peut-être été supprimée.';
      429: Result := 'Trop de requêtes. Veuillez patienter quelques instants.';
      500: Result := 'Erreur du serveur. Veuillez réessayer plus tard.';
      503: Result := 'Service temporairement indisponible. Réessayez dans quelques minutes.';
    else
      Result := Format('Une erreur est survenue (Code %d). Veuillez réessayer.', [StatusCode]);
    end;
  end;
end;

// Utilisation
try
  Response := Client.Get(URL);
except
  on E: Exception do
  begin
    ErrorMsg := GetUserFriendlyError(E.Message, Client.ResponseStatusCode);
    ShowMessage(ErrorMsg);  // Dans une application GUI
  end;
end;
```

## Mode Hors Ligne / En Ligne

### Détecter la Connexion Internet

```pascal
function IsInternetAvailable: Boolean;
var
  Client: TFPHttpClient;
begin
  Result := False;
  Client := TFPHttpClient.Create(nil);
  try
    try
      Client.ConnectTimeout := 3000;  // 3 secondes
      // Ping un serveur fiable
      Client.Get('https://www.google.com');
      Result := Client.ResponseStatusCode = 200;
    except
      Result := False;
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
if IsInternetAvailable then
  WriteLn('Connexion Internet disponible')
else
  WriteLn('Aucune connexion Internet');
```

### Système de Cache Simple

```pascal
type
  TCachedData = record
    Data: String;
    Timestamp: TDateTime;
    Valid: Boolean;
  end;

var
  Cache: TCachedData;

function GetDataWithCache(const URL: String; CacheMinutes: Integer): String;
var
  MinutesSinceCache: Double;
begin
  // Vérifier si le cache est valide
  if Cache.Valid then
  begin
    MinutesSinceCache := (Now - Cache.Timestamp) * 24 * 60;
    if MinutesSinceCache < CacheMinutes then
    begin
      WriteLn('Utilisation du cache');
      Exit(Cache.Data);
    end;
  end;

  // Cache invalide ou expiré, récupérer les données
  try
    Result := Client.Get(URL);
    if Client.ResponseStatusCode = 200 then
    begin
      // Mettre à jour le cache
      Cache.Data := Result;
      Cache.Timestamp := Now;
      Cache.Valid := True;
      WriteLn('Données mises en cache');
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur réseau : ', E.Message);
      // Si le cache existe, l'utiliser même s'il est expiré
      if Cache.Valid then
      begin
        WriteLn('Utilisation du cache expiré (mode dégradé)');
        Result := Cache.Data;
      end
      else
        Result := '';
    end;
  end;
end;
```

## Classe Complète de Gestion HTTP

Voici une classe réutilisable qui intègre toutes les bonnes pratiques.

```pascal
unit HTTPManager;

{$mode objfpc}{$H+}

interface

uses
  fphttpclient, opensslsockets, Classes, SysUtils, Math;

type
  THTTPResult = record
    Success: Boolean;
    StatusCode: Integer;
    Data: String;
    ErrorMessage: String;
  end;

  THTTPManager = class
  private
    FClient: TFPHttpClient;
    FTimeout: Integer;
    FMaxRetries: Integer;
    FLogErrors: Boolean;

    procedure LogError(const Message: String);
    function ShouldRetry(StatusCode: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const URL: String): THTTPResult;
    function Post(const URL: String; const JsonData: String): THTTPResult;

    property Timeout: Integer read FTimeout write FTimeout;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property LogErrors: Boolean read FLogErrors write FLogErrors;
  end;

implementation

constructor THTTPManager.Create;
begin
  inherited Create;
  FClient := TFPHttpClient.Create(nil);
  FClient.AddHeader('User-Agent', 'FreePascal-App/1.0');
  FClient.AddHeader('Accept', 'application/json');

  FTimeout := 10000;      // 10 secondes par défaut
  FMaxRetries := 3;       // 3 tentatives par défaut
  FLogErrors := True;     // Logger par défaut
end;

destructor THTTPManager.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

procedure THTTPManager.LogError(const Message: String);
var
  F: TextFile;
begin
  if not FLogErrors then
    Exit;

  try
    AssignFile(F, 'network_errors.log');
    if FileExists('network_errors.log') then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, '[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ', Message);
    CloseFile(F);
  except
    // Silencieux si échec
  end;
end;

function THTTPManager.ShouldRetry(StatusCode: Integer): Boolean;
begin
  Result := False;
  case StatusCode of  // case et non 'in' : les sets FPC ne supportent pas les valeurs > 255
    0, 500, 502, 503, 504: Result := True;
  end;
end;

function THTTPManager.Get(const URL: String): THTTPResult;
var
  Attempt: Integer;
  WaitTime: Integer;
begin
  Result.Success := False;
  Result.StatusCode := 0;
  Result.Data := '';
  Result.ErrorMessage := '';

  for Attempt := 1 to FMaxRetries do
  begin
    try
      FClient.ConnectTimeout := FTimeout;
      Result.Data := FClient.Get(URL);
      Result.StatusCode := FClient.ResponseStatusCode;

      if Result.StatusCode = 200 then
      begin
        Result.Success := True;
        Break;
      end
      else if not ShouldRetry(Result.StatusCode) then
      begin
        Result.ErrorMessage := Format('Erreur HTTP %d', [Result.StatusCode]);
        LogError(Format('GET %s : %s', [URL, Result.ErrorMessage]));
        Break;
      end;

    except
      on E: Exception do
      begin
        Result.ErrorMessage := E.Message;
        Result.StatusCode := 0;
        LogError(Format('GET %s : %s', [URL, E.Message]));
      end;
    end;

    // Attendre avant de réessayer (backoff exponentiel)
    if (not Result.Success) and (Attempt < FMaxRetries) then
    begin
      WaitTime := Round(Power(2, Attempt - 1)) * 1000;
      Sleep(WaitTime);
    end;
  end;

  if not Result.Success and (Result.ErrorMessage = '') then
    Result.ErrorMessage := Format('Échec après %d tentatives', [FMaxRetries]);
end;

function THTTPManager.Post(const URL: String; const JsonData: String): THTTPResult;
begin
  Result.Success := False;
  Result.StatusCode := 0;
  Result.Data := '';
  Result.ErrorMessage := '';

  try
    FClient.ConnectTimeout := FTimeout;
    FClient.AddHeader('Content-Type', 'application/json');
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result.Data := FClient.Post(URL);
      Result.StatusCode := FClient.ResponseStatusCode;

      if Result.StatusCode in [200, 201] then
        Result.Success := True
      else
        Result.ErrorMessage := Format('Erreur HTTP %d', [Result.StatusCode]);

    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
    begin
      Result.ErrorMessage := E.Message;
      LogError(Format('POST %s : %s', [URL, E.Message]));
    end;
  end;
end;

end.
```

### Utilisation de la Classe

```pascal
program UseHTTPManager;

uses
  HTTPManager, SysUtils;

var
  HTTP: THTTPManager;
  Result: THTTPResult;
begin
  HTTP := THTTPManager.Create;
  try
    // Configuration
    HTTP.Timeout := 15000;   // 15 secondes
    HTTP.MaxRetries := 3;
    HTTP.LogErrors := True;

    // Effectuer une requête GET
    Result := HTTP.Get('https://api.github.com/users/octocat');

    if Result.Success then
    begin
      WriteLn('Succès !');
      WriteLn(Result.Data);
    end
    else
    begin
      WriteLn('Échec : ', Result.ErrorMessage);
      WriteLn('Code : ', Result.StatusCode);
    end;

  finally
    HTTP.Free;
  end;

  ReadLn;
end.
```

## Bonnes Pratiques Résumées

### 1. Toujours Gérer les Exceptions

```pascal
✅ TOUJOURS
try
  Response := Client.Get(URL);
except
  on E: Exception do
    // Gérer l'erreur
end;
```

### 2. Définir des Timeouts

```pascal
✅ TOUJOURS
Client.ConnectTimeout := 10000;  // Ne pas attendre indéfiniment
```

### 3. Vérifier les Codes de Statut

```pascal
✅ TOUJOURS
if Client.ResponseStatusCode = 200 then
  // Traiter les données
else
  // Gérer l'erreur
```

### 4. Logger les Erreurs

```pascal
✅ RECOMMANDÉ
except
  on E: Exception do
  begin
    LogError(E.Message);
    // Continuer la gestion
  end;
end;
```

### 5. Messages Utilisateurs Clairs

```pascal
✅ BON
'Impossible de se connecter. Vérifiez votre connexion Internet.'

❌ MAUVAIS
'ESocketError: Host not found'
```

### 6. Réessayer Intelligemment

```pascal
✅ RÉESSAYER
- Timeouts
- Erreurs 5xx (serveur)

❌ NE PAS RÉESSAYER
- Erreurs 4xx (client)
- Authentification ratée
```

### 7. Implémenter un Cache

```pascal
✅ UTILE
Pour réduire les requêtes et fonctionner hors ligne
```

## Résumé des Points Clés

1. **Exceptions** : Toujours utiliser try-except pour les requêtes réseau
2. **Codes de statut** : Vérifier ResponseStatusCode avant de traiter les données
3. **Timeouts** : Définir ConnectTimeout pour éviter les attentes infinies
4. **Retry logic** : Réessayer intelligemment pour les erreurs temporaires
5. **Backoff exponentiel** : Augmenter progressivement le délai entre les tentatives
6. **Logging** : Garder une trace des erreurs pour le débogage
7. **Messages clairs** : Traduire les erreurs techniques en messages compréhensibles
8. **Cache** : Stocker les données pour fonctionner en mode dégradé
9. **Validation JSON** : Vérifier le JSON avant de le parser
10. **Extraction sécurisée** : Vérifier l'existence des clés avant accès

## Ce Qu'il Faut Retenir pour la Suite

Vous savez maintenant créer des applications robustes qui :
- Gèrent gracieusement les erreurs réseau
- Informent clairement l'utilisateur des problèmes
- Réessaient automatiquement en cas d'erreur temporaire
- Gardent une trace des erreurs pour le débogage
- Fonctionnent en mode dégradé quand c'est possible

Ces compétences sont essentielles pour créer des applications professionnelles et fiables qui communiquent avec des API et des serveurs distants !

⏭️ [Headers et authentification basique](/17-communications-reseau-api-rest/09-headers-authentification-basique.md)
