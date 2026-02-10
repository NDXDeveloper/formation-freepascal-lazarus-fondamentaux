🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.5 Utilisation de TFPHttpClient

## Introduction

Maintenant que vous connaissez HTTP, les API REST et le format JSON, il est temps de mettre tout cela en pratique avec FreePascal ! La classe **TFPHttpClient** est l'outil principal pour effectuer des requêtes HTTP depuis vos applications.

## Qu'est-ce que TFPHttpClient ?

**TFPHttpClient** est une classe de la bibliothèque standard de FreePascal (Free Pascal Component Library) qui permet de :
- Envoyer des requêtes HTTP/HTTPS
- Récupérer des données depuis des serveurs web
- Consommer des API REST
- Télécharger des fichiers
- Gérer l'authentification

### Analogie

Si vous envoyez une lettre par la poste :
- **Vous** = votre programme FreePascal
- **TFPHttpClient** = le facteur qui transporte la lettre
- **Le serveur** = le destinataire qui reçoit et répond

## Unités Nécessaires

Pour utiliser TFPHttpClient, vous devez inclure les unités suivantes :

```pascal
uses
  fphttpclient,  // La classe TFPHttpClient
  opensslsockets, // Support HTTPS (SSL/TLS)
  Classes,        // Pour TStringStream
  SysUtils;       // Fonctions utilitaires
```

**Important :** L'unité `opensslsockets` est essentielle pour HTTPS. Sans elle, seul HTTP fonctionnera !

## Première Requête GET Simple

### Exemple de base

Commençons par le plus simple : récupérer une page web ou des données d'API.

```pascal
program SimpleGet;

uses
  fphttpclient, opensslsockets,  // opensslsockets : nécessaire pour HTTPS
  SysUtils;

var
  Client: TFPHttpClient;
  Response: String;
begin
  // Créer une instance du client HTTP
  Client := TFPHttpClient.Create(nil);
  try
    // GitHub exige un User-Agent
    Client.AddHeader('User-Agent', 'FreePascal-App/1.0');

    // Effectuer une requête GET
    Response := Client.Get('https://api.github.com');

    // Afficher la réponse
    WriteLn('Réponse reçue :');
    WriteLn(Response);
  finally
    // Ne pas oublier de libérer la mémoire
    Client.Free;
  end;
end.
```

**Explication ligne par ligne :**
1. On crée une instance de `TFPHttpClient`
2. On utilise la méthode `Get()` qui renvoie la réponse sous forme de String
3. On affiche le contenu
4. On libère la mémoire dans le bloc `finally`

### Méthode Get : Deux Variantes

**Variante 1 : Retour sous forme de String**
```pascal
var
  Response: String;
begin
  Response := Client.Get('https://api.example.com/data');
  // Response contient directement les données
end;
```

**Variante 2 : Écriture dans un Stream**
```pascal
uses Classes;

var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    Client.Get('https://api.example.com/data', Stream);
    // Les données sont maintenant dans Stream.DataString
    WriteLn(Stream.DataString);
  finally
    Stream.Free;
  end;
end;
```

## Gestion des Erreurs

Les requêtes HTTP peuvent échouer pour diverses raisons. Il est **essentiel** de gérer les erreurs.

### Gestion de base avec Try-Except

```pascal
program GetWithErrorHandling;

uses
  fphttpclient, opensslsockets, SysUtils;

var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      Response := Client.Get('https://api.example.com/users');
      WriteLn('Succès ! Données reçues :');
      WriteLn(Response);
    except
      on E: Exception do
      begin
        WriteLn('Erreur lors de la requête :');
        WriteLn(E.Message);
      end;
    end;
  finally
    Client.Free;
  end;
end.
```

### Vérification du Code de Statut

```pascal
var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    try
      Response := Client.Get('https://api.example.com/users/999');

      // Vérifier le code de statut HTTP
      case Client.ResponseStatusCode of
        200: WriteLn('Succès !');
        404: WriteLn('Ressource non trouvée');
        500: WriteLn('Erreur serveur');
      else
        WriteLn('Code de statut : ', Client.ResponseStatusCode);
      end;

    except
      on E: Exception do
        WriteLn('Erreur réseau : ', E.Message);
    end;
  finally
    Client.Free;
  end;
end.
```

### Gestion Complète des Erreurs

```pascal
function FetchData(const URL: String): String;  
var  
  Client: TFPHttpClient;
begin
  Result := '';
  Client := TFPHttpClient.Create(nil);
  try
    try
      Result := Client.Get(URL);

      // Vérifier le succès (codes 2xx)
      if (Client.ResponseStatusCode < 200) or
         (Client.ResponseStatusCode >= 300) then
      begin
        WriteLn('Erreur HTTP ', Client.ResponseStatusCode);
        Result := '';
      end;

    except
      on E: EHTTPClient do
      begin
        WriteLn('Erreur HTTP : ', E.Message);
        WriteLn('Code : ', Client.ResponseStatusCode);
      end;
      on E: Exception do
      begin
        WriteLn('Erreur générale : ', E.Message);
      end;
    end;
  finally
    Client.Free;
  end;
end;
```

## En-têtes HTTP (Headers)

Les en-têtes permettent d'envoyer des informations supplémentaires au serveur.

### Ajouter des En-têtes Personnalisés

```pascal
var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Ajouter des en-têtes
    Client.AddHeader('User-Agent', 'MonAppFreePascal/1.0');
    Client.AddHeader('Accept', 'application/json');
    Client.AddHeader('Accept-Language', 'fr-FR');

    Response := Client.Get('https://api.example.com/data');
    WriteLn(Response);
  finally
    Client.Free;
  end;
end;
```

### En-tête d'Authentification

```pascal
var
  Client: TFPHttpClient;
  Token: String;
begin
  Token := 'votre_token_secret';
  Client := TFPHttpClient.Create(nil);
  try
    // Authentification Bearer Token
    Client.AddHeader('Authorization', 'Bearer ' + Token);

    Response := Client.Get('https://api.example.com/private/data');
  finally
    Client.Free;
  end;
end;
```

### Lire les En-têtes de Réponse

```pascal
var
  Client: TFPHttpClient;
  Response: String;
  ContentType: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    Response := Client.Get('https://api.example.com/data');

    // Lire un en-tête de réponse
    ContentType := Client.GetHeader(Client.ResponseHeaders, 'Content-Type');
    WriteLn('Type de contenu : ', ContentType);

    // Afficher tous les en-têtes de réponse
    WriteLn('=== En-têtes de réponse ===');
    WriteLn(Client.ResponseHeaders.Text);
  finally
    Client.Free;
  end;
end;
```

## Requêtes POST - Envoyer des Données

POST permet d'envoyer des données au serveur (création, soumission de formulaire, etc.).

### POST avec Données de Formulaire

```pascal
var
  Client: TFPHttpClient;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Indiquer le type de contenu
    Client.AddHeader('Content-Type', 'application/x-www-form-urlencoded');

    // Envoyer la requête POST (FormPost accepte une String ou un TStrings)
    Response := Client.FormPost('https://api.example.com/users',
      'nom=Dupont&prenom=Jean&email=jean@example.com');

    WriteLn('Réponse du serveur :');
    WriteLn(Response);
  finally
    Client.Free;
  end;
end;
```

### POST avec JSON

```pascal
var
  Client: TFPHttpClient;
  JsonData: String;
  Response: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Créer les données JSON
    JsonData := '{"nom":"Dupont","prenom":"Jean","email":"jean@example.com"}';

    // Configurer les en-têtes pour JSON
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('Accept', 'application/json');

    // Envoyer la requête POST
    Client.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Response := Client.Post('https://api.example.com/users');

      WriteLn('Utilisateur créé :');
      WriteLn(Response);
      WriteLn('Code de statut : ', Client.ResponseStatusCode);
    finally
      Client.RequestBody.Free;
    end;
  finally
    Client.Free;
  end;
end;
```

### Fonction POST Réutilisable

```pascal
function PostJSON(const URL, JsonData: String): String;  
var  
  Client: TFPHttpClient;
begin
  Result := '';
  Client := TFPHttpClient.Create(nil);
  try
    try
      Client.AddHeader('Content-Type', 'application/json');
      Client.AddHeader('Accept', 'application/json');

      Client.RequestBody := TRawByteStringStream.Create(JsonData);
      try
        Result := Client.Post(URL);

        if Client.ResponseStatusCode <> 201 then
          WriteLn('Attention : Code ', Client.ResponseStatusCode);

      finally
        Client.RequestBody.Free;
      end;

    except
      on E: Exception do
      begin
        WriteLn('Erreur POST : ', E.Message);
        Result := '';
      end;
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation
var
  JsonData, Response: String;
begin
  JsonData := '{"title":"Mon Article","content":"Contenu..."}';
  Response := PostJSON('https://api.example.com/articles', JsonData);
  if Response <> '' then
    WriteLn('Article créé : ', Response);
end;
```

## Requêtes PUT - Modifier des Données

PUT permet de mettre à jour une ressource existante.

```pascal
function PutJSON(const URL, JsonData: String): String;  
var  
  Client: TFPHttpClient;
  Response: String;
begin
  Result := '';
  Client := TFPHttpClient.Create(nil);
  try
    try
      Client.AddHeader('Content-Type', 'application/json');
      Client.AddHeader('Accept', 'application/json');

      Client.RequestBody := TRawByteStringStream.Create(JsonData);
      try
        Response := Client.Put(URL);

        if (Client.ResponseStatusCode >= 200) and
           (Client.ResponseStatusCode < 300) then
          Result := Response
        else
          WriteLn('Erreur PUT : Code ', Client.ResponseStatusCode);

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
var
  JsonData, Response: String;
begin
  JsonData := '{"id":42,"nom":"Dupont","prenom":"Jean","email":"nouveau@example.com"}';
  Response := PutJSON('https://api.example.com/users/42', JsonData);
  if Response <> '' then
    WriteLn('Utilisateur mis à jour : ', Response);
end;
```

## Requêtes DELETE - Supprimer des Données

DELETE permet de supprimer une ressource.

```pascal
function DeleteResource(const URL: String): Boolean;  
var  
  Client: TFPHttpClient;
begin
  Result := False;
  Client := TFPHttpClient.Create(nil);
  try
    try
      Client.Delete(URL);

      // Vérifier le succès (204 No Content ou 200 OK)
      if (Client.ResponseStatusCode = 204) or
         (Client.ResponseStatusCode = 200) then
      begin
        Result := True;
        WriteLn('Ressource supprimée avec succès');
      end
      else
        WriteLn('Erreur DELETE : Code ', Client.ResponseStatusCode);

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
  if DeleteResource('https://api.example.com/users/42') then
    WriteLn('Utilisateur 42 supprimé')
  else
    WriteLn('Échec de la suppression');
end;
```

## Configuration Avancée

### Timeouts (Délais d'Attente)

Évitez que votre programme attende indéfiniment en cas de problème réseau.

```pascal
var
  Client: TFPHttpClient;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Timeout de connexion (en millisecondes)
    Client.ConnectTimeout := 5000;  // 5 secondes

    // Timeout de lecture (pas directement supporté)
    // Utilisez IOTimeout si disponible dans votre version

    Response := Client.Get('https://api.example.com/slow-endpoint');
  finally
    Client.Free;
  end;
end;
```

### User-Agent Personnalisé

Identifiez votre application auprès des serveurs.

```pascal
Client.AddHeader('User-Agent', 'MonAppFreePascal/2.0 (Windows; FreePascal 3.2.2)');
```

### Suivre les Redirections

```pascal
var
  Client: TFPHttpClient;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Activer le suivi automatique des redirections (301, 302, etc.)
    Client.AllowRedirect := True;

    // Nombre maximum de redirections à suivre
    Client.MaxRedirects := 5;

    Response := Client.Get('https://example.com/redirect-page');
  finally
    Client.Free;
  end;
end;
```

### Cookies (si nécessaire)

```pascal
var
  Client: TFPHttpClient;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // TFPHttpClient ne gère pas automatiquement les cookies
    // Il faut les gérer manuellement via les headers

    // Envoyer un cookie
    Client.AddHeader('Cookie', 'session_id=abc123; user_pref=dark_mode');

    Response := Client.Get('https://example.com/protected');

    // Lire les cookies reçus
    CookieHeader := Client.GetHeader(Client.ResponseHeaders, 'Set-Cookie');
    WriteLn('Cookies reçus : ', CookieHeader);
  finally
    Client.Free;
  end;
end;
```

## Téléchargement de Fichiers

### Télécharger un Fichier

```pascal
uses Classes;

procedure DownloadFile(const URL, DestinationFile: String);  
var  
  Client: TFPHttpClient;
  FileStream: TFileStream;
begin
  Client := TFPHttpClient.Create(nil);
  FileStream := TFileStream.Create(DestinationFile, fmCreate);
  try
    try
      WriteLn('Téléchargement de : ', URL);
      Client.Get(URL, FileStream);
      WriteLn('Fichier sauvegardé : ', DestinationFile);
      WriteLn('Taille : ', FileStream.Size, ' octets');
    except
      on E: Exception do
        WriteLn('Erreur de téléchargement : ', E.Message);
    end;
  finally
    FileStream.Free;
    Client.Free;
  end;
end;

// Utilisation
begin
  DownloadFile('https://example.com/fichier.pdf', 'C:\temp\fichier.pdf');
end;
```

### Téléchargement avec Barre de Progression

```pascal
type
  TMyHttpClient = class(TFPHttpClient)
  protected
    procedure DoDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64); override;
  end;

procedure TMyHttpClient.DoDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);  
var  
  Percent: Integer;
begin
  if ContentLength > 0 then
  begin
    Percent := Round((CurrentPos / ContentLength) * 100);
    Write(#13, 'Téléchargement : ', Percent, '% (', CurrentPos, ' / ', ContentLength, ' octets)');
  end;
end;

// Utilisation
var
  Client: TMyHttpClient;
  FileStream: TFileStream;
begin
  Client := TMyHttpClient.Create(nil);
  FileStream := TFileStream.Create('fichier.zip', fmCreate);
  try
    Client.Get('https://example.com/gros-fichier.zip', FileStream);
    WriteLn;
    WriteLn('Téléchargement terminé !');
  finally
    FileStream.Free;
    Client.Free;
  end;
end;
```

## Exemple Complet : Client API REST

Voici un exemple complet qui combine tous les concepts.

```pascal
program RestApiClient;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, Classes, SysUtils;

const
  API_BASE_URL = 'https://jsonplaceholder.typicode.com';

type
  TApiClient = class
  private
    FClient: TFPHttpClient;
  public
    constructor Create;
    destructor Destroy; override;

    function GetUsers: String;
    function GetUser(UserID: Integer): String;
    function CreateUser(const JsonData: String): String;
    function UpdateUser(UserID: Integer; const JsonData: String): String;
    function DeleteUser(UserID: Integer): Boolean;
  end;

constructor TApiClient.Create;  
begin  
  inherited Create;
  FClient := TFPHttpClient.Create(nil);
  FClient.AddHeader('Content-Type', 'application/json');
  FClient.AddHeader('Accept', 'application/json');
end;

destructor TApiClient.Destroy;  
begin  
  FClient.Free;
  inherited Destroy;
end;

function TApiClient.GetUsers: String;  
begin  
  try
    Result := FClient.Get(API_BASE_URL + '/users');
  except
    on E: Exception do
    begin
      WriteLn('Erreur GetUsers : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.GetUser(UserID: Integer): String;  
begin  
  try
    Result := FClient.Get(API_BASE_URL + '/users/' + IntToStr(UserID));
  except
    on E: Exception do
    begin
      WriteLn('Erreur GetUser : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.CreateUser(const JsonData: String): String;  
begin  
  try
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);  // Corps JSON de la requête
    try
      Result := FClient.Post(API_BASE_URL + '/users');
      WriteLn('Code statut : ', FClient.ResponseStatusCode);
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur CreateUser : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.UpdateUser(UserID: Integer; const JsonData: String): String;  
begin  
  try
    FClient.RequestBody := TRawByteStringStream.Create(JsonData);
    try
      Result := FClient.Put(API_BASE_URL + '/users/' + IntToStr(UserID));
    finally
      FClient.RequestBody.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur UpdateUser : ', E.Message);
      Result := '';
    end;
  end;
end;

function TApiClient.DeleteUser(UserID: Integer): Boolean;  
begin  
  try
    FClient.Delete(API_BASE_URL + '/users/' + IntToStr(UserID));
    Result := (FClient.ResponseStatusCode = 200) or
              (FClient.ResponseStatusCode = 204);
  except
    on E: Exception do
    begin
      WriteLn('Erreur DeleteUser : ', E.Message);
      Result := False;
    end;
  end;
end;

// Programme principal
var
  ApiClient: TApiClient;
  Response: String;
  NewUser: String;
begin
  ApiClient := TApiClient.Create;
  try
    // Lister tous les utilisateurs
    WriteLn('=== Tous les utilisateurs ===');
    Response := ApiClient.GetUsers;
    if Response <> '' then
      WriteLn(Copy(Response, 1, 200), '...'); // Afficher un extrait

    WriteLn;

    // Obtenir un utilisateur spécifique
    WriteLn('=== Utilisateur #1 ===');
    Response := ApiClient.GetUser(1);
    if Response <> '' then
      WriteLn(Response);

    WriteLn;

    // Créer un nouvel utilisateur
    WriteLn('=== Créer un utilisateur ===');
    NewUser := '{"name":"Jean Dupont","email":"jean@example.com"}';
    Response := ApiClient.CreateUser(NewUser);
    if Response <> '' then
      WriteLn(Response);

    WriteLn;

    // Mettre à jour un utilisateur
    WriteLn('=== Mettre à jour l''utilisateur #1 ===');
    NewUser := '{"id":1,"name":"Jean Martin","email":"jean.martin@example.com"}';
    Response := ApiClient.UpdateUser(1, NewUser);
    if Response <> '' then
      WriteLn(Response);

    WriteLn;

    // Supprimer un utilisateur
    WriteLn('=== Supprimer l''utilisateur #1 ===');
    if ApiClient.DeleteUser(1) then
      WriteLn('Suppression réussie')
    else
      WriteLn('Échec de la suppression');

  finally
    ApiClient.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## HTTPS et Certificats SSL

### Configuration de Base

Pour HTTPS, il suffit d'inclure l'unité `opensslsockets` :

```pascal
uses
  fphttpclient, opensslsockets;
```

C'est tout ! TFPHttpClient gère automatiquement HTTPS.

### Problèmes de Certificats SSL

Si vous rencontrez des erreurs SSL, c'est souvent lié aux certificats :

```pascal
// Sur Windows, assurez-vous que les DLL OpenSSL sont disponibles :
// - libeay32.dll
// - ssleay32.dll
// Ou pour les versions récentes :
// - libcrypto-1_1.dll (ou libcrypto-3.dll)
// - libssl-1_1.dll (ou libssl-3.dll)
```

**Où trouver ces DLL ?**
- Depuis le site officiel d'OpenSSL
- Incluses dans certaines installations de logiciels
- Téléchargeables depuis des dépôts fiables

## Bonnes Pratiques

### 1. Toujours Libérer les Ressources

```pascal
✅ CORRECT
var
  Client: TFPHttpClient;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // ... utilisation ...
  finally
    Client.Free;  // Toujours libérer !
  end;
end;

❌ À ÉVITER
var
  Client: TFPHttpClient;
begin
  Client := TFPHttpClient.Create(nil);
  Response := Client.Get(URL);
  // Oubli de Free → fuite mémoire !
end;
```

### 2. Gérer les Exceptions

```pascal
✅ CORRECT
try
  Response := Client.Get(URL);
except
  on E: Exception do
    WriteLn('Erreur : ', E.Message);
end;

❌ À ÉVITER
Response := Client.Get(URL);  // Crash si erreur réseau !
```

### 3. Vérifier les Codes de Statut

```pascal
✅ CORRECT
Response := Client.Get(URL);  
if Client.ResponseStatusCode = 200 then  
  // Traiter les données
else
  WriteLn('Erreur : ', Client.ResponseStatusCode);

❌ À ÉVITER
Response := Client.Get(URL);
// Utiliser Response sans vérifier le code
```

### 4. Utiliser des Fonctions Réutilisables

Au lieu de répéter le code, créez des fonctions :

```pascal
function HttpGet(const URL: String; out StatusCode: Integer): String;  
function HttpPost(const URL, JsonData: String; out StatusCode: Integer): String;  
```

### 5. Logger les Requêtes (pour le débogage)

```pascal
procedure LogRequest(const Method, URL: String);  
begin  
  WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', Method, ' ', URL);
end;

// Utilisation
LogRequest('GET', 'https://api.example.com/users');  
Response := Client.Get('https://api.example.com/users');  
```

## Résumé des Points Clés

1. **TFPHttpClient** = classe pour effectuer des requêtes HTTP/HTTPS
2. **Unités nécessaires** : `fphttpclient`, `opensslsockets` (pour HTTPS)
3. **Méthodes principales** : Get, Post, Put, Delete
4. **Toujours** gérer les erreurs avec try-except
5. **Vérifier** le code de statut HTTP (ResponseStatusCode)
6. **Libérer** les ressources avec Free dans finally
7. **Headers** : utiliser AddHeader pour personnaliser les requêtes
8. **JSON** : définir Content-Type et Accept à 'application/json'
9. **Timeouts** : utiliser ConnectTimeout pour éviter les attentes infinies
10. **RequestBody** : utiliser TRawByteStringStream pour POST/PUT

## Ce Qu'il Faut Retenir pour la Suite

Maintenant que vous savez utiliser TFPHttpClient, vous êtes prêt à :
- Consommer des **API REST publiques** (météo, géolocalisation, etc.)
- **Parser les réponses JSON** avec l'unité fpjson
- Créer des applications qui communiquent avec des serveurs distants
- Intégrer des services tiers dans vos applications FreePascal

Dans la prochaine section, nous verrons comment **parser et manipuler le JSON** reçu des API avec la bibliothèque fpjson de FreePascal !

⏭️ [Consommation d'API publiques](/17-communications-reseau-api-rest/06-consommation-api-publiques.md)
