🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.7 Parsing JSON avec fpjson

## Introduction

Jusqu'à présent, nous avons récupéré des données JSON depuis des API, mais nous les avons simplement affichées telles quelles. Maintenant, il est temps d'apprendre à **extraire et utiliser** les informations contenues dans le JSON. C'est ce qu'on appelle le **parsing** (analyse syntaxique).

## Qu'est-ce que le Parsing ?

### Définition

**Parser** (analyser) du JSON signifie :
1. **Lire** une chaîne JSON
2. **Vérifier** qu'elle est valide
3. **Convertir** le texte en structures de données manipulables
4. **Extraire** les valeurs qui nous intéressent

### Analogie

Imaginez que vous recevez une lettre en français :
- **Sans parsing** : Vous voyez juste des caractères, mais ne comprenez pas
- **Avec parsing** : Vous comprenez le français et pouvez extraire les informations importantes

Le parsing JSON fait la même chose : il transforme du texte brut en données utilisables.

### Exemple Visuel

```
Texte JSON brut (String)
    ↓
  Parsing (fpjson)
    ↓
Structures de données Pascal
    ↓
Extraction des valeurs
    ↓
Utilisation dans votre programme
```

## L'Unité fpjson

FreePascal fournit l'unité **fpjson** pour parser et manipuler du JSON.

### Unités Nécessaires

```pascal
uses
  fpjson,      // Classes de base pour JSON
  jsonparser,  // Parser JSON (analyse)
  jsonscanner; // Scanner JSON (optionnel)
```

### Classes Principales

**Hiérarchie des classes JSON :**
```
TJSONData (classe de base abstraite)
    ├─ TJSONNumber      → Nombres (entiers, décimaux)
    ├─ TJSONString      → Chaînes de caractères
    ├─ TJSONBoolean     → Booléens (true/false)
    ├─ TJSONNull        → Valeur null
    ├─ TJSONArray       → Tableaux [...]
    └─ TJSONObject      → Objets {...}
```

**Chaque type correspond à un type JSON** que nous avons vu dans la section 17.4.

## Parser du JSON Simple

### Fonction Principale : GetJSON

La fonction `GetJSON` est le moyen le plus simple de parser du JSON.

```pascal
program SimpleJSONParsing;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser,  // fpjson : types JSON ; jsonparser : fonction GetJSON
  SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
begin
  // Notre chaîne JSON
  JsonString := '{"nom":"Dupont","prenom":"Jean","age":30}';

  try
    // Parser le JSON
    JsonData := GetJSON(JsonString);
    try
      WriteLn('JSON parsé avec succès !');
      WriteLn('Type : ', JsonData.JSONType);
    finally
      // Libérer la mémoire
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur de parsing : ', E.Message);
  end;

  ReadLn;
end.
```

**Points importants :**
1. `GetJSON` renvoie un `TJSONData`
2. Toujours utiliser `try-finally` pour libérer la mémoire
3. Gérer les exceptions de parsing avec `try-except`

### Vérifier le Type JSON

```pascal
var
  JsonData: TJSONData;
begin
  JsonData := GetJSON('{"nom":"Dupont"}');
  try
    case JsonData.JSONType of
      jtNumber:  WriteLn('C''est un nombre');
      jtString:  WriteLn('C''est une chaîne');
      jtBoolean: WriteLn('C''est un booléen');
      jtNull:    WriteLn('C''est null');
      jtArray:   WriteLn('C''est un tableau');
      jtObject:  WriteLn('C''est un objet');
    end;
  finally
    JsonData.Free;
  end;
end;
```

## Extraire des Valeurs d'un Objet JSON

### Méthode 1 : Cast en TJSONObject

```pascal
program ExtractFromObject;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Nom, Prenom: String;
  Age: Integer;
begin
  JsonString := '{"nom":"Dupont","prenom":"Jean","age":30}';

  JsonData := GetJSON(JsonString);
  try
    // Vérifier que c'est bien un objet
    if JsonData.JSONType = jtObject then
    begin
      JsonObj := TJSONObject(JsonData);

      // Extraire les valeurs
      Nom := JsonObj.Get('nom', '');        // Valeur par défaut : ''
      Prenom := JsonObj.Get('prenom', '');
      Age := JsonObj.Get('age', 0);         // Valeur par défaut : 0

      WriteLn('Nom complet : ', Prenom, ' ', Nom);
      WriteLn('Age : ', Age, ' ans');
    end;
  finally
    JsonData.Free;
  end;

  ReadLn;
end.
```

### Méthode 2 : Accès Direct avec FindPath

```pascal
var
  JsonData: TJSONData;
  Nom: String;
  Age: Integer;
begin
  JsonData := GetJSON('{"nom":"Dupont","prenom":"Jean","age":30}');
  try
    // FindPath retourne TJSONData qu'il faut convertir
    Nom := JsonData.FindPath('nom').AsString;
    Age := JsonData.FindPath('age').AsInteger;

    WriteLn('Nom : ', Nom);
    WriteLn('Age : ', Age);
  finally
    JsonData.Free;
  end;
end;
```

### Méthode 3 : Vérifier l'Existence avant l'Accès

```pascal
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Email: String;
begin
  JsonData := GetJSON('{"nom":"Dupont","prenom":"Jean"}');
  try
    JsonObj := TJSONObject(JsonData);

    // Vérifier si la clé existe
    if JsonObj.IndexOfName('email') >= 0 then
      Email := JsonObj.Get('email', '')
    else
    begin
      WriteLn('Email non présent dans le JSON');
      Email := 'inconnu@example.com';  // Valeur par défaut
    end;

    WriteLn('Email : ', Email);
  finally
    JsonData.Free;
  end;
end;
```

## Extraire des Valeurs d'un Tableau JSON

### Parcourir un Tableau

```pascal
program ParseArray;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonArray: TJSONArray;
  i: Integer;
begin
  JsonString := '["pomme","banane","orange","fraise"]';

  JsonData := GetJSON(JsonString);
  try
    if JsonData.JSONType = jtArray then
    begin
      JsonArray := TJSONArray(JsonData);

      WriteLn('Nombre d''éléments : ', JsonArray.Count);
      WriteLn('=== Liste des fruits ===');

      for i := 0 to JsonArray.Count - 1 do
      begin
        WriteLn(i + 1, '. ', JsonArray.Items[i].AsString);
      end;
    end;
  finally
    JsonData.Free;
  end;

  ReadLn;
end.
```

### Tableau d'Objets

```pascal
var
  JsonString: String;
  JsonData: TJSONData;
  JsonArray: TJSONArray;
  JsonObj: TJSONObject;
  i: Integer;
begin
  JsonString := '[' +
    '{"nom":"Dupont","age":30},' +
    '{"nom":"Martin","age":25},' +
    '{"nom":"Durand","age":35}' +
  ']';

  JsonData := GetJSON(JsonString);
  try
    JsonArray := TJSONArray(JsonData);

    WriteLn('=== Liste des personnes ===');
    for i := 0 to JsonArray.Count - 1 do
    begin
      JsonObj := TJSONObject(JsonArray.Items[i]);
      WriteLn(Format('%d. %s (%d ans)',
              [i+1, JsonObj.Get('nom', ''), JsonObj.Get('age', 0)]));
    end;
  finally
    JsonData.Free;
  end;
end;
```

## Parser du JSON Imbriqué (Complexe)

### Objets dans des Objets

```pascal
program NestedJSON;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonObj, AdresseObj: TJSONObject;
  Nom, Ville, Rue: String;
begin
  JsonString := '{' +
    '"nom":"Dupont",' +
    '"prenom":"Jean",' +
    '"adresse":{' +
      '"rue":"123 rue de la Paix",' +
      '"ville":"Paris",' +
      '"code_postal":"75001"' +
    '}' +
  '}';

  JsonData := GetJSON(JsonString);
  try
    JsonObj := TJSONObject(JsonData);

    // Extraire les données du niveau principal
    Nom := JsonObj.Get('nom', '');

    // Extraire l'objet imbriqué "adresse"
    AdresseObj := JsonObj.GetPath('adresse') as TJSONObject;
    Ville := AdresseObj.Get('ville', '');
    Rue := AdresseObj.Get('rue', '');

    WriteLn('Nom : ', Nom);
    WriteLn('Adresse : ', Rue, ', ', Ville);
  finally
    JsonData.Free;
  end;

  ReadLn;
end.
```

### Utiliser GetPath pour Accès Direct

```pascal
var
  JsonData: TJSONData;
  Ville: String;
begin
  JsonData := GetJSON('{"personne":{"adresse":{"ville":"Paris"}}}');
  try
    // Accès direct avec notation pointée
    Ville := JsonData.GetPath('personne.adresse.ville').AsString;
    WriteLn('Ville : ', Ville);
  finally
    JsonData.Free;
  end;
end;
```

### Tableaux dans des Objets

```pascal
var
  JsonString: String;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  HobbiesArray: TJSONArray;
  i: Integer;
begin
  JsonString := '{' +
    '"nom":"Dupont",' +
    '"hobbies":["lecture","vélo","cuisine"]' +
  '}';

  JsonData := GetJSON(JsonString);
  try
    JsonObj := TJSONObject(JsonData);

    WriteLn('Nom : ', JsonObj.Get('nom', ''));

    // Extraire le tableau
    HobbiesArray := JsonObj.GetPath('hobbies') as TJSONArray;
    WriteLn('Hobbies :');
    for i := 0 to HobbiesArray.Count - 1 do
      WriteLn('  - ', HobbiesArray.Items[i].AsString);

  finally
    JsonData.Free;
  end;
end;
```

## Gestion des Erreurs de Parsing

### JSON Invalide

```pascal
program HandleParsingErrors;

{$mode objfpc}{$H+}

uses
  fpjson, jsonparser, SysUtils;

procedure TryParseJSON(const JsonString: String);  
var  
  JsonData: TJSONData;
begin
  try
    JsonData := GetJSON(JsonString);
    try
      WriteLn('JSON valide !');
    finally
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur de parsing : ', E.Message);
  end;
end;

begin
  // JSON valide
  WriteLn('=== Test 1 : JSON valide ===');
  TryParseJSON('{"nom":"Dupont"}');
  WriteLn;

  // JSON invalide : guillemets simples
  WriteLn('=== Test 2 : Guillemets simples (invalide) ===');
  TryParseJSON('{"nom":''Dupont''}');
  WriteLn;

  // JSON invalide : virgule finale
  WriteLn('=== Test 3 : Virgule finale (invalide) ===');
  TryParseJSON('{"nom":"Dupont",}');
  WriteLn;

  // JSON invalide : clé sans guillemets
  WriteLn('=== Test 4 : Clé sans guillemets (invalide) ===');
  TryParseJSON('{nom:"Dupont"}');

  ReadLn;
end.
```

### Vérifier les Valeurs Null

```pascal
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Email: String;
  EmailField: TJSONData;
begin
  JsonData := GetJSON('{"nom":"Dupont","email":null}');
  try
    JsonObj := TJSONObject(JsonData);

    EmailField := JsonObj.Find('email');

    if Assigned(EmailField) then
    begin
      if EmailField.JSONType = jtNull then
        Email := 'Non renseigné'
      else
        Email := EmailField.AsString;
    end
    else
      Email := 'Champ absent';

    WriteLn('Email : ', Email);
  finally
    JsonData.Free;
  end;
end;
```

## Exemple Pratique : Parser une Réponse API

### Exemple 1 : OpenWeatherMap

```pascal
program ParseWeather;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, fpjson, jsonparser, SysUtils;

const
  API_KEY = 'VOTRE_CLE_API';

procedure GetWeather(const City: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  JsonData: TJSONData;
  JsonObj, MainObj, WindObj: TJSONObject;
  WeatherArray: TJSONArray;
  WeatherObj: TJSONObject;
  CityName, Description: String;
  Temp, FeelsLike, Humidity, WindSpeed: Double;
begin
  Client := TFPHttpClient.Create(nil);
  try
    // Récupérer les données
    Response := Client.Get(Format(
      'https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s&units=metric&lang=fr',
      [City, API_KEY]
    ));

    if Client.ResponseStatusCode = 200 then
    begin
      // Parser le JSON
      JsonData := GetJSON(Response);
      try
        JsonObj := TJSONObject(JsonData);

        // Extraire les données
        CityName := JsonObj.Get('name', '');

        // Objet "main" avec température, etc.
        MainObj := JsonObj.GetPath('main') as TJSONObject;
        Temp := MainObj.Get('temp', 0.0);
        FeelsLike := MainObj.Get('feels_like', 0.0);
        Humidity := MainObj.Get('humidity', 0.0);

        // Objet "wind"
        WindObj := JsonObj.GetPath('wind') as TJSONObject;
        WindSpeed := WindObj.Get('speed', 0.0);

        // Tableau "weather" (prendre le premier élément)
        WeatherArray := JsonObj.GetPath('weather') as TJSONArray;
        if WeatherArray.Count > 0 then
        begin
          WeatherObj := TJSONObject(WeatherArray.Items[0]);
          Description := WeatherObj.Get('description', '');
        end;

        // Afficher les résultats
        WriteLn('=== Météo à ', CityName, ' ===');
        WriteLn('Description : ', Description);
        WriteLn('Température : ', Temp:0:1, '°C');
        WriteLn('Ressenti : ', FeelsLike:0:1, '°C');
        WriteLn('Humidité : ', Humidity:0:0, '%');
        WriteLn('Vent : ', WindSpeed:0:1, ' m/s');

      finally
        JsonData.Free;
      end;
    end
    else
      WriteLn('Erreur HTTP : ', Client.ResponseStatusCode);

  finally
    Client.Free;
  end;
end;

begin
  GetWeather('Paris');
  ReadLn;
end.
```

### Exemple 2 : JSONPlaceholder (Liste d'Utilisateurs)

```pascal
program ParseUsers;

{$mode objfpc}{$H+}

uses
  fphttpclient, opensslsockets, fpjson, jsonparser, SysUtils;

procedure ListUsers;  
var  
  Client: TFPHttpClient;
  Response: String;
  JsonData: TJSONData;
  JsonArray: TJSONArray;
  JsonObj, AddressObj, CompanyObj: TJSONObject;
  i: Integer;
  Name, Email, City, CompanyName: String;
begin
  Client := TFPHttpClient.Create(nil);
  try
    Response := Client.Get('https://jsonplaceholder.typicode.com/users');

    if Client.ResponseStatusCode = 200 then
    begin
      // Parser le JSON (c'est un tableau)
      JsonData := GetJSON(Response);
      try
        JsonArray := TJSONArray(JsonData);

        WriteLn('=== Liste des utilisateurs ===');
        WriteLn('Total : ', JsonArray.Count, ' utilisateurs');
        WriteLn;

        for i := 0 to JsonArray.Count - 1 do
        begin
          JsonObj := TJSONObject(JsonArray.Items[i]);

          // Extraire les données de base
          Name := JsonObj.Get('name', '');
          Email := JsonObj.Get('email', '');

          // Extraire l'adresse (objet imbriqué)
          AddressObj := JsonObj.GetPath('address') as TJSONObject;
          City := AddressObj.Get('city', '');

          // Extraire l'entreprise (objet imbriqué)
          CompanyObj := JsonObj.GetPath('company') as TJSONObject;
          CompanyName := CompanyObj.Get('name', '');

          // Afficher
          WriteLn(Format('%d. %s', [i+1, Name]));
          WriteLn('   Email : ', Email);
          WriteLn('   Ville : ', City);
          WriteLn('   Entreprise : ', CompanyName);
          WriteLn;
        end;

      finally
        JsonData.Free;
      end;
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

### Exemple 3 : REST Countries

```pascal
procedure GetCountryInfo(const CountryName: String);  
var  
  Client: TFPHttpClient;
  Response: String;
  JsonData: TJSONData;
  JsonArray: TJSONArray;
  JsonObj, NameObj: TJSONObject;
  CapitalArray: TJSONArray;
  CommonName, OfficialName, Capital, Region, Subregion: String;
  Population: Int64;
  Area: Double;
begin
  Client := TFPHttpClient.Create(nil);
  try
    Response := Client.Get('https://restcountries.com/v3.1/name/' + CountryName);

    if Client.ResponseStatusCode = 200 then
    begin
      JsonData := GetJSON(Response);
      try
        // L'API renvoie un tableau (même pour un seul pays)
        JsonArray := TJSONArray(JsonData);

        if JsonArray.Count > 0 then
        begin
          // Prendre le premier pays
          JsonObj := TJSONObject(JsonArray.Items[0]);

          // Nom (objet complexe)
          NameObj := JsonObj.GetPath('name') as TJSONObject;
          CommonName := NameObj.Get('common', '');
          OfficialName := NameObj.Get('official', '');

          // Capitale (tableau)
          if JsonObj.IndexOfName('capital') >= 0 then
          begin
            CapitalArray := JsonObj.GetPath('capital') as TJSONArray;
            if CapitalArray.Count > 0 then
              Capital := CapitalArray.Items[0].AsString;
          end
          else
            Capital := 'Non disponible';

          // Autres informations
          Region := JsonObj.Get('region', '');
          Subregion := JsonObj.Get('subregion', '');
          Population := JsonObj.Get('population', 0);
          Area := JsonObj.Get('area', 0.0);

          // Afficher
          WriteLn('=== ', CommonName, ' ===');
          WriteLn('Nom officiel : ', OfficialName);
          WriteLn('Capitale : ', Capital);
          WriteLn('Région : ', Region, ' (', Subregion, ')');
          WriteLn('Population : ', Population, ' habitants');
          WriteLn('Superficie : ', Area:0:0, ' km²');
        end;

      finally
        JsonData.Free;
      end;
    end;

  finally
    Client.Free;
  end;
end;

begin
  GetCountryInfo('France');
end;
```

## Créer du JSON (Génération)

Parfois, vous devez créer du JSON à envoyer à une API.

### Créer un Objet JSON Manuellement

```pascal
program CreateJSON;

{$mode objfpc}{$H+}

uses
  fpjson, SysUtils;

var
  JsonObj: TJSONObject;
  JsonString: String;
begin
  // Créer un objet JSON
  JsonObj := TJSONObject.Create;
  try
    // Ajouter des paires clé-valeur
    JsonObj.Add('nom', 'Dupont');
    JsonObj.Add('prenom', 'Jean');
    JsonObj.Add('age', 30);
    JsonObj.Add('actif', True);

    // Convertir en chaîne JSON
    JsonString := JsonObj.AsJSON;
    WriteLn('JSON créé :');
    WriteLn(JsonString);

  finally
    JsonObj.Free;
  end;

  ReadLn;
end.
```

### Créer un JSON Imbriqué

```pascal
var
  PersonObj, AddressObj: TJSONObject;
  JsonString: String;
begin
  PersonObj := TJSONObject.Create;
  try
    PersonObj.Add('nom', 'Dupont');
    PersonObj.Add('prenom', 'Jean');

    // Créer l'objet adresse
    AddressObj := TJSONObject.Create;
    AddressObj.Add('rue', '123 rue de la Paix');
    AddressObj.Add('ville', 'Paris');
    AddressObj.Add('code_postal', '75001');

    // Ajouter l'objet adresse à la personne
    PersonObj.Add('adresse', AddressObj);

    JsonString := PersonObj.AsJSON;
    WriteLn(JsonString);

  finally
    PersonObj.Free;
  end;
end;
```

### Créer un Tableau JSON

```pascal
var
  JsonArray: TJSONArray;
  JsonString: String;
begin
  JsonArray := TJSONArray.Create;
  try
    // Ajouter des éléments
    JsonArray.Add('pomme');
    JsonArray.Add('banane');
    JsonArray.Add('orange');

    JsonString := JsonArray.AsJSON;
    WriteLn(JsonString);
    // Résultat : ["pomme","banane","orange"]

  finally
    JsonArray.Free;
  end;
end;
```

### Créer un Tableau d'Objets

```pascal
var
  JsonArray: TJSONArray;
  PersonObj: TJSONObject;
  JsonString: String;
begin
  JsonArray := TJSONArray.Create;
  try
    // Premier objet
    PersonObj := TJSONObject.Create;
    PersonObj.Add('nom', 'Dupont');
    PersonObj.Add('age', 30);
    JsonArray.Add(PersonObj);

    // Deuxième objet
    PersonObj := TJSONObject.Create;
    PersonObj.Add('nom', 'Martin');
    PersonObj.Add('age', 25);
    JsonArray.Add(PersonObj);

    JsonString := JsonArray.AsJSON;
    WriteLn(JsonString);

  finally
    JsonArray.Free;
  end;
end;
```

### Formater le JSON (Pretty Print)

```pascal
var
  JsonObj: TJSONObject;
  JsonString: String;
begin
  JsonObj := TJSONObject.Create;
  try
    JsonObj.Add('nom', 'Dupont');
    JsonObj.Add('prenom', 'Jean');
    JsonObj.Add('age', 30);

    // Format compact (défaut)
    WriteLn('=== Format compact ===');
    WriteLn(JsonObj.AsJSON);
    WriteLn;

    // Format lisible (avec indentation)
    WriteLn('=== Format lisible ===');
    WriteLn(JsonObj.FormatJSON);

  finally
    JsonObj.Free;
  end;
end;
```

## Classe Helper pour Simplifier le Parsing

Créons une classe helper pour faciliter le parsing JSON.

```pascal
unit JSONHelper;

{$mode objfpc}{$H+}

interface

uses
  fpjson, jsonparser, SysUtils;

type
  TJSONHelper = class
  public
    class function ParseString(const JsonString: String): TJSONData;
    class function GetString(JsonObj: TJSONObject; const Key: String; const Default: String = ''): String;
    class function GetInteger(JsonObj: TJSONObject; const Key: String; const Default: Integer = 0): Integer;
    class function GetFloat(JsonObj: TJSONObject; const Key: String; const Default: Double = 0.0): Double;
    class function GetBoolean(JsonObj: TJSONObject; const Key: String; const Default: Boolean = False): Boolean;
    class function GetObject(JsonObj: TJSONObject; const Key: String): TJSONObject;
    class function GetArray(JsonObj: TJSONObject; const Key: String): TJSONArray;
    class function HasKey(JsonObj: TJSONObject; const Key: String): Boolean;
  end;

implementation

class function TJSONHelper.ParseString(const JsonString: String): TJSONData;  
begin  
  try
    Result := GetJSON(JsonString);
  except
    Result := nil;
  end;
end;

class function TJSONHelper.GetString(JsonObj: TJSONObject; const Key: String; const Default: String): String;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetInteger(JsonObj: TJSONObject; const Key: String; const Default: Integer): Integer;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetFloat(JsonObj: TJSONObject; const Key: String; const Default: Double): Double;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetBoolean(JsonObj: TJSONObject; const Key: String; const Default: Boolean): Boolean;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.Get(Key, Default)
  else
    Result := Default;
end;

class function TJSONHelper.GetObject(JsonObj: TJSONObject; const Key: String): TJSONObject;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.GetPath(Key) as TJSONObject
  else
    Result := nil;
end;

class function TJSONHelper.GetArray(JsonObj: TJSONObject; const Key: String): TJSONArray;  
begin  
  if HasKey(JsonObj, Key) then
    Result := JsonObj.GetPath(Key) as TJSONArray
  else
    Result := nil;
end;

class function TJSONHelper.HasKey(JsonObj: TJSONObject; const Key: String): Boolean;  
begin  
  Result := JsonObj.IndexOfName(Key) >= 0;
end;

end.
```

### Utilisation du Helper

```pascal
program UseJSONHelper;

uses
  JSONHelper, fpjson, SysUtils;

var
  JsonString: String;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Name: String;
  Age: Integer;
begin
  JsonString := '{"nom":"Dupont","prenom":"Jean","age":30}';

  JsonData := TJSONHelper.ParseString(JsonString);
  if Assigned(JsonData) then
  try
    JsonObj := TJSONObject(JsonData);

    // Utilisation simplifiée
    Name := TJSONHelper.GetString(JsonObj, 'nom', 'Inconnu');
    Age := TJSONHelper.GetInteger(JsonObj, 'age', 0);

    WriteLn('Nom : ', Name);
    WriteLn('Age : ', Age);

    // Vérifier l'existence d'une clé
    if TJSONHelper.HasKey(JsonObj, 'email') then
      WriteLn('Email : ', TJSONHelper.GetString(JsonObj, 'email'))
    else
      WriteLn('Email non renseigné');

  finally
    JsonData.Free;
  end
  else
    WriteLn('Erreur de parsing JSON');

  ReadLn;
end.
```

## Bonnes Pratiques

### 1. Toujours Libérer la Mémoire

```pascal
✅ CORRECT
var
  JsonData: TJSONData;
begin
  JsonData := GetJSON(JsonString);
  try
    // Utilisation
  finally
    JsonData.Free;  // Toujours libérer !
  end;
end;

❌ ERREUR
JsonData := GetJSON(JsonString);
// Utilisation sans Free → fuite mémoire !
```

### 2. Gérer les Exceptions

```pascal
✅ CORRECT
try
  JsonData := GetJSON(JsonString);
  try
    // Utilisation
  finally
    JsonData.Free;
  end;
except
  on E: Exception do
    WriteLn('Erreur : ', E.Message);
end;
```

### 3. Vérifier les Types

```pascal
✅ CORRECT
if JsonData.JSONType = jtObject then  
begin  
  JsonObj := TJSONObject(JsonData);
  // Utilisation sûre
end;

❌ DANGEREUX
JsonObj := TJSONObject(JsonData);  // Peut planter si ce n'est pas un objet !
```

### 4. Vérifier l'Existence des Clés

```pascal
✅ CORRECT
if JsonObj.IndexOfName('email') >= 0 then
  Email := JsonObj.Get('email', '')
else
  Email := 'Non renseigné';

❌ RISQUÉ
Email := JsonObj.Get('email', '');  // OK avec valeur par défaut
                                     // mais mieux de vérifier
```

### 5. Utiliser des Valeurs par Défaut

```pascal
✅ CORRECT
Age := JsonObj.Get('age', 0);          // 0 si absent  
Name := JsonObj.Get('nom', 'Inconnu'); // 'Inconnu' si absent  
```

## Dépannage des Problèmes Courants

### Problème : Exception lors du Parsing

**Cause :** JSON invalide

**Solution :**
```pascal
1. Vérifier la syntaxe JSON (jsonlint.com)
2. Afficher le JSON brut pour voir ce qui est reçu
3. Vérifier les guillemets doubles, virgules, etc.
```

### Problème : "Invalid typecast"

**Cause :** Tentative de cast vers le mauvais type

**Solution :**
```pascal
// Toujours vérifier le type avant de caster
if JsonData.JSONType = jtObject then
  JsonObj := TJSONObject(JsonData)
else
  WriteLn('Ce n''est pas un objet !');
```

### Problème : Valeur Nil/Null

**Cause :** Clé inexistante ou valeur null

**Solution :**
```pascal
// Vérifier avant d'accéder
EmailField := JsonObj.Find('email');  
if Assigned(EmailField) and (EmailField.JSONType <> jtNull) then  
  Email := EmailField.AsString;
```

### Problème : Fuite Mémoire

**Cause :** Oubli de Free

**Solution :**
```pascal
// Toujours utiliser try-finally
JsonData := GetJSON(JsonString);  
try  
  // Utilisation
finally
  JsonData.Free;  // Ne jamais oublier !
end;
```

## Résumé des Points Clés

1. **fpjson** = bibliothèque FreePascal pour manipuler JSON
2. **GetJSON** = fonction principale pour parser du JSON
3. **TJSONObject** = objets JSON {...}
4. **TJSONArray** = tableaux JSON [...]
5. **Toujours libérer** la mémoire avec Free
6. **Gérer les exceptions** lors du parsing
7. **Vérifier les types** avant de caster
8. **Vérifier l'existence** des clés avant accès
9. **Valeurs par défaut** pour éviter les erreurs
10. **GetPath** = accès direct aux propriétés imbriquées

## Ce Qu'il Faut Retenir pour la Suite

Vous savez maintenant :
- Parser du JSON simple et complexe
- Extraire des valeurs d'objets et tableaux
- Gérer les structures imbriquées
- Créer du JSON pour envoyer à des API
- Gérer les erreurs de parsing
- Utiliser fpjson dans des applications réelles

Dans la prochaine section, nous aborderons la **gestion des erreurs réseau** et comment rendre vos applications plus robustes face aux problèmes de connexion et aux réponses d'API inattendues !

⏭️ [Gestion des erreurs réseau](/17-communications-reseau-api-rest/08-gestion-erreurs-reseau.md)
