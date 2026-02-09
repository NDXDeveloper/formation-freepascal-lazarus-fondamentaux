🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.4 Format JSON : structure et syntaxe

## Introduction

**JSON** (JavaScript Object Notation) est le format de données le plus utilisé pour échanger des informations entre applications web. Si vous utilisez des API REST, vous manipulerez du JSON dans 95% des cas. Heureusement, JSON est très simple à comprendre !

## Qu'est-ce que JSON ?

### Définition

JSON est un **format de données texte** qui permet de représenter des informations structurées de manière lisible par les humains et facilement analysable par les machines.

### Origine

JSON a été créé par Douglas Crockford au début des années 2000 comme sous-ensemble de JavaScript, mais il est maintenant utilisé par **tous les langages de programmation**, y compris FreePascal !

### Pourquoi JSON est si populaire ?

✅ **Lisible** : facile à lire et à comprendre pour un humain  
✅ **Léger** : peu de caractères "de décoration"  
✅ **Universel** : supporté par tous les langages  
✅ **Flexible** : peut représenter des structures complexes  
✅ **Standard** : spécification officielle (RFC 8259)

### JSON vs autres formats

```
JSON                    XML                     CSV
────────────────────────────────────────────────────────
Léger                   Verbeux                 Très léger
Facile à parser         Plus complexe           Simple
Structures complexes    Structures complexes    Tableaux simples seulement
Pas de schéma strict    Schéma possible (XSD)   Pas de schéma
Moderne                 Plus ancien             Le plus ancien
```

## Les Types de Données JSON

JSON supporte **6 types de données** :

### 1. String (Chaîne de caractères)

Toujours entre **guillemets doubles** `"` :

```json
"Bonjour"
"Hello World"
"jean.dupont@example.com"
"C'est l'été"
"Ligne 1\nLigne 2"
```

**Caractères spéciaux à échapper :**
```json
"Guillemets: \" "
"Backslash: \\ "
"Slash: \/ "
"Retour chariot: \n "
"Tabulation: \t "
"Unicode: \u00E9"
```

### 2. Number (Nombre)

Entiers ou décimaux, **sans guillemets** :

```json
42
-17
3.14159
0.5
1.23e10
-2.5E-3
```

**Important :** Pas de séparateurs de milliers, utiliser le point pour les décimales.

```json
✅ CORRECT          ❌ INCORRECT
1000               1,000
3.14               3,14
-42                - 42
```

### 3. Boolean (Booléen)

Seulement deux valeurs possibles : `true` ou `false` (en minuscules) :

```json
true
false
```

**Erreurs courantes :**
```json
❌ INCORRECT        ✅ CORRECT
True               true
TRUE               true
"true"             true
1                  true (mais c'est un nombre, pas un booléen)
```

### 4. Null

Représente l'absence de valeur :

```json
null
```

**Important :** En minuscules uniquement.

```json
❌ INCORRECT        ✅ CORRECT
NULL               null
Null               null
"null"             null (c'est une chaîne, pas null)
```

### 5. Object (Objet)

Collection de **paires clé-valeur** entre accolades `{}` :

```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "age": 30
}
```

**Règles importantes :**
- Les **clés** doivent être des chaînes entre guillemets doubles
- Chaque paire est séparée par une **virgule** `,`
- **Pas de virgule** après le dernier élément

### 6. Array (Tableau)

Liste ordonnée de valeurs entre crochets `[]` :

```json
[1, 2, 3, 4, 5]
["pomme", "banane", "orange"]
[true, false, true]
```

**Les éléments peuvent être de types différents :**
```json
[42, "texte", true, null]
```

## Syntaxe de Base

### Structure d'un Objet JSON

Un objet JSON est une collection de paires `"clé": valeur` :

```json
{
  "clé1": "valeur1",
  "clé2": "valeur2",
  "clé3": "valeur3"
}
```

**Exemple concret :**
```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "age": 30,
  "actif": true,
  "email": "jean.dupont@example.com"
}
```

### Structure d'un Tableau JSON

Un tableau est une liste ordonnée de valeurs :

```json
[
  "élément1",
  "élément2",
  "élément3"
]
```

**Exemple concret :**
```json
[
  "Lundi",
  "Mardi",
  "Mercredi",
  "Jeudi",
  "Vendredi"
]
```

## Structures Imbriquées

La puissance de JSON vient de sa capacité à **imbriquer** des structures.

### Objet dans un Objet

```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "adresse": {
    "rue": "123 rue de la Paix",
    "ville": "Paris",
    "code_postal": "75001",
    "pays": "France"
  }
}
```

### Tableau dans un Objet

```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "hobbies": ["lecture", "vélo", "cuisine"],
  "langues": ["français", "anglais", "espagnol"]
}
```

### Objets dans un Tableau

```json
[
  {
    "nom": "Dupont",
    "prenom": "Jean",
    "age": 30
  },
  {
    "nom": "Martin",
    "prenom": "Sophie",
    "age": 25
  },
  {
    "nom": "Durand",
    "prenom": "Pierre",
    "age": 35
  }
]
```

### Structures Complexes

JSON peut représenter des structures très complexes :

```json
{
  "entreprise": "TechCorp",
  "fondee": 2010,
  "employes": [
    {
      "id": 1,
      "nom": "Dupont",
      "prenom": "Jean",
      "poste": "Développeur",
      "competences": ["Pascal", "Python", "SQL"],
      "salaire": 45000,
      "actif": true,
      "manager": null,
      "contact": {
        "email": "jean.dupont@techcorp.com",
        "telephone": "0612345678"
      }
    },
    {
      "id": 2,
      "nom": "Martin",
      "prenom": "Sophie",
      "poste": "Manager",
      "competences": ["Management", "Agile"],
      "salaire": 65000,
      "actif": true,
      "manager": null,
      "contact": {
        "email": "sophie.martin@techcorp.com",
        "telephone": "0623456789"
      }
    }
  ],
  "adresse": {
    "siege": {
      "rue": "10 avenue des Champs",
      "ville": "Paris",
      "code_postal": "75008"
    },
    "bureaux": [
      {
        "ville": "Lyon",
        "employes": 50
      },
      {
        "ville": "Marseille",
        "employes": 30
      }
    ]
  }
}
```

## Règles de Syntaxe Importantes

### ✅ Ce qui est OBLIGATOIRE

1. **Guillemets doubles** pour les clés et les chaînes :
```json
✅ { "nom": "Dupont" }
❌ { 'nom': 'Dupont' }
❌ { nom: "Dupont" }
```

2. **Virgule** entre les éléments (mais pas après le dernier) :
```json
✅ { "a": 1, "b": 2 }
❌ { "a": 1, "b": 2, }
❌ { "a": 1  "b": 2 }
```

3. **Deux-points** `:` entre clé et valeur :
```json
✅ { "nom": "Dupont" }
❌ { "nom" = "Dupont" }
❌ { "nom" -> "Dupont" }
```

### ❌ Ce qui est INTERDIT

1. **Commentaires** (JSON pur ne supporte pas les commentaires) :
```json
❌ INTERDIT
{
  // Ceci est un commentaire
  "nom": "Dupont"  /* commentaire */
}

✅ Si vraiment nécessaire, utiliser une clé spéciale :
{
  "_comment": "Ceci est une note",
  "nom": "Dupont"
}
```

2. **Virgule finale** (trailing comma) :
```json
❌ INTERDIT
{
  "nom": "Dupont",
  "prenom": "Jean",
}

✅ CORRECT
{
  "nom": "Dupont",
  "prenom": "Jean"
}
```

3. **Guillemets simples** :
```json
❌ INTERDIT          ✅ CORRECT
{ 'nom': 'Dupont' }  { "nom": "Dupont" }
```

4. **Clés dupliquées** (techniquement autorisé, mais déconseillé) :
```json
❌ À ÉVITER
{
  "nom": "Dupont",
  "nom": "Martin"
}
```

5. **Valeurs indéfinies** :
```json
❌ INTERDIT          ✅ UTILISER
{ "valeur": undefined } { "valeur": null }
```

## Formatage et Lisibilité

### JSON Compact (Minifié)

Sans espaces, sur une seule ligne :
```json
{"nom":"Dupont","prenom":"Jean","age":30,"adresse":{"ville":"Paris","cp":"75001"}}
```

**Usage :** Transmission réseau (économie de bande passante)

### JSON Formaté (Pretty-print)

Avec indentation pour la lisibilité :
```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "age": 30,
  "adresse": {
    "ville": "Paris",
    "cp": "75001"
  }
}
```

**Usage :** Développement, débogage, documentation

**Bonnes pratiques d'indentation :**
- 2 ou 4 espaces par niveau (pas de tabulations)
- Une propriété par ligne
- Accolades d'ouverture sur la même ligne
- Accolades de fermeture alignées avec le début

## Cas d'Usage Courants

### 1. Configuration d'Application

```json
{
  "app_name": "MonApp",
  "version": "1.0.0",
  "database": {
    "host": "localhost",
    "port": 5432,
    "name": "mydb"
  },
  "logging": {
    "level": "info",
    "file": "/var/log/app.log"
  },
  "features": {
    "dark_mode": true,
    "notifications": true,
    "analytics": false
  }
}
```

### 2. Réponse d'API REST

```json
{
  "status": "success",
  "data": {
    "user": {
      "id": 42,
      "username": "jean_dupont",
      "email": "jean@example.com"
    }
  },
  "timestamp": "2025-10-15T10:30:00Z"
}
```

### 3. Liste de Produits (E-commerce)

```json
{
  "products": [
    {
      "id": 1,
      "name": "Ordinateur portable",
      "price": 899.99,
      "currency": "EUR",
      "in_stock": true,
      "categories": ["électronique", "informatique"],
      "rating": 4.5,
      "reviews_count": 127
    },
    {
      "id": 2,
      "name": "Souris sans fil",
      "price": 29.99,
      "currency": "EUR",
      "in_stock": true,
      "categories": ["électronique", "accessoires"],
      "rating": 4.2,
      "reviews_count": 89
    }
  ],
  "total": 2,
  "page": 1
}
```

### 4. Données Météo

```json
{
  "location": {
    "city": "Paris",
    "country": "France",
    "coordinates": {
      "lat": 48.8566,
      "lon": 2.3522
    }
  },
  "current": {
    "temperature": 18.5,
    "feels_like": 17.2,
    "humidity": 65,
    "conditions": "partiellement nuageux",
    "wind": {
      "speed": 12.5,
      "direction": "NO"
    }
  },
  "forecast": [
    {
      "date": "2025-10-16",
      "temp_min": 14,
      "temp_max": 20,
      "conditions": "ensoleillé"
    },
    {
      "date": "2025-10-17",
      "temp_min": 13,
      "temp_max": 19,
      "conditions": "nuageux"
    }
  ]
}
```

## Gestion des Dates et Heures

JSON n'a **pas de type natif pour les dates**. Voici les conventions courantes :

### Format ISO 8601 (recommandé)

```json
{
  "created_at": "2025-10-15T10:30:00Z",
  "updated_at": "2025-10-15T14:45:30.123Z",
  "birth_date": "1990-05-15"
}
```

**Format :** `YYYY-MM-DDTHH:MM:SS.sssZ`
- `T` sépare la date et l'heure
- `Z` indique UTC (Zulu time)
- Avec fuseau horaire : `2025-10-15T10:30:00+02:00`

### Timestamp Unix (secondes depuis 1970)

```json
{
  "created_at": 1697368200,
  "updated_at": 1697383530
}
```

**Avantage :** Facile à manipuler en programmation
**Inconvénient :** Pas lisible par un humain

## Erreurs Courantes et Comment les Éviter

### 1. Virgule finale

```json
❌ ERREUR                        ✅ CORRECT
{                                {
  "nom": "Dupont",                 "nom": "Dupont",
  "prenom": "Jean",                "prenom": "Jean"
}                                }
```

### 2. Guillemets simples

```json
❌ ERREUR                        ✅ CORRECT
{ 'nom': 'Dupont' }              { "nom": "Dupont" }
```

### 3. Clé sans guillemets

```json
❌ ERREUR                        ✅ CORRECT
{ nom: "Dupont" }                { "nom": "Dupont" }
```

### 4. Caractères non échappés

```json
❌ ERREUR                        ✅ CORRECT
{ "message": "Il dit "oui"" }    { "message": "Il dit \"oui\"" }
{ "path": "C:\dossier\file" }    { "path": "C:\\dossier\\file" }
```

### 5. Nombres invalides

```json
❌ ERREUR                        ✅ CORRECT
{ "price": 1,234.56 }            { "price": 1234.56 }
{ "value": .5 }                  { "value": 0.5 }
{ "number": 1. }                 { "number": 1.0 }
```

### 6. Valeurs booléennes mal écrites

```json
❌ ERREUR                        ✅ CORRECT
{ "actif": True }                { "actif": true }
{ "valide": TRUE }               { "valide": true }
{ "present": "true" }            { "present": true }
```

## Validation JSON

Il est important de **valider** que votre JSON est correct avant de l'utiliser.

### Outils en ligne

- **JSONLint** (jsonlint.com) : validateur JSON
- **JSON Formatter** : valide et formate
- **JSON Schema Validator** : validation avancée avec schéma

### Validation en FreePascal

FreePascal possède l'unité `fpjson` qui détecte automatiquement les erreurs de syntaxe lors du parsing.

```pascal
uses fpjson, jsonparser;

var
  JsonData: TJSONData;
  JsonString: String;
begin
  JsonString := '{ "nom": "Dupont", "prenom": "Jean" }';

  try
    JsonData := GetJSON(JsonString);
    WriteLn('JSON valide !');
    JsonData.Free;
  except
    on E: Exception do
      WriteLn('JSON invalide : ', E.Message);
  end;
end;
```

## JSON Schema (Aperçu)

Pour des validations avancées, on peut utiliser **JSON Schema** qui définit la structure attendue :

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "nom": {
      "type": "string",
      "minLength": 1
    },
    "age": {
      "type": "integer",
      "minimum": 0,
      "maximum": 150
    },
    "email": {
      "type": "string",
      "format": "email"
    }
  },
  "required": ["nom", "email"]
}
```

Ce schéma définit qu'un objet valide doit avoir :
- Un `nom` (chaîne non vide, obligatoire)
- Un `age` (entier entre 0 et 150, optionnel)
- Un `email` (format email valide, obligatoire)

## JSON vs XML : Comparaison

### Même Données, Deux Formats

**JSON :**
```json
{
  "personne": {
    "nom": "Dupont",
    "prenom": "Jean",
    "age": 30,
    "hobbies": ["lecture", "vélo"]
  }
}
```

**XML :**
```xml
<personne>
  <nom>Dupont</nom>
  <prenom>Jean</prenom>
  <age>30</age>
  <hobbies>
    <hobby>lecture</hobby>
    <hobby>vélo</hobby>
  </hobbies>
</personne>
```

### Avantages de JSON

✅ Plus léger (moins de caractères)  
✅ Plus facile à lire  
✅ Plus rapide à parser  
✅ Types de données natifs (nombre, booléen)  
✅ Tableaux natifs

### Avantages de XML

✅ Support des attributs  
✅ Espaces de noms (namespaces)  
✅ Validation stricte avec XSD  
✅ Support des commentaires  
✅ Plus ancien, plus de bibliothèques legacy

## Conseils Pratiques

### 1. Nommage des Clés

**Utilisez des conventions cohérentes :**

```json
// snake_case (recommandé pour les API)
{
  "first_name": "Jean",
  "last_name": "Dupont",
  "birth_date": "1990-05-15"
}

// camelCase (JavaScript/Java)
{
  "firstName": "Jean",
  "lastName": "Dupont",
  "birthDate": "1990-05-15"
}

// PascalCase (moins courant)
{
  "FirstName": "Jean",
  "LastName": "Dupont",
  "BirthDate": "1990-05-15"
}
```

**Choisissez une convention et respectez-la partout !**

### 2. Organisation Logique

Groupez les propriétés liées :

```json
✅ BIEN ORGANISÉ
{
  "user": {
    "id": 42,
    "name": "Jean Dupont",
    "contact": {
      "email": "jean@example.com",
      "phone": "0612345678"
    },
    "address": {
      "street": "123 rue de la Paix",
      "city": "Paris"
    }
  }
}
```

### 3. Gestion des Valeurs Optionnelles

Deux approches :

**Option 1 : Omettre la clé**
```json
{
  "nom": "Dupont",
  "prenom": "Jean"
  // pas de "telephone"
}
```

**Option 2 : Utiliser null**
```json
{
  "nom": "Dupont",
  "prenom": "Jean",
  "telephone": null
}
```

Les deux sont valides, mais l'option 1 est plus légère.

### 4. Versioning des Structures

Incluez une version pour gérer l'évolution :

```json
{
  "version": "1.0",
  "data": {
    "nom": "Dupont",
    "prenom": "Jean"
  }
}
```

## Résumé des Points Clés

1. **JSON** = format de données texte, léger et universel
2. **6 types** : String, Number, Boolean, Null, Object, Array
3. **Guillemets doubles obligatoires** pour clés et chaînes
4. **Pas de virgule finale** après le dernier élément
5. **Structures imbriquées** : objets dans tableaux, tableaux dans objets
6. **Dates** : utiliser ISO 8601 ou timestamp Unix
7. **Validation** importante avant utilisation
8. **Conventions de nommage** : choisir et respecter
9. **Lisibilité** : indentation pour développement, minifié pour production

## Ce Qu'il Faut Retenir pour la Suite

Maintenant que vous maîtrisez la syntaxe JSON, vous êtes prêt à :
- **Parser** (analyser) du JSON reçu d'une API avec FreePascal
- **Générer** du JSON pour envoyer des données à une API
- Utiliser l'unité **fpjson** de FreePascal
- Manipuler des structures JSON complexes
- Gérer les erreurs de parsing

Dans la prochaine section, nous verrons comment utiliser **TFPHttpClient** pour faire des requêtes HTTP et manipuler le JSON reçu en réponse avec FreePascal !

⏭️ [Utilisation de TFPHttpClient](/17-communications-reseau-api-rest/05-utilisation-tfphttpclient.md)
