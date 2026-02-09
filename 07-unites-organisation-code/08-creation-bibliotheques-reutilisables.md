🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.8 Création de bibliothèques réutilisables

## Qu'est-ce qu'une bibliothèque réutilisable ?

Une **bibliothèque réutilisable** est une collection d'unités bien conçues qui peuvent être utilisées dans **plusieurs projets différents** sans modification.

### Analogie : La boîte à outils professionnelle

Imaginez deux artisans :

**Artisan amateur :**
- Fabrique un outil spécifique pour chaque projet
- Recommence à zéro à chaque fois
- Perd du temps

**Artisan professionnel :**
- Investit dans des outils de qualité
- Les range dans une boîte à outils bien organisée
- Les réutilise pour tous ses projets
- Gagne du temps et en efficacité

Créer une bibliothèque réutilisable, c'est être l'artisan professionnel du code !

## Caractéristiques d'une bonne bibliothèque

Une bibliothèque réutilisable doit être :

| Caractéristique | Description |
|-----------------|-------------|
| **Autonome** | Fonctionne indépendamment du projet |
| **Générique** | Utilisable dans différents contextes |
| **Documentée** | Facile à comprendre et à utiliser |
| **Testée** | Fiable et sans bugs |
| **Stable** | L'interface ne change pas fréquemment |
| **Simple** | Facile à intégrer et à utiliser |

## Différence entre code spécifique et bibliothèque

### Code spécifique à un projet

```pascal
// UniteCommandes.pas - Spécifique au projet "Gestion Restaurant"
unit UniteCommandes;

interface

procedure EnregistrerCommandeRestaurant(numeroTable: Integer; plats: String);
function CalculerTotalRestaurant(numeroTable: Integer): Real;

implementation

// Code qui dépend fortement de la base de données du restaurant
// Code qui utilise des constantes spécifiques
// Difficile à réutiliser ailleurs

end.
```

### Bibliothèque réutilisable

```pascal
// UniteGestionCommandes.pas - Bibliothèque générique
unit UniteGestionCommandes;

interface

type
  TCommande = record
    ID: Integer;
    Client: String;
    MontantTotal: Real;
    DateCreation: TDateTime;
  end;

procedure AjouterCommande(var commande: TCommande);
function CalculerTotal(const commande: TCommande): Real;
function RechercherCommande(ID: Integer): TCommande;

implementation

// Code générique qui ne dépend pas d'un contexte spécifique
// Peut être utilisé pour un restaurant, une boutique, etc.

end.
```

**Différence clé :** La bibliothèque ne fait **aucune hypothèse** sur le contexte d'utilisation.

## Étapes pour créer une bibliothèque

### 1. Identifier le besoin récurrent

Repérez le code que vous écrivez souvent dans différents projets.

**Exemples de besoins récurrents :**
- Validation d'adresses email
- Conversion de formats de dates
- Calculs mathématiques spécifiques
- Gestion de fichiers de configuration
- Formatage de données

### 2. Extraire et généraliser

Prenez votre code spécifique et rendez-le générique.

**Avant (spécifique) :**
```pascal
function ValiderEmailUtilisateur: Boolean;
begin
  Result := Pos('@', EmailUtilisateur) > 0;
end;
```

**Après (générique) :**
```pascal
function EstEmailValide(email: String): Boolean;
begin
  Result := (Pos('@', email) > 0) and (Pos('.', email) > 0);
end;
```

### 3. Organiser en unités thématiques

Regroupez les fonctions par thème dans des unités séparées.

```
UniteValidation.pas    → Fonctions de validation
UniteConversion.pas    → Fonctions de conversion
UniteFormatage.pas     → Fonctions de formatage
UniteFichiers.pas      → Gestion de fichiers
```

### 4. Définir une interface claire

L'interface doit être intuitive et facile à utiliser.

### 5. Implémenter de manière robuste

Gérez tous les cas d'erreur possibles.

### 6. Tester exhaustivement

Testez avec différentes valeurs, y compris les cas limites.

### 7. Documenter complètement

Expliquez comment utiliser chaque fonction.

## Exemple complet : Bibliothèque de validation

Créons une bibliothèque pour valider différents types de données.

```pascal
{*******************************************************************************
  UniteValidation - Bibliotheque de validation de donnees

  Cette unite fournit des fonctions pour valider differents formats :
  - Adresses email
  - Numeros de telephone francais
  - Codes postaux francais
  - Dates

  Auteur  : Formation FreePascal
  Version : 1.0
*******************************************************************************}

unit UniteValidation;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils;

{
  Vérifie si une chaîne est une adresse email valide.

  @param(email L'adresse email à vérifier)
  @returns(True si l'email est valide, False sinon)

  Exemple :
    if EstEmailValide('user@example.com') then
      WriteLn('Email valide');
}
function EstEmailValide(const email: String): Boolean;

{
  Vérifie si une chaîne est un numéro de téléphone français valide.
  Formats acceptés : 0601020304, 06 01 02 03 04, 06.01.02.03.04

  @param(telephone Le numéro à vérifier)
  @returns(True si le numéro est valide, False sinon)
}
function EstTelephoneValide(const telephone: String): Boolean;

{
  Vérifie si une chaîne est un code postal français valide (5 chiffres).

  @param(codePostal Le code postal à vérifier)
  @returns(True si le code postal est valide, False sinon)
}
function EstCodePostalValide(const codePostal: String): Boolean;

{
  Vérifie si une date est comprise dans un intervalle.

  @param(date La date à vérifier)
  @param(dateMin Date minimale)
  @param(dateMax Date maximale)
  @returns(True si la date est dans l'intervalle, False sinon)
}
function EstDateDansIntervalle(date, dateMin, dateMax: TDateTime): Boolean;

{
  Vérifie si une chaîne n'est ni vide ni composée uniquement d'espaces.

  @param(texte La chaîne à vérifier)
  @returns(True si la chaîne contient du texte, False sinon)
}
function EstNonVide(const texte: String): Boolean;

implementation

function EstEmailValide(const email: String): Boolean;
var
  posArobase, posPoint: Integer;
begin
  Result := False;

  // Verifications de base
  if Length(email) < 5 then
    Exit;  // Trop court (a@b.c minimum)

  // Rechercher @
  posArobase := Pos('@', email);
  if posArobase <= 1 then
    Exit;  // @ absent ou au debut

  // Rechercher . apres @
  posPoint := PosEx('.', email, posArobase);
  if posPoint <= posArobase + 1 then
    Exit;  // . absent ou juste apres @

  // Verifier qu'il y a quelque chose apres le dernier point
  if posPoint >= Length(email) then
    Exit;

  Result := True;
end;

function EstTelephoneValide(const telephone: String): Boolean;
var
  numeros: String;
  i: Integer;
begin
  Result := False;
  numeros := '';

  // Extraire seulement les chiffres
  for i := 1 to Length(telephone) do
  begin
    if telephone[i] in ['0'..'9'] then
      numeros := numeros + telephone[i];
  end;

  // Verifier : 10 chiffres, commence par 0
  if (Length(numeros) = 10) and (numeros[1] = '0') then
    Result := True;
end;

function EstCodePostalValide(const codePostal: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Doit faire exactement 5 caracteres
  if Length(codePostal) <> 5 then
    Exit;

  // Tous les caracteres doivent etre des chiffres
  for i := 1 to 5 do
  begin
    if not (codePostal[i] in ['0'..'9']) then
      Exit;
  end;

  Result := True;
end;

function EstDateDansIntervalle(date, dateMin, dateMax: TDateTime): Boolean;
begin
  Result := (date >= dateMin) and (date <= dateMax);
end;

function EstNonVide(const texte: String): Boolean;
begin
  Result := Length(Trim(texte)) > 0;
end;

end.
```

## Utilisation de la bibliothèque

```pascal
{$mode objfpc}{$H+}
program TestValidation;

uses
  SysUtils, UniteValidation;

var
  email, tel, cp: String;

begin
  WriteLn('=== Test de la bibliothèque de validation ===');
  WriteLn;

  // Test email
  email := 'jean.dupont@example.com';
  if EstEmailValide(email) then
    WriteLn('V ', email, ' est valide')
  else
    WriteLn('X ', email, ' est invalide');

  // Test telephone
  tel := '06 01 02 03 04';
  if EstTelephoneValide(tel) then
    WriteLn('V ', tel, ' est valide')
  else
    WriteLn('X ', tel, ' est invalide');

  // Test code postal
  cp := '75001';
  if EstCodePostalValide(cp) then
    WriteLn('V ', cp, ' est valide')
  else
    WriteLn('X ', cp, ' est invalide');
end.
```

## Rendre une bibliothèque flexible

### Utiliser des paramètres optionnels

```pascal
{
  Formate un nombre avec options personnalisables.

  @param(nombre Le nombre à formater)
  @param(decimales Nombre de décimales (défaut: 2))
  @param(separateurMilliers Si true, ajoute des espaces (défaut: false))
}
function FormaterNombre(nombre: Real; decimales: Integer = 2;
                        separateurMilliers: Boolean = False): String;
begin
  if separateurMilliers then
    Result := FormatFloat('#,##0.' + StringOfChar('0', decimales), nombre)
  else
    Result := FormatFloat('0.' + StringOfChar('0', decimales), nombre);
end;
```

**Utilisation :**
```pascal
WriteLn(FormaterNombre(1234.567));              // '1234.57'
WriteLn(FormaterNombre(1234.567, 3));           // '1234.567'
WriteLn(FormaterNombre(1234.567, 2, True));     // '1 234.57'
```

### Utiliser des types personnalisés

```pascal
type
  TOptionsValidation = record
    AccepterChampsVides: Boolean;
    LongueurMin: Integer;
    LongueurMax: Integer;
    CasseSensible: Boolean;
  end;

function ValiderTexte(const texte: String;
                      options: TOptionsValidation): Boolean;
begin
  // Validation selon les options...
end;
```

### Utiliser des callbacks (procédures/fonctions en paramètre)

```pascal
type
  TFonctionFiltre = function(const valeur: String): Boolean;

function FiltrerListe(const liste: array of String;
                      filtre: TFonctionFiltre): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := Low(liste) to High(liste) do
  begin
    if filtre(liste[i]) then
      Result.Add(liste[i]);
  end;
end;

// Fonction de filtre personnalisée
function CommenceParA(const valeur: String): Boolean;
begin
  Result := (Length(valeur) > 0) and (UpCase(valeur[1]) = 'A');
end;

// Utilisation
var
  mots: array[1..4] of String = ('Arbre', 'Banane', 'Avocat', 'Cerise');
  resultat: TStringList;
begin
  resultat := FiltrerListe(mots, @CommenceParA);
  // resultat contient : Arbre, Avocat
end;
```

## Gestion des erreurs dans une bibliothèque

### Approche 1 : Valeurs de retour

```pascal
function ChargerFichier(nomFichier: String; var contenu: String): Boolean;
begin
  Result := False;

  if not FileExists(nomFichier) then
    Exit;

  try
    // Charger le fichier
    contenu := '...';
    Result := True;
  except
    // En cas d'erreur
    contenu := '';
  end;
end;

// Utilisation
var
  texte: String;
begin
  if ChargerFichier('data.txt', texte) then
    WriteLn('Chargé : ', texte)
  else
    WriteLn('Erreur de chargement');
end;
```

### Approche 2 : Exceptions personnalisées

```pascal
type
  EValidationError = class(Exception);

function ValiderEmail(email: String): String;
begin
  if not EstEmailValide(email) then
    raise EValidationError.Create('Email invalide : ' + email);

  Result := LowerCase(email);
end;

// Utilisation
try
  email := ValiderEmail('utilisateur@example.com');
except
  on E: EValidationError do
    WriteLn('Erreur : ', E.Message);
end;
```

### Approche 3 : Codes d'erreur

```pascal
const
  ERR_OK = 0;
  ERR_FICHIER_INTROUVABLE = 1;
  ERR_ACCES_REFUSE = 2;
  ERR_FORMAT_INVALIDE = 3;

function ChargerConfiguration(nomFichier: String; var config: TConfig): Integer;
begin
  if not FileExists(nomFichier) then
    Exit(ERR_FICHIER_INTROUVABLE);

  // Charger...

  Result := ERR_OK;
end;

// Utilisation
var
  config: TConfig;
  code: Integer;
begin
  code := ChargerConfiguration('config.ini', config);
  case code of
    ERR_OK: WriteLn('Configuration chargée');
    ERR_FICHIER_INTROUVABLE: WriteLn('Fichier introuvable');
    ERR_ACCES_REFUSE: WriteLn('Accès refusé');
  end;
end;
```

## Structure d'un projet de bibliothèque

### Organisation des fichiers

```
MaBibliotheque/
├── src/                    # Code source
│   ├── UniteValidation.pas
│   ├── UniteConversion.pas
│   └── UniteFormatage.pas
├── examples/               # Exemples d'utilisation
│   ├── exemple1.pas
│   └── exemple2.pas
├── tests/                  # Tests unitaires
│   ├── test_validation.pas
│   └── test_conversion.pas
├── docs/                   # Documentation
│   ├── guide_utilisation.md
│   └── reference_api.html
├── README.md               # Description du projet
├── LICENSE.txt             # Licence
└── CHANGELOG.md            # Historique des versions
```

### Fichier README.md

````markdown
# MaBibliotheque - Validation et Formatage

## Description
Bibliothèque Pascal pour valider et formater différents types de données.

## Installation
Copiez les fichiers .pas dans votre projet et ajoutez :
```pascal
uses UniteValidation, UniteConversion, UniteFormatage;
```

## Exemples d'utilisation

### Validation d'email
```pascal
if EstEmailValide('user@example.com') then
  WriteLn('Email valide');
```

### Formatage de nombre
```pascal
resultat := FormaterNombre(1234.56, 2);  // '1234.56'
```

## Documentation complète
Consultez le dossier `docs/` pour la documentation complète.

## Licence
MIT License - Libre d'utilisation
````

## Versionnement de votre bibliothèque

Utilisez le **versionnement sémantique** : MAJEUR.MINEUR.CORRECTIF

```pascal
const
  VERSION_MAJEURE = 1;    // Changements incompatibles
  VERSION_MINEURE = 2;    // Nouvelles fonctionnalités compatibles
  VERSION_CORRECTIF = 3;  // Corrections de bugs

  VERSION_COMPLETE = '1.2.3';
```

**Exemples :**
- `1.0.0` → Version initiale
- `1.0.1` → Correction d'un bug
- `1.1.0` → Ajout d'une nouvelle fonction
- `2.0.0` → Changement majeur incompatible

## Bonnes pratiques

### 1. Ne pas utiliser de variables globales

```pascal
// ❌ Mauvais
var
  ConfigGlobale: TConfig;

// ✅ Bon - Passer en paramètre
function Traiter(config: TConfig): Boolean;
```

### 2. Préfixer les noms pour éviter les conflits

```pascal
// Préfixer les types avec T, E, I
type
  TValidateur = class
  EValidationError = class(Exception)
  IFormateur = interface

// Préfixer les constantes
const
  VAL_MAX_LENGTH = 100;
  VAL_MIN_LENGTH = 5;
```

### 3. Fournir des valeurs par défaut

```pascal
// Avec valeurs par défaut
function FormaterDate(date: TDateTime;
                      format: String = 'dd/mm/yyyy'): String;
```

### 4. Documenter les dépendances

```pascal
{
  DÉPENDANCES :
    - SysUtils (standard)
    - StrUtils (standard)
    - Pas de dépendance externe
}
```

### 5. Tester sur différentes plateformes

Si votre bibliothèque doit être multi-plateforme, testez sur Windows et Linux.

```pascal
{$IFDEF WINDOWS}
  // Code spécifique Windows
{$ENDIF}

{$IFDEF LINUX}
  // Code spécifique Linux
{$ENDIF}
```

## Distribuer votre bibliothèque

### Option 1 : Fichiers sources

Fournissez les fichiers .pas que les utilisateurs copient dans leur projet.

### Option 2 : Package Lazarus

Créez un package (.lpk) installable dans Lazarus.

**Menu :** Paquet → Nouveau paquet

### Option 3 : Bibliothèque compilée (.ppu)

Fournissez les fichiers compilés (moins courant, réduit la flexibilité).

### Option 4 : Dépôt Git

Partagez sur GitHub, GitLab, etc.

```bash
git clone https://github.com/utilisateur/mabibliothque.git
```

## Maintenance d'une bibliothèque

### Tenir un CHANGELOG

```markdown
# Changelog

## [1.2.0] - 2025-10-13
### Ajouté
- Fonction EstTelephoneValide
- Support des numéros internationaux

### Modifié
- EstEmailValide plus stricte

### Corrigé
- Bug dans EstCodePostalValide avec codes commençant par 0
```

### Garder la compatibilité

Évitez de casser le code existant des utilisateurs :

```pascal
// ✅ Bon - Ajouter une nouvelle fonction
function EstEmailValideV2(email: String; strict: Boolean): Boolean;

// ❌ Mauvais - Changer la signature d'une fonction existante
// function EstEmailValide(email: String; strict: Boolean): Boolean;
```

### Marquer les fonctions obsolètes

```pascal
{$WARN DEPRECATED ON}

// Ancienne fonction - à ne plus utiliser
function AncienneMethode: Integer; deprecated 'Utilisez NouvelleMethode à la place';

// Nouvelle fonction
function NouvelleMethode: Integer;
```

## Résumé

- Une **bibliothèque réutilisable** est générique et autonome
- Identifiez les **besoins récurrents** dans vos projets
- **Généralisez** votre code pour le rendre réutilisable
- Organisez en **unités thématiques** cohérentes
- Définissez une **interface claire** et intuitive
- **Documentez** complètement votre bibliothèque
- **Testez** exhaustivement sur différents cas
- Utilisez le **versionnement sémantique**
- Facilitez la **distribution** et l'intégration
- Maintenez la **compatibilité** entre versions
- Évitez les **variables globales** et **dépendances cachées**

Créer des bibliothèques réutilisables demande un effort initial, mais vous fera gagner énormément de temps sur le long terme !

Vous avez maintenant toutes les connaissances pour créer des bibliothèques professionnelles et réutilisables en Pascal.

⏭️ [Documentation des unités](/07-unites-organisation-code/09-documentation-unites.md)
