🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.5 Variables et procédures publiques/privées

## Qu'est-ce que la visibilité ?

La **visibilité** détermine qui peut voir et utiliser vos variables et procédures. C'est comme la différence entre :
- **Public** : Ce que vous montrez à tout le monde (comme votre nom sur une carte de visite)
- **Privé** : Ce que vous gardez pour vous (comme votre journal intime)

En Pascal, la section dans laquelle vous déclarez une variable ou une procédure détermine sa visibilité.

## Les deux niveaux de visibilité

| Déclaration | Visibilité | Accessible de l'extérieur ? |
|-------------|------------|---------------------------|
| Dans **interface** | **Publique** | ✅ Oui |
| Dans **implementation** uniquement | **Privée** | ❌ Non |

## Procédures et fonctions publiques

Une procédure ou fonction est **publique** si elle est déclarée dans la section `interface`.

### Exemple d'unité avec fonctions publiques

```pascal
unit UniteMaths;

interface

// Fonctions PUBLIQUES - déclarées dans interface
function Addition(a, b: Integer): Integer;
function Soustraction(a, b: Integer): Integer;

implementation

// Implémentation des fonctions publiques
function Addition(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Soustraction(a, b: Integer): Integer;
begin
  Result := a - b;
end;

end.
```

**Utilisation dans un programme :**
```pascal
program Test;

uses
  UniteMaths;

begin
  WriteLn(Addition(5, 3));        // ✅ Fonctionne - Addition est publique
  WriteLn(Soustraction(10, 4));   // ✅ Fonctionne - Soustraction est publique
end.
```

## Procédures et fonctions privées

Une procédure ou fonction est **privée** si elle est déclarée **uniquement** dans la section `implementation`.

### Exemple avec fonctions privées

```pascal
unit UniteCalculs;

interface

// Fonction PUBLIQUE
function CalculerMoyenne(notes: array of Integer): Real;

implementation

// Fonction PRIVÉE - pas déclarée dans interface
function SommeTableau(notes: array of Integer): Integer;
var
  i, somme: Integer;
begin
  somme := 0;
  for i := Low(notes) to High(notes) do
    somme := somme + notes[i];
  Result := somme;
end;

// Implémentation de la fonction publique
function CalculerMoyenne(notes: array of Integer): Real;
var
  somme: Integer;
begin
  somme := SommeTableau(notes);  // ✅ Accessible ici (même unité)
  Result := somme / Length(notes);
end;

end.
```

**Utilisation dans un programme :**
```pascal
program Test;

uses
  UniteCalculs;

var
  mesNotes: array[1..3] of Integer = (15, 12, 18);

begin
  WriteLn(CalculerMoyenne(mesNotes));  // ✅ Fonctionne

  // WriteLn(SommeTableau(mesNotes));  // ❌ ERREUR - SommeTableau est privée !
end.
```

## Analogie : Le restaurant

Imaginez une unité comme un **restaurant** :

| Élément | Restaurant | Unité Pascal |
|---------|-----------|--------------|
| **Menu** (interface) | Plats que les clients peuvent commander | Fonctions publiques |
| **Cuisine** (implementation) | Recettes et techniques secrètes du chef | Fonctions privées |
| **Service** | Le serveur prend les commandes | Le programme appelle les fonctions publiques |

Le client (votre programme) peut commander un plat (fonction publique), mais il ne voit pas la recette secrète (fonction privée) utilisée par le chef pour le préparer.

## Variables publiques

Les variables déclarées dans la section `interface` sont **publiques** et accessibles de l'extérieur.

```pascal
unit UniteConfig;

interface

var
  NomApplication: String = 'Mon Super Programme';  // Variable PUBLIQUE
  VersionMajeure: Integer = 1;                     // Variable PUBLIQUE
  VersionMineure: Integer = 0;                     // Variable PUBLIQUE

procedure AfficherVersion;

implementation

procedure AfficherVersion;
begin
  WriteLn(NomApplication, ' v', VersionMajeure, '.', VersionMineure);
end;

end.
```

**Utilisation :**
```pascal
program Test;

uses
  UniteConfig;

begin
  WriteLn('Nom : ', NomApplication);    // ✅ Accessible
  VersionMajeure := 2;                  // ✅ Modifiable de l'extérieur !
  AfficherVersion;                      // Affiche "Mon Super Programme v2.0"
end.
```

### ⚠️ Attention : Danger des variables publiques !

Permettre l'accès direct à une variable publique peut être **dangereux** :

```pascal
program Danger;

uses
  UniteConfig;

begin
  VersionMajeure := -5;        // ❌ Valeur invalide mais acceptée !
  NomApplication := '';        // ❌ Nom vide mais accepté !
end.
```

**Problème :** N'importe quel programme peut mettre n'importe quelle valeur, même invalide !

## Variables privées

Les variables déclarées dans la section `implementation` sont **privées**.

```pascal
{$mode objfpc}{$H+}
unit UniteCompteur;

interface

procedure Incrementer;
procedure Reinitialiser;
function ObtenirValeur: Integer;

implementation

var
  compteur: Integer = 0;  // Variable PRIVÉE - dans implementation

procedure Incrementer;
begin
  compteur := compteur + 1;  // ✅ Accessible ici
end;

procedure Reinitialiser;
begin
  compteur := 0;
end;

function ObtenirValeur: Integer;
begin
  Result := compteur;
end;

end.
```

**Utilisation :**
```pascal
program TestCompteur;

uses
  UniteCompteur;

begin
  Incrementer;
  Incrementer;
  WriteLn('Compteur : ', ObtenirValeur);  // Affiche 2

  // compteur := 100;  // ❌ ERREUR - compteur est privée !
end.
```

**Avantage :** La valeur de `compteur` est **protégée**. On ne peut la modifier que via les procédures prévues à cet effet.

## Encapsulation : Un concept fondamental

L'**encapsulation** consiste à cacher les détails internes et n'exposer que ce qui est nécessaire.

### Mauvais exemple (sans encapsulation)

```pascal
unit CompteEnBanque;

interface

var
  Solde: Real;  // ❌ Variable publique

implementation
end.
```

```pascal
program Pirate;

uses
  CompteEnBanque;

begin
  Solde := Solde + 1000000;  // ❌ On peut tricher !
end.
```

### Bon exemple (avec encapsulation)

```pascal
{$mode objfpc}{$H+}
unit CompteEnBanque;

interface

// Fonctions publiques pour interagir avec le compte
procedure Deposer(montant: Real);
procedure Retirer(montant: Real);
function ObtenirSolde: Real;

implementation

var
  Solde: Real = 0;  // ✅ Variable privée

procedure Deposer(montant: Real);
begin
  if montant > 0 then
    Solde := Solde + montant
  else
    WriteLn('Erreur : montant invalide');
end;

procedure Retirer(montant: Real);
begin
  if (montant > 0) and (montant <= Solde) then
    Solde := Solde - montant
  else
    WriteLn('Erreur : retrait impossible');
end;

function ObtenirSolde: Real;
begin
  Result := Solde;
end;

end.
```

```pascal
program ClientBanque;

uses
  CompteEnBanque;

begin
  Deposer(1000);
  Retirer(200);
  WriteLn('Solde : ', ObtenirSolde:0:2);  // Affiche 800.00

  // Solde := 1000000;  // ❌ IMPOSSIBLE - Solde est privée
  Retirer(1000);       // Affiche "Erreur : retrait impossible"
end.
```

**Avantages :**
- ✅ La variable `Solde` est protégée
- ✅ Les validations sont garanties
- ✅ On ne peut pas mettre de valeur invalide

## Bonnes pratiques

### 1. Privilégier les fonctions publiques aux variables publiques

```pascal
// ❌ À éviter
interface
var
  Compteur: Integer;

// ✅ Préférable
interface
function ObtenirCompteur: Integer;
procedure DefinirCompteur(valeur: Integer);
```

### 2. Garder les détails d'implémentation privés

```pascal
unit GestionFichier;

interface

procedure SauvegarderDonnees(donnees: String);
function ChargerDonnees: String;

implementation

var
  CheminFichier: String = 'data.txt';  // ✅ Privé - détail d'implémentation

// Fonction privée utilitaire
function VerifierExistenceFichier: Boolean;
begin
  // Code de vérification
  Result := True;
end;

procedure SauvegarderDonnees(donnees: String);
begin
  if VerifierExistenceFichier then
    // Sauvegarde
end;

function ChargerDonnees: String;
begin
  if VerifierExistenceFichier then
    // Chargement
end;

end.
```

Les utilisateurs de l'unité n'ont pas besoin de savoir :
- Où le fichier est stocké
- Comment la vérification est faite

Ils utilisent simplement `SauvegarderDonnees` et `ChargerDonnees`.

### 3. Utiliser des constantes publiques au lieu de variables

```pascal
interface

const
  VERSION_MAJEURE = 1;      // ✅ Constante - ne peut pas être modifiée
  VERSION_MINEURE = 0;      // ✅ Constante - ne peut pas être modifiée
  NOM_APP = 'MonApp';       // ✅ Constante - ne peut pas être modifiée
```

### 4. Découper les grosses fonctions en fonctions privées

```pascal
unit TraitementComplexe;

interface

procedure TraiterDonnees(donnees: String);

implementation

uses
  SysUtils;  // Pour Trim et UpperCase

// Fonctions privées pour découper la logique
function ValiderDonnees(donnees: String): Boolean;
begin
  // Validation
  Result := Length(donnees) > 0;
end;

function NettoyerDonnees(donnees: String): String;
begin
  // Nettoyage
  Result := Trim(donnees);
end;

function TransformerDonnees(donnees: String): String;
begin
  // Transformation
  Result := UpperCase(donnees);
end;

// Fonction publique qui orchestre
procedure TraiterDonnees(donnees: String);
var
  donneesNettoyees, donneesFinales: String;
begin
  if not ValiderDonnees(donnees) then
    Exit;

  donneesNettoyees := NettoyerDonnees(donnees);
  donneesFinales := TransformerDonnees(donneesNettoyees);

  WriteLn('Résultat : ', donneesFinales);
end;

end.
```

**Avantages :**
- Code plus lisible
- Fonctions plus courtes et focalisées
- Détails cachés de l'utilisateur externe

## Récapitulatif visuel

```pascal
unit MonUnite;

interface
  // ════════════════════════════════════
  //        ZONE PUBLIQUE
  // ════════════════════════════════════

  const
    MA_CONSTANTE = 100;  // ✅ Public

  var
    MaVariablePublique: Integer;  // ⚠️ Public (mais à éviter)

  procedure ProcedurePublique;    // ✅ Public
  function FonctionPublique: Integer;  // ✅ Public

implementation
  // ════════════════════════════════════
  //        ZONE PRIVÉE
  // ════════════════════════════════════

  var
    MaVariablePrivee: Integer;  // ✅ Privé - protégé

  // Fonction privée (pas dans interface)
  function FonctionPrivee: Boolean;
  begin
    Result := True;
  end;

  // Implémentation des fonctions publiques
  procedure ProcedurePublique;
  begin
    FonctionPrivee;  // ✅ Accessible ici
  end;

  function FonctionPublique: Integer;
  begin
    Result := MaVariablePrivee;  // ✅ Accessible ici
  end;

end.
```

## Résumé

- **Public** (interface) = Visible et utilisable de l'extérieur
- **Privé** (implementation uniquement) = Invisible de l'extérieur
- **Préférez** les fonctions publiques aux variables publiques
- **L'encapsulation** protège vos données et garantit leur validité
- Les **détails d'implémentation** doivent rester privés
- Utilisez des **constantes** publiques plutôt que des variables quand possible
- Les **fonctions privées** sont utiles pour organiser le code interne

Le principe clé : **montrez le minimum nécessaire, cachez tout le reste** !

Dans la section suivante, nous verrons comment utiliser les sections `initialization` et `finalization` pour initialiser et nettoyer vos unités automatiquement.

⏭️ [Sections initialization et finalization](/07-unites-organisation-code/06-sections-initialization-finalization.md)
