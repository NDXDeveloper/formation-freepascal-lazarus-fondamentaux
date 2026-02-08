🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.2 Structure d'une unité (interface/implementation)

## Vue d'ensemble

Une unité Pascal est composée de plusieurs sections obligatoires et optionnelles. Voici le squelette de base d'une unité :

```pascal
unit NomDeLUnite;

interface
  // Section PUBLIQUE : ce qui est visible de l'extérieur

implementation
  // Section PRIVÉE : le code qui fait fonctionner l'unité

end.
```

## Les trois sections principales

### 1. L'en-tête : `unit`

```pascal
unit MathsSimples;
```

C'est le **nom de l'unité**. Ce nom doit correspondre exactement au nom du fichier (sans l'extension .pas).

**Exemple :** Si votre fichier s'appelle `MathsSimples.pas`, l'unité doit commencer par `unit MathsSimples;`

### 2. La section `interface`

La section `interface` est comme la **vitrine d'un magasin** : elle montre ce qui est disponible, mais ne montre pas comment c'est fabriqué.

```pascal
interface

uses
  SysUtils;  // Unités dont CETTE unité a besoin

type
  // Types de données publics

const
  // Constantes publiques

var
  // Variables publiques (à éviter généralement)

// Déclarations des fonctions et procédures publiques
function Additionner(a, b: Integer): Integer;
function Multiplier(a, b: Integer): Integer;
```

**Règle importante :** Tout ce qui est dans la section `interface` est **visible et utilisable** par les programmes qui utilisent cette unité.

### 3. La section `implementation`

La section `implementation` est comme **l'arrière-boutique** : c'est là que le vrai travail se fait, mais le client ne le voit pas.

```pascal
implementation

// Le CODE RÉEL des fonctions déclarées dans interface
function Additionner(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Multiplier(a, b: Integer): Integer;
begin
  Result := a * b;
end;

// On peut aussi avoir des fonctions privées (non déclarées dans interface)
function FonctionPrivee: String;
begin
  Result := 'Invisible de l''extérieur';
end;

end.
```

## Exemple complet d'une unité simple

Créons une unité pour gérer des opérations sur les cercles :

```pascal
{$mode objfpc}{$H+}
unit UniteCercles;

interface

const
  PI_APPROX = 3.14159;

// Déclarations publiques
function CalculerPerimetre(rayon: Real): Real;
function CalculerSurface(rayon: Real): Real;
function EstRayonValide(rayon: Real): Boolean;

implementation

// Implémentation des fonctions publiques
function CalculerPerimetre(rayon: Real): Real;
begin
  Result := 2 * Pi * rayon;
end;

function CalculerSurface(rayon: Real): Real;
begin
  Result := Pi * rayon * rayon;
end;

function EstRayonValide(rayon: Real): Boolean;
begin
  Result := rayon > 0;
end;

// Fonction privée (non déclarée dans interface)
function MessageErreur: String;
begin
  Result := 'Le rayon doit être positif !';
end;

end.
```

## Utilisation de cette unité dans un programme

```pascal
program TestCercles;

uses
  UniteCercles;

var
  r, perimetre, surface: Real;

begin
  WriteLn('Entrez le rayon du cercle :');
  ReadLn(r);

  if EstRayonValide(r) then
  begin
    perimetre := CalculerPerimetre(r);
    surface := CalculerSurface(r);

    WriteLn('Périmètre : ', perimetre:0:2);
    WriteLn('Surface : ', surface:0:2);
  end
  else
    WriteLn('Rayon invalide !');
end.
```

## Analogie : Le restaurant

Imaginez une unité comme un **restaurant** :

| Section | Restaurant | Unité Pascal |
|---------|-----------|--------------|
| **interface** | Le menu affiché | Les fonctions que vous pouvez utiliser |
| **implementation** | La cuisine | Le code qui fait le travail |
| **Fonctions publiques** | Plats au menu | Fonctions déclarées dans interface |
| **Fonctions privées** | Recettes secrètes du chef | Fonctions uniquement dans implementation |

Le client (votre programme) voit le menu (interface) et commande un plat (appelle une fonction), mais il ne voit pas comment le chef le prépare (implementation).

## Pourquoi cette séparation ?

### 1. **Encapsulation**
Vous cachez les détails complexes et n'exposez que ce qui est nécessaire.

### 2. **Simplicité d'utilisation**
L'utilisateur de l'unité voit uniquement les fonctions disponibles, pas tout le code interne.

### 3. **Maintenance**
Vous pouvez changer l'implémentation sans affecter les programmes qui utilisent l'unité, tant que l'interface reste la même.

### 4. **Organisation**
Séparer "quoi" (interface) et "comment" (implementation) rend le code plus clair.

## Règles importantes à retenir

1. **Déclaration vs Implémentation**
   - Dans `interface` : on déclare juste l'en-tête de la fonction
   - Dans `implementation` : on écrit le code complet

2. **Cohérence des signatures**
   - L'en-tête dans `implementation` doit correspondre exactement à celui de `interface`

3. **Fonctions privées**
   - Si une fonction n'est déclarée que dans `implementation`, elle est invisible de l'extérieur
   - Utile pour les fonctions auxiliaires internes

4. **Le point final**
   - N'oubliez jamais le `end.` (avec un point) à la fin de l'unité

## Structure complète avec toutes les sections possibles

```pascal
unit MonUniteComplete;

{$mode objfpc}{$H+}  // Directives de compilation (optionnel)

interface

uses
  Classes, SysUtils;  // Unités nécessaires

const
  MA_CONSTANTE = 100;

type
  TMonType = Integer;

  TMonRecord = record
    Nom: String;
    Age: Integer;
  end;

var
  VariableGlobale: Integer;  // À éviter généralement

// Déclarations de fonctions et procédures
procedure MaProcedure(parametre: String);
function MaFonction(x: Integer): Integer;

implementation

uses
  Math;  // Unités nécessaires uniquement pour l'implementation

// Implémentation des fonctions publiques
procedure MaProcedure(parametre: String);
begin
  WriteLn(parametre);
end;

function MaFonction(x: Integer): Integer;
begin
  Result := x * 2;
end;

// Fonctions privées
function FonctionInterne: Boolean;
begin
  Result := True;
end;

end.
```

## Résumé

- Une unité Pascal a **deux sections principales** : `interface` et `implementation`
- **interface** = Ce qui est visible et utilisable de l'extérieur (la vitrine)
- **implementation** = Le code réel qui fait fonctionner l'unité (l'arrière-boutique)
- Les fonctions doivent être **déclarées** dans `interface` et **implémentées** dans `implementation`
- Les fonctions uniquement dans `implementation` sont **privées**
- Cette structure permet l'**encapsulation** et facilite la **maintenance**

Dans la prochaine section, nous verrons comment utiliser plusieurs unités ensemble avec la clause `uses`.

⏭️ [Clauses Uses et dépendances](/07-unites-organisation-code/03-clauses-uses-dependances.md)
