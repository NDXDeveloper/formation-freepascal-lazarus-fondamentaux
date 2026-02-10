🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 7.3 Clauses Uses et dépendances

## Qu'est-ce que la clause `uses` ?

La clause `uses` est comme une **liste de courses** : elle indique à Pascal quelles unités vous voulez utiliser dans votre programme ou dans votre unité.

```pascal
program MonProgramme;

uses
  SysUtils,    // J'ai besoin de cette unité
  Math,        // Et de celle-ci aussi
  Classes;     // Et de celle-là également

begin
  // Maintenant je peux utiliser tout ce qui est dans ces unités
end.
```

## Où placer la clause `uses` ?

### 1. Dans un programme principal

```pascal
program CalculMoyenne;

uses
  Math, SysUtils;  // Juste après l'en-tête du programme

var
  notes: array[1..3] of Real;
  moyenne: Real;

begin
  // Votre code ici
end.
```

La clause `uses` doit être placée **juste après** l'en-tête du programme, **avant** toute déclaration de variables.

### 2. Dans une unité : deux emplacements possibles

Dans une unité, vous pouvez placer `uses` à **deux endroits** différents selon vos besoins :

```pascal
unit MathsAvancees;

interface

uses
  Math, SysUtils;  // Uses dans l'INTERFACE

type
  TCalculatrice = class
    // ...
  end;

implementation

uses
  DateUtils, StrUtils;  // Uses dans l'IMPLEMENTATION

// Code de l'unité

end.
```

### Différence entre les deux emplacements

| Emplacement | Visibilité | Usage |
|-------------|------------|-------|
| **uses dans interface** | Public - visible par ceux qui utilisent votre unité | Pour les types et fonctions utilisés dans la partie publique |
| **uses dans implementation** | Privé - invisible de l'extérieur | Pour les unités nécessaires uniquement au code interne |

## Exemple concret de la différence

```pascal
unit GestionEleves;

interface

uses
  Classes;  // Nécessaire car TStringList est utilisé dans l'interface

type
  TGestionnaireEleves = class
    procedure AjouterEleve(nom: String);
    function ObtenirListe: TStringList;  // TStringList vient de Classes
  end;

implementation

uses
  SysUtils, DateUtils;  // Utilisées seulement dans le code interne

procedure TGestionnaireEleves.AjouterEleve(nom: String);  
var  
  date: TDateTime;
begin
  date := Now;  // Now vient de SysUtils
  // ...
end;

function TGestionnaireEleves.ObtenirListe: TStringList;  
begin  
  Result := TStringList.Create;
end;

end.
```

## Analogie : La construction d'une maison

Imaginez que vous construisez une maison :

- **uses dans interface** = Les matériaux visibles de l'extérieur (briques de façade, fenêtres)
  - Si quelqu'un regarde votre maison, il voit ces éléments
  - Si quelqu'un veut construire une maison similaire, il doit aussi avoir ces matériaux

- **uses dans implementation** = Les matériaux cachés dans les murs (tuyauterie, électricité)
  - Nécessaires pour que la maison fonctionne
  - Mais invisibles de l'extérieur

## Ordre des unités dans la clause `uses`

L'ordre dans lequel vous listez les unités peut être important, surtout s'il y a des **conflits de noms**.

```pascal
uses
  Unit1,    // Si Unit1 et Unit2 ont une fonction avec le même nom
  Unit2;    // C'est celle de Unit2 qui sera utilisée (la dernière)
```

**Règle :** En cas de conflit, c'est la **dernière unité** listée qui a la priorité.

### Exemple de conflit

```pascal
unit UniteA;  
interface  
  procedure Afficher;
implementation
  procedure Afficher;
  begin
    WriteLn('Version A');
  end;
end.
```

```pascal
unit UniteB;  
interface  
  procedure Afficher;
implementation
  procedure Afficher;
  begin
    WriteLn('Version B');
  end;
end.
```

```pascal
program TestOrdre;

uses
  UniteA, UniteB;  // UniteB est en dernier

begin
  Afficher;           // Affichera "Version B" (dernière unité)
  UniteA.Afficher;    // Affichera "Version A" (appel explicite)
end.
```

## Les dépendances entre unités

Une **dépendance** signifie qu'une unité a besoin d'une autre pour fonctionner.

### Chaîne de dépendances

```
Programme Principal
    ↓ (utilise)
UniteGraphique
    ↓ (utilise)
UniteCalculs
    ↓ (utilise)
UniteMaths
```

Le compilateur résout automatiquement toute la chaîne de dépendances et compile les unités dans le bon ordre.

### Exemple pratique

```pascal
// UniteMaths.pas
unit UniteMaths;  
interface  
  function Carre(x: Integer): Integer;
implementation
  function Carre(x: Integer): Integer;
  begin
    Result := x * x;
  end;
end.
```

```pascal
// UniteCalculs.pas
unit UniteCalculs;  
interface  
uses  
  UniteMaths;  // Dépendance

function SommesCarres(a, b: Integer): Integer;

implementation  
function SommesCarres(a, b: Integer): Integer;  
begin  
  Result := Carre(a) + Carre(b);  // Utilise Carre de UniteMaths
end;  
end.  
```

```pascal
// Programme principal
program MonProgramme;

uses
  UniteCalculs;  // Le compilateur sait qu'il doit aussi compiler UniteMaths

begin
  WriteLn(SommesCarres(3, 4));  // 3² + 4² = 9 + 16 = 25

  // WriteLn(Carre(5));  // ERREUR ! Carre n'est pas directement accessible
  // Pour utiliser Carre directement, il faudrait ajouter UniteMaths dans uses
end.
```

> **Attention :** Utiliser `UniteCalculs` ne rend **pas** les identifiants de `UniteMaths` accessibles dans votre programme. Chaque unité doit être explicitement listée dans la clause `uses` si vous voulez utiliser ses fonctions directement.

## Le danger des dépendances circulaires

Une **dépendance circulaire** se produit quand deux unités s'utilisent mutuellement dans leur section `interface`. C'est **interdit** en Pascal !

### ❌ Exemple incorrect (ne compile pas)

```pascal
// UniteA.pas
unit UniteA;  
interface  
uses  
  UniteB;  // A a besoin de B

procedure ProcA;  
implementation  
  procedure ProcA;
  begin
    ProcB;
  end;
end.
```

```pascal
// UniteB.pas
unit UniteB;  
interface  
uses  
  UniteA;  // B a besoin de A - ERREUR CIRCULAIRE !

procedure ProcB;  
implementation  
  procedure ProcB;
  begin
    ProcA;
  end;
end.
```

**Erreur de compilation :** "Circular unit reference"

### ✅ Solution : uses dans implementation

```pascal
// UniteA.pas
unit UniteA;  
interface  
procedure ProcA;  
implementation  
uses  
  UniteB;  // Déplacé dans implementation

procedure ProcA;  
begin  
  ProcB;
end;  
end.  
```

```pascal
// UniteB.pas
unit UniteB;  
interface  
procedure ProcB;  
implementation  
uses  
  UniteA;  // Déplacé dans implementation

procedure ProcB;  
begin  
  ProcA;
end;  
end.  
```

Maintenant ça compile ! Les deux unités peuvent s'utiliser mutuellement via leur section `implementation`.

## Bonnes pratiques

### 1. Minimiser les dépendances
N'incluez que les unités dont vous avez **réellement besoin**.

```pascal
// ❌ Mauvais : inclure trop d'unités "au cas où"
uses
  SysUtils, Classes, Math, StrUtils, DateUtils, Forms, Controls, Graphics;

// ✅ Bon : seulement ce qui est nécessaire
uses
  SysUtils, Math;
```

### 2. Privilégier uses dans implementation quand possible

```pascal
unit MonUnite;

interface
// Minimum d'unités ici

implementation

uses
  // La plupart des unités ici
  SysUtils, StrUtils, Math;
```

**Avantage :** Réduit les dépendances visibles et évite les problèmes circulaires.

### 3. Organiser les uses par thème

```pascal
uses
  // Unités système
  SysUtils, Classes,

  // Unités interface graphique
  Forms, Controls, StdCtrls,

  // Mes unités personnelles
  MesOutils, MaBase;
```

### 4. Éviter les uses globaux inutiles

Si une unité n'est utilisée que dans une seule procédure, réfléchissez à votre architecture.

## Visualisation des dépendances

Lazarus IDE vous permet de visualiser les dépendances de votre projet :
- Menu **Projet** → **Inspecteur de projet**
- Affiche la liste des unités et leurs relations

C'est très utile pour comprendre la structure de votre programme !

## Résumé

- La clause **uses** déclare les unités dont vous avez besoin
- Elle se place après l'en-tête du programme ou de l'unité
- Dans une unité, on peut avoir **deux uses** : un dans `interface` et un dans `implementation`
- **uses interface** = dépendances publiques et visibles
- **uses implementation** = dépendances privées et cachées
- L'**ordre** des unités peut être important en cas de conflit de noms
- Les **dépendances circulaires** dans interface sont interdites
- **Bonne pratique** : minimiser les dépendances et privilégier uses dans implementation

Maintenant que vous comprenez les dépendances, nous allons voir dans la section suivante comment Pascal gère l'ordre de compilation de toutes ces unités.

⏭️ [Ordre de compilation](/07-unites-organisation-code/04-ordre-compilation.md)
