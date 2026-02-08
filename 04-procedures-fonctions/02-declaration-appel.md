🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.2 Déclaration et appel

## Introduction

Maintenant que nous savons qu'il existe des procédures et des fonctions, apprenons comment les **déclarer** (créer) et les **appeler** (utiliser) correctement dans nos programmes Pascal.

## Structure générale d'un programme avec procédures/fonctions

```pascal
program MonProgramme;

// 1. Déclarations des procédures et fonctions
procedure MaProcedure;
begin
  // Code de la procédure
end;

function MaFonction: Integer;
begin
  // Code de la fonction
end;

// 2. Programme principal
begin
  // Appels des procédures et fonctions
  MaProcedure;
  WriteLn(MaFonction);
end.
```

**Règle importante :** Les procédures et fonctions doivent être **déclarées avant** le programme principal (avant le `begin` final).

## Déclaration de procédures

### Syntaxe de base

```pascal
procedure NomDeLaProcedure;
begin
  // Instructions
end;
```

### Exemple simple

```pascal
program ExempleDeclaration;

procedure DireBonjour;
begin
  WriteLn('Bonjour tout le monde !');
  WriteLn('Comment allez-vous ?');
end;

begin
  DireBonjour;  // Appel de la procédure
end.
```

**Résultat à l'écran :**
```
Bonjour tout le monde !
Comment allez-vous ?
```

### Règles de nommage

- Le nom doit commencer par une lettre
- Peut contenir des lettres, chiffres et underscore (_)
- Pas d'espaces ni de caractères spéciaux
- Évitez les mots réservés du Pascal (begin, end, if, etc.)

**Exemples valides :**
```pascal
procedure AfficherMenu;
procedure Calculer_Total;
procedure Etape1;
procedure InitialiserVariables;
```

**Exemples invalides :**
```pascal
procedure 1ereProcedure;   // ❌ Commence par un chiffre
procedure Afficher Menu;    // ❌ Contient un espace
procedure begin;            // ❌ Mot réservé
```

## Déclaration de fonctions

### Syntaxe de base

```pascal
function NomDeLaFonction: TypeDeRetour;
begin
  // Instructions
  Result := valeur;  // ou NomDeLaFonction := valeur;
end;
```

### Exemple simple

```pascal
{$mode objfpc}{$H+}
program ExempleFonction;

function ObtenirAge: Integer;
begin
  Result := 25;  // Result nécessite {$mode objfpc} ; sinon utiliser NomFonction := valeur
end;

var
  age: Integer;
begin
  age := ObtenirAge;  // Appel de la fonction
  WriteLn('L''âge est : ', age);
end.
```

### Types de retour courants

```pascal
function RetourneEntier: Integer;
begin
  Result := 42;
end;

function RetourneReel: Real;
begin
  Result := 3.14;
end;

function RetourneTexte: String;
begin
  Result := 'Bonjour';
end;

function RetourneBooleen: Boolean;
begin
  Result := True;
end;

function RetourneCaractere: Char;
begin
  Result := 'A';
end;
```

## Appel de procédures

Une procédure s'appelle simplement par son nom, suivi d'un point-virgule.

```pascal
program AppelProcedure;

procedure Message1;
begin
  WriteLn('Premier message');
end;

procedure Message2;
begin
  WriteLn('Deuxième message');
end;

begin
  Message1;      // Premier appel
  Message2;      // Deuxième appel
  Message1;      // On peut rappeler la même procédure
end.
```

**Résultat :**
```
Premier message
Deuxième message
Premier message
```

### Appels multiples

On peut appeler une procédure autant de fois qu'on le souhaite :

```pascal
procedure AfficherEtoile;
begin
  Write('* ');
end;

begin
  AfficherEtoile;
  AfficherEtoile;
  AfficherEtoile;
  AfficherEtoile;
  WriteLn;  // Retour à la ligne
end.
```

**Résultat :** `* * * * `

## Appel de fonctions

Une fonction s'appelle dans une **expression** ou une **affectation** car elle retourne une valeur.

### Dans une affectation

```pascal
{$mode objfpc}{$H+}
program AppelFonction;

function ObtenirNombre: Integer;
begin
  Result := 100;
end;

var
  x: Integer;
begin
  x := ObtenirNombre;  // La valeur 100 est affectée à x
  WriteLn('x vaut : ', x);
end.
```

### Dans une expression

```pascal
function ObtenirDix: Integer;
begin
  Result := 10;
end;

var
  total: Integer;
begin
  total := ObtenirDix + 5;        // 10 + 5 = 15
  total := ObtenirDix * 2;        // 10 * 2 = 20
  total := ObtenirDix + ObtenirDix;  // 10 + 10 = 20
end;
```

### Directement dans WriteLn

```pascal
function ObtenirMessage: String;
begin
  Result := 'Ceci est un message';
end;

begin
  WriteLn(ObtenirMessage);  // Affiche directement le résultat
end.
```

### Dans une condition

```pascal
function EstPositif: Boolean;
begin
  Result := True;
end;

begin
  if EstPositif then
    WriteLn('Le résultat est positif');
end;
```

## Ordre de déclaration

En Pascal, les procédures et fonctions doivent être déclarées **avant** d'être utilisées.

### ✅ Correct

```pascal
program Correct;

procedure Procedure1;
begin
  WriteLn('Procédure 1');
end;

procedure Procedure2;
begin
  Procedure1;  // OK : Procedure1 est déjà déclarée au-dessus
end;

begin
  Procedure2;
end.
```

### ❌ Incorrect

```pascal
program Incorrect;

procedure Procedure2;
begin
  Procedure1;  // ❌ ERREUR : Procedure1 n'est pas encore déclarée
end;

procedure Procedure1;
begin
  WriteLn('Procédure 1');
end;

begin
  Procedure2;
end.
```

### Solution : déclaration anticipée (forward)

Si vous devez appeler une procédure qui sera définie plus tard, utilisez `forward` :

```pascal
program AvecForward;

procedure Procedure1; forward;  // Déclaration anticipée

procedure Procedure2;
begin
  Procedure1;  // OK maintenant
end;

procedure Procedure1;  // Définition complète
begin
  WriteLn('Procédure 1');
end;

begin
  Procedure2;
end.
```

## Procédures et fonctions sans paramètres

Jusqu'ici, nous avons vu uniquement des procédures/fonctions **sans paramètres**. Elles s'exécutent toujours de la même manière.

```pascal
function DonnerReponse: Integer;
begin
  Result := 42;  // Retourne toujours 42
end;

procedure DireBonjour;
begin
  WriteLn('Bonjour');  // Affiche toujours la même chose
end;
```

**Note :** Dans les prochaines sections (4.3, 4.4, 4.5), nous verrons comment ajouter des **paramètres** pour rendre nos procédures et fonctions plus flexibles et réutilisables.

## Variables locales dans les procédures/fonctions

On peut déclarer des variables **locales** qui n'existent que dans la procédure ou fonction.

### Dans une procédure

```pascal
procedure AfficherCalcul;
var
  a, b, somme: Integer;  // Variables locales
begin
  a := 10;
  b := 20;
  somme := a + b;
  WriteLn('La somme est : ', somme);
end;
```

### Dans une fonction

```pascal
function CalculerSomme: Integer;
var
  x, y: Integer;  // Variables locales
begin
  x := 15;
  y := 25;
  Result := x + y;
end;
```

**Important :** Ces variables locales sont **créées** quand la procédure/fonction est appelée et **détruites** quand elle se termine. Elles ne sont pas accessibles depuis le programme principal.

## Exemple complet récapitulatif

```pascal
{$mode objfpc}{$H+}
program ExempleComplet;

// Déclaration d'une procédure
procedure AfficherSeparateur;
begin
  WriteLn('====================');
end;

// Déclaration d'une fonction
function ObtenirAnneeActuelle: Integer;
begin
  Result := 2025;
end;

// Déclaration d'une fonction avec calcul
function CalculerDoubleAnnee: Integer;
var
  annee: Integer;  // Variable locale
begin
  annee := ObtenirAnneeActuelle;
  Result := annee * 2;
end;

// Programme principal
var
  resultat: Integer;
begin
  AfficherSeparateur;
  WriteLn('Année actuelle : ', ObtenirAnneeActuelle);

  resultat := CalculerDoubleAnnee;
  WriteLn('Le double : ', resultat);

  AfficherSeparateur;
end.
```

**Résultat à l'écran :**
```
====================
Année actuelle : 2025
Le double : 4050
====================
```

## Erreurs courantes à éviter

### 1. Oublier le type de retour d'une fonction

```pascal
function MaFonction;  // ❌ ERREUR : type de retour manquant
begin
  Result := 10;
end;

function MaFonction: Integer;  // ✅ CORRECT
begin
  Result := 10;
end;
```

### 2. Oublier d'affecter une valeur de retour

```pascal
function ObtenirNombre: Integer;
begin
  WriteLn('Fonction appelée');
  // ❌ ERREUR : pas de Result := ...
end;

function ObtenirNombre: Integer;
begin
  WriteLn('Fonction appelée');
  Result := 5;  // ✅ CORRECT
end;
```

### 3. Utiliser une fonction comme une procédure

```pascal
function Calculer: Integer;
begin
  Result := 100;
end;

begin
  Calculer;  // ⚠️ Mauvaise pratique : la valeur retournée est perdue

  WriteLn(Calculer);  // ✅ MIEUX : on utilise la valeur retournée
end;
```

### 4. Essayer d'affecter une valeur de retour à une procédure

```pascal
procedure MaProcedure;
begin
  Result := 10;  // ❌ ERREUR : les procédures ne retournent rien
end;
```

## Points clés à retenir

1. Les procédures/fonctions doivent être **déclarées avant le programme principal**
2. Une procédure s'appelle seule : `MaProcedure;`
3. Une fonction s'appelle dans une expression : `x := MaFonction;`
4. Les fonctions doivent **toujours** avoir un type de retour
5. Les fonctions doivent **toujours** affecter une valeur à `Result` (ou au nom de la fonction)
6. Les variables déclarées dans une procédure/fonction sont **locales**
7. L'ordre de déclaration est important (sauf avec `forward`)
8. On peut appeler une procédure/fonction autant de fois qu'on le souhaite

---

**Prochaine étape :** Dans la section 4.3, nous découvrirons les **paramètres par valeur** qui permettent de passer des informations à nos procédures et fonctions pour les rendre beaucoup plus flexibles et réutilisables.

⏭️ [Paramètres par valeur](/04-procedures-fonctions/03-parametres-par-valeur.md)
