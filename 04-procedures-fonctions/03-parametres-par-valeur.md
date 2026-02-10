🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.3 Paramètres par valeur

## Introduction

Jusqu'à présent, nos procédures et fonctions fonctionnaient toujours de la même manière. Les **paramètres** (aussi appelés **arguments**) permettent de leur transmettre des informations pour les rendre plus flexibles et réutilisables.

Un **paramètre par valeur** est la façon la plus simple et la plus courante de passer des données à une procédure ou fonction.

## Qu'est-ce qu'un paramètre ?

Imaginons que vous voulez une fonction pour calculer le carré d'un nombre. Sans paramètre, vous seriez limité :

```pascal
function CarreDeCinq: Integer;  
begin  
  Result := 5 * 5;  // Calcule toujours le carré de 5
end;
```

Avec un paramètre, la fonction devient flexible :

```pascal
function Carre(nombre: Integer): Integer;  
begin  
  Result := nombre * nombre;  // Calcule le carré du nombre reçu
end;
```

Maintenant vous pouvez calculer le carré de n'importe quel nombre :
```pascal
WriteLn(Carre(5));   // 25  
WriteLn(Carre(10));  // 100  
WriteLn(Carre(3));   // 9  
```

## Déclaration de paramètres dans une procédure

### Syntaxe de base

```pascal
procedure NomProcedure(nomParametre: Type);  
begin  
  // Utilisation de nomParametre
end;
```

### Exemple simple

```pascal
program ExempleParametre;

procedure DireBonjour(prenom: String);  
begin  
  WriteLn('Bonjour ', prenom, ' !');
end;

begin
  DireBonjour('Marie');   // Affiche : Bonjour Marie !
  DireBonjour('Pierre');  // Affiche : Bonjour Pierre !
  DireBonjour('Sophie');  // Affiche : Bonjour Sophie !
end.
```

### Avec plusieurs paramètres

On peut passer plusieurs paramètres en les séparant par des point-virgules :

```pascal
procedure AfficherPersonne(nom: String; age: Integer);  
begin  
  WriteLn('Nom : ', nom);
  WriteLn('Age : ', age, ' ans');
  WriteLn('---');
end;

begin
  AfficherPersonne('Dupont', 25);
  AfficherPersonne('Martin', 30);
end.
```

**Résultat :**
```
Nom : Dupont  
Age : 25 ans  
---
Nom : Martin  
Age : 30 ans  
---
```

### Paramètres du même type

Si plusieurs paramètres ont le même type, on peut les regrouper :

```pascal
// Version longue
procedure Additionner(a: Integer; b: Integer; c: Integer);

// Version courte (équivalente)
procedure Additionner(a, b, c: Integer);  
begin  
  WriteLn('Somme : ', a + b + c);
end;
```

## Déclaration de paramètres dans une fonction

### Syntaxe de base

```pascal
function NomFonction(nomParametre: Type): TypeRetour;  
begin  
  // Utilisation de nomParametre
  Result := ...;
end;
```

### Exemple simple

```pascal
function Carre(nombre: Integer): Integer;  
begin  
  Result := nombre * nombre;
end;

var
  resultat: Integer;
begin
  resultat := Carre(7);
  WriteLn('Le carré de 7 est : ', resultat);  // 49
end.
```

### Avec plusieurs paramètres

```pascal
function Additionner(a, b: Integer): Integer;  
begin  
  Result := a + b;
end;

function Multiplier(x, y: Real): Real;  
begin  
  Result := x * y;
end;

begin
  WriteLn('5 + 3 = ', Additionner(5, 3));        // 8
  WriteLn('2.5 * 4.0 = ', Multiplier(2.5, 4.0)); // 10.0
end.
```

### Paramètres de types différents

```pascal
function RepeterTexte(texte: String; fois: Integer): String;  
var  
  i: Integer;
  resultat: String;
begin
  resultat := '';
  for i := 1 to fois do
    resultat := resultat + texte;
  Result := resultat;
end;

begin
  WriteLn(RepeterTexte('Ha', 3));     // HaHaHa
  WriteLn(RepeterTexte('Bla ', 4));   // Bla Bla Bla Bla
end.
```

## Comment fonctionnent les paramètres par valeur ?

Quand vous passez un paramètre **par valeur**, le programme :
1. **Copie** la valeur dans un nouvel emplacement mémoire
2. La procédure/fonction travaille avec cette **copie**
3. La variable originale **n'est pas modifiée**

### Démonstration

```pascal
program DemonstrationValeur;

procedure ModifierNombre(n: Integer);  
begin  
  WriteLn('Dans la procédure, n vaut : ', n);
  n := n + 10;  // Modification de la copie
  WriteLn('Dans la procédure, n vaut maintenant : ', n);
end;

var
  nombre: Integer;
begin
  nombre := 5;
  WriteLn('Avant l''appel, nombre vaut : ', nombre);

  ModifierNombre(nombre);

  WriteLn('Après l''appel, nombre vaut : ', nombre);
end.
```

**Résultat :**
```
Avant l'appel, nombre vaut : 5  
Dans la procédure, n vaut : 5  
Dans la procédure, n vaut maintenant : 15  
Après l'appel, nombre vaut : 5  
```

**Explication :** La variable `nombre` conserve sa valeur originale (5) car la procédure a travaillé avec une **copie**.

## Appel avec différents types de valeurs

### Avec des constantes

```pascal
procedure Afficher(nombre: Integer);  
begin  
  WriteLn('Nombre : ', nombre);
end;

begin
  Afficher(42);  // Valeur directe
end.
```

### Avec des variables

```pascal
var
  x: Integer;
begin
  x := 100;
  Afficher(x);  // Passage de la variable
end.
```

### Avec des expressions

```pascal
var
  a, b: Integer;
begin
  a := 10;
  b := 20;
  Afficher(a + b);      // Expression : 30
  Afficher(a * 2);      // Expression : 20
  Afficher(Carre(5));   // Résultat d'une fonction
end.
```

## Exemples pratiques

### 1. Fonction de calcul de surface

```pascal
function CalculerAireRectangle(longueur, largeur: Real): Real;  
begin  
  Result := longueur * largeur;
end;

var
  aire: Real;
begin
  aire := CalculerAireRectangle(5.0, 3.0);
  WriteLn('Aire du rectangle : ', aire:0:2, ' m²');

  aire := CalculerAireRectangle(10.5, 7.2);
  WriteLn('Aire du rectangle : ', aire:0:2, ' m²');
end.
```

### 2. Fonction de vérification

```pascal
function EstPair(nombre: Integer): Boolean;  
begin  
  Result := (nombre mod 2 = 0);
end;

begin
  if EstPair(10) then
    WriteLn('10 est pair');

  if not EstPair(7) then
    WriteLn('7 est impair');
end.
```

### 3. Procédure d'affichage formaté

```pascal
procedure AfficherLigne(caractere: Char; longueur: Integer);  
var  
  i: Integer;
begin
  for i := 1 to longueur do
    Write(caractere);
  WriteLn;  // Retour à la ligne
end;

begin
  AfficherLigne('=', 30);
  WriteLn('Titre du document');
  AfficherLigne('=', 30);
  WriteLn('Contenu...');
  AfficherLigne('-', 30);
end.
```

**Résultat :**
```
==============================
Titre du document
==============================
Contenu...
------------------------------
```

### 4. Fonction de calcul de moyenne

```pascal
function CalculerMoyenne(note1, note2, note3: Real): Real;  
begin  
  Result := (note1 + note2 + note3) / 3;
end;

var
  moyenne: Real;
begin
  moyenne := CalculerMoyenne(15.5, 12.0, 14.5);
  WriteLn('Moyenne : ', moyenne:0:2);  // 14.00
end.
```

### 5. Fonction de conversion de température

```pascal
function CelsiusVersFahrenheit(celsius: Real): Real;  
begin  
  Result := (celsius * 9 / 5) + 32;
end;

function FahrenheitVersCelsius(fahrenheit: Real): Real;  
begin  
  Result := (fahrenheit - 32) * 5 / 9;
end;

begin
  WriteLn('20°C = ', CelsiusVersFahrenheit(20):0:1, '°F');
  WriteLn('68°F = ', FahrenheitVersCelsius(68):0:1, '°C');
end.
```

## Ordre des paramètres

L'ordre des paramètres lors de l'appel doit correspondre à l'ordre de la déclaration.

```pascal
procedure AfficherCoordonnees(x: Integer; y: Integer; z: Integer);  
begin  
  WriteLn('X = ', x, ', Y = ', y, ', Z = ', z);
end;

begin
  AfficherCoordonnees(10, 20, 30);  // X=10, Y=20, Z=30
  AfficherCoordonnees(30, 10, 20);  // X=30, Y=10, Z=20
end.
```

## Portée des paramètres

Les paramètres se comportent comme des **variables locales** :
- Ils existent uniquement pendant l'exécution de la procédure/fonction
- Ils ne sont pas accessibles en dehors
- Ils disparaissent quand la procédure/fonction se termine

```pascal
function Calculer(a, b: Integer): Integer;  
begin  
  Result := a + b;
  // a et b existent uniquement ici
end;

begin
  WriteLn(Calculer(5, 3));
  // WriteLn(a);  // ❌ ERREUR : a n'existe pas ici
end.
```

## Erreurs courantes à éviter

### 1. Nombre de paramètres incorrect

```pascal
procedure Afficher(texte: String; nombre: Integer);  
begin  
  WriteLn(texte, ' : ', nombre);
end;

begin
  Afficher('Test');           // ❌ ERREUR : manque le 2e paramètre
  Afficher('Test', 10);       // ✅ CORRECT
  Afficher('Test', 10, 20);   // ❌ ERREUR : trop de paramètres
end;
```

### 2. Type de paramètre incorrect

```pascal
function Doubler(n: Integer): Integer;  
begin  
  Result := n * 2;
end;

begin
  WriteLn(Doubler(5));        // ✅ CORRECT
  WriteLn(Doubler(3.14));     // ⚠️ Attention : conversion automatique
  WriteLn(Doubler('Texte'));  // ❌ ERREUR : type incompatible
end;
```

### 3. Confusion entre paramètre et variable globale

```pascal
var
  x: Integer;

procedure Test(x: Integer);  // Ce x est différent du x global  
begin  
  x := 10;  // Modifie le paramètre, pas la variable globale
end;

begin
  x := 5;
  Test(x);
  WriteLn(x);  // Affiche 5, pas 10
end.
```

## Avantages des paramètres par valeur

1. **Sécurité** : La variable originale ne peut pas être modifiée accidentellement
2. **Clarté** : On sait que la procédure/fonction ne modifiera pas nos variables
3. **Simplicité** : C'est la méthode la plus simple et la plus intuitive
4. **Réutilisabilité** : Une même fonction peut traiter différentes valeurs

## Exemple complet récapitulatif

```pascal
{$mode objfpc}{$H+}
program GestionNotes;

// Fonction avec un paramètre
function EstReussi(note: Real): Boolean;  
begin  
  Result := note >= 10;
end;

// Fonction avec plusieurs paramètres
function CalculerMoyenne(n1, n2, n3: Real): Real;  
begin  
  Result := (n1 + n2 + n3) / 3;
end;

// Procédure avec plusieurs paramètres
procedure AfficherResultat(nom: String; moyenne: Real);  
begin  
  WriteLn('Étudiant : ', nom);
  WriteLn('Moyenne : ', moyenne:0:2);
  if EstReussi(moyenne) then
    WriteLn('Résultat : ADMIS')
  else
    WriteLn('Résultat : REFUSÉ');
  WriteLn('---');
end;

var
  moy: Real;
begin
  moy := CalculerMoyenne(12.5, 15.0, 13.5);
  AfficherResultat('Dupont Jean', moy);

  moy := CalculerMoyenne(8.0, 9.5, 7.5);
  AfficherResultat('Martin Sophie', moy);
end.
```

**Résultat :**
```
Étudiant : Dupont Jean
Moyenne : 13.67  
Résultat : ADMIS  
---
Étudiant : Martin Sophie
Moyenne : 8.33  
Résultat : REFUSÉ  
---
```

## Points clés à retenir

1. Les paramètres rendent les procédures/fonctions **flexibles et réutilisables**
2. Un paramètre par valeur est une **copie** de la valeur d'origine
3. La variable originale **n'est jamais modifiée**
4. Syntaxe : `procedure Nom(param: Type);` ou `function Nom(param: Type): TypeRetour;`
5. On peut avoir **plusieurs paramètres** de types différents
6. L'**ordre des paramètres** est important lors de l'appel
7. Les paramètres se comportent comme des **variables locales**
8. On peut passer des **constantes, variables ou expressions** en paramètre

---

**Prochaine étape :** Dans la section 4.4, nous découvrirons les **paramètres par référence (var)** qui permettent, contrairement aux paramètres par valeur, de **modifier** les variables originales passées en paramètre.

⏭️ [Paramètres par référence (var)](/04-procedures-fonctions/04-parametres-par-reference-var.md)
