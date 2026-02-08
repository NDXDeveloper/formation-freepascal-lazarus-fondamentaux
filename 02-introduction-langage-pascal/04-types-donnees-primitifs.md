🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.4 Types de données primitifs (Integer, Real, Boolean, Char)

## Comprendre les types de données

### Qu'est-ce qu'un type de données ?

Imaginez que vous rangez différentes choses dans votre maison : les vêtements vont dans l'armoire, les livres sur les étagères, la nourriture dans le réfrigérateur. Vous ne mettez pas un livre dans le frigo, ni un pull dans le four !

En programmation, c'est pareil. Un **type de données** définit :
- **Quelle sorte** d'information peut être stockée dans une variable
- **Combien d'espace mémoire** est nécessaire
- **Quelles opérations** sont possibles avec cette donnée

### Pourquoi typer les variables ?

Pascal est un langage à **typage fort** : chaque variable doit avoir un type déclaré. Cela peut sembler contraignant, mais c'est en fait une protection :

```pascal
var
  age: integer;
  nom: string;
begin
  age := 25;          // OK
  age := 'vingt';     // ERREUR : on ne peut pas mettre du texte dans un integer
end.
```

Le compilateur vous empêche de faire des erreurs absurdes, comme additionner un nom et un âge !

## Le type Integer (nombres entiers)

### Qu'est-ce qu'un Integer ?

Le type `integer` sert à stocker des **nombres entiers** (sans virgule) : positifs, négatifs ou nul.

**Exemples de valeurs integer :**
- `0`
- `42`
- `-15`
- `1000`
- `-999`

**Ce qui N'est PAS un integer :**
- `3.14` (contient une virgule → c'est un real)
- `'123'` (entre guillemets → c'est une string)
- `vrai` (c'est un boolean)

### Plage de valeurs

En Free Pascal, le type `integer` peut stocker des valeurs entre **-2 147 483 648** et **+2 147 483 647**.

C'est largement suffisant pour la plupart des usages quotidiens (compter des personnes, des jours, des articles, etc.).

### Déclaration et utilisation

```pascal
program ExempleInteger;
var
  age: integer;
  nombreEleves: integer;
  temperature: integer;
  annee: integer;
begin
  age := 25;
  nombreEleves := 30;
  temperature := -5;
  annee := 2024;

  writeln('Âge : ', age);
  writeln('Nombre d''élèves : ', nombreEleves);  // '' double l'apostrophe pour en afficher une
  writeln('Température : ', temperature, ' °C');
  writeln('Année : ', annee);
end.
```

### Opérations sur les integers

**Opérations arithmétiques de base :**

```pascal
var
  a, b, resultat: integer;
begin
  a := 10;
  b := 3;

  resultat := a + b;    // Addition : 13
  writeln('Addition : ', resultat);

  resultat := a - b;    // Soustraction : 7
  writeln('Soustraction : ', resultat);

  resultat := a * b;    // Multiplication : 30
  writeln('Multiplication : ', resultat);

  resultat := a div b;  // Division entière : 3
  writeln('Division entière : ', resultat);

  resultat := a mod b;  // Modulo (reste) : 1
  writeln('Reste : ', resultat);
end.
```

**Important :** Pour la division d'entiers, on utilise `div` (pas `/`). Le résultat est toujours un entier, la partie décimale est tronquée.

### Variantes du type Integer

Free Pascal propose plusieurs variantes selon vos besoins :

| Type | Plage | Usage |
|------|-------|-------|
| `byte` | 0 à 255 | Petites valeurs positives |
| `shortint` | -128 à 127 | Très petits nombres |
| `word` | 0 à 65535 | Valeurs positives moyennes |
| `integer` | -2 147 483 648 à 2 147 483 647 | Usage général |
| `longint` | -2 147 483 648 à 2 147 483 647 | Identique à integer |
| `int64` | Très grand | Nombres astronomiques |

**En pratique :** utilisez `integer` par défaut, sauf besoin spécifique.

## Le type Real (nombres décimaux)

### Qu'est-ce qu'un Real ?

Le type `real` sert à stocker des **nombres décimaux** (avec virgule, appelée "point décimal" en informatique).

**Exemples de valeurs real :**
- `3.14`
- `-2.5`
- `0.001`
- `1000.99`
- `0.0`

### Notation importante

En Pascal (et dans la plupart des langages), on utilise le **point** comme séparateur décimal, jamais la virgule :

```pascal
var
  prix: real;
begin
  prix := 19.99;     // CORRECT
  prix := 19,99;     // ERREUR : la virgule n'est pas acceptée
end.
```

### Déclaration et utilisation

```pascal
program ExempleReal;
var
  prix: real;
  taille: real;
  temperature: real;
  pi: real;
begin
  prix := 29.99;
  taille := 1.75;
  temperature := 20.5;
  pi := 3.14159;

  writeln('Prix : ', prix:0:2, ' €');            // :0:2 = afficher avec 2 decimales
  writeln('Taille : ', taille:0:2, ' m');
  writeln('Température : ', temperature:0:1, ' °C');
  writeln('Pi : ', pi:0:5);
end.
```

**Note sur le formatage :** `prix:0:2` signifie "afficher avec 2 décimales". Nous verrons cela en détail plus tard.

### Opérations sur les reals

```pascal
var
  a, b, resultat: real;
begin
  a := 10.5;
  b := 3.2;

  resultat := a + b;    // Addition : 13.7
  writeln('Addition : ', resultat:0:2);

  resultat := a - b;    // Soustraction : 7.3
  writeln('Soustraction : ', resultat:0:2);

  resultat := a * b;    // Multiplication : 33.6
  writeln('Multiplication : ', resultat:0:2);

  resultat := a / b;    // Division : 3.28125
  writeln('Division : ', resultat:0:5);
end.
```

**Important :** Pour les reals, on utilise `/` pour la division (pas `div`).

### Integer vs Real : quand utiliser quoi ?

**Utilisez Integer pour :**
- Compter des choses (personnes, objets, jours)
- Les âges, les années
- Les quantités entières (nombre de produits)
- Les index, les compteurs

**Utilisez Real pour :**
- Les prix, les montants d'argent
- Les mesures (taille, poids, distance)
- Les températures
- Les pourcentages
- Les calculs scientifiques

```pascal
var
  nombrePersonnes: integer;     // On ne peut pas avoir 2.5 personnes
  prixUnitaire: real;           // Un prix peut avoir des centimes
  quantite: integer;            // Nombre d'articles commandés
  total: real;                  // Le total peut avoir des décimales
begin
  nombrePersonnes := 5;
  prixUnitaire := 12.50;
  quantite := 3;
  total := prixUnitaire * quantite;   // 37.50
end.
```

### Variantes du type Real

| Type | Précision | Usage |
|------|-----------|-------|
| `single` | ~7 chiffres | Calculs simples |
| `real` | ~15 chiffres | Usage général |
| `double` | ~15 chiffres | Haute précision |
| `extended` | ~19 chiffres | Très haute précision |

**En pratique :** utilisez `real` par défaut.

### Attention aux approximations

Les nombres décimaux sont stockés de manière approximative en mémoire. Cela peut causer des surprises :

```pascal
var
  x, y: real;
begin
  x := 0.1 + 0.2;
  y := 0.3;

  writeln(x:0:20);  // Peut afficher 0.30000000000000004441
  writeln(y:0:20);  // Affiche 0.30000000000000000000

  // Ne testez jamais l'égalité exacte de reals !
end.
```

## Le type Boolean (vrai/faux)

### Qu'est-ce qu'un Boolean ?

Le type `boolean` ne peut prendre que **deux valeurs** :
- `true` (vrai)
- `false` (faux)

C'est comme un interrupteur : allumé ou éteint, il n'y a pas d'état intermédiaire.

### Déclaration et utilisation

```pascal
program ExempleBoolean;
var
  estMajeur: boolean;
  aReussi: boolean;
  estConnecte: boolean;
begin
  estMajeur := true;
  aReussi := false;
  estConnecte := true;

  writeln('Majeur : ', estMajeur);
  writeln('A réussi : ', aReussi);
  writeln('Connecté : ', estConnecte);
end.
```

### Opérateurs logiques

Les booleans utilisent des opérateurs spéciaux :

```pascal
var
  a, b, resultat: boolean;
begin
  a := true;
  b := false;

  resultat := a and b;    // ET logique : false
  writeln('a AND b : ', resultat);

  resultat := a or b;     // OU logique : true
  writeln('a OR b : ', resultat);

  resultat := not a;      // NON logique : false
  writeln('NOT a : ', resultat);
end.
```

**Table de vérité du AND (et) :**
- `true and true` → `true`
- `true and false` → `false`
- `false and true` → `false`
- `false and false` → `false`

**Table de vérité du OR (ou) :**
- `true or true` → `true`
- `true or false` → `true`
- `false or true` → `true`
- `false or false` → `false`

**Opérateur NOT (négation) :**
- `not true` → `false`
- `not false` → `true`

### Comparaisons produisent des booleans

Les opérateurs de comparaison retournent des booleans :

```pascal
var
  age: integer;
  estMajeur: boolean;
  estEnfant: boolean;
begin
  age := 25;

  estMajeur := age >= 18;     // true
  estEnfant := age < 12;      // false

  writeln('Majeur : ', estMajeur);
  writeln('Enfant : ', estEnfant);
end.
```

**Opérateurs de comparaison :**
- `=` : égal à
- `<>` : différent de
- `<` : plus petit que
- `>` : plus grand que
- `<=` : plus petit ou égal à
- `>=` : plus grand ou égal à

### Utilisation pratique

```pascal
program ValidationAge;
var
  age: integer;
  peutConduire: boolean;
  peutVoter: boolean;
  estRetraite: boolean;
begin
  age := 25;

  peutConduire := age >= 18;
  peutVoter := age >= 18;
  estRetraite := age >= 65;

  writeln('Âge : ', age, ' ans');
  writeln('Peut conduire : ', peutConduire);
  writeln('Peut voter : ', peutVoter);
  writeln('Est retraité : ', estRetraite);
end.
```

## Le type Char (caractère)

### Qu'est-ce qu'un Char ?

Le type `char` stocke **un seul caractère** : une lettre, un chiffre, un symbole ou un espace.

**Exemples de valeurs char :**
- `'A'` (une lettre majuscule)
- `'z'` (une lettre minuscule)
- `'5'` (le chiffre 5 en tant que caractère, pas le nombre 5)
- `' '` (un espace)
- `'@'` (un symbole)

**Important :** Les caractères sont toujours entre **apostrophes simples** (`'`), jamais entre guillemets.

### Déclaration et utilisation

```pascal
program ExempleChar;
var
  initiale: char;
  note: char;
  symbole: char;
begin
  initiale := 'M';
  note := 'A';
  symbole := '@';

  writeln('Initiale : ', initiale);
  writeln('Note : ', note);
  writeln('Symbole : ', symbole);
end.
```

### Char vs String

**Ne confondez pas :**
- `'A'` → un char (un seul caractère)
- `'Bonjour'` → une string (plusieurs caractères)

```pascal
var
  lettre: char;
  mot: string;
begin
  lettre := 'X';           // OK : un seul caractère
  lettre := 'XY';          // ERREUR : trop de caractères pour un char

  mot := 'Bonjour';        // OK : plusieurs caractères
  mot := 'X';              // OK : une string peut contenir un seul caractère
end.
```

### Codes ASCII

Chaque caractère a un code numérique (code ASCII). Vous pouvez convertir entre char et integer :

```pascal
var
  lettre: char;
  code: integer;
begin
  lettre := 'A';
  code := ord(lettre);      // ord() donne le code ASCII
  writeln('Code de A : ', code);  // Affiche 65

  code := 66;
  lettre := chr(code);      // chr() convertit un code en char
  writeln('Caractère 66 : ', lettre);  // Affiche B
end.
```

**Codes ASCII courants :**
- `'0'` à `'9'` : codes 48 à 57
- `'A'` à `'Z'` : codes 65 à 90
- `'a'` à `'z'` : codes 97 à 122
- `' '` (espace) : code 32

### Opérations sur les chars

```pascal
var
  c1, c2: char;
  estIdentique: boolean;
begin
  c1 := 'A';
  c2 := 'a';

  // Comparaison
  estIdentique := c1 = c2;      // false (majuscule ≠ minuscule)
  writeln('Identiques : ', estIdentique);

  // Conversion majuscule/minuscule
  writeln(upcase(c2));          // Convertit 'a' en majuscule : A
  writeln(lowercase(c1));       // Convertit 'A' en minuscule : a (nécessite uses SysUtils)
end.
```

## Conversions entre types

### Pourquoi convertir ?

Parfois, vous avez besoin de changer le type d'une donnée. Par exemple, transformer un nombre entier en nombre décimal, ou vice-versa.

### Integer vers Real

La conversion est **automatique** et **sûre** :

```pascal
var
  entier: integer;
  decimal: real;
begin
  entier := 10;
  decimal := entier;     // Conversion automatique : 10.0
  writeln(decimal:0:1);  // Affiche 10.0
end.
```

### Real vers Integer

La conversion doit être **explicite** car elle perd de l'information (la partie décimale) :

```pascal
var
  decimal: real;
  entier: integer;
begin
  decimal := 3.7;

  entier := trunc(decimal);   // Tronque (coupe) : 3
  writeln('Trunc : ', entier);

  entier := round(decimal);   // Arrondit : 4
  writeln('Round : ', entier);
end.
```

**Fonctions de conversion :**
- `trunc(x)` : tronque (coupe la partie décimale) → `trunc(3.9)` = `3`
- `round(x)` : arrondit au plus proche → `round(3.9)` = `4`

### Integer/Real vers String

```pascal
uses SysUtils;

var
  nombre: integer;
  texte: string;
begin
  nombre := 42;

  // Méthode 1 : concaténation avec conversion explicite
  texte := 'Le nombre est ' + IntToStr(nombre);
  writeln(texte);

  // Méthode 2 : conversion seule
  texte := IntToStr(nombre);
  writeln(texte);
end.
```

**Note :** `IntToStr` nécessite `uses SysUtils`.

### String vers Integer/Real

```pascal
var
  texte: string;
  nombre: integer;
  decimal: real;
  code: integer;
begin
  texte := '123';

  Val(texte, nombre, code);    // Convertit string vers integer
  if code = 0 then
    writeln('Nombre : ', nombre)
  else
    writeln('Erreur de conversion');

  // Ou plus simplement (nécessite uses SysUtils)
  nombre := StrToInt('456');
  decimal := StrToFloat('3.14');
end.
```

## Tableau récapitulatif des types primitifs

| Type | Contient | Exemple de valeur | Plage / Détails |
|------|----------|-------------------|-----------------|
| `integer` | Nombres entiers | `42`, `-15`, `0` | -2 147 483 648 à 2 147 483 647 |
| `real` | Nombres décimaux | `3.14`, `-2.5` | ~15 chiffres de précision |
| `boolean` | Vrai ou faux | `true`, `false` | Seulement 2 valeurs possibles |
| `char` | Un caractère | `'A'`, `'5'`, `'@'` | Entre apostrophes simples |
| `string` | Texte | `'Bonjour'` | Plusieurs caractères |

## Exemples pratiques combinés

### Exemple 1 : Calcul de moyenne

```pascal
program CalculMoyenne;
var
  note1, note2, note3: real;
  moyenne: real;
  aReussi: boolean;
begin
  note1 := 15.5;
  note2 := 12.0;
  note3 := 14.5;

  moyenne := (note1 + note2 + note3) / 3;
  aReussi := moyenne >= 10.0;  // La comparaison >= produit true ou false

  writeln('Note 1 : ', note1:0:1);
  writeln('Note 2 : ', note2:0:1);
  writeln('Note 3 : ', note3:0:1);
  writeln('Moyenne : ', moyenne:0:2);
  writeln('A réussi : ', aReussi);
end.
```

### Exemple 2 : Informations de produit

```pascal
program InfoProduit;
const
  TauxTVA = 20.0;  // const = valeur fixe, non modifiable
var
  nomProduit: string;
  prixHT: real;
  quantite: integer;
  enStock: boolean;
  categorie: char;
  prixTTC: real;
  montantTotal: real;
begin
  nomProduit := 'Clavier sans fil';
  prixHT := 35.50;
  quantite := 5;
  enStock := true;
  categorie := 'I';  // I pour Informatique

  prixTTC := prixHT * (1 + TauxTVA / 100);
  montantTotal := prixTTC * quantite;

  writeln('=== FICHE PRODUIT ===');
  writeln('Produit : ', nomProduit);
  writeln('Catégorie : ', categorie);
  writeln('Prix HT : ', prixHT:0:2, ' €');
  writeln('Prix TTC : ', prixTTC:0:2, ' €');
  writeln('Quantité : ', quantite);
  writeln('En stock : ', enStock);
  writeln('Montant total : ', montantTotal:0:2, ' €');
end.
```

### Exemple 3 : Validation d'âge

```pascal
program ValidationAgeComplete;
var
  nom: string;
  age: integer;
  estMajeur: boolean;
  estSenior: boolean;
  peutConduire: boolean;
  initiale: char;
begin
  nom := 'Dupont';
  age := 68;
  initiale := 'D';

  estMajeur := age >= 18;
  estSenior := age >= 65;
  peutConduire := age >= 18;

  writeln('=== INFORMATIONS ===');
  writeln('Initiale : ', initiale, '.');
  writeln('Nom : ', nom);
  writeln('Âge : ', age, ' ans');
  writeln('Majeur : ', estMajeur);
  writeln('Senior : ', estSenior);
  writeln('Peut conduire : ', peutConduire);
end.
```

## Erreurs courantes à éviter

### 1. Mélanger les types incompatibles

```pascal
var
  nombre: integer;
  texte: string;
begin
  nombre := 'cinq';     // ERREUR : type incompatible
end.
```

### 2. Utiliser la virgule au lieu du point

```pascal
var
  prix: real;
begin
  prix := 19,99;        // ERREUR : utiliser un point
  prix := 19.99;        // CORRECT
end.
```

### 3. Confondre char et string

```pascal
var
  lettre: char;
begin
  lettre := 'AB';       // ERREUR : trop de caractères
  lettre := 'A';        // CORRECT
end.
```

### 4. Utiliser div avec des reals

```pascal
var
  a, b, resultat: real;
begin
  a := 10.5;
  b := 3.2;
  resultat := a div b;  // ERREUR : div n'existe que pour les integers
  resultat := a / b;    // CORRECT
end.
```

### 5. Oublier les apostrophes pour un char

```pascal
var
  lettre: char;
begin
  lettre := A;          // ERREUR : A est une variable, pas un caractère
  lettre := 'A';        // CORRECT
end.
```

## Récapitulatif

**Les 4 types primitifs essentiels :**

1. **Integer** : nombres entiers, pour compter et indexer
2. **Real** : nombres décimaux, pour mesurer et calculer
3. **Boolean** : vrai/faux, pour les décisions et conditions
4. **Char** : un caractère, pour manipuler du texte caractère par caractère

**Règles importantes :**
- Déclarez toujours le type de vos variables
- Utilisez le type approprié selon vos besoins
- Attention aux conversions qui perdent de l'information
- Le point est le séparateur décimal, pas la virgule
- Les chars utilisent des apostrophes simples

---

**Point clé :** Bien choisir ses types de données est fondamental en programmation. Un mauvais choix peut causer des erreurs ou des imprécisions. Prenez l'habitude de vous demander : "Quel type de donnée correspond le mieux à l'information que je veux stocker ?"

⏭️ [Opérateurs arithmétiques et logiques](/02-introduction-langage-pascal/05-operateurs-arithmetiques-logiques.md)
