🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.5 Opérateurs arithmétiques et logiques

## Qu'est-ce qu'un opérateur ?

Un **opérateur** est un symbole qui indique à l'ordinateur d'effectuer une opération sur une ou plusieurs valeurs. C'est comme les symboles mathématiques que vous connaissez : `+`, `-`, `×`, `÷`.

En programmation, nous utilisons des opérateurs pour :
- Faire des calculs (arithmétiques)
- Comparer des valeurs (comparaison)
- Effectuer des opérations logiques (logiques)

## Les opérateurs arithmétiques

Les opérateurs arithmétiques permettent d'effectuer des calculs mathématiques.

### Addition (+)

L'opérateur `+` additionne deux valeurs.

```pascal
var
  a, b, somme: integer;
begin
  a := 10;
  b := 5;
  somme := a + b;           // somme vaut 15
  writeln('10 + 5 = ', somme);
end.
```

**Fonctionne avec :**
- Integer : `5 + 3` → `8`
- Real : `3.5 + 2.1` → `5.6`
- Mixte : `5 + 2.5` → `7.5` (résultat en real)

### Soustraction (-)

L'opérateur `-` soustrait la deuxième valeur de la première.

```pascal
var
  a, b, difference: integer;
begin
  a := 10;
  b := 5;
  difference := a - b;      // difference vaut 5
  writeln('10 - 5 = ', difference);
end.
```

**Exemples :**
- `10 - 3` → `7`
- `3.5 - 1.2` → `2.3`
- `5 - 10` → `-5` (résultat négatif)

### Multiplication (*)

L'opérateur `*` multiplie deux valeurs.

```pascal
var
  a, b, produit: integer;
begin
  a := 10;
  b := 5;
  produit := a * b;         // produit vaut 50
  writeln('10 * 5 = ', produit);
end.
```

**Exemples :**
- `4 * 3` → `12`
- `2.5 * 4` → `10.0`
- `-3 * 5` → `-15`

### Division décimale (/)

L'opérateur `/` divise deux valeurs et **retourne toujours un résultat décimal (real)**.

```pascal
var
  a, b: integer;
  quotient: real;           // Attention : le résultat est toujours real
begin
  a := 10;
  b := 4;
  quotient := a / b;        // quotient vaut 2.5
  writeln('10 / 4 = ', quotient:0:2);
end.
```

**Important :** Même si vous divisez deux integers, le résultat est un real !

```pascal
var
  resultat: real;
begin
  resultat := 10 / 5;       // 2.0 (pas 2)
  resultat := 7 / 2;        // 3.5
  resultat := 1 / 3;        // 0.333333...
end.
```

### Division entière (div)

L'opérateur `div` effectue une **division entière** : il ne garde que la partie entière du résultat.

```pascal
var
  a, b, quotient: integer;
begin
  a := 10;
  b := 3;
  quotient := a div b;      // quotient vaut 3 (pas 3.333...)
  writeln('10 div 3 = ', quotient);
end.
```

**Exemples :**
- `10 div 3` → `3` (on ignore 0.333...)
- `17 div 5` → `3` (on ignore 0.4)
- `20 div 4` → `5`
- `7 div 10` → `0` (car 7/10 = 0.7, partie entière = 0)

**Note :** `div` ne fonctionne qu'avec des integers. Si vous avez des reals, utilisez `trunc(a / b)`.

### Modulo (mod)

L'opérateur `mod` retourne le **reste** d'une division entière.

```pascal
var
  a, b, reste: integer;
begin
  a := 10;
  b := 3;
  reste := a mod b;         // reste vaut 1
  writeln('10 mod 3 = ', reste);
end.
```

**Comprendre le modulo :**
- `10 mod 3` → `1` (car 10 = 3×3 + 1, reste 1)
- `17 mod 5` → `2` (car 17 = 5×3 + 2, reste 2)
- `20 mod 4` → `0` (car 20 = 4×5 + 0, division exacte)
- `7 mod 10` → `7` (car 7 = 10×0 + 7)

**Applications pratiques du modulo :**

```pascal
// Savoir si un nombre est pair ou impair
var
  nombre: integer;
begin
  nombre := 15;
  if nombre mod 2 = 0 then
    writeln('Pair')
  else
    writeln('Impair');     // Affiche "Impair"
end.

// Obtenir le dernier chiffre d'un nombre
var
  nombre, dernierChiffre: integer;
begin
  nombre := 12345;
  dernierChiffre := nombre mod 10;  // 5
end.

// Vérifier si un nombre est divisible par un autre
var
  nombre: integer;
begin
  nombre := 20;
  if nombre mod 5 = 0 then
    writeln('20 est divisible par 5');
end.
```

### Négation (-)

Le signe `-` peut aussi servir à inverser le signe d'un nombre (opérateur unaire).

```pascal
var
  nombre: integer;
  oppose: integer;
begin
  nombre := 10;
  oppose := -nombre;        // oppose vaut -10

  nombre := -5;
  oppose := -nombre;        // oppose vaut 5
end.
```

### Tableau récapitulatif des opérateurs arithmétiques

| Opérateur | Nom | Exemple | Résultat | Type de résultat |
|-----------|-----|---------|----------|------------------|
| `+` | Addition | `5 + 3` | `8` | Selon les opérandes |
| `-` | Soustraction | `5 - 3` | `2` | Selon les opérandes |
| `*` | Multiplication | `5 * 3` | `15` | Selon les opérandes |
| `/` | Division décimale | `5 / 2` | `2.5` | Toujours real |
| `div` | Division entière | `5 div 2` | `2` | Integer uniquement |
| `mod` | Modulo (reste) | `5 mod 2` | `1` | Integer uniquement |
| `-` | Négation | `-5` | `-5` | Selon l'opérande |

## Les opérateurs de comparaison

Les opérateurs de comparaison permettent de comparer deux valeurs. Le résultat est **toujours un boolean** (true ou false).

### Égal (=)

Vérifie si deux valeurs sont égales.

```pascal
var
  a, b: integer;
  resultat: boolean;
begin
  a := 5;
  b := 5;
  resultat := a = b;        // true

  a := 5;
  b := 3;
  resultat := a = b;        // false
end.
```

**Attention :** Ne confondez pas `=` (comparaison) avec `:=` (affectation) !

```pascal
x := 5;      // Affectation : on met 5 dans x  
x = 5        // Comparaison : est-ce que x vaut 5 ?  
```

### Différent (<>)

Vérifie si deux valeurs sont différentes.

```pascal
var
  a, b: integer;
  resultat: boolean;
begin
  a := 5;
  b := 3;
  resultat := a <> b;       // true (5 est différent de 3)

  a := 5;
  b := 5;
  resultat := a <> b;       // false (5 n'est pas différent de 5)
end.
```

### Plus petit que (<)

Vérifie si la première valeur est strictement inférieure à la seconde.

```pascal
var
  resultat: boolean;
begin
  resultat := 3 < 5;        // true
  resultat := 5 < 5;        // false (pas strictement inférieur)
  resultat := 7 < 5;        // false
end.
```

### Plus grand que (>)

Vérifie si la première valeur est strictement supérieure à la seconde.

```pascal
var
  resultat: boolean;
begin
  resultat := 7 > 5;        // true
  resultat := 5 > 5;        // false (pas strictement supérieur)
  resultat := 3 > 5;        // false
end.
```

### Plus petit ou égal (<=)

Vérifie si la première valeur est inférieure ou égale à la seconde.

```pascal
var
  resultat: boolean;
begin
  resultat := 3 <= 5;       // true
  resultat := 5 <= 5;       // true (égal compte)
  resultat := 7 <= 5;       // false
end.
```

### Plus grand ou égal (>=)

Vérifie si la première valeur est supérieure ou égale à la seconde.

```pascal
var
  resultat: boolean;
begin
  resultat := 7 >= 5;       // true
  resultat := 5 >= 5;       // true (égal compte)
  resultat := 3 >= 5;       // false
end.
```

### Exemple pratique de comparaisons

```pascal
program ComparaisonsAge;  
var  
  age: integer;
  estEnfant: boolean;
  estAdolescent: boolean;
  estAdulte: boolean;
  estSenior: boolean;
begin
  age := 25;

  // Le resultat d'une comparaison est un boolean (true/false)
  estEnfant := age < 13;
  // En Pascal, and a priorite sur >= et <, donc les parentheses
  // autour de chaque comparaison sont obligatoires
  estAdolescent := (age >= 13) and (age < 18);
  estAdulte := (age >= 18) and (age < 65);
  estSenior := age >= 65;

  writeln('Âge : ', age);
  writeln('Enfant : ', estEnfant);          // false
  writeln('Adolescent : ', estAdolescent);  // false
  writeln('Adulte : ', estAdulte);          // true
  writeln('Senior : ', estSenior);          // false
end.
```

### Tableau récapitulatif des opérateurs de comparaison

| Opérateur | Signification | Exemple | Résultat |
|-----------|---------------|---------|----------|
| `=` | Égal à | `5 = 5` | `true` |
| `<>` | Différent de | `5 <> 3` | `true` |
| `<` | Plus petit que | `3 < 5` | `true` |
| `>` | Plus grand que | `7 > 5` | `true` |
| `<=` | Plus petit ou égal | `5 <= 5` | `true` |
| `>=` | Plus grand ou égal | `5 >= 5` | `true` |

## Les opérateurs logiques

Les opérateurs logiques permettent de combiner des valeurs booléennes.

### AND (et logique)

L'opérateur `and` retourne `true` seulement si **les deux** conditions sont vraies.

```pascal
var
  a, b, resultat: boolean;
begin
  a := true;
  b := true;
  resultat := a and b;      // true

  a := true;
  b := false;
  resultat := a and b;      // false

  a := false;
  b := true;
  resultat := a and b;      // false

  a := false;
  b := false;
  resultat := a and b;      // false
end.
```

**Table de vérité du AND :**

| A | B | A and B |
|---|---|---------|
| true | true | **true** |
| true | false | false |
| false | true | false |
| false | false | false |

**Exemple pratique :**

```pascal
var
  age: integer;
  aPermis: boolean;
  peutConduire: boolean;
begin
  age := 20;
  aPermis := true;

  // On peut conduire SI on a 18 ans ou plus ET qu'on a le permis
  peutConduire := (age >= 18) and aPermis;
  writeln('Peut conduire : ', peutConduire);  // true
end.
```

### OR (ou logique)

L'opérateur `or` retourne `true` si **au moins une** des conditions est vraie.

```pascal
var
  a, b, resultat: boolean;
begin
  a := true;
  b := true;
  resultat := a or b;       // true

  a := true;
  b := false;
  resultat := a or b;       // true

  a := false;
  b := true;
  resultat := a or b;       // true

  a := false;
  b := false;
  resultat := a or b;       // false
end.
```

**Table de vérité du OR :**

| A | B | A or B |
|---|---|--------|
| true | true | **true** |
| true | false | **true** |
| false | true | **true** |
| false | false | false |

**Exemple pratique :**

```pascal
var
  estWeekend: boolean;
  estJourFerie: boolean;
  peutSeReposer: boolean;
begin
  estWeekend := true;
  estJourFerie := false;

  // On peut se reposer SI c'est le weekend OU un jour férié
  peutSeReposer := estWeekend or estJourFerie;
  writeln('Peut se reposer : ', peutSeReposer);  // true
end.
```

### NOT (négation logique)

L'opérateur `not` inverse une valeur booléenne.

```pascal
var
  a, resultat: boolean;
begin
  a := true;
  resultat := not a;        // false

  a := false;
  resultat := not a;        // true
end.
```

**Table de vérité du NOT :**

| A | not A |
|---|-------|
| true | false |
| false | true |

**Exemple pratique :**

```pascal
var
  estConnecte: boolean;
  estDeconnecte: boolean;
begin
  estConnecte := true;
  estDeconnecte := not estConnecte;

  writeln('Connecté : ', estConnecte);        // true
  writeln('Déconnecté : ', estDeconnecte);    // false
end.
```

### XOR (ou exclusif)

L'opérateur `xor` retourne `true` si **une seule** des deux conditions est vraie (mais pas les deux).

```pascal
var
  a, b, resultat: boolean;
begin
  a := true;
  b := false;
  resultat := a xor b;      // true

  a := true;
  b := true;
  resultat := a xor b;      // false (les deux sont vrais)

  a := false;
  b := false;
  resultat := a xor b;      // false (aucun n'est vrai)
end.
```

**Table de vérité du XOR :**

| A | B | A xor B |
|---|---|---------|
| true | true | false |
| true | false | **true** |
| false | true | **true** |
| false | false | false |

**Exemple pratique :**

```pascal
// Un interrupteur à deux positions : on ou off, mais pas les deux
var
  bouton1Actif: boolean;
  bouton2Actif: boolean;
  systemeActif: boolean;
begin
  bouton1Actif := true;
  bouton2Actif := false;

  systemeActif := bouton1Actif xor bouton2Actif;
  writeln('Système actif : ', systemeActif);  // true
end.
```

### Tableau récapitulatif des opérateurs logiques

| Opérateur | Signification | Retourne true si... |
|-----------|---------------|---------------------|
| `and` | ET logique | Les deux conditions sont vraies |
| `or` | OU logique | Au moins une condition est vraie |
| `not` | NON logique | Inverse la valeur |
| `xor` | OU exclusif | Une seule condition est vraie |

## Priorité des opérateurs

Quand plusieurs opérateurs sont utilisés dans la même expression, ils ne sont pas tous évalués dans le même ordre. Il existe une **priorité** (ou **précédence**).

### Ordre de priorité (du plus prioritaire au moins prioritaire)

1. **Parenthèses** `()`
2. **NOT** et **négation** (`-`)
3. **Multiplication, division** (`*`, `/`, `div`, `mod`, `and`)
4. **Addition, soustraction** (`+`, `-`, `or`, `xor`)
5. **Comparaisons** (`=`, `<>`, `<`, `>`, `<=`, `>=`)

### Exemples sans parenthèses

```pascal
var
  resultat: real;
begin
  resultat := 2 + 3 * 4;    // 14 (pas 20)
  // Équivalent à : 2 + (3 * 4)
  // La multiplication est effectuée en premier

  resultat := 10 - 3 + 2;   // 9
  // Équivalent à : (10 - 3) + 2
  // Opérations de même priorité : de gauche à droite

  resultat := 20 / 4 * 2;   // 10.0
  // Équivalent à : (20 / 4) * 2
end.
```

### Utilisation des parenthèses

Les parenthèses permettent de **forcer l'ordre** d'évaluation :

```pascal
var
  resultat: integer;
begin
  resultat := 2 + 3 * 4;        // 14
  resultat := (2 + 3) * 4;      // 20 (parenthèses forcent l'addition d'abord)

  resultat := 10 - 3 + 2;       // 9
  resultat := 10 - (3 + 2);     // 5 (parenthèses changent l'ordre)
end.
```

**Bonne pratique :** En cas de doute, utilisez des parenthèses ! Elles rendent le code plus lisible même si elles ne sont pas strictement nécessaires.

```pascal
// Sans parenthèses (correct mais moins clair)
moyenne := note1 + note2 + note3 / 3;    // FAUX ! Division d'abord

// Avec parenthèses (intention claire)
moyenne := (note1 + note2 + note3) / 3;  // CORRECT
```

### Priorité des opérateurs logiques

```pascal
var
  a, b, c: boolean;
  resultat: boolean;
begin
  a := true;
  b := false;
  c := true;

  // AND a priorité sur OR
  resultat := a or b and c;     // true or (false and true) = true
  resultat := (a or b) and c;   // (true or false) and true = true

  // NOT a la plus haute priorité
  resultat := not a and b;      // (not true) and false = false
  resultat := not (a and b);    // not (true and false) = true
end.
```

## Expressions complexes

### Combiner plusieurs opérateurs

```pascal
program ExpressionsComplexes;  
var  
  a, b, c, resultat: integer;
begin
  a := 10;
  b := 5;
  c := 2;

  // Expression complexe
  resultat := (a + b) * c - (a div b);
  // Étapes :
  // 1. (10 + 5) = 15
  // 2. (10 div 5) = 2
  // 3. 15 * 2 = 30
  // 4. 30 - 2 = 28

  writeln('Résultat : ', resultat);  // 28
end.
```

### Expressions logiques complexes

```pascal
program ConditionsComplexes;  
var  
  age: integer;
  aPermis: boolean;
  aVoiture: boolean;
  peutPartirEnVacances: boolean;
begin
  age := 25;
  aPermis := true;
  aVoiture := false;

  // Condition complexe
  peutPartirEnVacances := (age >= 18) and (aPermis or aVoiture);
  // Étapes :
  // 1. (age >= 18) = true
  // 2. (aPermis or aVoiture) = (true or false) = true
  // 3. true and true = true

  writeln('Peut partir en vacances : ', peutPartirEnVacances);
end.
```

## Exemples pratiques complets

### Exemple 1 : Calculatrice simple

```pascal
program CalculatriceSimple;  
var  
  a, b: real;
  choix: integer;
  resultat: real;
begin
  a := 15.0;
  b := 4.0;
  choix := 1;  // 1=addition, 2=soustraction, 3=multiplication, 4=division

  // :0:2 formate un real : 0 = largeur minimale, 2 = decimales
  writeln('Nombre 1 : ', a:0:2);
  writeln('Nombre 2 : ', b:0:2);
  writeln;

  // Addition
  if choix = 1 then
  begin
    resultat := a + b;
    writeln('Résultat : ', a:0:2, ' + ', b:0:2, ' = ', resultat:0:2);
  end;

  // Soustraction
  if choix = 2 then
  begin
    resultat := a - b;
    writeln('Résultat : ', a:0:2, ' - ', b:0:2, ' = ', resultat:0:2);
  end;

  // Multiplication
  if choix = 3 then
  begin
    resultat := a * b;
    writeln('Résultat : ', a:0:2, ' * ', b:0:2, ' = ', resultat:0:2);
  end;

  // Division
  if choix = 4 then
  begin
    if b <> 0 then
    begin
      resultat := a / b;
      writeln('Résultat : ', a:0:2, ' / ', b:0:2, ' = ', resultat:0:2);
    end
    else
      writeln('Erreur : division par zéro !');
  end;
end.
```

### Exemple 2 : Validation de conditions multiples

```pascal
program ValidationInscription;  
var  
  age: integer;
  aParent: boolean;
  estResident: boolean;
  peutSInscrire: boolean;
begin
  age := 16;
  aParent := true;
  estResident := true;

  // Conditions :
  // - Avoir 18 ans OU avoir l'accord d'un parent
  // - ET être résident
  peutSInscrire := ((age >= 18) or aParent) and estResident;

  writeln('=== VALIDATION INSCRIPTION ===');
  writeln('Âge : ', age, ' ans');
  writeln('Accord parental : ', aParent);
  writeln('Résident : ', estResident);
  writeln('Peut s''inscrire : ', peutSInscrire);
end.
```

### Exemple 3 : Calcul de remise

```pascal
program CalculRemise;  
const  
  SeuilRemise1 = 50.0;
  SeuilRemise2 = 100.0;
  TauxRemise1 = 5.0;   // 5%
  TauxRemise2 = 10.0;  // 10%
var
  montantAchat: real;
  remise: real;
  montantFinal: real;
  aRemise: boolean;
begin
  montantAchat := 75.0;

  // Calcul de la remise selon le montant
  if montantAchat >= SeuilRemise2 then
    remise := montantAchat * TauxRemise2 / 100
  else if montantAchat >= SeuilRemise1 then
    remise := montantAchat * TauxRemise1 / 100
  else
    remise := 0;

  montantFinal := montantAchat - remise;
  aRemise := remise > 0;

  writeln('=== CALCUL DE REMISE ===');
  writeln('Montant d''achat : ', montantAchat:0:2, ' €');
  writeln('Remise : ', remise:0:2, ' €');
  writeln('Montant final : ', montantFinal:0:2, ' €');
  writeln('Remise appliquée : ', aRemise);
end.
```

### Exemple 4 : Test de divisibilité

```pascal
program TestDivisibilite;  
var  
  nombre: integer;
  divisiblePar2: boolean;
  divisiblePar3: boolean;
  divisiblePar5: boolean;
  divisiblePar10: boolean;
begin
  nombre := 30;

  // mod retourne le reste de la division entiere
  // Si le reste vaut 0, le nombre est divisible
  divisiblePar2 := (nombre mod 2) = 0;
  divisiblePar3 := (nombre mod 3) = 0;
  divisiblePar5 := (nombre mod 5) = 0;
  divisiblePar10 := (nombre mod 10) = 0;

  writeln('=== TEST DE DIVISIBILITÉ ===');
  writeln('Nombre : ', nombre);
  writeln('Divisible par 2 : ', divisiblePar2);
  writeln('Divisible par 3 : ', divisiblePar3);
  writeln('Divisible par 5 : ', divisiblePar5);
  writeln('Divisible par 10 : ', divisiblePar10);

  // Un nombre divisible par 10 est forcément divisible par 2 et 5
  writeln;
  writeln('Vérification : ', divisiblePar10 = (divisiblePar2 and divisiblePar5));
end.
```

## Erreurs courantes à éviter

### 1. Confondre = et :=

```pascal
var
  x: integer;
begin
  x = 10;       // ERREUR : utiliser :=
  x := 10;      // CORRECT (affectation)

  if x = 10 then   // CORRECT (comparaison)
    writeln('x vaut 10');
end.
```

### 2. Oublier les parenthèses dans les expressions complexes

```pascal
var
  moyenne: real;
begin
  moyenne := 10 + 15 + 12 / 3;   // FAUX : 10 + 15 + 4 = 29
  moyenne := (10 + 15 + 12) / 3; // CORRECT : 37 / 3 = 12.33
end.
```

### 3. Division par zéro

```pascal
var
  resultat: real;
  diviseur: integer;
begin
  diviseur := 0;
  resultat := 10 / diviseur;     // ERREUR : division par zéro !

  // Toujours vérifier avant de diviser
  if diviseur <> 0 then
    resultat := 10 / diviseur;
end.
```

### 4. Utiliser div avec des reals

```pascal
var
  a, b: real;
  resultat: integer;
begin
  a := 10.5;
  b := 3.2;
  resultat := a div b;           // ERREUR : div n'existe que pour integer
  resultat := trunc(a / b);      // CORRECT
end.
```

### 5. Mauvais ordre de priorité avec and/or

```pascal
var
  resultat: boolean;
begin
  // Attention : and a priorité sur or
  resultat := true or false and false;   // true (pas false !)
  // Équivalent à : true or (false and false)

  // Mieux : utilisez des parenthèses
  resultat := (true or false) and false; // false (intention claire)
end.
```

## Récapitulatif

**Opérateurs arithmétiques :**
- `+`, `-`, `*` : addition, soustraction, multiplication
- `/` : division décimale (résultat toujours real)
- `div` : division entière (integers uniquement)
- `mod` : reste de la division (integers uniquement)

**Opérateurs de comparaison :**
- `=`, `<>` : égal, différent
- `<`, `>` : plus petit, plus grand
- `<=`, `>=` : plus petit ou égal, plus grand ou égal
- Retournent toujours un boolean

**Opérateurs logiques :**
- `and` : vrai si les deux conditions sont vraies
- `or` : vrai si au moins une condition est vraie
- `not` : inverse la valeur
- `xor` : vrai si exactement une condition est vraie

**Priorité des opérateurs :**
1. Parenthèses
2. NOT, négation
3. *, /, div, mod, and
4. +, -, or, xor
5. Comparaisons

**En cas de doute, utilisez des parenthèses !**

---

**Point clé :** Les opérateurs sont les outils de base pour manipuler les données. Maîtriser leur fonctionnement et leur priorité est essentiel pour écrire des expressions correctes. N'hésitez jamais à utiliser des parenthèses pour clarifier vos intentions !

⏭️ [Entrées/Sorties console (Read, Write, ReadLn, WriteLn)](/02-introduction-langage-pascal/06-entrees-sorties-console.md)
