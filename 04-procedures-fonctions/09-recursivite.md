🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.9 Récursivité

## Introduction

La **récursivité** est une technique de programmation où une fonction **s'appelle elle-même** pour résoudre un problème. C'est un concept qui peut sembler étrange au début, mais qui est très puissant pour certains types de problèmes.

## Analogie de la vie réelle

Imaginez que vous cherchez un livre dans une pile :
1. Regardez le livre du dessus
2. Si c'est le bon livre → Terminé !
3. Sinon → Enlevez-le et **recommencez avec la pile restante** (étape 1)

Vous utilisez la **même méthode** sur un **problème plus petit** (la pile avec un livre en moins). C'est ça, la récursivité !

## Qu'est-ce qu'une fonction récursive ?

Une **fonction récursive** est une fonction qui s'appelle elle-même, directement ou indirectement.

### Exemple simple (conceptuel)

```pascal
procedure CompterARebours(n: Integer);
begin
  WriteLn(n);
  if n > 0 then
    CompterARebours(n - 1);  // La fonction s'appelle elle-même
end;

begin
  CompterARebours(5);
end.
```

**Résultat :**
```
5
4
3
2
1
0
```

**Ce qui se passe :**
1. `CompterARebours(5)` affiche 5, puis appelle `CompterARebours(4)`
2. `CompterARebours(4)` affiche 4, puis appelle `CompterARebours(3)`
3. `CompterARebours(3)` affiche 3, puis appelle `CompterARebours(2)`
4. `CompterARebours(2)` affiche 2, puis appelle `CompterARebours(1)`
5. `CompterARebours(1)` affiche 1, puis appelle `CompterARebours(0)`
6. `CompterARebours(0)` affiche 0, puis ne s'appelle plus (condition `n > 0` est fausse)

## Les deux éléments essentiels

Toute fonction récursive doit avoir **DEUX éléments obligatoires** :

### 1. Le cas de base (condition d'arrêt)

C'est la condition qui **arrête** la récursivité. Sans elle, la fonction s'appellerait indéfiniment !

```pascal
if n = 0 then  // Cas de base
begin
  // On ne s'appelle plus récursivement
  Result := ...;
end;
```

### 2. Le cas récursif

C'est l'appel de la fonction **sur un problème plus petit**.

```pascal
else  // Cas récursif
begin
  Result := ... + MaFonction(n - 1);  // Problème plus petit
end;
```

## Exemple classique : la factorielle

La **factorielle** d'un nombre n (notée n!) est le produit de tous les entiers de 1 à n :
- 5! = 5 × 4 × 3 × 2 × 1 = 120
- 4! = 4 × 3 × 2 × 1 = 24
- 1! = 1
- 0! = 1 (par définition)

### Définition mathématique récursive

```
n! = 1              si n = 0 (cas de base)
n! = n × (n-1)!     si n > 0 (cas récursif)
```

### Implémentation en Pascal

```pascal
function Factorielle(n: Integer): Int64;
begin
  if n = 0 then
    Result := 1  // Cas de base
  else
    Result := n * Factorielle(n - 1);  // Cas récursif
end;

begin
  WriteLn('5! = ', Factorielle(5));  // 120
  WriteLn('3! = ', Factorielle(3));  // 6
  WriteLn('0! = ', Factorielle(0));  // 1
end.
```

### Déroulement de Factorielle(5)

```
Factorielle(5)
  = 5 × Factorielle(4)
      = 4 × Factorielle(3)
          = 3 × Factorielle(2)
              = 2 × Factorielle(1)
                  = 1 × Factorielle(0)
                      = 1  ← Cas de base

Remontée :
                      = 1
                  = 1 × 1 = 1
              = 2 × 1 = 2
          = 3 × 2 = 6
      = 4 × 6 = 24
  = 5 × 24 = 120
```

## Exemple : puissance

Calculer x^n (x à la puissance n) :
- x^0 = 1 (cas de base)
- x^n = x × x^(n-1) (cas récursif)

```pascal
function Puissance(x, n: Integer): Int64;
begin
  if n = 0 then
    Result := 1  // Cas de base
  else
    Result := x * Puissance(x, n - 1);  // Cas récursif
end;

begin
  WriteLn('2^5 = ', Puissance(2, 5));   // 32
  WriteLn('3^4 = ', Puissance(3, 4));   // 81
  WriteLn('5^0 = ', Puissance(5, 0));   // 1
end.
```

## Exemple : somme des nombres de 1 à n

Calculer 1 + 2 + 3 + ... + n :

```pascal
function Somme(n: Integer): Integer;
begin
  if n = 0 then
    Result := 0  // Cas de base
  else
    Result := n + Somme(n - 1);  // Cas récursif
end;

begin
  WriteLn('Somme(5) = ', Somme(5));  // 1+2+3+4+5 = 15
  WriteLn('Somme(10) = ', Somme(10));  // 55
end.
```

### Déroulement de Somme(5)

```
Somme(5) = 5 + Somme(4)
             = 4 + Somme(3)
                 = 3 + Somme(2)
                     = 2 + Somme(1)
                         = 1 + Somme(0)
                             = 0  ← Cas de base

Remontée :
= 5 + 4 + 3 + 2 + 1 + 0 = 15
```

## Exemple : suite de Fibonacci

La suite de Fibonacci : 0, 1, 1, 2, 3, 5, 8, 13, 21...

Définition :
- F(0) = 0 (cas de base)
- F(1) = 1 (cas de base)
- F(n) = F(n-1) + F(n-2) pour n ≥ 2 (cas récursif)

```pascal
function Fibonacci(n: Integer): Integer;
begin
  if n = 0 then
    Result := 0  // Premier cas de base
  else if n = 1 then
    Result := 1  // Deuxième cas de base
  else
    Result := Fibonacci(n - 1) + Fibonacci(n - 2);  // Cas récursif
end;

var
  i: Integer;
begin
  Write('Fibonacci : ');
  for i := 0 to 10 do
    Write(Fibonacci(i), ' ');
  WriteLn;
end.
```

**Résultat :** `0 1 1 2 3 5 8 13 21 34 55`

## Récursivité vs Itération

La plupart des problèmes récursifs peuvent aussi être résolus avec des boucles (itération).

### Factorielle : version itérative

```pascal
function FactorielleIterative(n: Integer): Int64;
var
  i: Integer;
  resultat: Int64;
begin
  resultat := 1;
  for i := 1 to n do
    resultat := resultat * i;
  Result := resultat;
end;
```

### Factorielle : version récursive

```pascal
function FactorielleRecursive(n: Integer): Int64;
begin
  if n = 0 then
    Result := 1
  else
    Result := n * FactorielleRecursive(n - 1);
end;
```

### Comparaison

| Critère | Récursivité | Itération |
|---------|-------------|-----------|
| **Lisibilité** | Souvent plus claire et élégante | Peut être plus verbeuse |
| **Performance** | Plus lente (appels de fonction) | Plus rapide |
| **Mémoire** | Consomme plus (pile d'appels) | Consomme moins |
| **Risque** | Débordement de pile si trop profond | Pas de risque |
| **Adaptation** | Naturelle pour problèmes récursifs | Parfois moins intuitive |

## Comment ça fonctionne en mémoire : la pile d'appels

Chaque appel de fonction est stocké dans une **pile d'appels** en mémoire.

### Exemple visuel avec Factorielle(3)

```
Appel initial :
[Factorielle(3)]

Après premier appel récursif :
[Factorielle(3)]
[Factorielle(2)]

Après deuxième appel récursif :
[Factorielle(3)]
[Factorielle(2)]
[Factorielle(1)]

Après troisième appel récursif :
[Factorielle(3)]
[Factorielle(2)]
[Factorielle(1)]
[Factorielle(0)] ← Cas de base atteint, retourne 1

Remontée (dépilage) :
[Factorielle(3)]
[Factorielle(2)]
[Factorielle(1)] → retourne 1 × 1 = 1

[Factorielle(3)]
[Factorielle(2)] → retourne 2 × 1 = 2

[Factorielle(3)] → retourne 3 × 2 = 6

Pile vide, résultat final : 6
```

## Exemples pratiques avancés

### 1. Inversion d'une chaîne

```pascal
function InverserChaine(const s: String): String;
begin
  if Length(s) <= 1 then
    Result := s  // Cas de base
  else
    Result := s[Length(s)] + InverserChaine(Copy(s, 1, Length(s) - 1));
end;

begin
  WriteLn(InverserChaine('Bonjour'));  // ruojnoB
  WriteLn(InverserChaine('Pascal'));   // lacsaP
end.
```

### 2. Calcul du PGCD (Plus Grand Commun Diviseur)

Algorithme d'Euclide :
- PGCD(a, 0) = a (cas de base)
- PGCD(a, b) = PGCD(b, a mod b) (cas récursif)

```pascal
function PGCD(a, b: Integer): Integer;
begin
  if b = 0 then
    Result := a  // Cas de base
  else
    Result := PGCD(b, a mod b);  // Cas récursif
end;

begin
  WriteLn('PGCD(48, 18) = ', PGCD(48, 18));  // 6
  WriteLn('PGCD(100, 35) = ', PGCD(100, 35));  // 5
end.
```

### 3. Nombre de chiffres dans un entier

```pascal
function NombreChiffres(n: Integer): Integer;
begin
  if n < 10 then
    Result := 1  // Cas de base
  else
    Result := 1 + NombreChiffres(n div 10);  // Cas récursif
end;

begin
  WriteLn('Chiffres dans 12345 : ', NombreChiffres(12345));  // 5
  WriteLn('Chiffres dans 7 : ', NombreChiffres(7));          // 1
end.
```

### 4. Affichage d'un tableau à l'envers

```pascal
procedure AfficherTableauEnvers(const tab: array of Integer; index: Integer);
begin
  if index >= 0 then
  begin
    WriteLn(tab[index]);
    AfficherTableauEnvers(tab, index - 1);  // Cas récursif
  end;
  // Cas de base : index < 0, ne fait rien
end;

var
  nombres: array[0..4] of Integer = (10, 20, 30, 40, 50);
begin
  AfficherTableauEnvers(nombres, High(nombres));
end.
```

**Résultat :**
```
50
40
30
20
10
```

### 5. Conversion décimal vers binaire

```pascal
uses SysUtils;

function DecimalVersBinaire(n: Integer): String;
begin
  if n = 0 then
    Result := '0'  // Cas de base
  else if n = 1 then
    Result := '1'  // Cas de base
  else
    Result := DecimalVersBinaire(n div 2) + IntToStr(n mod 2);  // Cas récursif
end;

begin
  WriteLn('5 en binaire : ', DecimalVersBinaire(5));    // 101
  WriteLn('10 en binaire : ', DecimalVersBinaire(10));  // 1010
  WriteLn('15 en binaire : ', DecimalVersBinaire(15));  // 1111
end.
```

## Récursivité multiple

Une fonction peut s'appeler **plusieurs fois** dans le cas récursif (comme Fibonacci).

```pascal
function Fibonacci(n: Integer): Integer;
begin
  if (n = 0) or (n = 1) then
    Result := n
  else
    Result := Fibonacci(n - 1) + Fibonacci(n - 2);  // Deux appels récursifs
end;
```

**Attention :** La récursivité multiple peut être très inefficace pour certains problèmes (comme Fibonacci).

### Arbre d'appels pour Fibonacci(5)

```
                    Fib(5)
                   /      \
              Fib(4)      Fib(3)
              /    \      /    \
          Fib(3)  Fib(2) Fib(2) Fib(1)
          /   \    /  \   /  \
      Fib(2) Fib(1) ...  ...  ...
      /   \
   Fib(1) Fib(0)
```

Beaucoup de calculs sont **répétés** ! Fib(2) est calculé 3 fois, Fib(3) 2 fois...

## Erreurs courantes à éviter

### 1. Oublier le cas de base

```pascal
// ❌ ERREUR : pas de cas de base
function Mauvais(n: Integer): Integer;
begin
  Result := n + Mauvais(n - 1);  // S'appelle indéfiniment !
end;

// ✅ CORRECT : avec cas de base
function Correct(n: Integer): Integer;
begin
  if n = 0 then
    Result := 0  // Cas de base
  else
    Result := n + Correct(n - 1);
end;
```

### 2. Cas de base jamais atteint

```pascal
// ❌ ERREUR : le cas de base (n = 0) n'est jamais atteint si n est impair
function Mauvais(n: Integer): Integer;
begin
  if n = 0 then
    Result := 0
  else
    Result := n + Mauvais(n - 2);  // Saute le 0 si n est impair !
end;

// ✅ CORRECT
function Correct(n: Integer): Integer;
begin
  if n <= 0 then  // Condition plus large
    Result := 0
  else
    Result := n + Correct(n - 2);
end;
```

### 3. Ne pas réduire le problème

```pascal
// ❌ ERREUR : le problème ne devient pas plus petit
function Mauvais(n: Integer): Integer;
begin
  if n = 0 then
    Result := 0
  else
    Result := n + Mauvais(n);  // Appelle avec le même n !
end;
```

### 4. Débordement de pile (stack overflow)

```pascal
// ⚠️ DANGER : récursion trop profonde
function TropProfond(n: Integer): Integer;
begin
  if n = 0 then
    Result := 0
  else
    Result := n + TropProfond(n - 1);
end;

begin
  WriteLn(TropProfond(100000));  // ❌ ERREUR : Stack overflow !
end;
```

**Solution :** Utiliser l'itération pour les problèmes nécessitant beaucoup d'appels.

## Quand utiliser la récursivité ?

### ✅ La récursivité est appropriée pour :

1. **Problèmes naturellement récursifs**
   - Parcours d'arbres
   - Calculs mathématiques récursifs
   - Algorithmes de type "diviser pour régner"

2. **Code plus lisible**
   - Quand la version récursive est beaucoup plus claire
   - Tours de Hanoï, parcours de répertoires, etc.

3. **Structures de données récursives**
   - Listes chaînées
   - Arbres
   - Graphes

### ❌ Évitez la récursivité pour :

1. **Calculs simples** pouvant se faire avec une boucle
2. **Récursions très profondes** (risque de débordement)
3. **Situations critiques en performance**
4. **Problèmes avec beaucoup de calculs répétés** (sans optimisation)

## Exemple complet : Tours de Hanoï

Problème classique où la récursivité est naturelle et élégante.

**Règle :** Déplacer n disques d'une tour A vers une tour C en utilisant une tour intermédiaire B.

```pascal
program ToursHanoi;

procedure DeplacerDisques(n: Integer; source, destination, auxiliaire: Char);
begin
  if n = 1 then
  begin
    // Cas de base : déplacer un seul disque
    WriteLn('Déplacer disque de ', source, ' vers ', destination);
  end
  else
  begin
    // Cas récursif
    // 1. Déplacer n-1 disques de source vers auxiliaire
    DeplacerDisques(n - 1, source, auxiliaire, destination);

    // 2. Déplacer le disque restant de source vers destination
    WriteLn('Déplacer disque de ', source, ' vers ', destination);

    // 3. Déplacer n-1 disques d'auxiliaire vers destination
    DeplacerDisques(n - 1, auxiliaire, destination, source);
  end;
end;

begin
  WriteLn('Solution pour 3 disques :');
  DeplacerDisques(3, 'A', 'C', 'B');
end.
```

**Résultat :**
```
Solution pour 3 disques :
Déplacer disque de A vers C
Déplacer disque de A vers B
Déplacer disque de C vers B
Déplacer disque de A vers C
Déplacer disque de B vers A
Déplacer disque de B vers C
Déplacer disque de A vers C
```

## Bonnes pratiques

1. **Toujours avoir un cas de base clair**
2. **S'assurer que le problème diminue** à chaque appel
3. **Tester avec de petites valeurs** d'abord
4. **Documenter** la logique récursive
5. **Considérer l'alternative itérative** si la performance est critique
6. **Éviter** les récursions trop profondes
7. **Attention** aux récursions multiples inefficaces

## Points clés à retenir

1. Une fonction **récursive** s'appelle elle-même
2. **Deux éléments obligatoires** : cas de base et cas récursif
3. Le **cas de base** arrête la récursivité
4. Le **cas récursif** réduit le problème
5. Chaque appel est stocké dans la **pile d'appels**
6. La récursivité est **élégante** mais peut être **moins performante**
7. Risque de **débordement de pile** si trop profond
8. Utile pour les **problèmes naturellement récursifs**
9. Souvent une **alternative itérative** existe
10. **Tester** avec de petites valeurs pour comprendre le comportement

---

**Prochaine étape :** Dans la section 4.10, nous découvrirons les **fonctions prédéfinies utiles** fournies par Pascal et FreePascal pour faciliter le développement.

⏭️ [Fonctions prédéfinies utiles](/04-procedures-fonctions/10-fonctions-predefinies-utiles.md)
