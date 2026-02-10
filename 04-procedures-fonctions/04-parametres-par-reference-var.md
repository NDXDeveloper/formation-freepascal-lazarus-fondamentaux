🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.4 Paramètres par référence (var)

## Introduction

Dans la section précédente, nous avons vu les **paramètres par valeur** qui créent une copie de la variable. Les **paramètres par référence** fonctionnent différemment : ils permettent de **modifier directement** la variable originale.

## Le problème avec les paramètres par valeur

Imaginons que vous voulez créer une procédure qui double un nombre :

```pascal
procedure DoublerNombre(n: Integer);  
begin  
  n := n * 2;
end;

var
  nombre: Integer;
begin
  nombre := 5;
  DoublerNombre(nombre);
  WriteLn(nombre);  // Affiche 5, pas 10 !
end.
```

**Problème :** `nombre` garde sa valeur originale (5) car la procédure a modifié une **copie**, pas l'original.

## La solution : paramètres par référence (var)

En ajoutant le mot-clé `var` devant le paramètre, on indique à Pascal de passer la **référence** (l'adresse mémoire) de la variable plutôt qu'une copie.

```pascal
procedure DoublerNombre(var n: Integer);  
begin  
  n := n * 2;
end;

var
  nombre: Integer;
begin
  nombre := 5;
  DoublerNombre(nombre);
  WriteLn(nombre);  // Affiche 10 !
end.
```

**Résultat :** Maintenant `nombre` vaut bien 10, car la procédure a modifié la variable originale.

## Syntaxe

### Pour une procédure

```pascal
procedure NomProcedure(var nomParametre: Type);  
begin  
  // Modifications de nomParametre affectent la variable originale
end;
```

### Pour une fonction

```pascal
function NomFonction(var nomParametre: Type): TypeRetour;  
begin  
  // Modifications de nomParametre affectent la variable originale
  Result := ...;
end;
```

## Comparaison directe : par valeur vs par référence

### Avec paramètre par valeur (sans var)

```pascal
procedure ModifierParValeur(n: Integer);  
begin  
  n := 100;
  WriteLn('Dans la procédure : ', n);  // 100
end;

var
  x: Integer;
begin
  x := 5;
  WriteLn('Avant : ', x);              // 5
  ModifierParValeur(x);
  WriteLn('Après : ', x);              // 5 (inchangé)
end.
```

### Avec paramètre par référence (avec var)

```pascal
procedure ModifierParReference(var n: Integer);  
begin  
  n := 100;
  WriteLn('Dans la procédure : ', n);  // 100
end;

var
  x: Integer;
begin
  x := 5;
  WriteLn('Avant : ', x);              // 5
  ModifierParReference(x);
  WriteLn('Après : ', x);              // 100 (modifié !)
end.
```

## Comment ça fonctionne ?

### Paramètre par valeur
```
Programme : x = 5
              ↓ (copie)
Procédure : n = 5 (copie indépendante)  
Modification de n n'affecte pas x  
```

### Paramètre par référence
```
Programme : x = 5
              ↓ (référence/adresse)
Procédure : n pointe vers x (même emplacement mémoire)  
Modification de n = modification de x  
```

## Exemples pratiques

### 1. Échanger deux valeurs

```pascal
procedure Echanger(var a, b: Integer);  
var  
  temp: Integer;
begin
  temp := a;
  a := b;
  b := temp;
end;

var
  x, y: Integer;
begin
  x := 10;
  y := 20;
  WriteLn('Avant : x = ', x, ', y = ', y);

  Echanger(x, y);

  WriteLn('Après : x = ', x, ', y = ', y);
end.
```

**Résultat :**
```
Avant : x = 10, y = 20  
Après : x = 20, y = 10  
```

### 2. Incrémenter une variable

```pascal
procedure Incrementer(var n: Integer);  
begin  
  n := n + 1;
end;

var
  compteur: Integer;
begin
  compteur := 0;
  WriteLn('Compteur : ', compteur);  // 0

  Incrementer(compteur);
  WriteLn('Compteur : ', compteur);  // 1

  Incrementer(compteur);
  WriteLn('Compteur : ', compteur);  // 2
end.
```

### 3. Lire des valeurs de l'utilisateur

```pascal
procedure DemanderAge(var age: Integer);  
begin  
  Write('Entrez votre âge : ');
  ReadLn(age);
end;

procedure DemanderNom(var nom: String);  
begin  
  Write('Entrez votre nom : ');
  ReadLn(nom);
end;

var
  monAge: Integer;
  monNom: String;
begin
  DemanderNom(monNom);
  DemanderAge(monAge);

  WriteLn('Bonjour ', monNom, ', vous avez ', monAge, ' ans.');
end.
```

### 4. Calculer plusieurs résultats

Une fonction ne peut retourner qu'une seule valeur. Avec `var`, on peut obtenir plusieurs résultats :

```pascal
procedure CalculerCercle(rayon: Real; var aire, perimetre: Real);  
const  
  PI = 3.14159;
begin
  aire := PI * rayon * rayon;
  perimetre := 2 * PI * rayon;
end;

var
  r, a, p: Real;
begin
  r := 5.0;
  CalculerCercle(r, a, p);

  WriteLn('Rayon : ', r:0:2);
  WriteLn('Aire : ', a:0:2);
  WriteLn('Périmètre : ', p:0:2);
end.
```

**Résultat :**
```
Rayon : 5.00  
Aire : 78.54  
Périmètre : 31.42  
```

### 5. Fonction avec retour + modification de paramètre

```pascal
function Diviser(dividende, diviseur: Integer; var reste: Integer): Integer;  
begin  
  Result := dividende div diviseur;  // Quotient
  reste := dividende mod diviseur;   // Reste
end;

var
  quotient, reste: Integer;
begin
  quotient := Diviser(17, 5, reste);

  WriteLn('17 ÷ 5 = ', quotient);
  WriteLn('Reste : ', reste);
end.
```

**Résultat :**
```
17 ÷ 5 = 3
Reste : 2
```

## Mélanger paramètres par valeur et par référence

On peut combiner les deux types dans une même procédure/fonction :

```pascal
procedure AjouterTaxe(prixHT: Real; tauxTaxe: Real; var prixTTC: Real);  
begin  
  prixTTC := prixHT * (1 + tauxTaxe / 100);
end;

var
  prix: Real;
begin
  AjouterTaxe(100.0, 20.0, prix);
  WriteLn('Prix TTC : ', prix:0:2, ' €');  // 120.00 €
end.
```

**Explication :**
- `prixHT` et `tauxTaxe` : par valeur (lecture seule, pas de modification)
- `prixTTC` : par référence (var) pour récupérer le résultat

## Cas d'usage typiques

### Utilisez `var` quand :

1. **Vous devez modifier la variable originale**
   ```pascal
   procedure Reinitialiser(var compteur: Integer);
   begin
     compteur := 0;
   end;
   ```

2. **Vous devez retourner plusieurs valeurs**
   ```pascal
   procedure ObtenirMinMax(tableau: ...; var min, max: Integer);
   ```

3. **Vous lisez une valeur de l'utilisateur**
   ```pascal
   procedure LireNombre(var n: Integer);
   begin
     ReadLn(n);
   end;
   ```

4. **Vous échangez des valeurs**
   ```pascal
   procedure Echanger(var a, b: Integer);
   ```

5. **Optimisation avec de grandes structures** (on verra ça plus tard)
   ```pascal
   procedure TraiterGrandeStructure(var donnees: TGrandeStructure);
   ```

### N'utilisez PAS `var` quand :

1. Vous voulez seulement **lire** la valeur sans la modifier
2. Vous passez une constante ou une expression
3. La clarté du code en souffrirait

## Contraintes importantes

### 1. On ne peut passer que des variables

```pascal
procedure Modifier(var n: Integer);  
begin  
  n := 10;
end;

var
  x: Integer;
begin
  Modifier(x);        // ✅ CORRECT : x est une variable
  Modifier(5);        // ❌ ERREUR : 5 est une constante
  Modifier(x + 2);    // ❌ ERREUR : x+2 est une expression
end.
```

**Pourquoi ?** On ne peut pas modifier une constante ou une expression !

### 2. Le type doit correspondre exactement

```pascal
procedure ModifierEntier(var n: Integer);  
begin  
  n := 10;
end;

var
  x: Integer;
  y: LongInt;
begin
  ModifierEntier(x);  // ✅ CORRECT
  ModifierEntier(y);  // ❌ ERREUR : LongInt ≠ Integer
end.
```

## Exemple complet : système de coordonnées

```pascal
program GestionCoordonnees;

// Initialiser des coordonnées
procedure InitialiserPosition(var x, y: Integer);  
begin  
  x := 0;
  y := 0;
end;

// Déplacer vers la droite
procedure DeplacerDroite(var x: Integer; distance: Integer);  
begin  
  x := x + distance;
end;

// Déplacer vers le haut
procedure DeplacerHaut(var y: Integer; distance: Integer);  
begin  
  y := y + distance;
end;

// Afficher la position
procedure AfficherPosition(x, y: Integer);  
begin  
  WriteLn('Position actuelle : (', x, ', ', y, ')');
end;

var
  posX, posY: Integer;
begin
  InitialiserPosition(posX, posY);
  AfficherPosition(posX, posY);  // (0, 0)

  DeplacerDroite(posX, 5);
  AfficherPosition(posX, posY);  // (5, 0)

  DeplacerHaut(posY, 3);
  AfficherPosition(posX, posY);  // (5, 3)

  DeplacerDroite(posX, -2);
  AfficherPosition(posX, posY);  // (3, 3)
end.
```

**Résultat :**
```
Position actuelle : (0, 0)  
Position actuelle : (5, 0)  
Position actuelle : (5, 3)  
Position actuelle : (3, 3)  
```

## Erreurs courantes à éviter

### 1. Oublier var alors qu'on veut modifier

```pascal
procedure Doubler(n: Integer);  // ❌ Oubli de var  
begin  
  n := n * 2;
end;

var
  x: Integer;
begin
  x := 5;
  Doubler(x);
  WriteLn(x);  // Affiche 5, pas 10 !
end.
```

**Correction :**
```pascal
procedure Doubler(var n: Integer);  // ✅ Ajout de var
```

### 2. Utiliser var inutilement

```pascal
// ❌ Mauvaise pratique : on ne modifie pas n
function Carre(var n: Integer): Integer;  
begin  
  Result := n * n;
end;

// ✅ Meilleure pratique
function Carre(n: Integer): Integer;  
begin  
  Result := n * n;
end;
```

### 3. Passer une expression avec var

```pascal
procedure Test(var n: Integer);  
begin  
  n := 10;
end;

var
  x: Integer;
begin
  x := 5;
  Test(x + 1);  // ❌ ERREUR : ne compile pas
  Test(x);      // ✅ CORRECT
end.
```

## Tableau comparatif

| Critère | Par valeur | Par référence (var) |
|---------|------------|---------------------|
| **Mot-clé** | Aucun | `var` |
| **Comportement** | Copie de la valeur | Référence à la variable |
| **Modification** | Variable originale inchangée | Variable originale modifiée |
| **Passage** | Variables, constantes, expressions | Variables uniquement |
| **Usage** | Lecture seule | Lecture et écriture |
| **Performance** | Copie (peut être lent pour grandes structures) | Pas de copie (rapide) |

## Bonnes pratiques

1. **Utilisez var uniquement quand nécessaire** : si vous ne modifiez pas le paramètre, n'utilisez pas var
2. **Nommage explicite** : utilisez des noms qui indiquent qu'une modification aura lieu (`Modifier...`, `Incrementer...`, `Reinitialiser...`)
3. **Documentation** : commentez pourquoi vous utilisez var si ce n'est pas évident
4. **Prudence** : var peut rendre le code moins prévisible, utilisez-le judicieusement

## Points clés à retenir

1. Le mot-clé `var` avant un paramètre crée un **paramètre par référence**
2. Avec `var`, les modifications **affectent la variable originale**
3. Sans `var`, on a un **paramètre par valeur** (copie)
4. On ne peut passer que des **variables** avec var (pas de constantes ni d'expressions)
5. Utile pour : modifier des variables, retourner plusieurs valeurs, optimiser
6. Le type doit **correspondre exactement** entre le paramètre et la variable passée
7. **Règle d'or** : utilisez var uniquement quand vous devez vraiment modifier la variable

---

**Prochaine étape :** Dans la section 4.5, nous découvrirons les **paramètres constants (const)** qui offrent un compromis intéressant entre performance et sécurité.

⏭️ [Paramètres constants (const)](/04-procedures-fonctions/05-parametres-constants-const.md)
