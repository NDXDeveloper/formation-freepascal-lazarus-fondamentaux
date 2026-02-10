🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.2 Déclaration et Utilisation de Pointeurs

## Introduction

Maintenant que vous comprenez le concept de pointeur et d'adresse mémoire, nous allons explorer en détail comment déclarer et utiliser les pointeurs dans vos programmes FreePascal. Cette section vous montrera toutes les façons de travailler avec les pointeurs de manière pratique et sécurisée.

## Déclaration de Pointeurs

### Syntaxe de Base

La déclaration d'un pointeur suit toujours le même schéma :

```pascal
var
  nomDuPointeur: ^TypePointé;
```

Le symbole `^` placé avant le type signifie "pointeur vers".

### Exemples de Déclarations

```pascal
var
  // Pointeurs vers des types simples
  pEntier: ^Integer;
  pReel: ^Real;
  pCaractere: ^Char;
  pBooleen: ^Boolean;
  pChaine: ^String;

  // Pointeurs vers des types complexes
  pTableau: ^array[1..10] of Integer;
  pEnregistrement: ^TPerson;  // TPerson défini ailleurs
```

### Déclarations Multiples

Vous pouvez déclarer plusieurs pointeurs du même type sur une seule ligne :

```pascal
var
  p1, p2, p3: ^Integer;  // Trois pointeurs vers Integer
```

**Attention :** Chaque pointeur est une variable distincte avec sa propre adresse !

## Types de Pointeurs Personnalisés

Pour améliorer la lisibilité et la maintenance de votre code, vous pouvez créer des types de pointeurs personnalisés :

```pascal
type
  PInteger = ^Integer;      // P comme "Pointer"
  PReal = ^Real;
  PString = ^String;

var
  nombre: Integer;
  pNombre: PInteger;        // Utilisation du type personnalisé
begin
  nombre := 42;
  pNombre := @nombre;
end;
```

**Convention de nommage :** En Pascal, on préfixe souvent les types de pointeurs avec la lettre `P` majuscule.

## Initialisation des Pointeurs

### Initialisation à nil

**Règle d'or :** Toujours initialiser un pointeur avant de l'utiliser !

```pascal
var
  p: ^Integer;
begin
  p := nil;  // Initialisation sécurisée

  // Vérification avant utilisation
  if p <> nil then
    WriteLn(p^)
  else
    WriteLn('Pointeur non initialisé');
end;
```

### Initialisation vers une variable existante

```pascal
var
  valeur: Integer;
  p: ^Integer;
begin
  valeur := 100;
  p := @valeur;  // p pointe maintenant vers valeur

  WriteLn('Valeur via pointeur : ', p^);  // Affiche 100
end;
```

## Utilisation des Pointeurs

### Lecture de Valeur

Pour lire la valeur pointée, utilisez l'opérateur de déréférencement `^` :

```pascal
var
  temperature: Real;
  pTemp: ^Real;
begin
  temperature := 22.5;
  pTemp := @temperature;

  // Lecture via le pointeur
  WriteLn('Température : ', pTemp^:0:1, '°C');
end;
```

### Modification de Valeur

Vous pouvez modifier la valeur pointée directement via le pointeur :

```pascal
var
  score: Integer;
  pScore: ^Integer;
begin
  score := 0;
  pScore := @score;

  WriteLn('Score initial : ', score);      // Affiche 0

  pScore^ := 100;                          // Modification via pointeur
  WriteLn('Score après modification : ', score);  // Affiche 100

  // Le pointeur et la variable accèdent à la même case mémoire
  score := 200;
  WriteLn('Valeur via pointeur : ', pScore^);     // Affiche 200
end;
```

### Visualisation en Mémoire

```
État initial :
┌───────────┬────────┬─────────┐
│  Adresse  │  Nom   │ Valeur  │
├───────────┼────────┼─────────┤
│ $00001000 │ score  │    0    │
│ $00001004 │ pScore │  $1000  │ ← pointe vers score
└───────────┴────────┴─────────┘

Après pScore^ := 100 :
┌───────────┬────────┬─────────┐
│  Adresse  │  Nom   │ Valeur  │
├───────────┼────────┼─────────┤
│ $00001000 │ score  │   100   │ ← modifié
│ $00001004 │ pScore │  $1000  │
└───────────┴────────┴─────────┘
```

## Pointeurs et Procédures/Fonctions

### Passage de Pointeur en Paramètre

Les pointeurs sont particulièrement utiles pour passer des données volumineuses aux fonctions sans les copier :

```pascal
program PassagePointeurParametre;  
type  
  TGrosseStructure = record
    donnees: array[1..1000] of Integer;
  end;
  PGrosseStructure = ^TGrosseStructure;

procedure TraiterDonnees(p: PGrosseStructure);  
begin  
  // Accès direct aux données via le pointeur
  if p <> nil then
  begin
    p^.donnees[1] := 999;  // p^ déréférence, puis .donnees accède au champ du record
    WriteLn('Première donnée modifiée');
  end;
end;

var
  maStructure: TGrosseStructure;
begin
  TraiterDonnees(@maStructure);
  WriteLn(maStructure.donnees[1]);  // Affiche 999
end.
```

**Avantage :** Pas de copie de 1000 entiers, seule l'adresse est passée !

### Retourner un Pointeur

Une fonction peut retourner un pointeur :

```pascal
{$mode objfpc}{$H+}
program RetournerPointeur;  
type  
  PInteger = ^Integer;

function TrouverMaximum(var a, b: Integer): PInteger;  // var obligatoire : sans var, @a pointerait vers une copie locale détruite au retour  
begin  
  if a > b then
    Result := @a
  else
    Result := @b;
end;

var
  x, y: Integer;
  pMax: PInteger;
begin
  x := 10;
  y := 20;

  pMax := TrouverMaximum(x, y);
  WriteLn('Maximum : ', pMax^);  // Affiche 20
end.
```

**⚠️ Attention :** Ne jamais retourner un pointeur vers une variable locale (nous verrons pourquoi plus tard) !

## Opérations avec les Pointeurs

### Comparaison de Pointeurs

Vous pouvez comparer deux pointeurs pour savoir s'ils pointent vers la même adresse :

```pascal
var
  a, b: Integer;
  p1, p2, p3: ^Integer;
begin
  a := 10;
  b := 10;

  p1 := @a;
  p2 := @a;
  p3 := @b;

  if p1 = p2 then
    WriteLn('p1 et p2 pointent vers la même adresse');  // VRAI

  if p1 = p3 then
    WriteLn('p1 et p3 pointent vers la même adresse')   // FAUX
  else
    WriteLn('p1 et p3 pointent vers des adresses différentes');
end;
```

**Important :** `p1 = p3` compare les **adresses**, pas les valeurs pointées !

### Affectation de Pointeurs

Un pointeur peut être affecté à un autre pointeur du même type :

```pascal
var
  valeur: Integer;
  p1, p2: ^Integer;
begin
  valeur := 42;
  p1 := @valeur;
  p2 := p1;  // p2 pointe maintenant vers la même adresse que p1

  WriteLn(p2^);  // Affiche 42
end;
```

```
Avant p2 := p1 :
┌───────────┬────────┬──────────┐
│  Adresse  │  Nom   │  Valeur  │
├───────────┼────────┼──────────┤
│ $00001000 │ valeur │    42    │
│ $00001004 │   p1   │  $1000   │ ← pointe vers valeur
│ $00001008 │   p2   │   nil    │
└───────────┴────────┴──────────┘

Après p2 := p1 :
┌───────────┬────────┬──────────┐
│  Adresse  │  Nom   │  Valeur  │
├───────────┼────────┼──────────┤
│ $00001000 │ valeur │    42    │
│ $00001004 │   p1   │  $1000   │ ← pointe vers valeur
│ $00001008 │   p2   │  $1000   │ ← pointe aussi vers valeur
└───────────┴────────┴──────────┘
```

## Pointeurs vers Différents Types

### Pointeurs vers Chaînes de Caractères

```pascal
var
  nom: String;
  pNom: ^String;
begin
  nom := 'Alice';
  pNom := @nom;

  WriteLn('Nom : ', pNom^);           // Affiche Alice
  WriteLn('Longueur : ', Length(pNom^));  // Affiche 5

  pNom^ := 'Bob';
  WriteLn('Nouveau nom : ', nom);     // Affiche Bob
end;
```

### Pointeurs vers Enregistrements

```pascal
type
  TPerson = record
    nom: String;
    age: Integer;
  end;
  PPerson = ^TPerson;

var
  personne: TPerson;
  pPers: PPerson;
begin
  personne.nom := 'Marie';
  personne.age := 30;

  pPers := @personne;

  // Accès aux champs via le pointeur
  WriteLn('Nom : ', pPers^.nom);
  WriteLn('Age : ', pPers^.age);

  // Modification
  pPers^.age := 31;
  WriteLn('Nouvel age : ', personne.age);  // Affiche 31
end;
```

### Pointeurs vers Tableaux

```pascal
var
  nombres: array[1..5] of Integer;
  pNombres: ^array[1..5] of Integer;
  i: Integer;
begin
  // Initialisation du tableau
  for i := 1 to 5 do
    nombres[i] := i * 10;

  pNombres := @nombres;

  // Accès via le pointeur
  for i := 1 to 5 do
    WriteLn('Element ', i, ' : ', pNombres^[i]);

  // Modification
  pNombres^[3] := 999;
  WriteLn('Nouvel element 3 : ', nombres[3]);  // Affiche 999
end;
```

## Cas d'Usage Pratiques

### Partage de Données entre Fonctions

Au lieu de passer de grosses structures en paramètre :

```pascal
program PartageDonnees;  
type  
  TConfiguration = record
    serveur: String;
    port: Integer;
    timeout: Integer;
  end;
  PConfiguration = ^TConfiguration;

procedure AfficherConfig(cfg: PConfiguration);  
begin  
  if cfg <> nil then
  begin
    WriteLn('Serveur : ', cfg^.serveur);
    WriteLn('Port : ', cfg^.port);
  end;
end;

procedure ModifierPort(cfg: PConfiguration; nouveauPort: Integer);  
begin  
  if cfg <> nil then
    cfg^.port := nouveauPort;
end;

var
  config: TConfiguration;
begin
  config.serveur := 'localhost';
  config.port := 8080;

  AfficherConfig(@config);
  ModifierPort(@config, 9090);
  AfficherConfig(@config);
end.
```

### Échange de Valeurs

Une fonction classique utilisant des pointeurs :

```pascal
program EchangeValeurs;  
type  
  PInteger = ^Integer;

procedure Echanger(a, b: PInteger);  
var  
  temp: Integer;
begin
  if (a <> nil) and (b <> nil) then
  begin
    temp := a^;
    a^ := b^;
    b^ := temp;
  end;
end;

var
  x, y: Integer;
begin
  x := 5;
  y := 10;

  WriteLn('Avant : x=', x, ' y=', y);
  Echanger(@x, @y);
  WriteLn('Après : x=', x, ' y=', y);
end.
```

## Bonnes Pratiques

### 1. Toujours Initialiser

```pascal
var
  p: ^Integer;
begin
  p := nil;  // ✓ Bon : initialisation explicite

  // ... code ...

  if p <> nil then
    WriteLn(p^);  // Sécurisé
end;
```

### 2. Vérifier Avant Déréférencer

```pascal
procedure Afficher(p: ^Integer);  
begin  
  if p <> nil then        // ✓ Bon : vérification
    WriteLn(p^)
  else
    WriteLn('Pointeur nil');
end;
```

### 3. Nommer Clairement

```pascal
var
  pScore: ^Integer;        // ✓ Bon : préfixe 'p' indique un pointeur
  scorePtr: ^Integer;      // ✓ Bon : suffixe 'Ptr' aussi valable
```

### 4. Documenter l'Ownership

```pascal
// Cette fonction NE libère PAS la mémoire pointée
procedure TraiterDonnees(p: ^TData);  
begin  
  // ...
end;
```

## Erreurs Courantes à Éviter

### 1. Déréférencement sans Vérification

```pascal
var
  p: ^Integer;
begin
  // p := nil;  ← Oubli d'initialisation
  WriteLn(p^);  // ✗ ERREUR : Access Violation !
end;
```

### 2. Confusion entre Adresse et Valeur

```pascal
var
  x: Integer;
  p: ^Integer;
begin
  x := 10;
  p := x;      // ✗ ERREUR : x est une valeur, pas une adresse
  p := @x;     // ✓ CORRECT : @x est l'adresse de x
end;
```

### 3. Oubli du ^

```pascal
var
  a: Integer;
  p: ^Integer;
begin
  a := 10;
  p := @a;

  WriteLn(p);   // ✗ Affiche l'adresse (un nombre bizarre)
  WriteLn(p^);  // ✓ Affiche la valeur (10)
end;
```

## Le Type Pointer Générique

Pascal offre aussi un type `Pointer` générique qui peut pointer vers n'importe quoi :

```pascal
var
  p: Pointer;
  x: Integer;
  s: String;
begin
  x := 42;
  p := @x;  // p peut pointer vers x

  s := 'Hello';
  p := @s;  // p peut aussi pointer vers s

  // ⚠️ Attention : pas de déréférencement direct avec Pointer
  // WriteLn(p^);  // Ne compile pas
end;
```

**Utilisation :** Principalement pour des API de bas niveau. En tant que débutant, préférez les pointeurs typés (`^Integer`, `^String`, etc.).

## Résumé

| Opération | Syntaxe | Description |
|-----------|---------|-------------|
| Déclaration | `p: ^Integer` | Déclare un pointeur vers Integer |
| Obtenir adresse | `p := @variable` | p pointe vers variable |
| Lire valeur | `valeur := p^` | Lit la valeur pointée |
| Modifier valeur | `p^ := nouvelle` | Modifie la valeur pointée |
| Initialiser | `p := nil` | Pointeur vide |
| Comparer | `if p1 = p2` | Compare les adresses |
| Vérifier | `if p <> nil` | Vérifie si pointeur valide |

## Points Clés à Retenir

1. **Toujours initialiser** les pointeurs à `nil` ou vers une adresse valide
2. **Toujours vérifier** qu'un pointeur n'est pas `nil` avant de le déréférencer
3. **L'opérateur @** donne l'adresse d'une variable
4. **L'opérateur ^** accède à la valeur pointée
5. Les pointeurs **partagent** l'accès aux données, ils ne les copient pas
6. **Nommer clairement** les pointeurs (préfixe `p` ou suffixe `Ptr`)

## Prochaine Étape

Vous maîtrisez maintenant la déclaration et l'utilisation basique des pointeurs. La prochaine section abordera l'**allocation dynamique de mémoire** avec `New` et `Dispose`, qui vous permettra de créer des structures de données vraiment flexibles !

⏭️ [Allocation dynamique (New, Dispose)](/06-pointeurs-gestion-memoire-basique/03-allocation-dynamique-new-dispose.md)
