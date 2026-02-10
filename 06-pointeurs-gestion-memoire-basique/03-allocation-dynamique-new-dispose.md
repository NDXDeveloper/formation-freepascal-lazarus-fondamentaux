🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.3 Allocation Dynamique (New, Dispose)

## Introduction

Jusqu'à présent, nous avons utilisé des pointeurs pour accéder à des variables déjà existantes. Mais la véritable puissance des pointeurs se révèle avec l'**allocation dynamique de mémoire**, qui permet de créer et détruire des variables pendant l'exécution du programme.

## Qu'est-ce que l'Allocation Dynamique ?

### Mémoire Statique vs Dynamique

Il existe deux façons de réserver de la mémoire :

#### 1. Allocation Statique (ce que nous connaissons déjà)

```pascal
var
  nombre: Integer;  // Mémoire réservée à la compilation
begin
  nombre := 42;
end;
```

**Caractéristiques :**
- La taille et le nombre de variables sont **fixés** à la compilation
- La mémoire est allouée automatiquement au démarrage
- La mémoire est libérée automatiquement à la fin
- **Inflexible** : impossible de créer plus de variables en cours d'exécution

#### 2. Allocation Dynamique (nouvelle méthode)

```pascal
var
  p: ^Integer;  // Déclaration du pointeur
begin
  New(p);       // Création de la variable en mémoire
  p^ := 42;     // Utilisation
  Dispose(p);   // Libération de la mémoire
end;
```

**Caractéristiques :**
- La mémoire est réservée **pendant l'exécution** du programme
- Vous décidez quand créer et détruire les variables
- **Flexible** : vous pouvez créer autant de variables que nécessaire
- **Responsabilité** : vous devez libérer la mémoire vous-même

### Analogie de l'Hôtel

Imaginez la mémoire comme un hôtel :

**Allocation statique :**
- Vous réservez des chambres avant d'arriver
- Le nombre de chambres est fixe
- Les chambres restent réservées tout le séjour

**Allocation dynamique :**
- Vous demandez des chambres à la réception quand vous en avez besoin (`New`)
- Vous libérez les chambres quand vous n'en avez plus besoin (`Dispose`)
- Vous pouvez demander plus de chambres à tout moment
- **Important :** Si vous ne libérez pas les chambres, elles restent occupées inutilement !

## La Procédure New

### Syntaxe

```pascal
New(pointeur);
```

`New` demande au système d'exploitation de réserver un emplacement mémoire pour le type pointé.

### Exemple de Base

```pascal
var
  p: ^Integer;
begin
  New(p);          // Allocation : crée un Integer en mémoire
  p^ := 100;       // Utilisation : stocke la valeur 100
  WriteLn(p^);     // Affiche 100
  Dispose(p);      // Libération : rend la mémoire au système
end;
```

### Ce qui se passe en mémoire

```
Avant New(p) :
┌───────────┬─────────┬─────────┐
│  Adresse  │   Nom   │ Valeur  │
├───────────┼─────────┼─────────┤
│ $00001000 │    p    │   nil   │
└───────────┴─────────┴─────────┘

Après New(p) :
┌───────────┬─────────┬──────────┐
│  Adresse  │   Nom   │  Valeur  │
├───────────┼─────────┼──────────┤
│ $00001000 │    p    │ $0000A000│ ← pointe vers nouvelle zone
├───────────┼─────────┼──────────┤
│ $0000A000 │  (p^)   │    ???   │ ← zone allouée (non initialisée)
└───────────┴─────────┴──────────┘

Après p^ := 100 :
┌───────────┬─────────┬──────────┐
│  Adresse  │   Nom   │  Valeur  │
├───────────┼─────────┼──────────┤
│ $00001000 │    p    │ $0000A000│
├───────────┼─────────┼──────────┤
│ $0000A000 │  (p^)   │   100    │ ← initialisée
└───────────┴─────────┴──────────┘
```

### Initialisation après New

**Important :** `New` alloue la mémoire mais **ne l'initialise pas** ! La zone contient des données aléatoires.

```pascal
var
  p: ^Integer;
begin
  New(p);

  // ✗ DANGER : p^ contient n'importe quoi !
  WriteLn(p^);  // Affiche une valeur aléatoire

  // ✓ BON : Toujours initialiser après New
  p^ := 0;
  WriteLn(p^);  // Affiche 0

  Dispose(p);
end;
```

## La Procédure Dispose

### Syntaxe

```pascal
Dispose(pointeur);
```

`Dispose` libère la mémoire allouée par `New` et la rend au système d'exploitation.

### Règle d'Or

**Pour chaque `New`, il doit y avoir un `Dispose` correspondant !**

```pascal
var
  p: ^Integer;
begin
  New(p);       // ← Allocation
  p^ := 42;
  WriteLn(p^);
  Dispose(p);   // ← Libération obligatoire !
end;
```

### Ce qui se passe avec Dispose

```
Avant Dispose(p) :
┌───────────┬─────────┬──────────┐
│  Adresse  │   Nom   │  Valeur  │
├───────────┼─────────┼──────────┤
│ $00001000 │    p    │ $0000A000│ ← pointe vers zone allouée
├───────────┼─────────┼──────────┤
│ $0000A000 │  (p^)   │    42    │ ← zone utilisée
└───────────┴─────────┴──────────┘

Après Dispose(p) :
┌───────────┬─────────┬──────────┐
│  Adresse  │   Nom   │  Valeur  │
├───────────┼─────────┼──────────┤
│ $00001000 │    p    │ $0000A000│ ← contient encore l'adresse !
├───────────┼─────────┼──────────┤
│ $0000A000 │  (p^)   │  LIBÉRÉ  │ ← zone rendue au système
└───────────┴─────────┴──────────┘
```

**Attention :** Après `Dispose`, le pointeur contient toujours l'ancienne adresse, mais cette zone n'est plus valide !

### Bonne Pratique : Mettre à nil après Dispose

```pascal
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;

  Dispose(p);
  p := nil;  // ✓ Bonne pratique : évite les erreurs

  // Maintenant, on peut vérifier :
  if p <> nil then
    WriteLn(p^)  // Ne s'exécute pas
  else
    WriteLn('Pointeur libéré');
end;
```

## Cycle de Vie Complet d'un Pointeur Dynamique

### Les 4 Étapes

```pascal
program CycleViePointeur;  
var  
  p: ^Integer;
begin
  // ÉTAPE 1 : Déclaration
  // Le pointeur existe mais ne pointe vers rien

  // ÉTAPE 2 : Allocation
  New(p);
  // Le pointeur pointe vers une zone mémoire réservée

  // ÉTAPE 3 : Utilisation
  p^ := 100;
  WriteLn('Valeur : ', p^);
  p^ := p^ + 50;
  WriteLn('Nouvelle valeur : ', p^);

  // ÉTAPE 4 : Libération
  Dispose(p);
  p := nil;
  // La mémoire est rendue au système
end.
```

## Exemples Pratiques

### Allocation de Chaînes Dynamiques

```pascal
program ChaineDynamique;  
var  
  pNom: ^String;
begin
  New(pNom);
  pNom^ := 'Alice Dubois';

  WriteLn('Nom : ', pNom^);
  WriteLn('Longueur : ', Length(pNom^));

  Dispose(pNom);
  pNom := nil;
end.
```

### Allocation d'Enregistrements

```pascal
program RecordDynamique;  
type  
  TPerson = record
    nom: String;
    age: Integer;
  end;
  PPerson = ^TPerson;

var
  personne: PPerson;
begin
  // Allocation
  New(personne);

  // Initialisation
  personne^.nom := 'Jean Martin';
  personne^.age := 35;

  // Utilisation
  WriteLn('Nom : ', personne^.nom);
  WriteLn('Age : ', personne^.age);

  // Libération
  Dispose(personne);
  personne := nil;
end.
```

### Création de Plusieurs Variables Dynamiques

```pascal
program PlusieursDynamiques;  
var  
  p1, p2, p3: ^Integer;
begin
  // Création de 3 entiers dynamiques
  New(p1);
  New(p2);
  New(p3);

  // Initialisation
  p1^ := 10;
  p2^ := 20;
  p3^ := 30;

  WriteLn('Somme : ', p1^ + p2^ + p3^);  // Affiche 60

  // Libération de chacun
  Dispose(p1);
  Dispose(p2);
  Dispose(p3);

  p1 := nil;
  p2 := nil;
  p3 := nil;
end.
```

## Allocation Dynamique dans les Procédures

### Fonction Créant un Objet Dynamique

```pascal
{$mode objfpc}{$H+}
program CreerLibererPersonne;  
type  
  PPerson = ^TPerson;
  TPerson = record
    nom: String;
    age: Integer;
  end;

function CreerPersonne(n: String; a: Integer): PPerson;  
begin  
  New(Result);           // Allocation
  Result^.nom := n;      // Initialisation
  Result^.age := a;
end;

procedure LibererPersonne(var p: PPerson);  // var nécessaire pour que p := nil modifie le pointeur de l'appelant  
begin  
  if p <> nil then
  begin
    Dispose(p);          // Libération
    p := nil;
  end;
end;

var
  pers: PPerson;
begin
  pers := CreerPersonne('Sophie', 28);
  WriteLn(pers^.nom, ' a ', pers^.age, ' ans');

  LibererPersonne(pers);  // Ne pas oublier !
end.
```

**Note :** Le paramètre `var` dans `LibererPersonne` permet de mettre le pointeur à `nil` dans la fonction appelante.

## Pourquoi Utiliser l'Allocation Dynamique ?

### 1. Taille Inconnue à la Compilation

```pascal
var
  n, i: Integer;
  tableau: ^array of Integer;  // Taille variable
begin
  Write('Combien d''éléments ? ');
  ReadLn(n);

  // Impossible avec un tableau statique !
  SetLength(tableau, n);  // Allocation dynamique

  for i := 0 to n-1 do
    tableau[i] := i * 2;

  // ... utilisation ...

  SetLength(tableau, 0);  // Libération
end;
```

### 2. Structures de Données Flexibles

Création de listes, arbres, graphes qui grandissent selon les besoins :

```pascal
type
  PNoeud = ^TNoeud;
  TNoeud = record
    valeur: Integer;
    suivant: PNoeud;  // Pointeur vers le noeud suivant
  end;

var
  premier, nouveau: PNoeud;
begin
  // Création du premier noeud
  New(premier);
  premier^.valeur := 1;
  premier^.suivant := nil;

  // Ajout d'un deuxième noeud
  New(nouveau);
  nouveau^.valeur := 2;
  nouveau^.suivant := nil;
  premier^.suivant := nouveau;

  // ... etc ...

  // Ne pas oublier de libérer tous les noeuds !
end;
```

### 3. Optimisation Mémoire

Libérer la mémoire quand elle n'est plus nécessaire :

```pascal
var
  pGrossesDonnees: ^array[1..10000] of Integer;
begin
  New(pGrossesDonnees);

  // Traitement intensif
  // ... utilisation des données ...

  Dispose(pGrossesDonnees);  // Libère 40 Ko de mémoire
  pGrossesDonnees := nil;

  // Suite du programme sans ces données
end;
```

## Erreurs Courantes et Dangers

### 1. Fuite Mémoire (Memory Leak)

**Problème :** Oublier de libérer la mémoire allouée.

```pascal
// ✗ MAUVAIS : Fuite mémoire
procedure ProblemeFuite;  
var  
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  // Oups ! Pas de Dispose(p)
end;  // La mémoire reste occupée !

// Si appelée 1000 fois, perd 1000 * 4 octets = 4 Ko
```

**Solution :**

```pascal
// ✓ BON
procedure Correct;  
var  
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  Dispose(p);  // Toujours libérer !
  p := nil;
end;
```

### 2. Double Libération

**Problème :** Appeler `Dispose` deux fois sur le même pointeur.

```pascal
// ✗ MAUVAIS
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;

  Dispose(p);
  Dispose(p);  // ✗ ERREUR : déjà libéré !
end;
```

**Solution :**

```pascal
// ✓ BON
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;

  Dispose(p);
  p := nil;    // Marque comme libéré

  // Protection supplémentaire
  if p <> nil then
    Dispose(p);  // Ne s'exécute pas
end;
```

### 3. Pointeur Dangling (Pointeur Pendant)

**Problème :** Utiliser un pointeur après `Dispose`.

```pascal
// ✗ MAUVAIS
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  Dispose(p);

  WriteLn(p^);  // ✗ DANGER : zone libérée !
end;
```

**Solution :**

```pascal
// ✓ BON
var
  p: ^Integer;
begin
  New(p);
  p^ := 42;
  WriteLn(p^);  // Utiliser AVANT Dispose

  Dispose(p);
  p := nil;     // Empêche toute utilisation ultérieure
end;
```

### 4. Oubli de New

**Problème :** Utiliser un pointeur sans allocation.

```pascal
// ✗ MAUVAIS
var
  p: ^Integer;
begin
  p^ := 42;  // ✗ ERREUR : p pointe vers rien !
end;
```

**Solution :**

```pascal
// ✓ BON
var
  p: ^Integer;
begin
  New(p);    // Toujours allouer d'abord
  p^ := 42;  // Maintenant c'est sûr
  Dispose(p);
  p := nil;
end;
```

## Bonnes Pratiques Résumées

### Checklist de Sécurité

```pascal
var
  p: ^Integer;
begin
  // ✓ 1. Initialiser à nil (optionnel mais recommandé)
  p := nil;

  // ✓ 2. Vérifier avant New (si réutilisation)
  if p = nil then
    New(p);

  // ✓ 3. Toujours initialiser après New
  p^ := 0;

  // ✓ 4. Utiliser le pointeur
  p^ := 42;
  WriteLn(p^);

  // ✓ 5. Vérifier avant Dispose
  if p <> nil then
  begin
    Dispose(p);
    p := nil;  // ✓ 6. Mettre à nil après Dispose
  end;
end;
```

### Modèle Try-Finally

Pour garantir la libération même en cas d'erreur :

```pascal
var
  p: ^Integer;
begin
  New(p);
  try
    p^ := 42;
    // ... code qui pourrait générer une erreur ...
    WriteLn(p^);
  finally
    Dispose(p);  // Exécuté même en cas d'exception
    p := nil;
  end;
end;
```

## GetMem et FreeMem (Alternative)

Pascal offre aussi `GetMem` et `FreeMem` pour une allocation plus bas niveau :

```pascal
var
  p: ^Integer;
begin
  GetMem(p, SizeOf(Integer));  // Allocation manuelle
  p^ := 42;
  WriteLn(p^);
  FreeMem(p);                   // Libération manuelle
  p := nil;
end;
```

**Différence avec New/Dispose :**
- `GetMem` nécessite de spécifier la taille en octets
- `New` connaît automatiquement la taille du type
- **Recommandation pour débutants :** Préférez `New`/`Dispose`, plus simples et sûrs

## Tableau Comparatif

| Aspect | Allocation Statique | Allocation Dynamique |
|--------|---------------------|----------------------|
| Déclaration | `var x: Integer;` | `var p: ^Integer; New(p);` |
| Moment | À la compilation | À l'exécution |
| Taille | Fixe | Flexible |
| Durée de vie | Tout le scope | Contrôlée par vous |
| Libération | Automatique | Manuelle (Dispose) |
| Risques | Aucun | Fuites mémoire, pointeurs dangling |
| Utilisation | Simple | Plus complexe mais puissante |

## Points Clés à Retenir

1. **`New(p)`** alloue de la mémoire pour le type pointé par `p`
2. **`Dispose(p)`** libère la mémoire allouée
3. **Toujours** initialiser la zone après `New`
4. **Toujours** mettre le pointeur à `nil` après `Dispose`
5. **Chaque `New` doit avoir son `Dispose`**
6. Ne jamais utiliser un pointeur après `Dispose`
7. Utiliser `try-finally` pour garantir la libération

## Prochaine Étape

Vous maîtrisez maintenant l'allocation dynamique de mémoire ! Dans les sections suivantes, nous verrons comment utiliser ces connaissances pour créer des **structures de données dynamiques** comme les listes chaînées et les arbres binaires, qui exploitent pleinement la puissance de l'allocation dynamique.

⏭️ [Pointeurs et tableaux](/06-pointeurs-gestion-memoire-basique/04-pointeurs-tableaux.md)
