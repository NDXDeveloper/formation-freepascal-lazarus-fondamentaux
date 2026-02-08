🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.6 Listes Chaînées Simples

## Introduction

Une **liste chaînée** est une structure de données dynamique composée de noeuds liés entre eux par des pointeurs. Contrairement aux tableaux qui occupent un bloc continu de mémoire, les listes chaînées permettent d'ajouter et de supprimer des éléments efficacement sans avoir à déplacer d'autres éléments. Cette section explore en profondeur cette structure fondamentale.

## Concept de Liste Chaînée

### Analogie du Train

Imaginez un train :
- Chaque **wagon** est un noeud contenant des données
- Chaque wagon est **accroché** au suivant par un crochet (le pointeur)
- Le **locomotive** (premier noeud) permet d'accéder à tout le train
- On peut **ajouter** ou **retirer** des wagons facilement
- Le **dernier wagon** n'est relié à rien (pointeur nil)

### Structure d'un Noeud

Chaque noeud contient :
1. **Les données** (la valeur stockée)
2. **Un pointeur** vers le noeud suivant

```pascal
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;      // Les données du noeud
    suivant: PNoeud;      // Pointeur vers le noeud suivant
  end;
```

### Représentation Visuelle

```
┌────────┬─────────┐    ┌────────┬─────────┐    ┌────────┬─────────┐
│ donnee │ suivant │    │ donnee │ suivant │    │ donnee │ suivant │
│   10   │    ●────┼───>│   20   │    ●────┼───>│   30   │   nil   │
└────────┴─────────┘    └────────┴─────────┘    └────────┴─────────┘
    Premier                                           Dernier
```

## Création et Initialisation

### Créer une Liste Vide

```pascal
var
  liste: PNoeud;
begin
  liste := nil;  // Liste vide

  if liste = nil then
    WriteLn('La liste est vide');
end;
```

**Important :** Une liste vide est représentée par un pointeur `nil`.

### Créer un Premier Noeud

```pascal
var
  liste: PNoeud;
begin
  // Créer le noeud
  New(liste);
  liste^.donnee := 42;
  liste^.suivant := nil;  // Dernier noeud

  WriteLn('Liste avec un élément : ', liste^.donnee);
end;
```

### Fonction de Création de Noeud

Encapsulation pour plus de clarté :

```pascal
function CreerNoeud(valeur: Integer): PNoeud;
begin
  New(Result);
  Result^.donnee := valeur;
  Result^.suivant := nil;
end;

var
  liste: PNoeud;
begin
  liste := CreerNoeud(100);
  WriteLn('Premier élément : ', liste^.donnee);
end;
```

## Insertion d'Éléments

### Insertion en Tête (Début)

**La plus simple et la plus rapide** : O(1)

```pascal
procedure InsererDebut(var liste: PNoeud; valeur: Integer);
var
  nouveau: PNoeud;
begin
  // Créer le nouveau noeud
  New(nouveau);
  nouveau^.donnee := valeur;

  // Le nouveau pointe vers l'ancien premier
  nouveau^.suivant := liste;

  // Le nouveau devient le premier
  liste := nouveau;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;

  InsererDebut(maListe, 30);  // Liste : 30
  InsererDebut(maListe, 20);  // Liste : 20 -> 30
  InsererDebut(maListe, 10);  // Liste : 10 -> 20 -> 30
end;
```

**Étapes visualisées :**

```
État initial (liste vide) :
liste = nil

Après InsererDebut(liste, 30) :
liste -> [30|nil]

Après InsererDebut(liste, 20) :
liste -> [20|●] -> [30|nil]

Après InsererDebut(liste, 10) :
liste -> [10|●] -> [20|●] -> [30|nil]
```

### Insertion en Queue (Fin)

Plus complexe car il faut parcourir toute la liste : O(n)

```pascal
procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  // Créer le nouveau noeud
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  // Cas 1 : liste vide
  if liste = nil then
  begin
    liste := nouveau;
    Exit;
  end;

  // Cas 2 : parcourir jusqu'au dernier
  courant := liste;
  while courant^.suivant <> nil do
    courant := courant^.suivant;

  // Accrocher le nouveau à la fin
  courant^.suivant := nouveau;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;

  InsererFin(maListe, 10);  // Liste : 10
  InsererFin(maListe, 20);  // Liste : 10 -> 20
  InsererFin(maListe, 30);  // Liste : 10 -> 20 -> 30
end;
```

### Insertion à une Position Donnée

Insérer après le n-ième élément :

```pascal
procedure InsererApres(liste: PNoeud; position, valeur: Integer);
var
  nouveau, courant: PNoeud;
  i: Integer;
begin
  if liste = nil then
  begin
    WriteLn('Erreur : liste vide');
    Exit;
  end;

  // Trouver le noeud à la position donnée
  courant := liste;
  for i := 1 to position - 1 do
  begin
    if courant^.suivant = nil then
    begin
      WriteLn('Erreur : position invalide');
      Exit;
    end;
    courant := courant^.suivant;
  end;

  // Créer et insérer le nouveau noeud
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := courant^.suivant;
  courant^.suivant := nouveau;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 30);
  InsererFin(maListe, 50);

  // Insérer 20 après la position 1 (entre 10 et 30)
  InsererApres(maListe, 1, 20);
  // Liste : 10 -> 20 -> 30 -> 50
end;
```

## Parcours de Liste

### Affichage Complet

```pascal
program AfficherListeDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  AfficherListe(maListe);  // Affiche : Liste : 10 -> 20 -> 30

  LibererListe(maListe);
end.
```

### Parcours avec Traitement

```pascal
program DoublerValeursDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure DoublerValeurs(liste: PNoeud);
var
  courant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    courant^.donnee := courant^.donnee * 2;
    courant := courant^.suivant;
  end;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 5);
  InsererFin(maListe, 10);
  InsererFin(maListe, 15);

  AfficherListe(maListe);   // 5 -> 10 -> 15
  DoublerValeurs(maListe);
  AfficherListe(maListe);   // 10 -> 20 -> 30

  LibererListe(maListe);
end.
```

## Recherche dans une Liste

### Rechercher une Valeur

```pascal
{$mode objfpc}{$H+}
program RechercherDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

function Rechercher(liste: PNoeud; valeur: Integer): Boolean;
var
  courant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    if courant^.donnee = valeur then
    begin
      Result := True;
      Exit;
    end;
    courant := courant^.suivant;
  end;
  Result := False;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  if Rechercher(maListe, 20) then
    WriteLn('20 trouvé')
  else
    WriteLn('20 non trouvé');

  LibererListe(maListe);
end.
```

### Trouver la Position

```pascal
{$mode objfpc}{$H+}
program TrouverPositionDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

function TrouverPosition(liste: PNoeud; valeur: Integer): Integer;
var
  courant: PNoeud;
  position: Integer;
begin
  courant := liste;
  position := 1;

  while courant <> nil do
  begin
    if courant^.donnee = valeur then
    begin
      Result := position;
      Exit;
    end;
    courant := courant^.suivant;
    Inc(position);
  end;

  Result := -1;  // Non trouvé
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
  pos: Integer;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  pos := TrouverPosition(maListe, 20);
  if pos <> -1 then
    WriteLn('20 trouvé à la position ', pos)
  else
    WriteLn('20 non trouvé');

  LibererListe(maListe);
end.
```

### Obtenir un Élément par Position

```pascal
function ObtenirElement(liste: PNoeud; position: Integer): Integer;
var
  courant: PNoeud;
  i: Integer;
begin
  if liste = nil then
  begin
    WriteLn('Erreur : liste vide');
    Result := 0;
    Exit;
  end;

  courant := liste;
  for i := 2 to position do
  begin
    if courant^.suivant = nil then
    begin
      WriteLn('Erreur : position invalide');
      Result := 0;
      Exit;
    end;
    courant := courant^.suivant;
  end;

  Result := courant^.donnee;
end;
```

## Suppression d'Éléments

### Supprimer le Premier Élément

```pascal
program SupprimerDebutDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure SupprimerDebut(var liste: PNoeud);
var
  temp: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Erreur : liste vide');
    Exit;
  end;

  temp := liste;              // Sauvegarder le premier
  liste := liste^.suivant;    // Le deuxième devient le premier
  Dispose(temp);              // Libérer l'ancien premier
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  AfficherListe(maListe);  // 10 -> 20 -> 30
  SupprimerDebut(maListe);
  AfficherListe(maListe);  // 20 -> 30

  LibererListe(maListe);
end.
```

**Visualisation :**

```
Avant SupprimerDebut :
liste -> [10|●] -> [20|●] -> [30|nil]
         temp

Après liste := liste^.suivant :
liste --------> [20|●] -> [30|nil]
temp -> [10|●]

Après Dispose(temp) :
liste -> [20|●] -> [30|nil]
temp = LIBÉRÉ
```

### Supprimer le Dernier Élément

```pascal
procedure SupprimerFin(var liste: PNoeud);
var
  courant, precedent: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Erreur : liste vide');
    Exit;
  end;

  // Cas 1 : un seul élément
  if liste^.suivant = nil then
  begin
    Dispose(liste);
    liste := nil;
    Exit;
  end;

  // Cas 2 : trouver l'avant-dernier
  courant := liste;
  precedent := nil;

  while courant^.suivant <> nil do
  begin
    precedent := courant;
    courant := courant^.suivant;
  end;

  // Supprimer le dernier
  precedent^.suivant := nil;
  Dispose(courant);
end;
```

### Supprimer par Valeur

```pascal
program SupprimerValeurDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure SupprimerValeur(var liste: PNoeud; valeur: Integer);
var
  courant, precedent: PNoeud;
begin
  if liste = nil then
    Exit;

  // Cas 1 : suppression en tête
  if liste^.donnee = valeur then
  begin
    courant := liste;
    liste := liste^.suivant;
    Dispose(courant);
    Exit;
  end;

  // Cas 2 : suppression ailleurs
  precedent := liste;
  courant := liste^.suivant;

  while courant <> nil do
  begin
    if courant^.donnee = valeur then
    begin
      precedent^.suivant := courant^.suivant;
      Dispose(courant);
      Exit;
    end;
    precedent := courant;
    courant := courant^.suivant;
  end;

  WriteLn('Valeur non trouvée');
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  AfficherListe(maListe);        // 10 -> 20 -> 30
  SupprimerValeur(maListe, 20);
  AfficherListe(maListe);        // 10 -> 30

  LibererListe(maListe);
end.
```

## Opérations Utiles

### Compter les Éléments

```pascal
{$mode objfpc}{$H+}
program CompterElementsDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

function CompterElements(liste: PNoeud): Integer;
var
  courant: PNoeud;
  compte: Integer;
begin
  compte := 0;
  courant := liste;

  while courant <> nil do
  begin
    Inc(compte);
    courant := courant^.suivant;
  end;

  Result := compte;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  WriteLn('Nombre d''éléments : ', CompterElements(maListe));  // 3

  LibererListe(maListe);
end.
```

### Vérifier si la Liste est Vide

```pascal
function EstVide(liste: PNoeud): Boolean;
begin
  Result := (liste = nil);
end;
```

### Trouver le Minimum

```pascal
function TrouverMin(liste: PNoeud): Integer;
var
  courant: PNoeud;
  minimum: Integer;
begin
  if liste = nil then
  begin
    WriteLn('Erreur : liste vide');
    Result := 0;
    Exit;
  end;

  minimum := liste^.donnee;
  courant := liste^.suivant;

  while courant <> nil do
  begin
    if courant^.donnee < minimum then
      minimum := courant^.donnee;
    courant := courant^.suivant;
  end;

  Result := minimum;
end;
```

### Trouver le Maximum

```pascal
function TrouverMax(liste: PNoeud): Integer;
var
  courant: PNoeud;
  maximum: Integer;
begin
  if liste = nil then
  begin
    WriteLn('Erreur : liste vide');
    Result := 0;
    Exit;
  end;

  maximum := liste^.donnee;
  courant := liste^.suivant;

  while courant <> nil do
  begin
    if courant^.donnee > maximum then
      maximum := courant^.donnee;
    courant := courant^.suivant;
  end;

  Result := maximum;
end;
```

### Calculer la Somme

```pascal
function Somme(liste: PNoeud): Integer;
var
  courant: PNoeud;
  total: Integer;
begin
  total := 0;
  courant := liste;

  while courant <> nil do
  begin
    total := total + courant^.donnee;
    courant := courant^.suivant;
  end;

  Result := total;
end;
```

## Libération de la Liste

### Libérer Tous les Noeuds

**Essentiel :** Ne jamais oublier de libérer toute la liste !

```pascal
program LibererListeDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;

  while courant <> nil do
  begin
    suivant := courant^.suivant;  // Sauvegarder le suivant
    Dispose(courant);              // Libérer le courant
    courant := suivant;            // Passer au suivant
  end;

  liste := nil;  // Liste vide
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  AfficherListe(maListe);

  // IMPORTANT : toujours libérer !
  LibererListe(maListe);
end.
```

**Pourquoi sauvegarder le suivant ?**

```
Sans sauvegarde (ERREUR) :
courant -> [10|●] -> [20|●] -> [30|nil]
Dispose(courant)  ← On perd l'accès au reste !

Avec sauvegarde (CORRECT) :
courant -> [10|●] -> [20|●] -> [30|nil]
           suivant
Dispose(courant), puis courant := suivant
```

## Opérations Avancées

### Inverser une Liste

```pascal
program InverserListeDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure InverserListe(var liste: PNoeud);
var
  precedent, courant, suivant: PNoeud;
begin
  if liste = nil then
    Exit;

  precedent := nil;
  courant := liste;

  while courant <> nil do
  begin
    suivant := courant^.suivant;    // Sauvegarder le suivant
    courant^.suivant := precedent;   // Inverser le lien
    precedent := courant;            // Avancer
    courant := suivant;
  end;

  liste := precedent;  // Le dernier devient le premier
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);

  AfficherListe(maListe);   // 10 -> 20 -> 30
  InverserListe(maListe);
  AfficherListe(maListe);   // 30 -> 20 -> 10

  LibererListe(maListe);
end.
```

### Copier une Liste

```pascal
{$mode objfpc}{$H+}
program CopierListeDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

function CopierListe(liste: PNoeud): PNoeud;
var
  nouvelle, courant, dernier: PNoeud;
begin
  if liste = nil then
  begin
    Result := nil;
    Exit;
  end;

  // Copier le premier élément
  New(nouvelle);
  nouvelle^.donnee := liste^.donnee;
  nouvelle^.suivant := nil;
  dernier := nouvelle;

  // Copier le reste
  courant := liste^.suivant;
  while courant <> nil do
  begin
    New(dernier^.suivant);
    dernier := dernier^.suivant;
    dernier^.donnee := courant^.donnee;
    dernier^.suivant := nil;
    courant := courant^.suivant;
  end;

  Result := nouvelle;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  liste1, liste2: PNoeud;
begin
  liste1 := nil;
  InsererFin(liste1, 10);
  InsererFin(liste1, 20);

  liste2 := CopierListe(liste1);

  AfficherListe(liste1);  // 10 -> 20
  AfficherListe(liste2);  // 10 -> 20 (copie indépendante)

  LibererListe(liste1);
  LibererListe(liste2);
end.
```

### Trier une Liste (Tri à Bulles)

```pascal
program TrierListeDemo;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure TrierListe(liste: PNoeud);
var
  i, j: PNoeud;
  temp: Integer;
begin
  if liste = nil then
    Exit;

  i := liste;
  while i <> nil do
  begin
    j := i^.suivant;
    while j <> nil do
    begin
      if i^.donnee > j^.donnee then
      begin
        // Échanger les valeurs
        temp := i^.donnee;
        i^.donnee := j^.donnee;
        j^.donnee := temp;
      end;
      j := j^.suivant;
    end;
    i := i^.suivant;
  end;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;
  InsererFin(maListe, 30);
  InsererFin(maListe, 10);
  InsererFin(maListe, 20);

  AfficherListe(maListe);  // 30 -> 10 -> 20
  TrierListe(maListe);
  AfficherListe(maListe);  // 10 -> 20 -> 30

  LibererListe(maListe);
end.
```

## Programme Complet : Gestionnaire de Liste

```pascal
program GestionnaireListe;

type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

procedure InsererFin(var liste: PNoeud; valeur: Integer);
var
  nouveau, courant: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if liste = nil then
    liste := nouveau
  else
  begin
    courant := liste;
    while courant^.suivant <> nil do
      courant := courant^.suivant;
    courant^.suivant := nouveau;
  end;
end;

procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  if liste = nil then
  begin
    WriteLn('Liste vide');
    Exit;
  end;

  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee);
    if courant^.suivant <> nil then
      Write(' -> ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

procedure LibererListe(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;

var
  maListe: PNoeud;
begin
  maListe := nil;

  WriteLn('=== Gestionnaire de Liste ===');

  InsererFin(maListe, 10);
  InsererFin(maListe, 20);
  InsererFin(maListe, 30);
  InsererFin(maListe, 40);
  InsererFin(maListe, 50);

  AfficherListe(maListe);

  LibererListe(maListe);
  WriteLn('Mémoire libérée');

  ReadLn;
end.
```

## Avantages et Inconvénients

### Avantages des Listes Chaînées

✅ **Taille dynamique** : Peut croître ou rétrécir facilement
✅ **Insertion/suppression rapides** : O(1) en début de liste
✅ **Pas de déplacement** : Pas besoin de déplacer les autres éléments
✅ **Utilisation flexible de la mémoire** : Pas de blocs contigus requis

### Inconvénients

❌ **Accès séquentiel** : Impossible d'accéder directement au n-ième élément
❌ **Surcoût mémoire** : Chaque noeud nécessite un pointeur supplémentaire
❌ **Parcours lent** : O(n) pour atteindre un élément
❌ **Gestion manuelle** : Risque de fuites mémoire

### Comparaison avec les Tableaux

| Opération | Tableau | Liste Chaînée |
|-----------|---------|---------------|
| Accès direct | O(1) ✅ | O(n) ❌ |
| Insertion début | O(n) ❌ | O(1) ✅ |
| Insertion fin | O(1) ✅ | O(n) ❌ |
| Suppression début | O(n) ❌ | O(1) ✅ |
| Recherche | O(n) | O(n) |
| Taille | Fixe ❌ | Dynamique ✅ |

## Erreurs Courantes à Éviter

### 1. Oublier d'Initialiser suivant

```pascal
// ✗ MAUVAIS
New(noeud);
noeud^.donnee := 10;
// Oubli : noeud^.suivant contient n'importe quoi !

// ✓ BON
New(noeud);
noeud^.donnee := 10;
noeud^.suivant := nil;
```

### 2. Perdre la Référence de la Tête

```pascal
// ✗ MAUVAIS
procedure Erreur(liste: PNoeud);
begin
  liste := liste^.suivant;  // Perd le premier !
end;

// ✓ BON
procedure Correct(var liste: PNoeud);  // Paramètre var
begin
  liste := liste^.suivant;
end;
```

### 3. Libération Incomplète

```pascal
// ✗ MAUVAIS
procedure MauvaiseLiberation(liste: PNoeud);
begin
  Dispose(liste);  // Ne libère que le premier !
end;

// ✓ BON
procedure BonneLiberation(var liste: PNoeud);
var
  courant, suivant: PNoeud;
begin
  courant := liste;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  liste := nil;
end;
```

### 4. Déréférencement de nil

```pascal
// ✗ MAUVAIS
if liste^.donnee = 10 then  // Crash si liste = nil !

// ✓ BON
if (liste <> nil) and (liste^.donnee = 10) then
```

## Bonnes Pratiques

1. **Toujours initialiser** `suivant` à `nil`
2. **Vérifier nil** avant tout déréférencement
3. **Utiliser `var`** pour les paramètres qui modifient la liste
4. **Sauvegarder le suivant** avant de libérer un noeud
5. **Libérer toute la liste** en fin de programme
6. **Encapsuler** les opérations dans des fonctions
7. **Documenter** l'ownership de la mémoire

## Points Clés à Retenir

1. Une **liste chaînée** est composée de noeuds liés par des pointeurs
2. **nil** indique la fin de la liste
3. **Insertion en tête** : O(1), la plus rapide
4. **Parcours** : toujours vérifier si le noeud n'est pas nil
5. **Libération** : parcourir et libérer tous les noeuds
6. **var** nécessaire pour modifier le pointeur de tête
7. Les listes sont **flexibles** mais nécessitent une **gestion rigoureuse**

## Prochaine Étape

Vous maîtrisez maintenant les listes chaînées simples ! La prochaine section sur les **arbres binaires** utilisera des concepts similaires mais avec deux pointeurs par noeud (gauche et droite), permettant de créer des structures hiérarchiques encore plus puissantes pour organiser et rechercher efficacement des données.

⏭️ [Arbres binaires basics](/06-pointeurs-gestion-memoire-basique/07-arbres-binaires-basics.md)
