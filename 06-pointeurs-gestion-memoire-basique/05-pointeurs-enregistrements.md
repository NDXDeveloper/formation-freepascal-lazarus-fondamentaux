🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.5 Pointeurs et Enregistrements

## Introduction

Les enregistrements (records) sont des structures de données qui regroupent plusieurs champs de types différents. Combinés avec les pointeurs, ils deviennent extrêmement puissants et permettent de créer des structures de données complexes comme des listes chaînées, des arbres et des graphes. Cette section explore en détail l'utilisation des pointeurs avec les enregistrements.

## Rappel : Les Enregistrements

### Structure de Base

Un enregistrement regroupe plusieurs variables sous un seul nom :

```pascal
type
  TPerson = record
    nom: String;
    prenom: String;
    age: Integer;
    salaire: Real;
  end;

var
  personne: TPerson;
begin
  personne.nom := 'Dupont';
  personne.prenom := 'Marie';
  personne.age := 28;
  personne.salaire := 2500.50;

  WriteLn(personne.nom, ' ', personne.prenom);
  WriteLn('Age : ', personne.age);
end;
```

**Avantage :** Regrouper des données liées logiquement.

## Pointeur vers un Enregistrement

### Déclaration

Pour déclarer un pointeur vers un enregistrement :

```pascal
type
  TPerson = record
    nom: String;
    age: Integer;
  end;
  PPerson = ^TPerson;  // Pointeur vers TPerson

var
  pers: TPerson;       // Enregistrement normal
  pPers: PPerson;      // Pointeur vers enregistrement
```

**Convention :** On préfixe souvent le type pointeur avec `P`.

### Utilisation Basique

```pascal
type
  TPerson = record
    nom: String;
    age: Integer;
  end;
  PPerson = ^TPerson;

var
  personne: TPerson;
  pPersonne: PPerson;
begin
  // Initialisation de l'enregistrement
  personne.nom := 'Alice';
  personne.age := 30;

  // Le pointeur pointe vers l'enregistrement
  pPersonne := @personne;

  // Accès via le pointeur
  WriteLn('Nom : ', pPersonne^.nom);
  WriteLn('Age : ', pPersonne^.age);

  // Modification via le pointeur
  pPersonne^.age := 31;
  WriteLn('Nouvel age : ', personne.age);  // Affiche 31
end;
```

### Syntaxe d'Accès

Deux notations possibles pour accéder aux champs :

```pascal
// Notation 1 : Avec ^
pPersonne^.nom := 'Bob';
pPersonne^.age := 25;

// Notation 2 : Avec With (plus lisible)
with pPersonne^ do
begin
  nom := 'Charlie';
  age := 35;
end;
```

### Visualisation en Mémoire

```
Enregistrement en mémoire :
┌───────────┬──────────────┬─────────┐
│  Adresse  │     Champ    │ Valeur  │
├───────────┼──────────────┼─────────┤
│ $00001000 │ personne.nom │ "Alice" │
│ $00001010 │ personne.age │   30    │
└───────────┴──────────────┴─────────┘

Pointeur :
┌───────────┬───────────┬──────────┐
│  Adresse  │    Nom    │  Valeur  │
├───────────┼───────────┼──────────┤
│ $00002000 │ pPersonne │ $00001000│ ← pointe vers personne
└───────────┴───────────┴──────────┘
```

## Allocation Dynamique d'Enregistrements

### Création avec New

La vraie puissance apparaît avec l'allocation dynamique :

```pascal
type
  TPerson = record
    nom: String;
    prenom: String;
    age: Integer;
  end;
  PPerson = ^TPerson;

var
  p: PPerson;
begin
  // Allocation dynamique
  New(p);

  // Initialisation
  p^.nom := 'Martin';
  p^.prenom := 'Jean';
  p^.age := 42;

  // Utilisation
  WriteLn(p^.nom, ' ', p^.prenom, ', ', p^.age, ' ans');

  // Libération
  Dispose(p);
  p := nil;
end;
```

### Pourquoi Utiliser New ?

**Avantages :**
1. Créer des structures de taille variable
2. Gérer la durée de vie des objets
3. Partager des données entre fonctions
4. Construire des structures complexes

### Fonction de Création

Encapsuler la création dans une fonction :

```pascal
type
  TPerson = record
    nom: String;
    age: Integer;
  end;
  PPerson = ^TPerson;

function CreerPersonne(n: String; a: Integer): PPerson;
begin
  New(Result);
  Result^.nom := n;
  Result^.age := a;
end;

procedure LibererPersonne(var p: PPerson);
begin
  if p <> nil then
  begin
    Dispose(p);
    p := nil;
  end;
end;

var
  personne: PPerson;
begin
  personne := CreerPersonne('Sophie', 28);
  WriteLn(personne^.nom, ' : ', personne^.age, ' ans');
  LibererPersonne(personne);
end.
```

## Enregistrements Contenant des Pointeurs

### Structure Auto-Référencée

Un enregistrement peut contenir un pointeur vers son propre type :

```pascal
type
  PNoeud = ^TNoeud;
  TNoeud = record
    valeur: Integer;
    suivant: PNoeud;  // Pointeur vers le noeud suivant
  end;
```

**Important :** La déclaration du pointeur (`PNoeud`) doit venir **avant** la définition de l'enregistrement (`TNoeud`).

### Création d'une Liste Simple

```pascal
program ListeSimple;
type
  PNoeud = ^TNoeud;  // Déclaration anticipée : Pascal autorise ^Type avant que Type soit défini
  TNoeud = record
    valeur: Integer;
    suivant: PNoeud;
  end;

var
  premier, deuxieme, troisieme: PNoeud;
begin
  // Créer le premier noeud
  New(premier);
  premier^.valeur := 10;
  premier^.suivant := nil;

  // Créer le deuxième noeud
  New(deuxieme);
  deuxieme^.valeur := 20;
  deuxieme^.suivant := nil;

  // Lier le premier au deuxième
  premier^.suivant := deuxieme;

  // Créer le troisième noeud
  New(troisieme);
  troisieme^.valeur := 30;
  troisieme^.suivant := nil;

  // Lier le deuxième au troisième
  deuxieme^.suivant := troisieme;

  // Parcourir la liste
  WriteLn('Premier : ', premier^.valeur);
  WriteLn('Deuxième : ', premier^.suivant^.valeur);
  WriteLn('Troisième : ', premier^.suivant^.suivant^.valeur);

  // Libération (à faire pour chaque noeud)
  Dispose(troisieme);
  Dispose(deuxieme);
  Dispose(premier);
end.
```

### Visualisation de la Liste Chaînée

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ Premier         │    │ Deuxième        │    │ Troisième       │
├─────────────────┤    ├─────────────────┤    ├─────────────────┤
│ valeur: 10      │    │ valeur: 20      │    │ valeur: 30      │
│ suivant: ─────┐ │    │ suivant: ─────┐ │    │ suivant: nil    │
└───────────────┼─┘    └───────────────┼─┘    └─────────────────┘
                │                       │
                └───────────────────────┘
```

## Liste Chaînée : Introduction Pratique

### Structure Complète

Créons une petite bibliothèque pour gérer une liste :

```pascal
{$mode objfpc}{$H+}
program ListeChainee;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: Integer;
    suivant: PNoeud;
  end;

// Ajouter un élément au début
function AjouterDebut(liste: PNoeud; valeur: Integer): PNoeud;
var
  nouveau: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := liste;
  Result := nouveau;
end;

// Afficher tous les éléments
procedure AfficherListe(liste: PNoeud);
var
  courant: PNoeud;
begin
  courant := liste;
  Write('Liste : ');
  while courant <> nil do
  begin
    Write(courant^.donnee, ' ');
    courant := courant^.suivant;
  end;
  WriteLn;
end;

// Libérer toute la liste
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
  maListe := nil;  // Liste vide

  // Ajouter des éléments
  maListe := AjouterDebut(maListe, 30);
  maListe := AjouterDebut(maListe, 20);
  maListe := AjouterDebut(maListe, 10);

  AfficherListe(maListe);  // Affiche : Liste : 10 20 30

  LibererListe(maListe);
end.
```

### Opérations Courantes sur les Listes

#### Compter les Éléments

```pascal
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
```

#### Rechercher une Valeur

```pascal
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
```

#### Ajouter à la Fin

```pascal
procedure AjouterFin(var liste: PNoeud; valeur: Integer);
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
```

## Enregistrements Complexes

### Enregistrement avec Plusieurs Pointeurs

```pascal
program RelationsPointeurs;
type
  PPersonne = ^TPersonne;
  TPersonne = record
    nom: String;
    age: Integer;
    conjoint: PPersonne;      // Pointeur vers le conjoint
    meilleurAmi: PPersonne;   // Pointeur vers l'ami
  end;

var
  alice, bob: PPersonne;
begin
  // Créer Alice
  New(alice);
  alice^.nom := 'Alice';
  alice^.age := 30;
  alice^.conjoint := nil;
  alice^.meilleurAmi := nil;

  // Créer Bob
  New(bob);
  bob^.nom := 'Bob';
  bob^.age := 32;
  bob^.conjoint := nil;
  bob^.meilleurAmi := nil;

  // Établir les relations
  alice^.conjoint := bob;
  bob^.conjoint := alice;

  // Afficher
  WriteLn(alice^.nom, ' est mariée à ', alice^.conjoint^.nom);
  WriteLn(bob^.nom, ' est marié à ', bob^.conjoint^.nom);

  // Libération
  Dispose(bob);
  Dispose(alice);
end.
```

### Enregistrement avec Tableau Dynamique

```pascal
program EtudiantNotes;
type
  PEtudiant = ^TEtudiant;
  TEtudiant = record
    nom: String;
    notes: array of Real;    // Tableau dynamique
  end;

var
  etudiant: PEtudiant;
  i: Integer;
begin
  New(etudiant);
  etudiant^.nom := 'Pierre';

  // Allouer le tableau de notes
  SetLength(etudiant^.notes, 5);

  // Remplir les notes
  for i := 0 to 4 do
    etudiant^.notes[i] := 10 + i * 2;

  // Afficher
  WriteLn('Notes de ', etudiant^.nom, ' :');
  for i := 0 to High(etudiant^.notes) do
    WriteLn('  Note ', i+1, ' : ', etudiant^.notes[i]:0:1);

  // Libération
  SetLength(etudiant^.notes, 0);  // Libérer le tableau
  Dispose(etudiant);
  etudiant := nil;
end.
```

## Passage en Paramètres

### Par Valeur (Copie du Pointeur)

```pascal
procedure AfficherPersonne(p: PPerson);
begin
  if p <> nil then
    WriteLn(p^.nom, ', ', p^.age, ' ans');
end;

var
  pers: PPerson;
begin
  New(pers);
  pers^.nom := 'Marie';
  pers^.age := 25;

  AfficherPersonne(pers);  // Passe une copie du pointeur

  Dispose(pers);
  pers := nil;
end;
```

### Par Référence (var)

Utile quand on veut modifier le pointeur lui-même :

```pascal
procedure CreerEtInitialiser(var p: PPerson; n: String; a: Integer);
begin
  New(p);
  p^.nom := n;
  p^.age := a;
end;

procedure Liberer(var p: PPerson);
begin
  if p <> nil then
  begin
    Dispose(p);
    p := nil;  // Peut modifier le pointeur original
  end;
end;

var
  personne: PPerson;
begin
  personne := nil;
  CreerEtInitialiser(personne, 'Luc', 40);
  WriteLn(personne^.nom);
  Liberer(personne);
  // personne est maintenant nil
end.
```

## Cas Pratiques

### 1. Carnet d'Adresses

```pascal
program CarnetAdresses;
type
  PContact = ^TContact;
  TContact = record
    nom: String;
    telephone: String;
    email: String;
    suivant: PContact;
  end;

procedure AjouterContact(var carnet: PContact; n, tel, mail: String);
var
  nouveau: PContact;
begin
  New(nouveau);
  nouveau^.nom := n;
  nouveau^.telephone := tel;
  nouveau^.email := mail;
  nouveau^.suivant := carnet;
  carnet := nouveau;
end;

procedure AfficherCarnet(carnet: PContact);
var
  courant: PContact;
begin
  courant := carnet;
  WriteLn('=== Carnet d''adresses ===');
  while courant <> nil do
  begin
    WriteLn('Nom : ', courant^.nom);
    WriteLn('Tel : ', courant^.telephone);
    WriteLn('Email : ', courant^.email);
    WriteLn('---');
    courant := courant^.suivant;
  end;
end;

procedure LibererCarnet(var carnet: PContact);
var
  courant, suivant: PContact;
begin
  courant := carnet;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  carnet := nil;
end;

var
  contacts: PContact;
begin
  contacts := nil;

  AjouterContact(contacts, 'Alice', '0601020304', 'alice@mail.com');
  AjouterContact(contacts, 'Bob', '0605060708', 'bob@mail.com');

  AfficherCarnet(contacts);

  LibererCarnet(contacts);
end.
```

### 2. File d'Attente (Queue)

```pascal
{$mode objfpc}{$H+}
program FileAttente;
type
  PNoeud = ^TNoeud;
  TNoeud = record
    donnee: String;
    suivant: PNoeud;
  end;

  TFile = record
    premier: PNoeud;
    dernier: PNoeud;
  end;

procedure Enfiler(var f: TFile; valeur: String);
var
  nouveau: PNoeud;
begin
  New(nouveau);
  nouveau^.donnee := valeur;
  nouveau^.suivant := nil;

  if f.dernier = nil then
  begin
    f.premier := nouveau;
    f.dernier := nouveau;
  end
  else
  begin
    f.dernier^.suivant := nouveau;
    f.dernier := nouveau;
  end;
end;

function Defiler(var f: TFile): String;
var
  temp: PNoeud;
begin
  if f.premier = nil then
  begin
    Result := '';
    Exit;
  end;

  Result := f.premier^.donnee;
  temp := f.premier;
  f.premier := f.premier^.suivant;

  if f.premier = nil then
    f.dernier := nil;

  Dispose(temp);
end;

var
  maFile: TFile;
begin
  maFile.premier := nil;
  maFile.dernier := nil;

  Enfiler(maFile, 'Client 1');
  Enfiler(maFile, 'Client 2');
  Enfiler(maFile, 'Client 3');

  WriteLn('Service : ', Defiler(maFile));  // Client 1
  WriteLn('Service : ', Defiler(maFile));  // Client 2
end.
```

## Gestion Mémoire Avancée

### Éviter les Fuites avec Try-Finally

```pascal
uses SysUtils;

type
  PPerson = ^TPerson;
  TPerson = record
    nom: String;
    age: Integer;
  end;

var
  p: PPerson;
begin
  New(p);
  try
    p^.nom := 'Test';
    p^.age := 25;

    // Code qui pourrait générer une erreur
    if p^.age < 0 then
      raise Exception.Create('Age invalide');

    WriteLn(p^.nom);
  finally
    Dispose(p);  // Toujours exécuté
    p := nil;
  end;
end;
```

### Comptage de Références Simple

Pour partager un enregistrement entre plusieurs pointeurs :

```pascal
type
  PDonnee = ^TDonnee;
  TDonnee = record
    valeur: String;
    compteurRef: Integer;  // Compte les utilisateurs
  end;

function Acquerir(p: PDonnee): PDonnee;
begin
  if p <> nil then
    Inc(p^.compteurRef);
  Result := p;
end;

procedure Liberer(var p: PDonnee);
begin
  if p <> nil then
  begin
    Dec(p^.compteurRef);
    if p^.compteurRef <= 0 then
    begin
      Dispose(p);
      p := nil;
    end;
  end;
end;

var
  original, copie1, copie2: PDonnee;
begin
  New(original);
  original^.valeur := 'Données partagées';
  original^.compteurRef := 1;

  copie1 := Acquerir(original);  // compteurRef = 2
  copie2 := Acquerir(original);  // compteurRef = 3

  Liberer(copie1);  // compteurRef = 2
  Liberer(copie2);  // compteurRef = 1
  Liberer(original); // compteurRef = 0, libération effective
end.
```

## Erreurs Courantes

### 1. Oubli d'Initialisation du Pointeur Suivant

```pascal
// ✗ MAUVAIS
var
  noeud: PNoeud;
begin
  New(noeud);
  noeud^.valeur := 10;
  // Oubli : noeud^.suivant n'est pas initialisé !
end;

// ✓ BON
var
  noeud: PNoeud;
begin
  New(noeud);
  noeud^.valeur := 10;
  noeud^.suivant := nil;  // Toujours initialiser !
end;
```

### 2. Perte de la Tête de Liste

```pascal
// ✗ MAUVAIS
var
  liste: PNoeud;
begin
  New(liste);
  liste^.valeur := 1;
  liste := liste^.suivant;  // Perd la référence au premier !
end;

// ✓ BON
var
  liste, courant: PNoeud;
begin
  New(liste);
  liste^.valeur := 1;
  courant := liste^.suivant;  // Utiliser une variable temporaire
end;
```

### 3. Libération Partielle

```pascal
// ✗ MAUVAIS : fuite mémoire
procedure LibererMal(liste: PNoeud);
begin
  if liste <> nil then
    Dispose(liste);  // Ne libère que le premier !
end;

// ✓ BON : libère tous les noeuds
procedure LibererBien(var liste: PNoeud);
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

### 4. Référence Circulaire

```pascal
// Attention aux boucles infinies !
type
  PNoeud = ^TNoeud;
  TNoeud = record
    valeur: Integer;
    suivant: PNoeud;
  end;

var
  a, b: PNoeud;
begin
  New(a);
  New(b);
  a^.suivant := b;
  b^.suivant := a;  // Référence circulaire !

  // Parcours infini si pas géré !
end;
```

## Bonnes Pratiques

### 1. Toujours Initialiser les Pointeurs

```pascal
New(noeud);
noeud^.valeur := 0;
noeud^.suivant := nil;  // ✓ Important !
```

### 2. Vérifier nil Avant Déréférencement

```pascal
if personne <> nil then
  WriteLn(personne^.nom);
```

### 3. Utiliser des Fonctions d'Encapsulation

```pascal
function CreerNoeud(val: Integer): PNoeud;
begin
  New(Result);
  Result^.valeur := val;
  Result^.suivant := nil;
end;
```

### 4. Libérer dans l'Ordre Inverse

Pour les structures imbriquées, libérer du plus profond au plus superficiel.

### 5. Documenter l'Ownership

```pascal
// Cette fonction TRANSFÈRE l'ownership au appelant
function CreerPersonne: PPerson;

// Cette fonction NE libère PAS la mémoire
procedure AfficherPersonne(p: PPerson);

// Cette fonction LIBÈRE la mémoire
procedure DetruirePersonne(var p: PPerson);
```

## Points Clés à Retenir

1. **Les pointeurs vers enregistrements** permettent de créer des structures flexibles
2. **`p^.champ`** pour accéder aux champs via un pointeur
3. **Toujours initialiser** les champs pointeurs à `nil`
4. **Les listes chaînées** utilisent des enregistrements auto-référencés
5. **Libérer tous les noeuds** d'une structure chaînée
6. **Try-finally** pour garantir la libération
7. **Vérifier nil** avant tout accès

## Prochaine Étape

Vous maîtrisez maintenant les pointeurs et les enregistrements ! La prochaine section sur les **listes chaînées** approfondira ces concepts pour créer des structures de données encore plus sophistiquées, et nous verrons également les **arbres binaires** qui utilisent des principes similaires.

⏭️ [Listes chaînées simples](/06-pointeurs-gestion-memoire-basique/06-listes-chainees-simples.md)
