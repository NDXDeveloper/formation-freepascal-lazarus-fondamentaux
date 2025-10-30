🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.4 Fichiers typés

## Introduction

Les **fichiers typés** (typed files) représentent un compromis parfait entre la simplicité des fichiers texte et la puissance des fichiers binaires. Ils combinent le meilleur des deux mondes !

**Analogie :**
- Un fichier texte, c'est comme un cahier où vous écrivez librement
- Un fichier binaire non typé, c'est comme une boîte où vous rangez n'importe quoi en vrac
- Un fichier typé, c'est comme un classeur avec des fiches toutes identiques : chaque case a un format précis et connu

---

## Qu'est-ce qu'un fichier typé ?

Un **fichier typé** est un fichier binaire où tous les éléments ont exactement le même type et la même taille. C'est comme un tableau stocké sur le disque dur.

### Caractéristiques principales

✅ **Structure homogène** : tous les éléments sont du même type

✅ **Accès direct** : on peut lire le 100ème élément sans lire les 99 premiers

✅ **Type sécurisé** : Pascal vérifie que vous manipulez le bon type

✅ **Efficace** : stockage compact et rapide

✅ **Simple à utiliser** : plus facile que les fichiers binaires non typés

### Différences avec les autres types de fichiers

| Caractéristique | Fichier Texte | Fichier Binaire Non Typé | Fichier Typé |
|-----------------|---------------|--------------------------|--------------|
| **Déclaration** | `TextFile` | `File` | `File of Type` |
| **Type des données** | Texte uniquement | Octets bruts | Type spécifique |
| **Structure** | Lignes variables | Libre | Éléments fixes |
| **Accès direct** | Non | Oui | Oui |
| **Sécurité de type** | Non | Non | Oui |
| **Facilité** | Très facile | Complexe | Facile |

---

## Déclaration de fichiers typés

### Syntaxe générale

```pascal
var
  NomFichier: File of TypeDonnees;
```

### Exemples de déclarations

```pascal
var
  FichierEntiers: File of Integer;
  FichierReels: File of Real;
  FichierCaracteres: File of Char;
  FichierBooleens: File of Boolean;
```

### Fichiers typés avec des records

C'est l'utilisation la plus courante et la plus utile :

```pascal
type
  TPersonne = record
    Nom: string[30];
    Prenom: string[30];
    Age: Integer;
    Salaire: Real;
  end;

var
  FichierPersonnes: File of TPersonne;
```

---

## Ouverture et fermeture

### Ouverture en lecture : Reset

```pascal
var
  F: File of Integer;
begin
  Assign(F, 'nombres.dat');
  Reset(F);  // Ouvre en lecture/écriture
  // ... traitement ...
  Close(F);
end;
```

**Important :** Contrairement aux fichiers texte, `Reset` ouvre le fichier typé en lecture **ET** en écriture simultanément !

### Ouverture en création : Rewrite

```pascal
var
  F: File of Integer;
begin
  Assign(F, 'nombres.dat');
  Rewrite(F);  // Crée le fichier (efface s'il existe)
  // ... traitement ...
  Close(F);
end;
```

**Attention :** `Rewrite` efface tout le contenu existant !

---

## Lecture de fichiers typés

### Lire un élément : Read

```pascal
var
  F: File of Integer;
  Nombre: Integer;
begin
  Assign(F, 'nombres.dat');
  Reset(F);

  Read(F, Nombre);  // Lit un entier
  WriteLn('Nombre lu : ', Nombre);

  Close(F);
end;
```

**Syntaxe :** `Read(Fichier, Variable)`

La variable doit être du même type que le fichier !

### Lire plusieurs éléments

```pascal
var
  F: File of Integer;
  N1, N2, N3: Integer;
begin
  Assign(F, 'nombres.dat');
  Reset(F);

  Read(F, N1, N2, N3);  // Lit 3 entiers d'un coup

  WriteLn('Nombres : ', N1, ', ', N2, ', ', N3);

  Close(F);
end;
```

### Lire tout le fichier

```pascal
var
  F: File of Integer;
  Nombre: Integer;
begin
  Assign(F, 'nombres.dat');
  Reset(F);

  while not EOF(F) do
  begin
    Read(F, Nombre);
    WriteLn(Nombre);
  end;

  Close(F);
end;
```

---

## Écriture dans des fichiers typés

### Écrire un élément : Write

```pascal
var
  F: File of Integer;
  Nombre: Integer;
begin
  Assign(F, 'nombres.dat');
  Rewrite(F);

  Nombre := 42;
  Write(F, Nombre);  // Écrit un entier

  Close(F);
end;
```

**Syntaxe :** `Write(Fichier, Valeur)`

### Écrire plusieurs éléments

```pascal
var
  F: File of Integer;
  i: Integer;
begin
  Assign(F, 'nombres.dat');
  Rewrite(F);

  // Écrire les 10 premiers carrés
  for i := 1 to 10 do
    Write(F, i * i);

  Close(F);
end;
```

### Écrire plusieurs valeurs d'un coup

```pascal
var
  F: File of Integer;
begin
  Assign(F, 'nombres.dat');
  Rewrite(F);

  Write(F, 10, 20, 30, 40, 50);  // Écrit 5 entiers en une seule instruction

  Close(F);
end;
```

---

## Exemple avec des records

Voici un exemple complet utilisant des enregistrements, qui est le cas d'usage le plus pratique.

```pascal
program GestionEmployes;

type
  TEmploye = record
    Numero: Integer;
    Nom: string[40];
    Poste: string[30];
    Salaire: Real;
  end;

var
  F: File of TEmploye;
  Employe: TEmploye;

procedure AjouterEmploye(Num: Integer; N: string; P: string; S: Real);
begin
  Employe.Numero := Num;
  Employe.Nom := N;
  Employe.Poste := P;
  Employe.Salaire := S;

  Write(F, Employe);
  WriteLn('Employé ajouté : ', N);
end;

procedure AfficherTous;
begin
  Seek(F, 0);  // Retour au début

  WriteLn('=== LISTE DES EMPLOYÉS ===');
  WriteLn;

  while not EOF(F) do
  begin
    Read(F, Employe);
    WriteLn('N° ', Employe.Numero:4, ' | ',
            Employe.Nom:20, ' | ',
            Employe.Poste:15, ' | ',
            Employe.Salaire:8:2, ' €');
  end;
end;

begin
  // Création du fichier
  Assign(F, 'employes.dat');
  Rewrite(F);

  // Ajout de quelques employés
  AjouterEmploye(1, 'Dupont Jean', 'Développeur', 35000);
  AjouterEmploye(2, 'Martin Sophie', 'Chef de projet', 45000);
  AjouterEmploye(3, 'Durand Pierre', 'Technicien', 28000);
  AjouterEmploye(4, 'Bernard Marie', 'Analyste', 38000);

  WriteLn;

  // Affichage de tous les employés
  AfficherTous;

  Close(F);
end.
```

**Résultat :**
```
Employé ajouté : Dupont Jean
Employé ajouté : Martin Sophie
Employé ajouté : Durand Pierre
Employé ajouté : Bernard Marie

=== LISTE DES EMPLOYÉS ===

N°    1 | Dupont Jean          | Développeur     | 35000.00 €
N°    2 | Martin Sophie        | Chef de projet  | 45000.00 €
N°    3 | Durand Pierre        | Technicien      | 28000.00 €
N°    4 | Bernard Marie        | Analyste        | 38000.00 €
```

---

## Accès direct dans les fichiers typés

L'un des grands avantages des fichiers typés est la possibilité d'accéder directement à n'importe quel élément.

### Fonctions de navigation

#### FileSize : Nombre d'éléments

```pascal
var
  F: File of Integer;
  NbElements: LongInt;
begin
  Assign(F, 'nombres.dat');
  Reset(F);

  NbElements := FileSize(F);
  WriteLn('Le fichier contient ', NbElements, ' éléments');

  Close(F);
end;
```

**Note :** `FileSize` retourne le nombre d'éléments (pas d'octets) dans le fichier.

#### FilePos : Position courante

```pascal
var
  Position: LongInt;
begin
  Position := FilePos(F);
  WriteLn('Position actuelle : ', Position);
end;
```

La position commence à 0 (premier élément = position 0).

#### Seek : Se déplacer

```pascal
Seek(F, Position);
```

Place le curseur à la position spécifiée.

### Exemple d'accès direct

```pascal
program AccesDirectEmploye;

type
  TEmploye = record
    Numero: Integer;
    Nom: string[40];
    Salaire: Real;
  end;

var
  F: File of TEmploye;
  Employe: TEmploye;
  NumRecherche: Integer;

begin
  Assign(F, 'employes.dat');
  Reset(F);

  WriteLn('Le fichier contient ', FileSize(F), ' employés');
  Write('Numéro de l''employé à afficher (0-', FileSize(F)-1, ') : ');
  ReadLn(NumRecherche);

  if (NumRecherche >= 0) and (NumRecherche < FileSize(F)) then
  begin
    // Accès direct à l'employé
    Seek(F, NumRecherche);
    Read(F, Employe);

    WriteLn;
    WriteLn('=== EMPLOYÉ #', NumRecherche, ' ===');
    WriteLn('Numéro   : ', Employe.Numero);
    WriteLn('Nom      : ', Employe.Nom);
    WriteLn('Salaire  : ', Employe.Salaire:0:2, ' €');
  end
  else
    WriteLn('Numéro invalide !');

  Close(F);
end.
```

---

## Modification d'un élément existant

Grâce à l'ouverture en lecture/écriture, on peut modifier un élément en place.

```pascal
program ModifierEmploye;

type
  TEmploye = record
    Numero: Integer;
    Nom: string[40];
    Salaire: Real;
  end;

var
  F: File of TEmploye;
  Employe: TEmploye;
  Position: Integer;
  NouveauSalaire: Real;

begin
  Assign(F, 'employes.dat');
  Reset(F);  // Ouvre en lecture ET écriture

  Write('Position de l''employé à modifier : ');
  ReadLn(Position);

  if (Position >= 0) and (Position < FileSize(F)) then
  begin
    // Lire l'employé actuel
    Seek(F, Position);
    Read(F, Employe);

    WriteLn('Employé actuel : ', Employe.Nom);
    WriteLn('Salaire actuel : ', Employe.Salaire:0:2);

    Write('Nouveau salaire : ');
    ReadLn(NouveauSalaire);

    // Modifier
    Employe.Salaire := NouveauSalaire;

    // Revenir à la position et réécrire
    Seek(F, Position);
    Write(F, Employe);

    WriteLn('Salaire modifié avec succès !');
  end
  else
    WriteLn('Position invalide !');

  Close(F);
end.
```

**Étapes importantes :**
1. `Reset` ouvre en lecture/écriture
2. `Seek` pour se positionner
3. `Read` pour lire l'élément
4. Modification dans la variable
5. `Seek` à nouveau pour revenir à la même position
6. `Write` pour réécrire l'élément modifié

---

## Suppression et ajout d'éléments

### Ajouter à la fin

```pascal
procedure AjouterEmploye(var F: File of TEmploye; E: TEmploye);
begin
  Seek(F, FileSize(F));  // Aller à la fin
  Write(F, E);
end;
```

### "Supprimer" un élément

Les fichiers typés ne permettent pas vraiment de supprimer un élément. Voici deux approches :

#### Méthode 1 : Marquer comme supprimé

```pascal
type
  TEmploye = record
    Numero: Integer;
    Nom: string[40];
    Salaire: Real;
    Actif: Boolean;  // True = actif, False = supprimé
  end;

procedure SupprimerEmploye(var F: File of TEmploye; Position: Integer);
var
  Emp: TEmploye;
begin
  Seek(F, Position);
  Read(F, Emp);

  Emp.Actif := False;  // Marquer comme supprimé

  Seek(F, Position);
  Write(F, Emp);
end;
```

#### Méthode 2 : Copier dans un nouveau fichier

```pascal
procedure SupprimerEmploye(Position: Integer);
var
  Ancien, Nouveau: File of TEmploye;
  Emp: TEmploye;
  i: Integer;
begin
  Assign(Ancien, 'employes.dat');
  Assign(Nouveau, 'employes_temp.dat');

  Reset(Ancien);
  Rewrite(Nouveau);

  // Copier tous les éléments sauf celui à supprimer
  for i := 0 to FileSize(Ancien) - 1 do
  begin
    Seek(Ancien, i);
    Read(Ancien, Emp);

    if i <> Position then
      Write(Nouveau, Emp);
  end;

  Close(Ancien);
  Close(Nouveau);

  // Remplacer l'ancien fichier
  Erase(Ancien);
  Rename(Nouveau, 'employes.dat');
end;
```

---

## Exemple complet : Carnet d'adresses

Voici une application complète de gestion de contacts.

```pascal
program CarnetAdresses;

type
  TContact = record
    ID: Integer;
    Nom: string[30];
    Prenom: string[30];
    Telephone: string[15];
    Email: string[50];
  end;

var
  Fichier: File of TContact;
  Contact: TContact;
  Choix: Integer;

procedure InitialiserFichier;
begin
  Assign(Fichier, 'contacts.dat');
  {$I-}
  Reset(Fichier);
  {$I+}
  if IOResult <> 0 then
    Rewrite(Fichier);
end;

function ProchainID: Integer;
var
  C: TContact;
  MaxID: Integer;
begin
  MaxID := 0;
  Seek(Fichier, 0);

  while not EOF(Fichier) do
  begin
    Read(Fichier, C);
    if C.ID > MaxID then
      MaxID := C.ID;
  end;

  ProchainID := MaxID + 1;
end;

procedure AjouterContact;
begin
  WriteLn('=== NOUVEAU CONTACT ===');

  Contact.ID := ProchainID;

  Write('Nom : ');
  ReadLn(Contact.Nom);

  Write('Prénom : ');
  ReadLn(Contact.Prenom);

  Write('Téléphone : ');
  ReadLn(Contact.Telephone);

  Write('Email : ');
  ReadLn(Contact.Email);

  Seek(Fichier, FileSize(Fichier));
  Write(Fichier, Contact);

  WriteLn('Contact ajouté avec l''ID : ', Contact.ID);
end;

procedure AfficherTous;
begin
  if FileSize(Fichier) = 0 then
  begin
    WriteLn('Aucun contact dans le carnet.');
    Exit;
  end;

  WriteLn('=== LISTE DES CONTACTS ===');
  WriteLn;

  Seek(Fichier, 0);

  while not EOF(Fichier) do
  begin
    Read(Fichier, Contact);
    WriteLn('ID      : ', Contact.ID);
    WriteLn('Nom     : ', Contact.Nom, ' ', Contact.Prenom);
    WriteLn('Tél     : ', Contact.Telephone);
    WriteLn('Email   : ', Contact.Email);
    WriteLn('----------------------------');
  end;

  WriteLn('Total : ', FileSize(Fichier), ' contact(s)');
end;

procedure RechercherParNom;
var
  NomRecherche: string;
  Trouve: Boolean;
begin
  Write('Nom à rechercher : ');
  ReadLn(NomRecherche);

  Trouve := False;
  Seek(Fichier, 0);

  while not EOF(Fichier) do
  begin
    Read(Fichier, Contact);

    if Pos(NomRecherche, Contact.Nom) > 0 then
    begin
      Trouve := True;
      WriteLn;
      WriteLn('Trouvé : ', Contact.Nom, ' ', Contact.Prenom);
      WriteLn('Tél    : ', Contact.Telephone);
      WriteLn('Email  : ', Contact.Email);
    end;
  end;

  if not Trouve then
    WriteLn('Aucun contact trouvé.');
end;

begin
  InitialiserFichier;

  repeat
    WriteLn;
    WriteLn('=== CARNET D''ADRESSES ===');
    WriteLn('1. Ajouter un contact');
    WriteLn('2. Afficher tous les contacts');
    WriteLn('3. Rechercher par nom');
    WriteLn('0. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1: AjouterContact;
      2: AfficherTous;
      3: RechercherParNom;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;

  Close(Fichier);
end.
```

---

## Avantages et inconvénients

### Avantages des fichiers typés

✅ **Sécurité de type** : Pascal vérifie que vous utilisez le bon type

✅ **Simplicité** : plus facile que les fichiers binaires non typés (pas besoin de BlockRead/BlockWrite)

✅ **Accès direct** : navigation rapide avec Seek

✅ **Lecture/Écriture simple** : Read et Write comme pour les fichiers texte

✅ **Idéal pour les bases de données simples** : parfait pour stocker des enregistrements

✅ **Efficacité** : stockage compact comme les fichiers binaires

### Inconvénients des fichiers typés

❌ **Type unique** : tous les éléments doivent être du même type

❌ **Taille fixe** : tous les éléments ont la même taille (peut gaspiller de l'espace)

❌ **Pas de suppression facile** : nécessite des workarounds

❌ **Non lisibles** : comme tous les fichiers binaires

❌ **Portabilité limitée** : dépend de la taille des types (Integer peut varier)

---

## Comparaison avec les autres types de fichiers

### Quand utiliser des fichiers typés ?

**Utilisez des fichiers typés quand :**
- Vous stockez une collection d'éléments identiques
- Vous avez besoin d'accès direct
- Vous voulez une solution simple et type-safe
- Vous créez une petite base de données

**N'utilisez PAS de fichiers typés quand :**
- Vous avez besoin de lisibilité humaine → utilisez des fichiers texte
- Vous manipulez des fichiers existants (images, etc.) → utilisez File
- Les éléments ont des tailles variables → utilisez des fichiers texte ou binaires
- Vous avez besoin de portabilité absolue → utilisez JSON ou XML

---

## Tableau récapitulatif

| Opération | Syntaxe | Description |
|-----------|---------|-------------|
| **Déclaration** | `var F: File of Type;` | Déclare un fichier typé |
| **Association** | `Assign(F, 'fichier.dat');` | Associe à un fichier physique |
| **Ouverture lecture/écriture** | `Reset(F);` | Ouvre en lecture ET écriture |
| **Création** | `Rewrite(F);` | Crée le fichier (efface) |
| **Lecture** | `Read(F, Variable);` | Lit un ou plusieurs éléments |
| **Écriture** | `Write(F, Valeur);` | Écrit un ou plusieurs éléments |
| **Taille** | `FileSize(F)` | Nombre d'éléments |
| **Position** | `FilePos(F)` | Position courante |
| **Déplacement** | `Seek(F, Position);` | Se déplacer |
| **Fin de fichier** | `EOF(F)` | Teste la fin |
| **Fermeture** | `Close(F);` | Ferme le fichier |

---

## Bonnes pratiques

✅ **Toujours vérifier EOF** avant de lire pour éviter les erreurs

✅ **Utiliser des records bien structurés** avec des tailles de string fixes

✅ **Gérer les erreurs** avec `{$I-}` et `IOResult`

✅ **Fermer les fichiers** immédiatement après usage

✅ **Documenter la structure** de vos records (pour vous en souvenir plus tard)

✅ **Faire des sauvegardes** avant de modifier des fichiers de données

✅ **Utiliser Seek avant Write** pour modifier un élément existant

✅ **Initialiser tous les champs** d'un record avant de l'écrire

❌ **Ne pas utiliser de string sans taille** : utilisez `string[N]` pour une taille fixe

❌ **Ne pas mélanger les types** : un fichier = un seul type

---

## Résumé

Les fichiers typés sont parfaits pour créer des **bases de données simples** en Pascal :

**Caractéristiques clés :**
- Déclaration : `File of Type`
- Manipulation simple avec `Read` et `Write`
- Accès direct avec `Seek`
- Type sécurisé et efficace

**Usage typique :**
- Gestion de contacts, employés, produits
- Petites bases de données
- Stockage de structures uniformes

**À retenir :**
- Tous les éléments ont le même type et la même taille
- `Reset` ouvre en lecture **ET** écriture
- Parfait compromis entre simplicité et performance

Dans la section suivante, nous verrons comment gérer les erreurs d'entrée/sortie de manière robuste !

---

> **Conseil pratique :** Les fichiers typés sont idéaux pour apprendre la gestion de données persistantes. Commencez par des exemples simples avant de créer des applications plus complexes.

⏭️ [Gestion des erreurs I/O](/08-gestion-fichiers-io/05-gestion-erreurs-io.md)
