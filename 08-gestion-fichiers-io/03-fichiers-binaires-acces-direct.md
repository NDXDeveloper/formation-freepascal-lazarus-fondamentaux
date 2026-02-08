🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.3 Fichiers binaires et accès direct

## Introduction

Dans la section précédente, nous avons vu les fichiers texte qui sont faciles à lire et à éditer. Maintenant, nous allons découvrir les **fichiers binaires**, qui sont plus rapides et plus compacts, mais non lisibles directement par l'humain.

**Analogie :**
- Un fichier texte, c'est comme un livre avec des phrases lisibles
- Un fichier binaire, c'est comme une cassette audio : l'information est là, mais sous forme de données brutes non lisibles à l'œil nu

---

## Qu'est-ce qu'un fichier binaire ?

Un **fichier binaire** stocke les données directement en mémoire, sous leur forme brute (en octets), sans conversion en texte.

### Différences avec les fichiers texte

| Caractéristique | Fichier Texte | Fichier Binaire |
|-----------------|---------------|-----------------|
| **Contenu** | Caractères lisibles | Octets bruts |
| **Taille** | Plus grand (conversion en texte) | Plus compact |
| **Vitesse** | Plus lent (conversion) | Plus rapide |
| **Lisibilité** | Lisible avec un éditeur | Illisible directement |
| **Usage** | Logs, config, CSV | Images, son, données brutes |

### Exemple concret

Supposons que vous voulez stocker le nombre `12345` :

**En fichier texte :**
- Stocké comme : `'1'`, `'2'`, `'3'`, `'4'`, `'5'` (5 caractères = 5 octets)
- Lisible dans un éditeur de texte

**En fichier binaire :**
- Stocké directement comme : `12345` en format Integer (2 ou 4 octets selon le système)
- Plus compact, plus rapide, mais illisible dans un éditeur de texte

---

## Déclaration et ouverture

### Déclaration d'un fichier binaire

```pascal
var
  MonFichier: File;  // Fichier binaire non typé
```

**Note :** On utilise simplement `File` sans préciser `TextFile` ou `of Type`.

### Association et ouverture

```pascal
Assign(MonFichier, 'donnees.dat');

// Ouverture en lecture
Reset(MonFichier, 1);  // Le "1" indique la taille d'un bloc = 1 octet

// Ouverture en écriture (efface le contenu)
Rewrite(MonFichier, 1);
```

**Le deuxième paramètre :** la taille du bloc de lecture/écriture
- `1` = traite les données octet par octet (le plus courant)
- `512` = traite par blocs de 512 octets (plus rapide pour les gros fichiers)

---

## Lecture et écriture : BlockRead et BlockWrite

Contrairement aux fichiers texte, on utilise `BlockRead` et `BlockWrite` pour les fichiers binaires.

### Écrire des données : BlockWrite

```pascal
var
  MonFichier: File;
  Buffer: array[1..100] of Byte;
  NbEcrits: Word;
begin
  Assign(MonFichier, 'donnees.dat');
  Rewrite(MonFichier, 1);

  // Remplir le buffer avec des données
  Buffer[1] := 65;   // 'A' en ASCII
  Buffer[2] := 66;   // 'B' en ASCII
  Buffer[3] := 67;   // 'C' en ASCII

  // Écrire 3 octets dans le fichier
  BlockWrite(MonFichier, Buffer, 3, NbEcrits);

  WriteLn('Octets écrits : ', NbEcrits);

  Close(MonFichier);
end;
```

**Syntaxe :** `BlockWrite(Fichier, Buffer, NombreElements, NombreEcrits)`

**Paramètres :**
- `Fichier` : la variable fichier
- `Buffer` : les données à écrire (tableau, variable, record...)
- `NombreElements` : combien d'éléments écrire
- `NombreEcrits` : variable qui reçoit le nombre réellement écrit (optionnel)

### Lire des données : BlockRead

```pascal
var
  MonFichier: File;
  Buffer: array[1..100] of Byte;
  NbLus: Word;
  i: Integer;
begin
  Assign(MonFichier, 'donnees.dat');
  Reset(MonFichier, 1);

  // Lire 3 octets du fichier
  BlockRead(MonFichier, Buffer, 3, NbLus);

  WriteLn('Octets lus : ', NbLus);

  // Afficher les données lues
  for i := 1 to NbLus do
    Write(Chr(Buffer[i]));  // Convertit en caractère

  Close(MonFichier);
end;
```

**Syntaxe :** `BlockRead(Fichier, Buffer, NombreElements, NombreLus)`

---

## Exemple pratique : Copier un fichier

Un usage très courant des fichiers binaires est la copie de fichiers, quelle que soit leur nature (image, exécutable, etc.).

```pascal
program CopierFichier;

const
  TAILLE_BUFFER = 8192;  // 8 Ko par bloc

var
  FichierSource, FichierDest: File;
  Buffer: array[1..TAILLE_BUFFER] of Byte;
  NbLus, NbEcrits: Word;
  TotalCopie: LongInt;
  FichierTest: TextFile;
  i: Integer;

begin
  // Creer un petit fichier source pour la demonstration
  Assign(FichierTest, 'test_source.txt');
  Rewrite(FichierTest);
  for i := 1 to 100 do
    WriteLn(FichierTest, 'Ligne de test numero ', i);
  Close(FichierTest);
  WriteLn('Fichier source créé.');

  // Association des fichiers
  Assign(FichierSource, 'test_source.txt');
  Assign(FichierDest, 'test_copie.txt');

  // Ouverture
  Reset(FichierSource, 1);
  Rewrite(FichierDest, 1);

  TotalCopie := 0;

  // Copie bloc par bloc
  repeat
    BlockRead(FichierSource, Buffer, TAILLE_BUFFER, NbLus);

    if NbLus > 0 then
    begin
      BlockWrite(FichierDest, Buffer, NbLus, NbEcrits);
      TotalCopie := TotalCopie + NbLus;
    end;

  until NbLus < TAILLE_BUFFER;  // Fin quand on lit moins qu'un bloc complet

  // Fermeture
  Close(FichierSource);
  Close(FichierDest);

  WriteLn('Fichier copié : ', TotalCopie, ' octets');

  // Nettoyage des fichiers de test
  Assign(FichierSource, 'test_source.txt');
  Erase(FichierSource);
  Assign(FichierDest, 'test_copie.txt');
  Erase(FichierDest);
end.
```

**Explication :**
- On lit des blocs de 8192 octets (8 Ko) à la fois
- On écrit chaque bloc lu dans le fichier destination
- On s'arrête quand on lit moins d'un bloc complet (= fin du fichier)

---

## Accès direct aux données

L'un des grands avantages des fichiers binaires est la possibilité de faire de **l'accès direct** : aller directement à une position précise dans le fichier sans lire tout ce qui précède.

### Fonctions de positionnement

#### FilePos : Position courante

```pascal
var
  Position: LongInt;
begin
  Position := FilePos(MonFichier);
  WriteLn('Position actuelle : ', Position);
end;
```

Retourne la position courante dans le fichier (en nombre de blocs depuis le début).

#### FileSize : Taille du fichier

```pascal
var
  Taille: LongInt;
begin
  Taille := FileSize(MonFichier);
  WriteLn('Taille du fichier : ', Taille, ' blocs');
end;
```

Retourne la taille totale du fichier en nombre de blocs.

#### Seek : Se déplacer dans le fichier

```pascal
Seek(MonFichier, Position);
```

Place le curseur de lecture/écriture à la position spécifiée.

**Exemples :**
```pascal
Seek(MonFichier, 0);               // Aller au début
Seek(MonFichier, 100);             // Aller au 100ème bloc
Seek(MonFichier, FileSize(MonFichier));  // Aller à la fin
```

---

## Exemple : Accès direct à un enregistrement

Imaginons un fichier contenant des enregistrements de personnes. Avec l'accès direct, on peut lire le 10ème enregistrement sans lire les 9 premiers.

```pascal
program AccesDirect;

type
  TPersonne = record
    Nom: string[30];
    Age: Integer;
  end;

var
  Fichier: File;
  Personne: TPersonne;
  NumeroEnreg: Integer;
  NbLus: Word;

begin
  Assign(Fichier, 'personnes.dat');
  Reset(Fichier, SizeOf(TPersonne));  // Taille d'un bloc = taille d'une personne

  Write('Numéro de l''enregistrement à lire (1-', FileSize(Fichier), ') : ');
  ReadLn(NumeroEnreg);

  // Aller directement à l'enregistrement demandé
  Seek(Fichier, NumeroEnreg - 1);  // -1 car le fichier commence à 0

  // Lire cet enregistrement
  BlockRead(Fichier, Personne, 1, NbLus);

  if NbLus = 1 then
  begin
    WriteLn('Nom : ', Personne.Nom);
    WriteLn('Age : ', Personne.Age);
  end
  else
    WriteLn('Erreur de lecture !');

  Close(Fichier);
end.
```

**Avantage :** Si le fichier contient 1000 personnes, on peut accéder instantanément à la 500ème sans lire les 499 premières !

---

## Accès séquentiel vs Accès direct

### Accès séquentiel

**Définition :** Lire les données dans l'ordre, du début à la fin.

**Analogie :** C'est comme une cassette VHS que vous devez rembobiner pour revenir au début.

```pascal
// Lire tout le fichier séquentiellement
Reset(Fichier, 1);
while FilePos(Fichier) < FileSize(Fichier) do
begin
  BlockRead(Fichier, Buffer, 1);
  // Traiter le buffer
end;
Close(Fichier);
```

**Avantages :**
- Simple à programmer
- Efficace pour traiter tous les éléments

**Inconvénients :**
- Lent si on cherche un élément précis
- Doit lire tout ce qui précède

### Accès direct

**Définition :** Aller directement à une position précise dans le fichier.

**Analogie :** C'est comme un DVD où vous pouvez sauter directement au chapitre 5.

```pascal
// Lire directement le 100ème bloc
Reset(Fichier, 1);
Seek(Fichier, 99);  // Position 99 = 100ème bloc (car on commence à 0)
BlockRead(Fichier, Buffer, 1);
Close(Fichier);
```

**Avantages :**
- Très rapide pour accéder à un élément précis
- Pas besoin de lire ce qui précède

**Inconvénients :**
- Nécessite de connaître la position exacte
- Plus complexe à programmer

---

## Fonctions utiles supplémentaires

### EOF : Fin du fichier

```pascal
if EOF(MonFichier) then
  WriteLn('Fin du fichier atteinte');
```

Retourne `True` si on est à la fin du fichier.

### Truncate : Tronquer le fichier

```pascal
Seek(MonFichier, 100);  // Se positionner
Truncate(MonFichier);   // Couper tout ce qui suit
```

Coupe le fichier à la position courante, supprimant tout ce qui suit.

### Rename : Renommer le fichier

```pascal
Close(MonFichier);  // Le fichier doit être fermé
Rename(MonFichier, 'nouveau_nom.dat');
```

Renomme le fichier associé.

### Erase : Supprimer le fichier

```pascal
Close(MonFichier);  // Le fichier doit être fermé
Erase(MonFichier);
```

Supprime définitivement le fichier du disque.

---

## Exemple complet : Mini base de données

Créons un petit programme qui gère une base de données de contacts avec accès direct.

```pascal
program BaseDeDonnees;

type
  TContact = record
    Nom: string[30];
    Telephone: string[15];
    Email: string[50];
  end;

var
  Fichier: File;
  Contact: TContact;
  Choix, Position: Integer;
  NomFichier: string;

procedure AjouterContact;
var
  NbEcrits: Word;
begin
  WriteLn('--- Ajouter un contact ---');
  Write('Nom : '); ReadLn(Contact.Nom);
  Write('Téléphone : '); ReadLn(Contact.Telephone);
  Write('Email : '); ReadLn(Contact.Email);

  // Aller à la fin du fichier
  Seek(Fichier, FileSize(Fichier));

  // Écrire le nouveau contact
  BlockWrite(Fichier, Contact, 1, NbEcrits);

  if NbEcrits = 1 then
    WriteLn('Contact ajouté avec succès !')
  else
    WriteLn('Erreur lors de l''ajout.');
end;

procedure AfficherContact(Num: Integer);
var
  NbLus: Word;
begin
  if (Num < 1) or (Num > FileSize(Fichier)) then
  begin
    WriteLn('Numéro invalide !');
    Exit;
  end;

  // Aller directement au contact demandé
  Seek(Fichier, Num - 1);
  BlockRead(Fichier, Contact, 1, NbLus);

  if NbLus = 1 then
  begin
    WriteLn('--- Contact #', Num, ' ---');
    WriteLn('Nom : ', Contact.Nom);
    WriteLn('Téléphone : ', Contact.Telephone);
    WriteLn('Email : ', Contact.Email);
  end;
end;

procedure ListerContacts;
var
  i: Integer;
  NbLus: Word;
begin
  WriteLn('--- Liste des contacts ---');
  WriteLn('Total : ', FileSize(Fichier), ' contact(s)');
  WriteLn;

  Seek(Fichier, 0);  // Retour au début

  for i := 1 to FileSize(Fichier) do
  begin
    BlockRead(Fichier, Contact, 1, NbLus);
    if NbLus = 1 then
      WriteLn(i:3, '. ', Contact.Nom);
  end;
end;

begin
  NomFichier := 'contacts.dat';
  Assign(Fichier, NomFichier);

  // Ouvrir ou créer le fichier
  {$I-}
  Reset(Fichier, SizeOf(TContact));  // Taille de bloc = taille d'un record, permet l'accès par numéro de contact
  {$I+}

  if IOResult <> 0 then
    Rewrite(Fichier, SizeOf(TContact));

  // Menu principal
  repeat
    WriteLn;
    WriteLn('=== GESTIONNAIRE DE CONTACTS ===');
    WriteLn('1. Ajouter un contact');
    WriteLn('2. Afficher un contact');
    WriteLn('3. Lister tous les contacts');
    WriteLn('0. Quitter');
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1: AjouterContact;
      2: begin
           Write('Numéro du contact : ');
           ReadLn(Position);
           AfficherContact(Position);
         end;
      3: ListerContacts;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;

  Close(Fichier);
end.
```

**Ce programme démontre :**
- L'ajout séquentiel de contacts (à la fin)
- L'accès direct pour lire un contact précis
- L'accès séquentiel pour lister tous les contacts
- La persistance des données entre les exécutions

---

## Avantages et inconvénients

### Avantages des fichiers binaires

✅ **Compacts** : prennent moins de place que les fichiers texte

✅ **Rapides** : pas de conversion, lecture/écriture directe

✅ **Accès direct** : aller instantanément à n'importe quelle position

✅ **Précis** : aucune perte de précision pour les nombres

✅ **Polyvalents** : peuvent stocker n'importe quel type de données

### Inconvénients des fichiers binaires

❌ **Illisibles** : impossible de lire avec un éditeur de texte

❌ **Pas portables** : dépendent parfois du système (ordre des octets)

❌ **Fragiles** : une erreur peut corrompre tout le fichier

❌ **Difficiles à déboguer** : on ne peut pas "voir" le contenu facilement

---

## Tableau récapitulatif des fonctions

| Fonction | Description | Usage |
|----------|-------------|-------|
| `Reset(F, TailleBloc)` | Ouvre en lecture | Lire un fichier existant |
| `Rewrite(F, TailleBloc)` | Ouvre en écriture | Créer un nouveau fichier |
| `BlockRead(F, Buffer, N, Lu)` | Lit N blocs | Lire des données |
| `BlockWrite(F, Buffer, N, Ecrit)` | Écrit N blocs | Écrire des données |
| `Seek(F, Position)` | Se déplacer | Accès direct |
| `FilePos(F)` | Position actuelle | Savoir où on est |
| `FileSize(F)` | Taille du fichier | Connaître la taille |
| `EOF(F)` | Fin du fichier ? | Contrôler les boucles |
| `Truncate(F)` | Couper le fichier | Réduire la taille |
| `Close(F)` | Fermer | Toujours en fin |

---

## Bonnes pratiques

✅ **Toujours utiliser SizeOf()** pour définir la taille des blocs quand vous travaillez avec des records

✅ **Vérifier le nombre d'octets lus/écrits** pour détecter les erreurs

✅ **Fermer les fichiers** immédiatement après usage

✅ **Utiliser des constantes** pour les tailles de buffer (facilite les modifications)

✅ **Gérer les erreurs** avec `{$I-}` et `IOResult`

✅ **Documenter** la structure de vos fichiers binaires (sinon, vous oublierez !)

✅ **Faire des sauvegardes** avant de modifier un fichier binaire

❌ **Ne pas mélanger** fichiers texte et binaires pour le même fichier

---

## Résumé

Les fichiers binaires offrent :

- **Performance** : lecture/écriture très rapide
- **Compacité** : stockage optimisé
- **Accès direct** : navigation instantanée avec `Seek()`
- **Flexibilité** : copie de n'importe quel type de fichier

**Fonctions clés :**
- `BlockRead` / `BlockWrite` pour lire/écrire
- `Seek` pour se déplacer
- `FilePos` / `FileSize` pour se repérer

Dans la section suivante, nous verrons les **fichiers typés**, qui combinent la structure des records avec la puissance des fichiers binaires !

---

> **À retenir :** Les fichiers binaires sont parfaits quand vous avez besoin de vitesse et d'accès direct, mais ils nécessitent plus de rigueur dans la programmation que les fichiers texte.

⏭️ [Fichiers typés](/08-gestion-fichiers-io/04-fichiers-types.md)
