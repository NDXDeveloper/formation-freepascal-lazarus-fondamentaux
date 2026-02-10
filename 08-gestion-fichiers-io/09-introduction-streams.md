🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.9 Introduction aux streams

## Introduction

Les **streams** (flux en français) sont une abstraction puissante pour manipuler des données, qu'elles proviennent d'un fichier, de la mémoire, d'un réseau ou d'une autre source. Ils offrent une interface unifiée et moderne pour lire et écrire des données.

**Analogie :**
Imaginez un stream comme un **tuyau d'eau** :
- L'eau (les données) coule dans le tuyau
- Vous pouvez **ouvrir le robinet** (lire) ou **y verser de l'eau** (écrire)
- Le tuyau peut être connecté à différentes sources : **réservoir** (fichier), **bouteille** (mémoire), **fontaine** (réseau)
- Peu importe la source, vous manipulez l'eau de la même façon !

---

## Qu'est-ce qu'un stream ?

Un **stream** est un objet qui représente une séquence d'octets (bytes) que vous pouvez lire ou écrire de manière séquentielle ou directe.

### Concept de base

```
[Source] ──→ [Stream] ──→ [Destination]
           ↑           ↓
         Read        Write
```

**Caractéristiques communes :**
- Position courante (curseur)
- Taille totale
- Opérations de lecture/écriture
- Déplacement dans le flux (seek)

### Avantages des streams

✅ **Interface unifiée** : même code pour fichier, mémoire, réseau

✅ **Flexibilité** : facile de changer de source sans modifier le code

✅ **Puissance** : copie, conversion, compression simplifiées

✅ **Moderne** : approche orientée objet

✅ **Efficacité** : buffering automatique, optimisations

✅ **Composabilité** : possibilité d'enchaîner les streams

---

## Hiérarchie des classes de streams

Pascal/Lazarus propose plusieurs types de streams héritant de la classe de base **TStream**.

```
TStream (classe abstraite)
├── THandleStream
│   └── TFileStream          → Fichiers sur disque
├── TMemoryStream            → Données en mémoire (RAM)
├── TStringStream            → Chaînes de caractères
├── TResourceStream          → Ressources embarquées
└── Autres (réseau, compression, etc.)
```

### TStream : La classe de base

**TStream** définit les méthodes communes à tous les streams :

| Méthode | Description |
|---------|-------------|
| `Read(Buffer, Count)` | Lire Count octets |
| `Write(Buffer, Count)` | Écrire Count octets |
| `Seek(Offset, Origin)` | Déplacer le curseur |
| `Position` | Position actuelle |
| `Size` | Taille totale |
| `CopyFrom(Source, Count)` | Copier depuis un autre stream |

---

## Unités nécessaires

```pascal
uses
  Classes,    // Pour TStream, TMemoryStream, TStringStream
  SysUtils;   // Pour les fonctions utilitaires
```

---

## TFileStream : Streams sur fichiers

**TFileStream** permet de manipuler des fichiers comme des streams.

### Création d'un TFileStream

```pascal
uses
  Classes, SysUtils;

var
  Stream: TFileStream;

begin
  // Créer/ouvrir un fichier
  Stream := TFileStream.Create('donnees.dat', fmCreate);
  try
    // Utiliser le stream
  finally
    Stream.Free;  // Toujours libérer !
  end;
end.
```

### Modes d'ouverture

| Mode | Description |
|------|-------------|
| `fmCreate` | Créer (écrase si existe) |
| `fmOpenRead` | Ouvrir en lecture seule |
| `fmOpenWrite` | Ouvrir en écriture seule |
| `fmOpenReadWrite` | Ouvrir en lecture/écriture |

**Combinaison avec les droits de partage :**

```pascal
// Ouvrir en lecture, autoriser les autres à lire aussi
Stream := TFileStream.Create('fichier.dat', fmOpenRead or fmShareDenyNone);
```

Modes de partage :
- `fmShareExclusive` : accès exclusif
- `fmShareDenyWrite` : autres peuvent lire
- `fmShareDenyRead` : autres peuvent écrire
- `fmShareDenyNone` : accès partagé complet

### Écrire dans un TFileStream

```pascal
{$mode objfpc}{$H+}
program EcrireStream;

uses
  Classes, SysUtils;

var
  Stream: TFileStream;
  Texte: string;
  Nombre: Integer;

begin
  Stream := TFileStream.Create('test.dat', fmCreate);
  try
    // Écrire une chaîne
    Texte := 'Bonjour le monde !';
    Stream.Write(Texte[1], Length(Texte));  // Texte[1] donne l'adresse du 1er caractère (Write attend un buffer, pas un pointeur de string)

    // Écrire un nombre
    Nombre := 42;
    Stream.Write(Nombre, SizeOf(Nombre));

    WriteLn('Données écrites : ', Stream.Size, ' octets');
  finally
    Stream.Free;
  end;
end.
```

### Lire depuis un TFileStream

```pascal
{$mode objfpc}{$H+}
program LireStream;

uses
  Classes, SysUtils;

var
  Stream: TFileStream;
  Buffer: array[0..255] of Char;
  NbLus: Integer;

begin
  if not FileExists('test.dat') then
  begin
    WriteLn('Fichier introuvable !');
    Exit;
  end;

  Stream := TFileStream.Create('test.dat', fmOpenRead);
  try
    // Lire des données
    NbLus := Stream.Read(Buffer, SizeOf(Buffer));

    WriteLn('Octets lus : ', NbLus);
    WriteLn('Taille totale du stream : ', Stream.Size);
  finally
    Stream.Free;
  end;
end.
```

### Navigation dans un stream

```pascal
var
  Stream: TFileStream;
  Position: Int64;

begin
  Stream := TFileStream.Create('donnees.dat', fmOpenReadWrite);
  try
    // Obtenir la position actuelle
    Position := Stream.Position;
    WriteLn('Position : ', Position);

    // Se déplacer à une position précise
    Stream.Position := 100;  // Aller à l'octet 100

    // Se déplacer relativement
    Stream.Seek(50, soFromCurrent);    // Avancer de 50 octets
    Stream.Seek(-20, soFromCurrent);   // Reculer de 20 octets
    Stream.Seek(0, soFromBeginning);   // Retour au début
    Stream.Seek(0, soFromEnd);         // Aller à la fin

    // Taille du stream
    WriteLn('Taille : ', Stream.Size, ' octets');
  finally
    Stream.Free;
  end;
end.
```

**Origines pour Seek :**
- `soFromBeginning` (ou `soBeginning`) : depuis le début
- `soFromCurrent` (ou `soCurrent`) : depuis la position actuelle
- `soFromEnd` (ou `soEnd`) : depuis la fin

---

## TMemoryStream : Streams en mémoire

**TMemoryStream** stocke les données en mémoire RAM. C'est très rapide mais limité par la mémoire disponible.

### Création et utilisation

```pascal
{$mode objfpc}{$H+}
program StreamMemoire;

uses
  Classes, SysUtils;

var
  Stream: TMemoryStream;
  Texte: string;
  i: Integer;

begin
  Stream := TMemoryStream.Create;
  try
    // Écrire des données en mémoire
    Texte := 'Données en mémoire';
    Stream.Write(Texte[1], Length(Texte));

    // Écrire des nombres
    for i := 1 to 10 do
      Stream.Write(i, SizeOf(i));

    WriteLn('Taille du stream en mémoire : ', Stream.Size, ' octets');

    // Retour au début pour lire
    Stream.Position := 0;

    // On pourrait lire ici...

  finally
    Stream.Free;
  end;
end.
```

### Sauvegarder et charger depuis un fichier

```pascal
var
  Stream: TMemoryStream;

begin
  Stream := TMemoryStream.Create;
  try
    // Écrire des données en mémoire
    // ...

    // Sauvegarder tout le stream dans un fichier
    Stream.SaveToFile('backup.dat');
    WriteLn('Stream sauvegardé');

    // Vider le stream
    Stream.Clear;

    // Charger depuis un fichier
    Stream.LoadFromFile('backup.dat');
    WriteLn('Stream chargé : ', Stream.Size, ' octets');

  finally
    Stream.Free;
  end;
end.
```

### Accéder directement aux données : Memory

```pascal
var
  Stream: TMemoryStream;
  P: PByte;
  i: Integer;

begin
  Stream := TMemoryStream.Create;
  try
    // Écrire des données
    for i := 0 to 9 do
      Stream.WriteByte(i * 10);

    // Accès direct au buffer mémoire
    P := Stream.Memory;

    WriteLn('Données dans le stream :');
    for i := 0 to Stream.Size - 1 do
      Write(P[i], ' ');
    WriteLn;

  finally
    Stream.Free;
  end;
end.
```

---

## TStringStream : Streams de chaînes

**TStringStream** est optimisé pour manipuler des chaînes de caractères.

### Création et utilisation

```pascal
{$mode objfpc}{$H+}
program StreamChaine;

uses
  Classes, SysUtils;

var
  Stream: TStringStream;

begin
  // Créer avec un contenu initial
  Stream := TStringStream.Create('Contenu initial');
  try
    WriteLn('Contenu : ', Stream.DataString);

    // Se placer à la fin pour ajouter du contenu
    Stream.Seek(0, soFromEnd);
    Stream.WriteString(' - Ajout de texte');

    WriteLn('Nouveau contenu : ', Stream.DataString);

    // Retour au début et lecture
    Stream.Position := 0;

    WriteLn('Taille : ', Stream.Size, ' octets');
  finally
    Stream.Free;
  end;
end.
```

### Conversion entre chaînes et streams

```pascal
var
  Stream: TStringStream;
  Texte: string;

begin
  // De chaîne vers stream
  Texte := 'Bonjour tout le monde !';
  Stream := TStringStream.Create(Texte);
  try
    WriteLn('Taille du stream : ', Stream.Size);

    // De stream vers chaîne
    Texte := Stream.DataString;
    WriteLn('Récupéré : ', Texte);
  finally
    Stream.Free;
  end;
end.
```

---

## Copier entre streams : CopyFrom

La méthode **CopyFrom** permet de copier facilement d'un stream vers un autre.

### Copier un fichier avec des streams

```pascal
{$mode objfpc}{$H+}
program CopierStream;

uses
  Classes, SysUtils;

procedure CopierFichierStream(Source, Destination: string);  
var  
  StreamSource, StreamDest: TFileStream;
begin
  StreamSource := TFileStream.Create(Source, fmOpenRead);
  try
    StreamDest := TFileStream.Create(Destination, fmCreate);
    try
      // Copier tout le contenu
      StreamDest.CopyFrom(StreamSource, 0);  // 0 = copier tout

      WriteLn('Fichier copié : ', StreamDest.Size, ' octets');
    finally
      StreamDest.Free;
    end;
  finally
    StreamSource.Free;
  end;
end;

var
  FSource: TextFile;

begin
  // Creer un fichier source de test
  Assign(FSource, 'source_test.txt');
  Rewrite(FSource);
  WriteLn(FSource, 'Ligne 1 : Bonjour');
  WriteLn(FSource, 'Ligne 2 : Ceci est un test de copie par stream');
  WriteLn(FSource, 'Ligne 3 : Au revoir');
  Close(FSource);
  WriteLn('Fichier source créé.');

  // Copier avec stream
  CopierFichierStream('source_test.txt', 'copie_test.txt');

  // Nettoyage
  if FileExists('source_test.txt') then
    DeleteFile('source_test.txt');
  if FileExists('copie_test.txt') then
    DeleteFile('copie_test.txt');

  WriteLn('Fichiers temporaires nettoyés.');
end.
```

### Copier partiellement

```pascal
var
  Source, Dest: TMemoryStream;

begin
  Source := TMemoryStream.Create;
  Dest := TMemoryStream.Create;
  try
    // Remplir le stream source
    Source.LoadFromFile('grand_fichier.dat');

    // Copier seulement les 1000 premiers octets
    Dest.CopyFrom(Source, 1000);

    WriteLn('Copié : ', Dest.Size, ' octets');
  finally
    Dest.Free;
    Source.Free;
  end;
end.
```

---

## Exemples pratiques

### Exemple 1 : Lire un fichier texte en mémoire

```pascal
{$mode objfpc}{$H+}
program LireFichierTexte;

uses
  Classes, SysUtils;

function LireFichierEnMemoire(NomFichier: string): string;  
var  
  Stream: TStringStream;
  FileStream: TFileStream;
begin
  Result := '';

  if not FileExists(NomFichier) then
  begin
    WriteLn('Fichier introuvable : ', NomFichier);
    Exit;
  end;

  FileStream := TFileStream.Create(NomFichier, fmOpenRead);
  Stream := TStringStream.Create('');
  try
    // Copier le fichier dans le string stream
    Stream.CopyFrom(FileStream, 0);
    Result := Stream.DataString;
  finally
    Stream.Free;
    FileStream.Free;
  end;
end;

var
  Contenu: string;

begin
  Contenu := LireFichierEnMemoire('test.txt');

  if Contenu <> '' then
  begin
    WriteLn('=== CONTENU DU FICHIER ===');
    WriteLn(Contenu);
    WriteLn('=========================');
    WriteLn('Taille : ', Length(Contenu), ' caractères');
  end;
end.
```

### Exemple 2 : Écrire des structures dans un stream

```pascal
{$mode objfpc}{$H+}
program EcrireStructures;

uses
  Classes, SysUtils;

type
  TPersonne = record
    ID: Integer;
    Nom: string[50];
    Age: Integer;
    Salaire: Double;
  end;

procedure EcrirePersonne(Stream: TStream; const P: TPersonne);  
begin  
  Stream.Write(P, SizeOf(P));
end;

function LirePersonne(Stream: TStream): TPersonne;  
begin  
  Stream.Read(Result, SizeOf(Result));
end;

var
  Stream: TFileStream;
  P1, P2, P3: TPersonne;
  PLue: TPersonne;

begin
  // Créer des personnes
  P1.ID := 1; P1.Nom := 'Dupont'; P1.Age := 30; P1.Salaire := 35000;
  P2.ID := 2; P2.Nom := 'Martin'; P2.Age := 25; P2.Salaire := 28000;
  P3.ID := 3; P3.Nom := 'Durand'; P3.Age := 45; P3.Salaire := 52000;

  // Écrire
  Stream := TFileStream.Create('personnes.dat', fmCreate);
  try
    EcrirePersonne(Stream, P1);
    EcrirePersonne(Stream, P2);
    EcrirePersonne(Stream, P3);
    WriteLn('3 personnes écrites');
  finally
    Stream.Free;
  end;

  // Lire
  Stream := TFileStream.Create('personnes.dat', fmOpenRead);
  try
    WriteLn;
    WriteLn('=== LECTURE ===');

    while Stream.Position < Stream.Size do
    begin
      PLue := LirePersonne(Stream);
      WriteLn('ID: ', PLue.ID, ' - ', PLue.Nom, ' - ', PLue.Age, ' ans - ',
              PLue.Salaire:0:2, ' €');
    end;
  finally
    Stream.Free;
  end;

  // Nettoyage
  if FileExists('personnes.dat') then
    DeleteFile('personnes.dat');

  WriteLn;
  WriteLn('Fichier temporaire nettoyé.');
end.
```

### Exemple 3 : Concaténer plusieurs fichiers

```pascal
{$mode objfpc}{$H+}
program ConcatenerFichiers;

uses
  Classes, SysUtils;

procedure ConcatenerFichiers(Fichiers: array of string; Sortie: string);  
var  
  StreamSortie: TFileStream;
  StreamEntree: TFileStream;
  i: Integer;
begin
  StreamSortie := TFileStream.Create(Sortie, fmCreate);
  try
    for i := Low(Fichiers) to High(Fichiers) do
    begin
      if FileExists(Fichiers[i]) then
      begin
        WriteLn('Ajout de : ', Fichiers[i]);

        StreamEntree := TFileStream.Create(Fichiers[i], fmOpenRead);
        try
          StreamSortie.CopyFrom(StreamEntree, 0);
        finally
          StreamEntree.Free;
        end;
      end
      else
        WriteLn('Fichier ignoré (introuvable) : ', Fichiers[i]);
    end;

    WriteLn;
    WriteLn('Fichier de sortie : ', Sortie);
    WriteLn('Taille totale : ', StreamSortie.Size, ' octets');
  finally
    StreamSortie.Free;
  end;
end;

begin
  ConcatenerFichiers(['partie1.txt', 'partie2.txt', 'partie3.txt'], 'complet.txt');
end.
```

### Exemple 4 : Stream avec buffer personnalisé

```pascal
{$mode objfpc}{$H+}
program LectureBufferisee;

uses
  Classes, SysUtils;

procedure LireFichierAvecBuffer(NomFichier: string);  
const  
  TAILLE_BUFFER = 4096;  // 4 Ko
var
  Stream: TFileStream;
  Buffer: array[0..TAILLE_BUFFER-1] of Byte;
  NbLus: Integer;
  TotalLu: Int64;
begin
  if not FileExists(NomFichier) then
  begin
    WriteLn('Fichier introuvable');
    Exit;
  end;

  Stream := TFileStream.Create(NomFichier, fmOpenRead);
  try
    TotalLu := 0;

    WriteLn('Lecture du fichier par blocs de ', TAILLE_BUFFER, ' octets...');

    repeat
      NbLus := Stream.Read(Buffer, TAILLE_BUFFER);
      TotalLu := TotalLu + NbLus;

      Write('.');  // Indicateur de progression

      // Traiter le buffer ici...

    until NbLus < TAILLE_BUFFER;

    WriteLn;
    WriteLn('Lecture terminée : ', TotalLu, ' octets');
  finally
    Stream.Free;
  end;
end;

begin
  LireFichierAvecBuffer('gros_fichier.dat');
end.
```

---

## Méthodes utiles supplémentaires

### ReadBuffer et WriteBuffer

**Différence avec Read/Write :** Ces méthodes lèvent une exception si elles ne peuvent pas lire/écrire la quantité demandée.

```pascal
var
  Stream: TFileStream;
  Nombre: Integer;

begin
  Stream := TFileStream.Create('data.dat', fmCreate);
  try
    Nombre := 12345;

    // WriteBuffer : exception si impossible d'écrire tout
    Stream.WriteBuffer(Nombre, SizeOf(Nombre));

    Stream.Position := 0;

    // ReadBuffer : exception si impossible de lire tout
    Stream.ReadBuffer(Nombre, SizeOf(Nombre));

    WriteLn('Nombre lu : ', Nombre);
  finally
    Stream.Free;
  end;
end.
```

### SetSize : Changer la taille

```pascal
var
  Stream: TMemoryStream;

begin
  Stream := TMemoryStream.Create;
  try
    // Pré-allouer de l'espace
    Stream.SetSize(10000);
    WriteLn('Taille allouée : ', Stream.Size);

    // La position reste à 0, mais l'espace est réservé
    WriteLn('Position : ', Stream.Position);
  finally
    Stream.Free;
  end;
end.
```

### Clear : Vider un stream

```pascal
var
  Stream: TMemoryStream;

begin
  Stream := TMemoryStream.Create;
  try
    // Écrire des données
    Stream.Write(PChar('Beaucoup de données...')^, Length('Beaucoup de données...'));
    WriteLn('Taille : ', Stream.Size);

    // Vider complètement
    Stream.Clear;
    WriteLn('Taille après Clear : ', Stream.Size);
  finally
    Stream.Free;
  end;
end.
```

---

## Comparaison : Fichiers traditionnels vs Streams

### Fichiers traditionnels (TextFile, File)

```pascal
var
  F: TextFile;
  Ligne: string;

begin
  Assign(F, 'test.txt');
  Reset(F);
  try
    while not EOF(F) do
    begin
      ReadLn(F, Ligne);
      WriteLn(Ligne);
    end;
  finally
    Close(F);
  end;
end.
```

**Caractéristiques :**
- ✅ Simple pour les cas basiques
- ✅ Familier (approche Pascal classique)
- ❌ Limité aux fichiers
- ❌ Moins flexible
- ❌ Pas d'approche objet

### Streams

```pascal
var
  Stream: TFileStream;
  StringStream: TStringStream;

begin
  Stream := TFileStream.Create('test.txt', fmOpenRead);
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(Stream, 0);
    WriteLn(StringStream.DataString);
  finally
    StringStream.Free;
    Stream.Free;
  end;
end.
```

**Caractéristiques :**
- ✅ Approche orientée objet
- ✅ Interface unifiée
- ✅ Très flexible (fichier, mémoire, réseau...)
- ✅ Fonctionnalités avancées
- ❌ Légèrement plus verbeux
- ❌ Courbe d'apprentissage

---

## Quand utiliser les streams ?

### Utilisez les streams pour :

✅ **Manipuler des fichiers binaires** complexes

✅ **Copier/transférer** des données entre sources

✅ **Traiter des données en mémoire** temporairement

✅ **Implémenter des protocoles** réseau

✅ **Créer des formats** de fichiers personnalisés

✅ **Bufferiser** des opérations pour la performance

✅ **Composer des opérations** (compression, chiffrement, etc.)

### Utilisez les méthodes traditionnelles pour :

✅ **Fichiers texte simples** ligne par ligne

✅ **Fichiers INI** et configuration

✅ **Scripts rapides** et prototypes

✅ **Code pédagogique** pour débutants

---

## Bonnes pratiques

### ✅ À faire

**Toujours utiliser try-finally** pour libérer les streams

**Vérifier Position et Size** pour éviter de lire au-delà de la fin

**Utiliser TMemoryStream** pour améliorer les performances avec de petits fichiers

**Préférer CopyFrom** pour copier entre streams

**Fermer/libérer les streams** dès que possible

**Utiliser SetSize** pour pré-allouer la mémoire si vous connaissez la taille

**Bufferiser** les lectures/écritures pour de gros fichiers

**Gérer les exceptions** (disque plein, accès refusé, etc.)

### ❌ À éviter

**Ne jamais oublier Free** après Create

**Ne pas supposer** qu'une lecture/écriture réussira

**Ne pas lire** au-delà de la fin du stream

**Ne pas utiliser TMemoryStream** pour de très gros fichiers (limites RAM)

**Ne pas négliger** la gestion des erreurs

**Ne pas laisser** des streams ouverts inutilement

**Ne pas mélanger** Read/Write avec ReadBuffer/WriteBuffer sans comprendre la différence

---

## Tableau récapitulatif

| Classe | Usage | Avantages | Inconvénients |
|--------|-------|-----------|---------------|
| **TFileStream** | Fichiers sur disque | Rapide, accès direct | Limité aux fichiers |
| **TMemoryStream** | Données en RAM | Très rapide | Limité par la RAM |
| **TStringStream** | Chaînes de caractères | Pratique pour le texte | Moins efficient pour le binaire |
| **TResourceStream** | Ressources embarquées | Lecture ressources | Lecture seule |

### Méthodes principales

| Méthode | Description | Retour |
|---------|-------------|--------|
| `Create(...)` | Créer le stream | Instance |
| `Read(Buffer, Count)` | Lire des octets | Nombre lu |
| `Write(Buffer, Count)` | Écrire des octets | Nombre écrit |
| `ReadBuffer(Buffer, Count)` | Lire (exception si échec) | - |
| `WriteBuffer(Buffer, Count)` | Écrire (exception si échec) | - |
| `Seek(Offset, Origin)` | Déplacer le curseur | Nouvelle position |
| `Position` | Position actuelle | Int64 |
| `Size` | Taille totale | Int64 |
| `CopyFrom(Source, Count)` | Copier depuis un autre | Octets copiés |
| `SaveToFile(FileName)` | Sauvegarder | - |
| `LoadFromFile(FileName)` | Charger | - |
| `Clear` | Vider le stream | - |
| `Free` | Libérer les ressources | - |

---

## Résumé

Les streams offrent une abstraction puissante et unifiée pour manipuler des données :

**Trois types principaux :**
- **TFileStream** : pour les fichiers
- **TMemoryStream** : pour la mémoire
- **TStringStream** : pour les chaînes

**Opérations de base :**
- `Read` / `Write` : lecture/écriture
- `Seek` / `Position` : navigation
- `CopyFrom` : copie entre streams
- `SaveToFile` / `LoadFromFile` : persistance

**Avantages clés :**
- Interface unifiée pour différentes sources
- Approche orientée objet
- Flexibilité et puissance
- Performance optimisée

**Règle d'or :** Les streams sont parfaits pour les manipulations binaires et les transferts de données. Pour les fichiers texte simples, les méthodes traditionnelles restent plus intuitives.

Vous voilà équipé pour manipuler efficacement les fichiers et les données en mémoire avec les streams Pascal !

---

> **Conseil pour la suite :** Les streams sont un outil puissant. Commencez par des exemples simples (copier un fichier, lire en mémoire) avant de vous attaquer à des cas plus complexes. La pratique vous permettra de comprendre quand utiliser chaque type de stream !

⏭️ [Introduction à FreePascal et Lazarus](/09-introduction-freepascal-lazarus/README.md)
