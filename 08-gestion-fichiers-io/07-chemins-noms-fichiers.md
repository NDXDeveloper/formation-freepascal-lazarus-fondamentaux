🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.7 Chemins et noms de fichiers

## Introduction

Manipuler les chemins et les noms de fichiers est une compétence essentielle en programmation. Que vous vouliez extraire le nom d'un fichier, changer son extension, ou construire un chemin complet, Pascal offre de nombreuses fonctions pour faciliter ces opérations.

**Analogie :**
Un chemin de fichier, c'est comme une adresse postale :
- **Rue et numéro** = répertoire
- **Ville** = lecteur ou racine
- **Nom de la personne** = nom du fichier
- **Titre (M./Mme)** = extension

```
Adresse complète : 123 rue Victor Hugo, Paris
Chemin complet   : C:\Users\Jean\Documents\rapport.pdf
```

---

## Anatomie d'un chemin de fichier

### Décomposition d'un chemin complet

Prenons l'exemple suivant :

**Windows :** `C:\Users\Jean\Documents\rapport.pdf`  
**Linux :** `/home/jean/documents/rapport.pdf`  

Un chemin complet se décompose en plusieurs parties :

| Partie | Windows | Linux | Description |
|--------|---------|-------|-------------|
| **Lecteur** | `C:` | *(absent)* | Unité de stockage |
| **Chemin** | `\Users\Jean\Documents\` | `/home/jean/documents/` | Répertoires |
| **Nom de base** | `rapport` | `rapport` | Nom sans extension |
| **Extension** | `.pdf` | `.pdf` | Type de fichier |
| **Nom complet** | `rapport.pdf` | `rapport.pdf` | Nom + extension |

### Structure hiérarchique

```
C:\Users\Jean\Documents\rapport.pdf
│   │     │    │          │      └─ Extension
│   │     │    │          └─ Nom de base
│   │     │    └─ Nom du fichier complet
│   │     └─ Répertoires parents
│   └─ Répertoire racine de l'utilisateur
└─ Lecteur (Windows uniquement)
```

---

## Chemins absolus vs chemins relatifs

### Chemin absolu

Un **chemin absolu** spécifie l'emplacement complet depuis la racine du système de fichiers.

**Exemples :**
```
Windows : C:\Program Files\MonApp\data.db
Linux   : /usr/local/bin/monapp
```

**Caractéristiques :**
- ✅ Non ambigu : pointe toujours vers le même fichier
- ✅ Fonctionne depuis n'importe quel répertoire
- ❌ Moins flexible : difficile à déplacer
- ❌ Spécifique à la machine

### Chemin relatif

Un **chemin relatif** est défini par rapport au répertoire courant.

**Exemples :**
```
data.db              → dans le répertoire courant
config\settings.ini  → dans le sous-répertoire config
..\images\logo.png   → dans le répertoire parent, sous-répertoire images
..\..\backup.zip     → deux répertoires au-dessus
```

**Caractéristiques :**
- ✅ Flexible : se déplace avec l'application
- ✅ Portable : fonctionne partout
- ❌ Dépend du répertoire courant
- ❌ Peut être ambigu

**Notation spéciale :**
- `.` = répertoire courant
- `..` = répertoire parent
- `~` = répertoire home de l'utilisateur (Linux/macOS)

---

## Unité nécessaire

Pour manipuler les chemins, importez l'unité SysUtils :

```pascal
uses
  SysUtils;
```

---

## Extraction des composants d'un chemin

Pascal fournit des fonctions pour extraire chaque partie d'un chemin de fichier.

### ExtractFilePath : Extraire le chemin (répertoire)

```pascal
uses
  SysUtils;

var
  CheminComplet, Repertoire: string;

begin
  CheminComplet := 'C:\Users\Jean\Documents\rapport.pdf';

  Repertoire := ExtractFilePath(CheminComplet);
  WriteLn('Répertoire : ', Repertoire);
  // Résultat : C:\Users\Jean\Documents\
end.
```

**Note :** Le résultat **inclut** le séparateur final (`\` ou `/`).

### ExtractFileName : Extraire le nom du fichier

```pascal
uses
  SysUtils;

var
  CheminComplet, NomFichier: string;

begin
  CheminComplet := 'C:\Users\Jean\Documents\rapport.pdf';

  NomFichier := ExtractFileName(CheminComplet);
  WriteLn('Nom du fichier : ', NomFichier);
  // Résultat : rapport.pdf
end.
```

**Retourne :** Nom complet avec extension.

### ExtractFileExt : Extraire l'extension

```pascal
uses
  SysUtils;

var
  CheminComplet, Extension: string;

begin
  CheminComplet := 'C:\Users\Jean\Documents\rapport.pdf';

  Extension := ExtractFileExt(CheminComplet);
  WriteLn('Extension : ', Extension);
  // Résultat : .pdf
end.
```

**Note :** Le point (`.`) est **inclus** dans le résultat.

### ExtractFileDir : Extraire le répertoire (sans séparateur final)

```pascal
uses
  SysUtils;

var
  CheminComplet, Repertoire: string;

begin
  CheminComplet := 'C:\Users\Jean\Documents\rapport.pdf';

  Repertoire := ExtractFileDir(CheminComplet);
  WriteLn('Répertoire : ', Repertoire);
  // Résultat : C:\Users\Jean\Documents
end.
```

**Différence avec ExtractFilePath :** Pas de séparateur à la fin.

### ExtractFileDrive : Extraire le lecteur (Windows uniquement)

```pascal
uses
  SysUtils;

var
  CheminComplet, Lecteur: string;

begin
  CheminComplet := 'C:\Users\Jean\Documents\rapport.pdf';

  Lecteur := ExtractFileDrive(CheminComplet);
  WriteLn('Lecteur : ', Lecteur);
  // Résultat sous Windows : C:
  // Résultat sous Linux : (chaîne vide)
end.
```

### Exemple complet : Décomposer un chemin

```pascal
{$mode objfpc}{$H+}
program DecomposerChemin;

uses
  SysUtils;

procedure AnalyserChemin(Chemin: string);
begin
  WriteLn('=== ANALYSE DU CHEMIN ===');
  WriteLn('Chemin complet  : ', Chemin);
  WriteLn('Lecteur         : ', ExtractFileDrive(Chemin));
  WriteLn('Répertoire (/)  : ', ExtractFilePath(Chemin));
  WriteLn('Répertoire      : ', ExtractFileDir(Chemin));
  WriteLn('Nom du fichier  : ', ExtractFileName(Chemin));
  WriteLn('Extension       : ', ExtractFileExt(Chemin));
  WriteLn('========================');
end;

begin
  AnalyserChemin('/home/jean/documents/rapport.pdf');
  WriteLn;
  AnalyserChemin('config/settings.ini');
end.
```

**Résultat sous Windows :**
```
=== ANALYSE DU CHEMIN ===
Chemin complet  : C:\Users\Jean\Documents\rapport.pdf
Lecteur         : C:
Répertoire (/)  : C:\Users\Jean\Documents\
Répertoire      : C:\Users\Jean\Documents
Nom du fichier  : rapport.pdf
Extension       : .pdf
========================
```

---

## Modification des chemins

### ChangeFileExt : Changer l'extension

```pascal
uses
  SysUtils;

var
  CheminOriginal, NouveauChemin: string;

begin
  CheminOriginal := 'rapport.txt';

  NouveauChemin := ChangeFileExt(CheminOriginal, '.pdf');
  WriteLn('Nouveau chemin : ', NouveauChemin);
  // Résultat : rapport.pdf

  // Supprimer l'extension
  NouveauChemin := ChangeFileExt(CheminOriginal, '');
  WriteLn('Sans extension : ', NouveauChemin);
  // Résultat : rapport
end.
```

**Note :** Si vous passez une extension, le point (`.`) est automatiquement ajouté si nécessaire.

### ExtractFileNameWithoutExt : Nom sans extension (FreePascal 3.0+)

```pascal
uses
  SysUtils;

var
  CheminComplet, NomSeul: string;

begin
  CheminComplet := 'C:\Users\Jean\Documents\rapport.pdf';

  NomSeul := ExtractFileNameWithoutExt(CheminComplet);
  WriteLn('Nom sans extension : ', NomSeul);
  // Résultat : rapport
end.
```

**Alternative pour anciennes versions :**
```pascal
function NomSansExtension(Chemin: string): string;
begin
  NomSansExtension := ChangeFileExt(ExtractFileName(Chemin), '');
end;
```

---

## Construction de chemins

### Concaténer des chemins : IncludeTrailingPathDelimiter

```pascal
uses
  SysUtils;

var
  Repertoire, NomFichier, CheminComplet: string;

begin
  Repertoire := 'C:\Users\Jean\Documents';
  NomFichier := 'rapport.pdf';

  // Méthode sûre : ajouter le séparateur si absent
  CheminComplet := IncludeTrailingPathDelimiter(Repertoire) + NomFichier;
  WriteLn(CheminComplet);
  // Résultat : C:\Users\Jean\Documents\rapport.pdf
end.
```

**Avantage :** Évite les doubles séparateurs (`\\` ou `//`).

### ExcludeTrailingPathDelimiter : Retirer le séparateur final

```pascal
uses
  SysUtils;

var
  Chemin, CheminNettoye: string;

begin
  Chemin := 'C:\Users\Jean\Documents\';

  CheminNettoye := ExcludeTrailingPathDelimiter(Chemin);
  WriteLn(CheminNettoye);
  // Résultat : C:\Users\Jean\Documents
end.
```

### ConcatPaths : Concaténer plusieurs segments (FPC 3.2+)

```pascal
uses
  SysUtils;

var
  CheminComplet: string;

begin
  CheminComplet := ConcatPaths(['C:\Users', 'Jean', 'Documents', 'rapport.pdf']);
  WriteLn(CheminComplet);
  // Résultat : C:\Users\Jean\Documents\rapport.pdf
end.
```

**Avantage :** Gère automatiquement les séparateurs de manière portable.

---

## Conversion et normalisation

### ExpandFileName : Convertir en chemin absolu

```pascal
uses
  SysUtils;

var
  CheminRelatif, CheminAbsolu: string;

begin
  // Supposons que le répertoire courant est C:\Users\Jean
  CheminRelatif := 'Documents\rapport.pdf';

  CheminAbsolu := ExpandFileName(CheminRelatif);
  WriteLn('Chemin absolu : ', CheminAbsolu);
  // Résultat : C:\Users\Jean\Documents\rapport.pdf
end.
```

**Usage :** Convertit un chemin relatif en chemin absolu basé sur le répertoire courant.

### ExpandFileNameCase : Normaliser la casse (Linux)

```pascal
uses
  SysUtils;

var
  Chemin, CheminNormalise: string;

begin
  Chemin := '/home/jean/DOCUMENTS/rapport.PDF';

  CheminNormalise := ExpandFileNameCase(Chemin, False);
  WriteLn('Normalisé : ', CheminNormalise);
  // Recherche le fichier avec la bonne casse
end.
```

**Usage sous Linux :** Les systèmes de fichiers Linux sont sensibles à la casse. Cette fonction trouve le fichier réel.

### ExtractRelativePath : Obtenir un chemin relatif

```pascal
uses
  SysUtils;

var
  Base, Cible, Relatif: string;

begin
  Base := 'C:\Users\Jean\Projets\MonApp\';
  Cible := 'C:\Users\Jean\Documents\rapport.pdf';

  Relatif := ExtractRelativePath(Base, Cible);
  WriteLn('Chemin relatif : ', Relatif);
  // Résultat : ..\..\Documents\rapport.pdf
end.
```

**Usage :** Calcule le chemin relatif pour aller de Base vers Cible.

---

## Vérification et tests

### FileExists : Vérifier l'existence d'un fichier

```pascal
uses
  SysUtils;

var
  Chemin: string;

begin
  Chemin := 'C:\Users\Jean\Documents\rapport.pdf';

  if FileExists(Chemin) then
    WriteLn('Le fichier existe')
  else
    WriteLn('Le fichier n''existe pas');
end.
```

### DirectoryExists : Vérifier l'existence d'un répertoire

```pascal
uses
  SysUtils;

var
  Chemin: string;

begin
  Chemin := 'C:\Users\Jean\Documents';

  if DirectoryExists(Chemin) then
    WriteLn('Le répertoire existe')
  else
    WriteLn('Le répertoire n''existe pas');
end.
```

### FileIsReadOnly : Vérifier si en lecture seule

```pascal
uses
  SysUtils;

var
  Chemin: string;

begin
  Chemin := 'C:\Windows\System32\config\system';

  if FileIsReadOnly(Chemin) then
    WriteLn('Fichier en lecture seule')
  else
    WriteLn('Fichier modifiable');
end.
```

---

## Obtenir des informations sur les fichiers

### FileAge : Date de dernière modification

```pascal
uses
  SysUtils, DateUtils;

var
  Chemin: string;
  DateModif: TDateTime;

begin
  Chemin := 'rapport.pdf';

  if FileAge(Chemin) <> -1 then
  begin
    DateModif := FileDateToDateTime(FileAge(Chemin));
    WriteLn('Dernière modification : ', DateTimeToStr(DateModif));
  end
  else
    WriteLn('Fichier introuvable');
end.
```

### FileSize : Taille d'un fichier

```pascal
uses
  SysUtils;

function TailleFichier(Chemin: string): Int64;
var
  F: File of Byte;
begin
  if not FileExists(Chemin) then
  begin
    TailleFichier := -1;
    Exit;
  end;

  Assign(F, Chemin);
  Reset(F);
  TailleFichier := FileSize(F);
  Close(F);
end;

var
  Taille: Int64;

begin
  Taille := TailleFichier('rapport.pdf');

  if Taille >= 0 then
    WriteLn('Taille : ', Taille, ' octets')
  else
    WriteLn('Erreur ou fichier inexistant');
end.
```

### Alternative moderne avec FileUtil

```pascal
uses
  SysUtils, FileUtil;

var
  Taille: Int64;

begin
  Taille := FileUtil.FileSize('rapport.pdf');
  WriteLn('Taille : ', Taille, ' octets');
end.
```

---

## Chemins portables Windows/Linux

### Utiliser PathDelim pour la portabilité

```pascal
uses
  SysUtils;

var
  Chemin: string;

begin
  // ❌ Mauvais (spécifique Windows)
  Chemin := 'C:\Users\Jean\Documents\rapport.pdf';

  // ✅ Bon (portable)
  Chemin := 'Data' + PathDelim + 'Config' + PathDelim + 'settings.ini';
  WriteLn(Chemin);

  // Sous Windows : Data\Config\settings.ini
  // Sous Linux   : Data/Config/settings.ini
end.
```

### Constantes utiles pour la portabilité

```pascal
uses
  SysUtils;

begin
  WriteLn('Séparateur de chemin    : ', PathDelim);        // \ ou /
  WriteLn('Séparateur de répertoire: ', DirectorySeparator); // Alias de PathDelim
  WriteLn('Séparateur de lecteur   : ', DriveDelim);       // : (Windows) ou vide
  WriteLn('Séparateur de PATH      : ', PathSep);          // ; ou :
  WriteLn('Suffixe bibliothèque    : ', SharedSuffix);     // dll ou so
end.
```

**Note :** `ExeExt` n'existe pas en FreePascal 3.2.2. Utilisez `SharedSuffix` pour le suffixe des bibliothèques partagées.

**Résultat sous Windows :**
```
Séparateur de chemin    : \
Séparateur de répertoire: \
Séparateur de lecteur   : :
Séparateur de PATH      : ;
Suffixe bibliothèque    : dll
```

**Résultat sous Linux :**
```
Séparateur de chemin    : /
Séparateur de répertoire: /
Séparateur de lecteur   :
Séparateur de PATH      : :
Suffixe bibliothèque    : so
```

### Fonction de construction portable

```pascal
uses
  SysUtils;

function ConstruireChemin(const Segments: array of string): string;
var
  i: Integer;
begin
  Result := '';

  for i := Low(Segments) to High(Segments) do
  begin
    if i = Low(Segments) then
      Result := Segments[i]
    else
      Result := IncludeTrailingPathDelimiter(Result) + Segments[i];
  end;
end;

var
  Chemin: string;

begin
  Chemin := ConstruireChemin(['Projet', 'Source', 'Units', 'Database.pas']);
  WriteLn(Chemin);

  // Sous Windows : Projet\Source\Units\Database.pas
  // Sous Linux   : Projet/Source/Units/Database.pas
end.
```

---

## Exemples pratiques

### Exemple 1 : Créer un nom de sauvegarde automatique

```pascal
uses
  SysUtils, DateUtils;

function NomFichierSauvegarde(CheminOriginal: string): string;
var
  Repertoire, Nom, Extension: string;
  Horodatage: string;
begin
  Repertoire := ExtractFilePath(CheminOriginal);
  Nom := ChangeFileExt(ExtractFileName(CheminOriginal), '');
  Extension := ExtractFileExt(CheminOriginal);

  Horodatage := FormatDateTime('yyyymmdd_hhnnss', Now);

  Result := Repertoire + Nom + '_backup_' + Horodatage + Extension;
end;

begin
  WriteLn(NomFichierSauvegarde('C:\Documents\rapport.pdf'));
  // Résultat : C:\Documents\rapport_backup_20251013_143052.pdf
end.
```

### Exemple 2 : Trouver tous les fichiers d'un type

```pascal
uses
  SysUtils;

procedure TrouverFichiersParExtension(Repertoire, Extension: string);
var
  Info: TSearchRec;
  CheminComplet: string;
begin
  if not DirectoryExists(Repertoire) then
  begin
    WriteLn('Répertoire inexistant : ', Repertoire);
    Exit;
  end;

  CheminComplet := IncludeTrailingPathDelimiter(Repertoire) + '*' + Extension;

  WriteLn('Recherche de fichiers ', Extension, ' dans ', Repertoire);
  WriteLn;

  if FindFirst(CheminComplet, faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Attr and faDirectory) = 0 then
          WriteLn('  ', Info.Name);
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end
  else
    WriteLn('Aucun fichier trouvé');
end;

begin
  TrouverFichiersParExtension('C:\Users\Jean\Documents', '.pdf');
end.
```

### Exemple 3 : Valider un nom de fichier

```pascal
uses
  SysUtils;

function NomFichierValide(Nom: string): Boolean;
const
  CaracteresInterdits = '\/:*?"<>|';
var
  i: Integer;
begin
  Result := False;

  if Nom = '' then
    Exit;

  // Vérifier les caractères interdits
  for i := 1 to Length(CaracteresInterdits) do
  begin
    if Pos(CaracteresInterdits[i], Nom) > 0 then
      Exit;
  end;

  // Vérifier les noms réservés Windows
  if (UpperCase(Nom) = 'CON') or
     (UpperCase(Nom) = 'PRN') or
     (UpperCase(Nom) = 'AUX') or
     (UpperCase(Nom) = 'NUL') then
    Exit;

  Result := True;
end;

begin
  WriteLn('test.txt valide ? ', NomFichierValide('test.txt'));      // True
  WriteLn('test*.txt valide ? ', NomFichierValide('test*.txt'));    // False
  WriteLn('CON valide ? ', NomFichierValide('CON'));                // False
end.
```

### Exemple 4 : Renommer avec numérotation automatique

```pascal
uses
  SysUtils;

function TrouverNomDisponible(CheminBase: string): string;
var
  Repertoire, NomBase, Extension: string;
  Compteur: Integer;
  CheminTest: string;
begin
  if not FileExists(CheminBase) then
  begin
    Result := CheminBase;
    Exit;
  end;

  Repertoire := ExtractFilePath(CheminBase);
  NomBase := ChangeFileExt(ExtractFileName(CheminBase), '');
  Extension := ExtractFileExt(CheminBase);

  Compteur := 1;

  repeat
    CheminTest := Repertoire + NomBase + '_' + IntToStr(Compteur) + Extension;
    Inc(Compteur);
  until not FileExists(CheminTest);

  Result := CheminTest;
end;

begin
  WriteLn(TrouverNomDisponible('rapport.pdf'));
  // Si rapport.pdf existe : rapport_1.pdf
  // Si rapport_1.pdf existe aussi : rapport_2.pdf
  // etc.
end.
```

---

## Application complète : Gestionnaire de chemins

```pascal
{$mode objfpc}{$H+}
program GestionnaireChemin;

uses
  SysUtils;

procedure AfficherMenu;
begin
  WriteLn;
  WriteLn('=== GESTIONNAIRE DE CHEMINS ===');
  WriteLn('1. Analyser un chemin');
  WriteLn('2. Changer l''extension');
  WriteLn('3. Convertir en absolu');
  WriteLn('4. Extraire le chemin relatif');
  WriteLn('5. Vérifier l''existence');
  WriteLn('6. Construire un chemin');
  WriteLn('0. Quitter');
  Write('Choix : ');
end;

procedure AnalyserChemin;
var
  Chemin: string;
begin
  Write('Chemin à analyser : ');
  ReadLn(Chemin);
  WriteLn;
  WriteLn('=== ANALYSE ===');
  WriteLn('Chemin complet     : ', Chemin);
  WriteLn('Lecteur            : ', ExtractFileDrive(Chemin));
  WriteLn('Répertoire         : ', ExtractFilePath(Chemin));
  WriteLn('Nom du fichier     : ', ExtractFileName(Chemin));
  WriteLn('Extension          : ', ExtractFileExt(Chemin));
  WriteLn('Nom sans extension : ', ChangeFileExt(ExtractFileName(Chemin), ''));
  WriteLn('Chemin absolu      : ', ExpandFileName(Chemin));
end;

procedure ChangerExtension;
var
  Chemin, NouvelleExt, Resultat: string;
begin
  Write('Chemin du fichier : ');
  ReadLn(Chemin);
  Write('Nouvelle extension (avec le point) : ');
  ReadLn(NouvelleExt);

  Resultat := ChangeFileExt(Chemin, NouvelleExt);
  WriteLn('Résultat : ', Resultat);
end;

procedure ConvertirEnAbsolu;
var
  CheminRelatif, CheminAbsolu: string;
begin
  WriteLn('Répertoire courant : ', GetCurrentDir);
  Write('Chemin relatif : ');
  ReadLn(CheminRelatif);

  CheminAbsolu := ExpandFileName(CheminRelatif);
  WriteLn('Chemin absolu : ', CheminAbsolu);
end;

procedure ExtraireCheminRelatif;
var
  Base, Cible, Relatif: string;
begin
  Write('Chemin de base : ');
  ReadLn(Base);
  Write('Chemin cible : ');
  ReadLn(Cible);

  if not DirectoryExists(Base) then
    Base := ExtractFilePath(Base);

  Relatif := ExtractRelativePath(Base, Cible);
  WriteLn('Chemin relatif : ', Relatif);
end;

procedure VerifierExistence;
var
  Chemin: string;
begin
  Write('Chemin à vérifier : ');
  ReadLn(Chemin);
  WriteLn;

  if FileExists(Chemin) then
    WriteLn('V Le fichier existe')
  else if DirectoryExists(Chemin) then
    WriteLn('V Le répertoire existe')
  else
    WriteLn('X N''existe pas');
end;

procedure ConstruireCheminInteractif;
var
  NbSegments, i: Integer;
  Segments: array[1..10] of string;
  Resultat: string;
begin
  Write('Nombre de segments (max 10) : ');
  ReadLn(NbSegments);

  if (NbSegments < 1) or (NbSegments > 10) then
  begin
    WriteLn('Nombre invalide !');
    Exit;
  end;

  for i := 1 to NbSegments do
  begin
    Write('Segment ', i, ' : ');
    ReadLn(Segments[i]);
  end;

  Resultat := Segments[1];

  for i := 2 to NbSegments do
    Resultat := IncludeTrailingPathDelimiter(Resultat) + Segments[i];

  WriteLn;
  WriteLn('Chemin construit : ', Resultat);
end;

var
  Choix: Integer;

begin
  repeat
    AfficherMenu;
    ReadLn(Choix);

    case Choix of
      1: AnalyserChemin;
      2: ChangerExtension;
      3: ConvertirEnAbsolu;
      4: ExtraireCheminRelatif;
      5: VerifierExistence;
      6: ConstruireCheminInteractif;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;
end.
```

---

## Tableau récapitulatif des fonctions

| Fonction | Description | Exemple |
|----------|-------------|---------|
| `ExtractFilePath(s)` | Répertoire avec `/` | `C:\Users\Jean\` |
| `ExtractFileDir(s)` | Répertoire sans `/` | `C:\Users\Jean` |
| `ExtractFileName(s)` | Nom avec extension | `rapport.pdf` |
| `ExtractFileExt(s)` | Extension avec point | `.pdf` |
| `ExtractFileDrive(s)` | Lecteur (Windows) | `C:` |
| `ChangeFileExt(s, ext)` | Changer extension | `rapport.txt` → `.pdf` |
| `ExpandFileName(s)` | Relatif → Absolu | `data.txt` → `C:\...\data.txt` |
| `ExtractRelativePath(b, c)` | Absolu → Relatif | `..\..\docs\file.txt` |
| `IncludeTrailingPathDelimiter(s)` | Ajouter `/` final | `C:\Users\` |
| `ExcludeTrailingPathDelimiter(s)` | Retirer `/` final | `C:\Users` |
| `FileExists(s)` | Fichier existe ? | `True/False` |
| `DirectoryExists(s)` | Répertoire existe ? | `True/False` |
| `PathDelim` | Séparateur portable | `\` ou `/` |

---

## Bonnes pratiques

### ✅ À faire

**Toujours utiliser PathDelim** pour construire des chemins portables

**Utiliser IncludeTrailingPathDelimiter** pour concaténer des chemins

**Vérifier l'existence** avec FileExists/DirectoryExists avant d'accéder

**Utiliser ExpandFileName** pour convertir les chemins relatifs en absolus

**Valider les noms** de fichiers avant de les créer

**Préférer ExtractFilePath** à ExtractFileDir pour la concaténation

**Gérer les cas d'erreur** (fichier inexistant, chemin invalide)

**Documenter** les chemins attendus (absolus/relatifs)

### ❌ À éviter

**Ne jamais coder en dur** `\` ou `/` dans les chemins

**Ne pas supposer** qu'un fichier existe toujours

**Ne pas oublier** le point dans les extensions (`.txt` pas `txt`)

**Ne pas utiliser** de caractères interdits dans les noms de fichiers

**Ne pas négliger** la casse sous Linux (systèmes sensibles)

**Ne pas confondre** ExtractFilePath (avec `/`) et ExtractFileDir (sans)

**Ne pas oublier** que certaines fonctions sont spécifiques à Windows

---

## Résumé

La manipulation des chemins et noms de fichiers est essentielle pour créer des applications robustes et portables.

**Extraction :**
- `ExtractFilePath` / `ExtractFileDir` : répertoire
- `ExtractFileName` : nom complet
- `ExtractFileExt` : extension

**Modification :**
- `ChangeFileExt` : changer l'extension
- `IncludeTrailingPathDelimiter` : ajouter séparateur

**Conversion :**
- `ExpandFileName` : relatif → absolu
- `ExtractRelativePath` : absolu → relatif

**Vérification :**
- `FileExists` / `DirectoryExists` : existence
- `PathDelim` : portabilité Windows/Linux

**Règle d'or :** Toujours utiliser les fonctions de SysUtils au lieu de manipuler manuellement les chaînes, pour garantir la portabilité et éviter les bugs !

Dans la section suivante, nous verrons comment utiliser les fichiers INI pour la configuration d'applications !

---

> **Conseil professionnel :** La maîtrise des fonctions de manipulation de chemins vous fera gagner énormément de temps et évitera de nombreux bugs subtils. Pratiquez-les régulièrement !

⏭️ [Fichiers INI pour configuration](/08-gestion-fichiers-io/08-fichiers-ini-configuration.md)
