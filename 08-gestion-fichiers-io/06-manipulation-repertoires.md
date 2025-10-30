🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.6 Manipulation de répertoires

## Introduction

Les **répertoires** (ou **dossiers**) sont les conteneurs qui organisent vos fichiers sur le disque dur. En programmation, vous aurez souvent besoin de créer, supprimer, lister ou naviguer dans ces répertoires.

**Analogie :**
Si les fichiers sont des documents, les répertoires sont comme les tiroirs d'un classeur :
- Vous pouvez **ouvrir** un tiroir (changer de répertoire)
- Vous pouvez **créer** un nouveau tiroir (créer un répertoire)
- Vous pouvez **vider** un tiroir (supprimer un répertoire)
- Vous pouvez **lister** le contenu d'un tiroir (lister les fichiers)

---

## Concepts de base

### Qu'est-ce qu'un répertoire ?

Un **répertoire** est une structure qui contient des fichiers et d'autres répertoires (sous-répertoires).

**Structure typique :**
```
C:\
├── Program Files\
│   ├── MonApp\
│   │   ├── config.ini
│   │   └── data.db
│   └── Autre\
├── Users\
│   └── Jean\
│       ├── Documents\
│       └── Images\
└── Windows\
```

### Répertoire courant

Le **répertoire courant** (current directory) est le répertoire dans lequel votre programme "se trouve" actuellement. C'est le point de départ pour les chemins relatifs.

**Exemple :**
- Si le répertoire courant est `C:\Users\Jean\`
- Et que vous ouvrez `documents.txt`
- Le programme cherchera `C:\Users\Jean\documents.txt`

---

## Unités nécessaires

Pour manipuler les répertoires, vous aurez besoin d'importer certaines unités :

```pascal
uses
  SysUtils,   // Pour les fonctions de base
  FileUtil;   // Pour les fonctions avancées (optionnel)
```

---

## Obtenir le répertoire courant

### GetCurrentDir : Répertoire actuel

```pascal
program AfficherRepertoireCourant;

uses
  SysUtils;

var
  RepCourant: string;

begin
  RepCourant := GetCurrentDir;
  WriteLn('Répertoire courant : ', RepCourant);
end.
```

**Résultat sous Windows :**
```
Répertoire courant : C:\Users\Jean\Documents
```

**Résultat sous Linux :**
```
Répertoire courant : /home/jean/documents
```

### GetDir : Alternative avec numéro de lecteur

```pascal
var
  Chemin: string;
begin
  GetDir(0, Chemin);  // 0 = lecteur actuel
  WriteLn('Répertoire : ', Chemin);
end.
```

**Note :** `GetCurrentDir` est plus moderne et recommandé.

---

## Changer de répertoire courant

### ChDir : Changer de répertoire

```pascal
program ChangerRepertoire;

uses
  SysUtils;

var
  Succes: Boolean;

begin
  WriteLn('Répertoire actuel : ', GetCurrentDir);

  // Changer de répertoire
  {$I-}
  ChDir('C:\Users');
  {$I+}

  if IOResult = 0 then
  begin
    WriteLn('Changement réussi !');
    WriteLn('Nouveau répertoire : ', GetCurrentDir);
  end
  else
    WriteLn('Erreur : Impossible de changer de répertoire');
end.
```

### SetCurrentDir : Alternative moderne

```pascal
uses
  SysUtils;

begin
  if SetCurrentDir('C:\Users') then
    WriteLn('Répertoire changé avec succès')
  else
    WriteLn('Erreur lors du changement de répertoire');
end.
```

**Avantage :** `SetCurrentDir` retourne un booléen (True = succès, False = échec).

---

## Créer un répertoire

### MkDir : Créer un répertoire

```pascal
program CreerRepertoire;

uses
  SysUtils;

var
  NouveauRep: string;

begin
  NouveauRep := 'MonDossier';

  WriteLn('Création du répertoire : ', NouveauRep);

  {$I-}
  MkDir(NouveauRep);
  {$I+}

  if IOResult = 0 then
    WriteLn('Répertoire créé avec succès !')
  else
    WriteLn('Erreur : Impossible de créer le répertoire');
end.
```

### CreateDir : Alternative moderne

```pascal
uses
  SysUtils;

begin
  if CreateDir('MonDossier') then
    WriteLn('Répertoire créé')
  else
    WriteLn('Échec de la création');
end.
```

### ForceDirectories : Créer une arborescence complète

```pascal
uses
  SysUtils;

var
  Chemin: string;

begin
  // Crée tous les répertoires nécessaires
  Chemin := 'Projet\Source\Units\Database';

  if ForceDirectories(Chemin) then
    WriteLn('Arborescence créée : ', Chemin)
  else
    WriteLn('Erreur lors de la création');
end.
```

**Avantage :** `ForceDirectories` crée automatiquement tous les répertoires parents manquants !

**Exemple :**
- Si `Projet\` n'existe pas, il est créé
- Puis `Projet\Source\` est créé
- Puis `Projet\Source\Units\`
- Enfin `Projet\Source\Units\Database\`

---

## Supprimer un répertoire

### RmDir : Supprimer un répertoire vide

```pascal
program SupprimerRepertoire;

uses
  SysUtils;

var
  RepASupprimer: string;

begin
  RepASupprimer := 'MonDossier';

  WriteLn('Suppression du répertoire : ', RepASupprimer);

  {$I-}
  RmDir(RepASupprimer);
  {$I+}

  if IOResult = 0 then
    WriteLn('Répertoire supprimé avec succès !')
  else
    WriteLn('Erreur : Le répertoire n''est pas vide ou n''existe pas');
end.
```

**Important :** `RmDir` ne fonctionne que sur des répertoires **vides** !

### RemoveDir : Alternative moderne

```pascal
uses
  SysUtils;

begin
  if RemoveDir('MonDossier') then
    WriteLn('Répertoire supprimé')
  else
    WriteLn('Échec de la suppression (répertoire non vide ?)');
end.
```

---

## Vérifier l'existence d'un répertoire

### DirectoryExists : Vérifier si un répertoire existe

```pascal
uses
  SysUtils;

var
  Chemin: string;

begin
  Chemin := 'C:\Users';

  if DirectoryExists(Chemin) then
    WriteLn('Le répertoire existe : ', Chemin)
  else
    WriteLn('Le répertoire n''existe pas : ', Chemin);
end.
```

**Usage typique :** Vérifier avant de créer ou d'accéder à un répertoire.

```pascal
if not DirectoryExists('Logs') then
  CreateDir('Logs');
```

---

## Lister le contenu d'un répertoire

### FindFirst, FindNext, FindClose : Parcourir un répertoire

Pour lister tous les fichiers et répertoires, on utilise trois fonctions ensemble :

```pascal
program ListerFichiers;

uses
  SysUtils;

var
  Info: TSearchRec;
  Resultat: Integer;

begin
  WriteLn('Contenu du répertoire courant :');
  WriteLn('--------------------------------');

  // Rechercher tous les fichiers et répertoires
  Resultat := FindFirst('*', faAnyFile, Info);

  try
    while Resultat = 0 do
    begin
      // Ignorer . et ..
      if (Info.Name <> '.') and (Info.Name <> '..') then
      begin
        if (Info.Attr and faDirectory) = faDirectory then
          WriteLn('[DIR]  ', Info.Name)
        else
          WriteLn('[FILE] ', Info.Name, ' (', Info.Size, ' octets)');
      end;

      Resultat := FindNext(Info);
    end;
  finally
    FindClose(Info);  // Toujours fermer !
  end;

  WriteLn('--------------------------------');
end.
```

**Explication des fonctions :**

- **FindFirst(Masque, Attributs, SearchRec)** : Démarre la recherche
  - Retourne 0 si succès, autre valeur si échec
  - `Masque` : motif de recherche (`*` = tout, `*.txt` = fichiers .txt)
  - `Attributs` : type de fichiers recherchés
  - `SearchRec` : structure qui reçoit les informations

- **FindNext(SearchRec)** : Trouve l'élément suivant
  - Retourne 0 si trouvé, autre valeur si fin

- **FindClose(SearchRec)** : Libère les ressources
  - **Obligatoire** après FindFirst !

### Attributs de fichiers

Les constantes d'attributs disponibles :

| Constante | Signification |
|-----------|---------------|
| `faReadOnly` | Fichiers en lecture seule |
| `faHidden` | Fichiers cachés |
| `faSysFile` | Fichiers système |
| `faVolumeID` | Label de volume |
| `faDirectory` | Répertoires |
| `faArchive` | Fichiers à archiver |
| `faAnyFile` | Tous les fichiers et répertoires |

### Structure TSearchRec

```pascal
type
  TSearchRec = record
    Name: string;        // Nom du fichier/répertoire
    Size: Int64;         // Taille en octets
    Attr: Integer;       // Attributs
    Time: LongInt;       // Date/heure de modification
    // ... autres champs
  end;
```

---

## Exemples pratiques

### Exemple 1 : Lister uniquement les fichiers

```pascal
program ListerSeulementFichiers;

uses
  SysUtils;

var
  Info: TSearchRec;

begin
  WriteLn('Fichiers dans le répertoire courant :');

  if FindFirst('*', faAnyFile, Info) = 0 then
  begin
    try
      repeat
        // Vérifier que ce n'est PAS un répertoire
        if (Info.Attr and faDirectory) = 0 then
          WriteLn('  ', Info.Name);
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;
end.
```

### Exemple 2 : Lister uniquement les sous-répertoires

```pascal
program ListerSousRepertoires;

uses
  SysUtils;

var
  Info: TSearchRec;

begin
  WriteLn('Sous-répertoires :');

  if FindFirst('*', faDirectory, Info) = 0 then
  begin
    try
      repeat
        if ((Info.Attr and faDirectory) = faDirectory) and
           (Info.Name <> '.') and (Info.Name <> '..') then
          WriteLn('  [', Info.Name, ']');
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;
end.
```

### Exemple 3 : Rechercher un type de fichier spécifique

```pascal
program ChercherFichiersTexte;

uses
  SysUtils;

var
  Info: TSearchRec;
  Compteur: Integer;

begin
  Compteur := 0;

  WriteLn('Recherche de fichiers .txt...');

  if FindFirst('*.txt', faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Attr and faDirectory) = 0 then
        begin
          WriteLn('  ', Info.Name, ' (', Info.Size, ' octets)');
          Inc(Compteur);
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;

  WriteLn;
  WriteLn('Total : ', Compteur, ' fichier(s) .txt trouvé(s)');
end.
```

### Exemple 4 : Parcours récursif (tous les sous-répertoires)

```pascal
program ParcoursRecursif;

uses
  SysUtils;

procedure ListerRecursif(Chemin: string; Niveau: Integer);
var
  Info: TSearchRec;
  CheminComplet: string;
  Indentation: string;
  i: Integer;
begin
  // Créer l'indentation selon le niveau
  Indentation := '';
  for i := 1 to Niveau do
    Indentation := Indentation + '  ';

  // Chercher dans le répertoire
  if FindFirst(Chemin + PathDelim + '*', faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          CheminComplet := Chemin + PathDelim + Info.Name;

          if (Info.Attr and faDirectory) = faDirectory then
          begin
            WriteLn(Indentation, '[', Info.Name, ']');
            // Appel récursif pour le sous-répertoire
            ListerRecursif(CheminComplet, Niveau + 1);
          end
          else
            WriteLn(Indentation, Info.Name);
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;
end;

begin
  WriteLn('Arborescence complète :');
  WriteLn('======================');
  ListerRecursif(GetCurrentDir, 0);
end.
```

---

## Exemple complet : Gestionnaire de répertoires

Créons une petite application interactive pour gérer les répertoires.

```pascal
program GestionnaireRepertoires;

uses
  SysUtils;

procedure AfficherRepertoireCourant;
begin
  WriteLn('Répertoire actuel : ', GetCurrentDir);
end;

procedure ListerContenu;
var
  Info: TSearchRec;
  NbFichiers, NbDossiers: Integer;
begin
  NbFichiers := 0;
  NbDossiers := 0;

  WriteLn;
  WriteLn('=== Contenu du répertoire ===');

  if FindFirst('*', faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          if (Info.Attr and faDirectory) = faDirectory then
          begin
            WriteLn('[DIR]  ', Info.Name);
            Inc(NbDossiers);
          end
          else
          begin
            WriteLn('[FILE] ', Info.Name:30, '  ', Info.Size:12, ' octets');
            Inc(NbFichiers);
          end;
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;

  WriteLn;
  WriteLn(NbDossiers, ' répertoire(s), ', NbFichiers, ' fichier(s)');
  WriteLn('=============================');
end;

procedure CreerNouveauRepertoire;
var
  Nom: string;
begin
  Write('Nom du nouveau répertoire : ');
  ReadLn(Nom);

  if Nom = '' then
  begin
    WriteLn('Nom invalide !');
    Exit;
  end;

  if DirectoryExists(Nom) then
  begin
    WriteLn('Ce répertoire existe déjà !');
    Exit;
  end;

  if CreateDir(Nom) then
    WriteLn('Répertoire "', Nom, '" créé avec succès !')
  else
    WriteLn('Erreur lors de la création du répertoire');
end;

procedure SupprimerRepertoire;
var
  Nom: string;
begin
  Write('Nom du répertoire à supprimer : ');
  ReadLn(Nom);

  if not DirectoryExists(Nom) then
  begin
    WriteLn('Ce répertoire n''existe pas !');
    Exit;
  end;

  if RemoveDir(Nom) then
    WriteLn('Répertoire "', Nom, '" supprimé avec succès !')
  else
    WriteLn('Erreur : Le répertoire n''est probablement pas vide');
end;

procedure ChangerRepertoire;
var
  Nom: string;
begin
  Write('Nom du répertoire (.. pour parent) : ');
  ReadLn(Nom);

  if SetCurrentDir(Nom) then
  begin
    WriteLn('Changement réussi !');
    AfficherRepertoireCourant;
  end
  else
    WriteLn('Erreur : Répertoire invalide ou inaccessible');
end;

procedure RechercherFichiers;
var
  Motif: string;
  Info: TSearchRec;
  Compteur: Integer;
begin
  Write('Motif de recherche (ex: *.txt) : ');
  ReadLn(Motif);

  if Motif = '' then
    Motif := '*';

  WriteLn;
  WriteLn('Résultats de la recherche :');

  Compteur := 0;

  if FindFirst(Motif, faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Attr and faDirectory) = 0 then
        begin
          WriteLn('  ', Info.Name, ' (', Info.Size, ' octets)');
          Inc(Compteur);
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;

  WriteLn;
  WriteLn(Compteur, ' fichier(s) trouvé(s)');
end;

var
  Choix: Integer;

begin
  WriteLn('================================');
  WriteLn('  GESTIONNAIRE DE RÉPERTOIRES  ');
  WriteLn('================================');

  repeat
    WriteLn;
    AfficherRepertoireCourant;
    WriteLn;
    WriteLn('1. Lister le contenu');
    WriteLn('2. Créer un répertoire');
    WriteLn('3. Supprimer un répertoire');
    WriteLn('4. Changer de répertoire');
    WriteLn('5. Rechercher des fichiers');
    WriteLn('0. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1: ListerContenu;
      2: CreerNouveauRepertoire;
      3: SupprimerRepertoire;
      4: ChangerRepertoire;
      5: RechercherFichiers;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;
end.
```

---

## Portabilité Windows/Linux

### Séparateur de chemin

Le séparateur de répertoires diffère selon les systèmes :
- **Windows** : backslash `\` (ex: `C:\Users\Jean`)
- **Linux** : slash `/` (ex: `/home/jean`)

**Solution portable :**

```pascal
uses
  SysUtils;

var
  Chemin: string;

begin
  // Utiliser PathDelim (défini dans SysUtils)
  Chemin := 'Projet' + PathDelim + 'Source' + PathDelim + 'main.pas';
  WriteLn(Chemin);

  // Sous Windows : Projet\Source\main.pas
  // Sous Linux   : Projet/Source/main.pas
end.
```

**Constantes utiles :**
- `PathDelim` : Séparateur de chemin (`\` ou `/`)
- `DirectorySeparator` : Alias de PathDelim
- `DriveDelim` : Séparateur de lecteur (`:` sous Windows, vide sous Linux)

### Fonctions portables

```pascal
uses
  SysUtils;

var
  CheminComplet: string;

begin
  // Concaténer des chemins de manière portable
  CheminComplet := ConcatPaths(['Projet', 'Source', 'Units']);
  WriteLn(CheminComplet);

  // Ou avec IncludeTrailingPathDelimiter
  CheminComplet := IncludeTrailingPathDelimiter('Projet') + 'Source';
  WriteLn(CheminComplet);
end.
```

### Répertoires spéciaux

```pascal
uses
  SysUtils;

begin
  // Répertoire temporaire (portable)
  WriteLn('Temp : ', GetTempDir);

  // Répertoire de l'application
  WriteLn('App  : ', ExtractFilePath(ParamStr(0)));

  // Répertoire home de l'utilisateur (avec FileUtil)
  // WriteLn('Home : ', GetUserDir);
end.
```

---

## Opérations avancées

### Copier un répertoire complet

```pascal
uses
  SysUtils, FileUtil;

begin
  if CopyDirTree('Source', 'Destination') then
    WriteLn('Répertoire copié avec succès')
  else
    WriteLn('Erreur lors de la copie');
end.
```

**Note :** Nécessite l'unité `FileUtil` de Lazarus.

### Supprimer un répertoire et son contenu

```pascal
uses
  FileUtil;

begin
  if DeleteDirectory('MonDossier', False) then
    WriteLn('Répertoire supprimé')
  else
    WriteLn('Erreur lors de la suppression');
end.
```

**Paramètres :**
- Premier : chemin du répertoire
- Second : `True` = suppression récursive, `False` = uniquement si vide

### Obtenir la taille d'un répertoire

```pascal
uses
  SysUtils;

function TailleRepertoire(Chemin: string): Int64;
var
  Info: TSearchRec;
  Total: Int64;
begin
  Total := 0;

  if FindFirst(Chemin + PathDelim + '*', faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          if (Info.Attr and faDirectory) = faDirectory then
            // Récursif pour les sous-répertoires
            Total := Total + TailleRepertoire(Chemin + PathDelim + Info.Name)
          else
            Total := Total + Info.Size;
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;

  TailleRepertoire := Total;
end;

var
  Taille: Int64;

begin
  Taille := TailleRepertoire(GetCurrentDir);
  WriteLn('Taille totale : ', Taille, ' octets');
  WriteLn('Soit : ', Taille div 1024, ' Ko');
end.
```

---

## Tableau récapitulatif des fonctions

| Fonction | Description | Retour |
|----------|-------------|--------|
| `GetCurrentDir` | Obtenir le répertoire courant | string |
| `SetCurrentDir(chemin)` | Changer de répertoire | Boolean |
| `CreateDir(nom)` | Créer un répertoire | Boolean |
| `RemoveDir(nom)` | Supprimer un répertoire vide | Boolean |
| `ForceDirectories(chemin)` | Créer une arborescence complète | Boolean |
| `DirectoryExists(chemin)` | Vérifier l'existence | Boolean |
| `FindFirst(masque, attr, sr)` | Démarrer une recherche | Integer |
| `FindNext(sr)` | Élément suivant | Integer |
| `FindClose(sr)` | Fermer la recherche | - |
| `PathDelim` | Séparateur de chemin portable | Char |

---

## Bonnes pratiques

### ✅ À faire

**Toujours utiliser `PathDelim`** pour la portabilité Windows/Linux

**Toujours appeler `FindClose`** après `FindFirst` (utilisez try-finally)

**Vérifier l'existence** avec `DirectoryExists` avant de créer ou supprimer

**Gérer les erreurs** avec IOResult ou try-except

**Utiliser `ForceDirectories`** pour créer des arborescences complètes

**Tester . et ..** pour les ignorer lors du parcours

**Documenter** les chemins attendus dans vos fonctions

**Prévoir des valeurs par défaut** pour les répertoires manquants

### ❌ À éviter

**Ne jamais oublier `FindClose`** (fuite de ressources)

**Ne pas coder en dur** les séparateurs `\` ou `/`

**Ne pas supposer** qu'un répertoire existe toujours

**Ne pas supprimer** un répertoire sans vérifier qu'il est vide (sauf intention)

**Ne pas parcourir** récursivement sans limite (risque de boucle infinie sur liens symboliques)

**Ne pas négliger** les permissions (accès refusé)

---

## Résumé

La manipulation de répertoires en Pascal est simple et puissante :

**Opérations de base :**
- `GetCurrentDir` / `SetCurrentDir` : navigation
- `CreateDir` / `RemoveDir` : création/suppression
- `DirectoryExists` : vérification

**Parcours de répertoires :**
- `FindFirst` / `FindNext` / `FindClose` : lister le contenu
- Toujours fermer avec `FindClose` !

**Portabilité :**
- Utiliser `PathDelim` au lieu de `\` ou `/`
- Tester sur Windows ET Linux

**Fonctions avancées :**
- `ForceDirectories` : créer une arborescence
- Parcours récursif pour explorer toute une arborescence

Dans la section suivante, nous verrons comment manipuler les chemins et noms de fichiers de manière professionnelle !

---

> **Conseil pratique :** La manipulation de répertoires est essentielle pour organiser vos données. Prenez le temps de bien comprendre `FindFirst`/`FindNext`/`FindClose`, car c'est la base de toute exploration de système de fichiers.

⏭️ [Chemins et noms de fichiers](/08-gestion-fichiers-io/07-chemins-noms-fichiers.md)
