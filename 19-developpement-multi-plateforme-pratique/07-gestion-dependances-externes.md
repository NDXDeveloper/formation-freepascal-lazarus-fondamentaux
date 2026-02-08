🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.7 Gestion des dépendances externes

## Introduction

Imaginez que vous construisez une maison. Vous ne fabriquez pas vous-même les briques, le ciment, les fenêtres et les portes. Vous les achetez auprès de fournisseurs spécialisés. De la même manière, vos programmes s'appuient souvent sur du code écrit par d'autres : ce sont les **dépendances externes**.

Une **dépendance externe** est une bibliothèque, un composant ou un morceau de code dont votre application a besoin pour fonctionner, mais qui n'est pas inclus de base avec FreePascal ou Lazarus.

Dans ce chapitre, nous allons apprendre à identifier, gérer et distribuer ces dépendances de manière efficace et multi-plateforme.

---

## 1. Qu'est-ce qu'une Dépendance Externe ?

### Définition Simple

Une **dépendance externe** est un élément dont votre programme a besoin mais qui n'est pas :
- Partie de FreePascal (comme `SysUtils`, `Classes`)
- Partie de Lazarus (comme la LCL)
- Écrit directement par vous dans votre projet

**Exemples courants :**
- Une bibliothèque pour accéder à une base de données (SQLite, PostgreSQL)
- Une bibliothèque de traitement d'images (GraphicsMagick)
- Un package Lazarus pour des composants avancés
- Une bibliothèque de cryptographie (OpenSSL)

### Types de Dépendances

**1. Bibliothèques dynamiques (runtime) :**
- **Windows :** fichiers `.dll` (Dynamic Link Library)
- **Linux :** fichiers `.so` (Shared Object)
- **macOS :** fichiers `.dylib` (Dynamic Library)

**Exemples :** `sqlite3.dll`, `libcurl.so`, `libssl.dylib`

**2. Packages Lazarus :**
- Extensions pour Lazarus IDE
- Nouveaux composants visuels
- Fonctionnalités additionnelles

**Exemples :** Indy (réseau), BGRABitmap (graphisme), Synapse (HTTP)

**3. Unités Pascal tierces :**
- Fichiers `.pas` d'autres développeurs
- Bibliothèques en code source

**Exemples :** fpJSON, Synapse, LazSerial

---

## 2. Identifier les Dépendances de Votre Projet

### Méthode 1 : Analyser la Clause Uses

Regardez les unités que vous utilisez dans vos fichiers :

```pascal
uses
  SysUtils,        // ✅ FreePascal standard - pas de dépendance
  Classes,         // ✅ FreePascal standard - pas de dépendance
  Forms,           // ✅ LCL Lazarus - pas de dépendance
  SQLite3Conn,     // ⚠️ Nécessite libsqlite3
  BGRABitmap,      // ⚠️ Package externe BGRABitmap
  Synapse,         // ⚠️ Bibliothèque Synapse
  OpenSSL;         // ⚠️ Nécessite OpenSSL (libssl, libcrypto)
```

**Comment savoir si c'est une dépendance externe ?**

1. Si l'unité n'est pas reconnue à la compilation : c'est une dépendance manquante
2. Si la compilation réussit mais le programme ne démarre pas : dépendance runtime manquante
3. Consultez la documentation de l'unité utilisée

### Méthode 2 : Vérifier les Packages Installés

Dans Lazarus :
1. **Paquet** → **Installer/Désinstaller des paquets**
2. Regardez la colonne de droite (paquets installés)
3. Tout ce qui n'est pas "LCL" ou "FCL" est une dépendance externe

### Méthode 3 : Outils Système

**Sous Linux :**
```bash
# Voir les dépendances dynamiques d'un exécutable
ldd MonProgramme

# Exemple de sortie :
#   linux-vdso.so.1 => (0x00007ffd8d3dd000)
#   libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0
#   libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6
#   libsqlite3.so.0 => /usr/lib/x86_64-linux-gnu/libsqlite3.so.0  ← Dépendance !
```

**Sous Windows :**
Utilisez l'outil **Dependency Walker** (depends.exe) ou **Dependencies** (outil moderne)

**Sous macOS :**
```bash
otool -L MonProgramme
```

---

## 3. Bibliothèques Dynamiques

### Comprendre le Chargement Dynamique

Quand votre programme utilise une bibliothèque dynamique, deux choses peuvent se produire :

**1. Chargement au démarrage (liaison implicite) :**
```pascal
uses
  SQLite3;  // Utilise sqlite3.dll automatiquement

begin
  // La DLL est chargée au démarrage du programme
  // Si elle n'est pas trouvée : erreur immédiate
end.
```

**2. Chargement à la demande (liaison explicite) :**
```pascal
uses
  DynLibs;

var
  LibHandle: TLibHandle;
begin
  // Charger la DLL quand on en a besoin
  LibHandle := LoadLibrary('sqlite3.dll');
  if LibHandle = 0 then
    ShowMessage('Bibliothèque non trouvée');
end.
```

### Où le Système Cherche les Bibliothèques

**Windows :**
1. Le répertoire de l'exécutable
2. Le répertoire Windows System32
3. Les répertoires dans la variable PATH

**Linux :**
1. Les répertoires dans `LD_LIBRARY_PATH`
2. `/lib`, `/usr/lib`, `/usr/local/lib`
3. Les répertoires configurés dans `/etc/ld.so.conf`

**macOS :**
1. Les répertoires dans `DYLD_LIBRARY_PATH`
2. `/usr/lib`, `/usr/local/lib`

### Nommage Multi-Plateforme des Bibliothèques

**Exemple : SQLite**

| Plateforme | Nom de fichier | Localisation typique |
|------------|---------------|---------------------|
| Windows | `sqlite3.dll` | Dossier de l'exe |
| Linux | `libsqlite3.so.0` | `/usr/lib` |
| macOS | `libsqlite3.dylib` | `/usr/local/lib` |

**Code portable pour charger :**

```pascal
unit SQLiteLoader;

{$mode objfpc}{$H+}

interface

uses
  DynLibs, SysUtils;

function LoadSQLite: Boolean;
procedure UnloadSQLite;

implementation

const
  {$IFDEF WINDOWS}
  SQLiteLib = 'sqlite3.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  SQLiteLib = 'libsqlite3.so.0';
  {$ENDIF}
  {$IFDEF DARWIN}
  SQLiteLib = 'libsqlite3.0.dylib';
  {$ENDIF}

var
  LibHandle: TLibHandle = 0;

function LoadSQLite: Boolean;
begin
  if LibHandle = 0 then
  begin
    LibHandle := LoadLibrary(SQLiteLib);
    if LibHandle = 0 then
      WriteLn('Erreur : Impossible de charger ', SQLiteLib);
  end;
  Result := LibHandle <> 0;
end;

procedure UnloadSQLite;
begin
  if LibHandle <> 0 then
  begin
    UnloadLibrary(LibHandle);
    LibHandle := 0;
  end;
end;

end.
```

---

## 4. Stratégies de Distribution

### Stratégie 1 : Inclure les Bibliothèques avec l'Application

**Avantages :**
- ✅ L'utilisateur n'a rien à installer
- ✅ Vous contrôlez la version exacte
- ✅ Fonctionne hors ligne

**Inconvénients :**
- ❌ Archive plus volumineuse
- ❌ Mises à jour de sécurité à gérer manuellement

**Structure recommandée :**

```
MonApplication/
├── MonApp.exe (ou MonApp sous Linux)
├── lib/
│   ├── sqlite3.dll           (Windows)
│   ├── libssl-1_1.dll
│   └── libcrypto-1_1.dll
└── README.txt
```

**Code pour chercher dans le sous-dossier lib/ :**

```pascal
function GetLibraryPath(const LibName: string): string;
var
  AppDir: string;
begin
  AppDir := ExtractFilePath(ParamStr(0));

  // Chercher d'abord dans lib/
  Result := AppDir + 'lib' + PathDelim + LibName;
  if not FileExists(Result) then
    // Sinon utiliser le nom simple (recherche système)
    Result := LibName;
end;

var
  SQLitePath: string;
  Handle: TLibHandle;
begin
  SQLitePath := GetLibraryPath('sqlite3.dll');
  Handle := LoadLibrary(SQLitePath);
  // ...
end;
```

### Stratégie 2 : Dépendre des Bibliothèques Système

**Avantages :**
- ✅ Archive plus petite
- ✅ Mises à jour de sécurité automatiques
- ✅ Réutilisation entre applications

**Inconvénients :**
- ❌ L'utilisateur doit installer les dépendances
- ❌ Versions potentiellement différentes
- ❌ Plus complexe pour l'utilisateur

**Documentation requise :**

**README.md :**
```markdown
## Prérequis

### Windows
Téléchargez et installez :
- SQLite : https://www.sqlite.org/download.html
- OpenSSL : https://slproweb.com/products/Win32OpenSSL.html

### Linux (Ubuntu/Debian)
```bash
sudo apt install libsqlite3-0 libssl1.1
```

### Linux (Fedora/RedHat)
```bash
sudo dnf install sqlite-libs openssl-libs
```

### macOS
```bash
brew install sqlite openssl
```
```

### Stratégie 3 : Hybride (Recommandé)

**Sous Windows :** Inclure les DLL
**Sous Linux/macOS :** Dépendre des bibliothèques système

**Pourquoi ?**
- Windows : Pas de gestionnaire de paquets standard, les utilisateurs préfèrent "tout inclus"
- Linux/macOS : Gestionnaires de paquets efficaces, les utilisateurs sont habitués à installer des dépendances

**Implémentation :**

```pascal
function VerifierDependances: Boolean;
var
  Manquantes: TStringList;
begin
  Manquantes := TStringList.Create;
  try
    // Vérifier SQLite
    if LoadLibrary('sqlite3.dll') = 0 then
      Manquantes.Add('SQLite3');

    // Vérifier OpenSSL
    if LoadLibrary('libssl-1_1.dll') = 0 then
      Manquantes.Add('OpenSSL');

    if Manquantes.Count > 0 then
    begin
      ShowMessage('Bibliothèques manquantes : ' + Manquantes.CommaText + sLineBreak +
                  'Veuillez consulter le fichier README.txt');
      Result := False;
    end
    else
      Result := True;
  finally
    Manquantes.Free;
  end;
end;

begin
  if not VerifierDependances then
    Halt(1);

  // Continuer normalement...
end.
```

---

## 5. Packages Lazarus

### Qu'est-ce qu'un Package Lazarus ?

Un **package** (.lpk) est une collection de composants, unités et ressources qui étend les capacités de Lazarus.

**Exemples populaires :**
- **BGRABitmap** : Graphisme avancé, anti-aliasing
- **Indy** : Composants réseau (HTTP, FTP, SMTP)
- **Synapse** : Alternative légère pour le réseau
- **LazSerial** : Communication série (COM ports)
- **Zeos** : Accès bases de données (MySQL, PostgreSQL, etc.)
- **VirtualTreeView** : Contrôle arborescent avancé

### Online Package Manager (OPM)

Lazarus dispose d'un gestionnaire de packages intégré similaire à npm, pip ou apt.

**Accéder à OPM :**
1. **Paquet** → **Gestionnaire de paquets en ligne**
2. Ou dans les versions récentes : **Outils** → **Gestionnaire de packages en ligne**

**Interface OPM :**
- Colonne de gauche : Liste des packages disponibles
- Panneau de droite : Détails et description
- Boutons : Installer, Désinstaller, Mettre à jour

### Installer un Package via OPM

**Exemple : Installer BGRABitmap**

1. Ouvrir **Paquet** → **Gestionnaire de paquets en ligne**
2. Dans la liste, chercher "bgrabitmappack"
3. Sélectionner le package
4. Cliquer sur **Installer**
5. OPM télécharge et installe automatiquement
6. Reconstruire Lazarus quand demandé
7. Redémarrer Lazarus

**Le package est maintenant disponible !**

### Installation Manuelle de Packages

Si OPM n'est pas disponible ou pour un package local :

**Méthode 1 : Via l'interface**

1. **Paquet** → **Ouvrir un fichier de paquet (.lpk)**
2. Naviguer vers le fichier `.lpk` du package
3. Dans la fenêtre qui s'ouvre, cliquer sur **Compiler**
4. Puis cliquer sur **Installer**
5. Lazarus se recompile avec le package
6. Redémarrer Lazarus

**Méthode 2 : En ligne de commande**

```bash
# Compiler le package
lazbuild MonPackage.lpk

# Installer dans Lazarus (nécessite configuration)
# Généralement fait via l'IDE
```

### Utiliser un Package dans Votre Projet

Une fois le package installé :

1. **Projet** → **Inspecteur de projet**
2. Cliquer sur **Ajouter** → **Nouvelle dépendance**
3. Sélectionner le package dans la liste
4. Cliquer sur **OK**

Ou manuellement dans le fichier `.lpi` :

```xml
<RequiredPackages Count="2">
  <Item1>
    <PackageName Value="LCL"/>
  </Item1>
  <Item2>
    <PackageName Value="BGRABitmapPack"/>
  </Item2>
</RequiredPackages>
```

**Dans votre code :**

```pascal
uses
  BGRABitmap, BGRABitmapTypes;

var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(800, 600, BGRAWhite);
  try
    // Utiliser le bitmap...
  finally
    Bitmap.Free;
  end;
end;
```

---

## 6. Gestion des Versions

### Problème de Compatibilité

**Scénario :** Votre application fonctionne avec SQLite 3.35 mais l'utilisateur a la version 3.20.

**Risques :**
- Fonctionnalités manquantes
- API incompatible
- Bugs potentiels

### Solution 1 : Version Minimale Requise

**Documenter dans README :**
```markdown
## Versions requises

- SQLite : 3.30 ou supérieur
- OpenSSL : 1.1.1 ou supérieur
```

**Vérifier au démarrage :**

```pascal
uses
  SQLite3, SysUtils;

function GetSQLiteVersion: string;
var
  DB: TSQLite3Connection;
begin
  DB := TSQLite3Connection.Create(nil);
  try
    // Version fournie par SQLite
    Result := sqlite3_libversion;
  finally
    DB.Free;
  end;
end;

function VerifierVersionSQLite: Boolean;
var
  Version: string;
begin
  Version := GetSQLiteVersion;
  WriteLn('SQLite version : ', Version);

  // Comparer avec version minimale (3.30.0)
  Result := CompareVersionStrings(Version, '3.30.0') >= 0;

  if not Result then
    ShowMessage('SQLite version ' + Version + ' trop ancienne.' + sLineBreak +
                'Version 3.30 ou supérieure requise.');
end;
```

### Solution 2 : Inclure la Version Exacte

Pour éviter tout problème, incluez la version exacte testée :

```
MonApp/
├── MonApp.exe
└── lib/
    ├── sqlite3.dll (version 3.35.5)
    └── VERSION.txt (documentation des versions)
```

**VERSION.txt :**
```
Bibliothèques incluses :
- SQLite 3.35.5
- OpenSSL 1.1.1k
- libcurl 7.74.0

Date de build : 2024-10-15
```

---

## 7. Dépendances Conditionnelles

### Fonctionnalités Optionnelles

Certaines fonctionnalités peuvent être optionnelles si la bibliothèque n'est pas disponible.

**Exemple : Support HTTPS optionnel**

```pascal
unit NetworkManager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DynLibs;

type
  TNetworkCapabilities = set of (ncHTTP, ncHTTPS, ncFTP);

function GetNetworkCapabilities: TNetworkCapabilities;
function DownloadFile(const URL, Destination: string): Boolean;

implementation

var
  OpenSSLAvailable: Boolean = False;

function GetNetworkCapabilities: TNetworkCapabilities;
begin
  Result := [ncHTTP];  // HTTP toujours disponible

  if OpenSSLAvailable then
    Include(Result, ncHTTPS);

  // Vérifier autres protocoles...
end;

function DownloadFile(const URL, Destination: string): Boolean;
var
  IsHTTPS: Boolean;
begin
  IsHTTPS := Pos('https://', LowerCase(URL)) = 1;

  if IsHTTPS and not OpenSSLAvailable then
  begin
    WriteLn('Erreur : HTTPS non supporté (OpenSSL manquant)');
    Result := False;
    Exit;
  end;

  // Télécharger...
  Result := True;
end;

procedure DetectOpenSSL;
{$IFDEF WINDOWS}
const
  SSLLibs: array[0..1] of string = ('libssl-1_1-x64.dll', 'libssl-1_1.dll');
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  for i := 0 to High(SSLLibs) do
  begin
    if LoadLibrary(SSLLibs[i]) <> 0 then
    begin
      OpenSSLAvailable := True;
      Exit;
    end;
  end;
  {$ELSE}
  // Sous Linux/macOS, OpenSSL est généralement disponible
  OpenSSLAvailable := LoadLibrary('libssl.so') <> 0;
  {$ENDIF}
end;

initialization
  DetectOpenSSL;
  if OpenSSLAvailable then
    WriteLn('OpenSSL détecté : Support HTTPS activé')
  else
    WriteLn('OpenSSL non trouvé : HTTPS désactivé');
end.
```

---

## 8. Créer des Installeurs

### Windows : Inno Setup

**Inno Setup** est un créateur d'installeurs gratuit pour Windows.

**Script exemple (setup.iss) :**

```ini
[Setup]
AppName=Mon Application
AppVersion=1.0
DefaultDirName={pf}\MonApp
DefaultGroupName=Mon Application
OutputDir=.
OutputBaseFilename=MonApp_Setup

[Files]
; Exécutable principal
Source: "bin\MonApp.exe"; DestDir: "{app}"

; Bibliothèques nécessaires
Source: "lib\sqlite3.dll"; DestDir: "{app}\lib"
Source: "lib\libssl-1_1.dll"; DestDir: "{app}\lib"
Source: "lib\libcrypto-1_1.dll"; DestDir: "{app}\lib"

; Documentation
Source: "README.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\Mon Application"; Filename: "{app}\MonApp.exe"
Name: "{group}\Désinstaller"; Filename: "{uninstallexe}"
```

**Compiler l'installeur :**
1. Installer Inno Setup
2. Ouvrir `setup.iss` dans Inno Setup
3. Menu **Build** → **Compile**
4. L'installeur `MonApp_Setup.exe` est créé

### Linux : Packages .deb et .rpm

**Pour Ubuntu/Debian (.deb) :**

**Structure du projet :**
```
monapp_1.0-1/
├── DEBIAN/
│   ├── control
│   └── postinst (optionnel)
└── usr/
    ├── bin/
    │   └── monapp
    └── share/
        ├── applications/
        │   └── monapp.desktop
        └── pixmaps/
            └── monapp.png
```

**Fichier control :**
```
Package: monapp
Version: 1.0-1
Section: utils
Priority: optional
Architecture: amd64
Depends: libsqlite3-0, libssl1.1
Maintainer: Votre Nom <email@example.com>
Description: Mon Application
 Description détaillée de l'application
```

**Créer le package :**
```bash
dpkg-deb --build monapp_1.0-1
```

**Pour Fedora/RedHat (.rpm) :**

Utilisez `rpmbuild` avec un fichier `.spec` similaire.

### macOS : .app Bundle

**Structure d'un bundle macOS :**
```
MonApp.app/
└── Contents/
    ├── Info.plist
    ├── MacOS/
    │   └── monapp (exécutable)
    ├── Resources/
    │   └── monapp.icns (icône)
    └── Frameworks/
        └── (bibliothèques .dylib)
```

---

## 9. Tests de Dépendances

### Script de Vérification

**verify-deps.pas :**

```pascal
program VerifyDeps;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs, Classes;  // DynLibs : LoadLibrary, UnloadLibrary, TLibHandle

type
  TDependency = record
    Name: string;
    {$IFDEF WINDOWS}
    FileName: string;
    {$ENDIF}
    {$IFDEF LINUX}
    FileName: string;
    {$ENDIF}
    {$IFDEF DARWIN}
    FileName: string;
    {$ENDIF}
    Required: Boolean;
  end;

const
  Dependencies: array[0..2] of TDependency = (
    (
      Name: 'SQLite';
      {$IFDEF WINDOWS} FileName: 'sqlite3.dll'; {$ENDIF}
      {$IFDEF LINUX} FileName: 'libsqlite3.so.0'; {$ENDIF}
      {$IFDEF DARWIN} FileName: 'libsqlite3.dylib'; {$ENDIF}
      Required: True
    ),
    (
      Name: 'OpenSSL';
      {$IFDEF WINDOWS} FileName: 'libssl-1_1.dll'; {$ENDIF}
      {$IFDEF LINUX} FileName: 'libssl.so.1.1'; {$ENDIF}
      {$IFDEF DARWIN} FileName: 'libssl.dylib'; {$ENDIF}
      Required: True
    ),
    (
      Name: 'libcurl';
      {$IFDEF WINDOWS} FileName: 'libcurl.dll'; {$ENDIF}
      {$IFDEF LINUX} FileName: 'libcurl.so.4'; {$ENDIF}
      {$IFDEF DARWIN} FileName: 'libcurl.dylib'; {$ENDIF}
      Required: False
    )
  );

function TestDependency(const Dep: TDependency): Boolean;
var
  Handle: TLibHandle;
begin
  Handle := LoadLibrary(Dep.FileName);
  Result := Handle <> 0;

  if Result then
  begin
    WriteLn('[OK] ', Dep.Name, ' trouvé (', Dep.FileName, ')');
    UnloadLibrary(Handle);
  end
  else
  begin
    if Dep.Required then
      WriteLn('[ERREUR] ', Dep.Name, ' MANQUANT (', Dep.FileName, ')')
    else
      WriteLn('[WARN] ', Dep.Name, ' non trouvé (optionnel)');
  end;
end;

var
  i: Integer;
  AllOK: Boolean;
begin
  WriteLn('=================================');
  WriteLn('Vérification des dépendances');
  WriteLn('=================================');
  WriteLn;

  AllOK := True;

  for i := 0 to High(Dependencies) do
  begin
    if not TestDependency(Dependencies[i]) then
    begin
      if Dependencies[i].Required then
        AllOK := False;
    end;
  end;

  WriteLn;
  WriteLn('=================================');
  if AllOK then
    WriteLn('Toutes les dépendances requises sont présentes !')
  else
  begin
    WriteLn('ERREUR : Des dépendances sont manquantes !');
    WriteLn('Consultez le fichier README pour les instructions d''installation.');
    Halt(1);
  end;

  WriteLn('Appuyez sur Entrée pour continuer...');
  ReadLn;
end.
```

**Utilisation :**
Incluez ce programme avec votre distribution pour que l'utilisateur vérifie son installation.

---

## 10. Documentation des Dépendances

### Fichier DEPENDENCIES.md

**Exemple complet :**

```markdown
# Dépendances de Mon Application

## Dépendances Requises

### SQLite (Base de données)
- **Version minimale :** 3.30.0
- **Windows :** Inclus dans lib/sqlite3.dll
- **Linux :** `sudo apt install libsqlite3-0`
- **macOS :** `brew install sqlite3`

### OpenSSL (Sécurité/HTTPS)
- **Version minimale :** 1.1.1
- **Windows :** Inclus dans lib/
- **Linux :** `sudo apt install libssl1.1`
- **macOS :** `brew install openssl@1.1`

## Dépendances Optionnelles

### libcurl (Téléchargements avancés)
- **Fonctionnalité :** Support FTP et protocoles avancés
- **Si absent :** L'application fonctionne mais sans support FTP
- **Installation :**
  - **Linux :** `sudo apt install libcurl4`
  - **macOS :** `brew install curl`

## Packages Lazarus Nécessaires

Si vous compilez depuis les sources :

1. **BGRABitmap**
   - Installation : Via OPM (Gestionnaire de paquets en ligne)
   - Usage : Graphisme anti-aliasing

2. **Synapse**
   - Installation : Via OPM
   - Usage : Communication réseau

## Vérification

Exécutez `verify-deps` pour vérifier que toutes les dépendances sont présentes.

## Problèmes Courants

### "Cannot find sqlite3.dll"
**Solution Windows :** Copiez sqlite3.dll dans le dossier de l'application.

### "Error loading shared library: libssl.so"
**Solution Linux :** Installez OpenSSL avec votre gestionnaire de paquets.
```

---

## 11. Bonnes Pratiques

### ✅ 1. Minimiser les Dépendances

**Principe :** Moins de dépendances = moins de problèmes.

**Questions à se poser :**
- Cette bibliothèque est-elle vraiment nécessaire ?
- Puis-je utiliser une fonctionnalité standard de FreePascal/Lazarus ?
- Puis-je écrire moi-même cette fonctionnalité simplement ?

**Exemple :**
```pascal
// Au lieu d'importer une grosse bibliothèque juste pour Base64
uses
  Base64;  // Unité standard FreePascal !

// Pas besoin de dépendance externe
```

### ✅ 2. Documenter Toutes les Dépendances

**Créez systématiquement :**
- `README.md` avec instructions d'installation
- `DEPENDENCIES.md` avec liste détaillée
- Script de vérification (`verify-deps`)

### ✅ 3. Versionner les Bibliothèques Incluses

**Créez VERSION.txt :**
```
Versions des bibliothèques incluses :
- SQLite : 3.36.0 (2021-06-18)
- OpenSSL : 1.1.1l (2021-08-24)
- zlib : 1.2.11

Build ID : 20241015-1830
```

### ✅ 4. Tester sur Système "Propre"

Testez votre application sur une machine virtuelle fraîche :
- Windows : VM Windows sans rien d'installé
- Linux : Container Docker ou VM Ubuntu minimale
- macOS : VM ou machine de test

**Objectif :** Vérifier que l'utilisateur lambda pourra lancer l'application.

### ✅ 5. Prévoir des Alternatives

Si une dépendance optionnelle manque, l'application doit continuer à fonctionner :

```pascal
if OpenSSLAvailable then
  Result := DownloadHTTPS(URL)
else
  Result := DownloadHTTP(URL);  // Fallback
```

### ✅ 6. Licence et Légalité

**Vérifiez les licences** des bibliothèques que vous distribuez !

**Exemples :**
- SQLite : Domaine public ✅ OK pour tout usage
- OpenSSL : Apache License 2.0 ⚠️ Mention requise
- Certaines bibliothèques : GPL ❌ Peut imposer que votre code soit GPL aussi

**Créez un fichier LICENSES.txt :**
```
Mon Application - Licences des dépendances

SQLite (domaine public)
  Version 3.36.0
  https://www.sqlite.org/copyright.html

OpenSSL (Apache License 2.0)
  Version 1.1.1l
  Copyright (c) 1998-2021 The OpenSSL Project
  https://www.openssl.org/source/license.html
```

---

## 12. Outils Utiles

### Gestionnaires de Dépendances

| Outil | Description |
|-------|-------------|
| **OPM** (Lazarus) | Packages Lazarus officiels |
| **FPCUpDeluxe** | Installation complète FPC/Lazarus + packages |
| **GetIt** (en développement) | Gestionnaire style npm pour Pascal |

### Outils de Vérification

| Outil | Plateforme | Usage |
|-------|-----------|-------|
| **Dependency Walker** | Windows | Analyse DLL (ancien mais fonctionnel) |
| **Dependencies** | Windows | Version moderne de Dependency Walker |
| **ldd** | Linux | Liste dépendances .so |
| **otool** | macOS | Liste dépendances .dylib |

### Créateurs d'Installeurs

| Outil | Plateforme | Type |
|-------|-----------|------|
| **Inno Setup** | Windows | Gratuit, populaire |
| **NSIS** | Windows | Gratuit, scriptable |
| **dpkg** | Linux | Packages .deb |
| **rpmbuild** | Linux | Packages .rpm |
| **pkgbuild** | macOS | Packages .pkg |

---

## 13. Cas Pratique : Application Complète

### Projet : "DataSync" - Synchronisation de Fichiers

**Dépendances :**
1. **SQLite** (requis) : Base locale
2. **OpenSSL** (requis) : HTTPS
3. **libcurl** (optionnel) : Protocoles avancés
4. **BGRABitmap** (package Lazarus) : Interface

**Structure :**
```
DataSync/
├── src/
│   ├── DataSync.lpr
│   ├── MainForm.pas
│   └── SyncEngine.pas
├── lib/
│   ├── windows/
│   │   ├── sqlite3.dll
│   │   ├── libssl-1_1.dll
│   │   └── libcrypto-1_1.dll
│   └── README.txt
├── bin/
├── docs/
│   ├── README.md
│   ├── DEPENDENCIES.md
│   └── LICENSES.txt
├── tools/
│   └── verify-deps.exe
└── DataSync.lpi
```

**Configuration du projet (.lpi) :**

```xml
<RequiredPackages Count="2">
  <Item1>
    <PackageName Value="LCL"/>
  </Item1>
  <Item2>
    <PackageName Value="BGRABitmapPack"/>
    <MinVersion Major="11" Minor="5" Valid="True"/>
  </Item2>
</RequiredPackages>
```

**Code de vérification au démarrage :**

```pascal
program DataSync;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, SysUtils, DynLibs,
  MainForm, SyncEngine;

{$R *.res}

function VerifyDependencies: Boolean;
var
  Missing: string;
begin
  Missing := '';

  // Vérifier SQLite
  if LoadLibrary('sqlite3.dll') = 0 then
    Missing := Missing + '- SQLite3' + sLineBreak;

  // Vérifier OpenSSL
  if LoadLibrary('libssl-1_1.dll') = 0 then
    Missing := Missing + '- OpenSSL' + sLineBreak;

  if Missing <> '' then
  begin
    MessageDlg('Dépendances manquantes',
      'Les bibliothèques suivantes sont manquantes :' + sLineBreak +
      sLineBreak + Missing + sLineBreak +
      'Veuillez consulter README.txt pour les instructions.',
      mtError, [mbOK], 0);
    Result := False;
  end
  else
    Result := True;
end;

begin
  if not VerifyDependencies then
    Halt(1);

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
```

**Script de build avec dépendances :**

```batch
@echo off
echo Construction de DataSync...

REM Compiler
lazbuild --build-mode=Release-Windows-64 DataSync.lpi

REM Créer structure de distribution
mkdir dist
mkdir dist\lib

REM Copier exécutable
copy bin\Release-Windows-64\DataSync.exe dist\

REM Copier dépendances
copy lib\windows\*.dll dist\lib\

REM Copier documentation
copy docs\*.txt dist\
copy docs\*.md dist\

REM Créer archive
7z a DataSync-Windows-v1.0.zip dist\*

echo Terminé ! Archive : DataSync-Windows-v1.0.zip
```

---

## 14. Résumé

La gestion des dépendances externes est cruciale pour :

1. **Fiabilité** : Votre application fonctionne chez l'utilisateur
2. **Maintenabilité** : Vous savez ce dont dépend votre projet
3. **Sécurité** : Mises à jour des bibliothèques vulnérables
4. **Portabilité** : Fonctionne sur différentes plateformes

**Checklist finale :**

- [ ] Identifier toutes les dépendances
- [ ] Décider de la stratégie (inclure ou dépendance système)
- [ ] Documenter dans README et DEPENDENCIES
- [ ] Créer un script de vérification
- [ ] Vérifier les licences
- [ ] Tester sur système propre
- [ ] Créer un installeur ou package adapté
- [ ] Prévoir des alternatives pour dépendances optionnelles

**Règle d'or :** Si vous devez expliquer à votre grand-mère comment installer votre application, c'est que la gestion des dépendances n'est pas assez simple. Simplifiez !

Dans le prochain chapitre, nous verrons comment effectuer des tests rigoureux sur les différentes plateformes pour s'assurer que tout fonctionne correctement.

⏭️ [Tests sur différentes plateformes](/19-developpement-multi-plateforme-pratique/08-tests-differentes-plateformes.md)
