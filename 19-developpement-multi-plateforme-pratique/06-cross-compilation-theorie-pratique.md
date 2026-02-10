🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.6 Cross-compilation : théorie et pratique

## Introduction

Imaginez que vous êtes un chef cuisinier français qui doit préparer un plat japonais traditionnel. Vous avez deux options :
1. **Aller au Japon** pour le préparer avec les ingrédients locaux
2. **Importer les ingrédients japonais en France** et préparer le plat dans votre propre cuisine

La cross-compilation, c'est la deuxième option ! Vous compilez un programme pour une plateforme différente (Linux, macOS) sans quitter votre système actuel (Windows).

**Avantage majeur :** Vous n'avez pas besoin d'une machine physique avec chaque système d'exploitation pour créer vos exécutables.

---

## 1. Qu'est-ce que la Cross-Compilation ?

### Définition Simple

La **cross-compilation** (compilation croisée) consiste à compiler un programme sur une plateforme A pour qu'il s'exécute sur une plateforme B.

**Exemples :**
- Compiler sous **Windows** pour créer un exécutable **Linux**
- Compiler sous **Linux** pour créer un exécutable **Windows**
- Compiler sur un PC **x86_64** pour créer un exécutable **ARM** (Raspberry Pi)

### Compilation Normale vs Cross-Compilation

**Compilation normale :**
```
Windows → Compilateur Windows → Exécutable Windows ✓  
Linux → Compilateur Linux → Exécutable Linux ✓  
```

**Cross-compilation :**
```
Windows → Cross-compilateur Linux → Exécutable Linux ✓  
Linux → Cross-compilateur Windows → Exécutable Windows ✓  
```

### Pourquoi C'est Utile ?

**Avantages :**
1. **Gain de temps** : pas besoin de changer de machine
2. **Économie** : pas besoin d'acheter plusieurs ordinateurs
3. **Efficacité** : développez et compilez depuis votre environnement habituel
4. **Automatisation** : compilez toutes les versions d'un coup

**Cas d'usage typique :**
Vous développez sous Windows mais devez livrer des versions Windows, Linux et macOS de votre application.

---

## 2. Comment Fonctionne la Cross-Compilation ?

### Les Composants Nécessaires

Pour faire de la cross-compilation, vous avez besoin de :

1. **Le compilateur croisé** (cross-compiler)
   - Version de FreePascal compilée pour générer du code pour une autre plateforme

2. **Les bibliothèques cibles** (binutils)
   - Lieur (linker), assembleur pour la plateforme cible

3. **Les en-têtes système** (headers)
   - Fichiers de définition pour la plateforme cible

**Analogie :** C'est comme avoir un traducteur (compilateur) qui connaît aussi la culture (bibliothèques) et les règles grammaticales (en-têtes) de la langue cible.

### Architecture du Processus

```
Votre code Pascal (.pas, .lpr)
         ↓
    FreePascal
         ↓
    Code assembleur pour plateforme cible
         ↓
    Assembleur cible
         ↓
    Fichiers objets (.o)
         ↓
    Lieur cible (linker)
         ↓
    Exécutable final pour plateforme cible
```

---

## 3. Limitations et Considérations

### Ce Qui Fonctionne Bien

✅ **Code pur FreePascal** : S'il n'utilise que les unités standard  
✅ **Applications console** : Plus simples à cross-compiler  
✅ **Applications Lazarus/LCL** : Fonctionnent généralement bien  
✅ **Logique métier** : Algorithmes, calculs, traitements de données

### Ce Qui Peut Poser Problème

❌ **Bibliothèques externes natives** : DLL Windows compilées, bibliothèques .so spécifiques  
❌ **Accès matériel direct** : Ports COM, GPIO, etc.  
❌ **API système spécifiques** : Registre Windows, fonctions kernel  
❌ **Interface graphique native** : Certains widgets peuvent avoir un rendu différent

### Point Important : Test Réel Indispensable

**La cross-compilation produit un exécutable, mais :**
- Vous ne pouvez pas le tester sur votre machine actuelle
- Vous **DEVEZ** le tester sur la plateforme cible
- Des bugs peuvent apparaître uniquement sur la plateforme cible

**Règle d'or :** Cross-compilez pour gagner du temps, mais testez toujours sur la vraie plateforme !

---

## 4. Installation des Outils de Cross-Compilation

### Option 1 : FPCUpDeluxe (Recommandé pour Débutants)

**FPCUpDeluxe** est un outil graphique qui installe automatiquement FreePascal, Lazarus ET les cross-compilateurs.

**Téléchargement :**
- Site : https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases
- Téléchargez la version pour votre OS actuel

**Installation des cross-compilateurs :**

1. Lancez FPCUpDeluxe
2. Allez dans l'onglet **"Cross"**
3. **Sélectionnez la plateforme cible** :
   - Pour compiler vers Linux : `x86_64-linux`
   - Pour compiler vers Windows : `x86_64-win64`
   - Pour compiler vers macOS : `x86_64-darwin`

4. Cliquez sur **"Install cross-compiler"**
5. Attendez le téléchargement et l'installation (peut prendre plusieurs minutes)
6. Vérifiez le message "Cross-compiler installed successfully"

**Avantages :**
- Automatique
- Gère toutes les dépendances
- Interface graphique simple
- Supporte de nombreuses plateformes

### Option 2 : Installation Manuelle (Avancé)

**Pour compiler de Windows vers Linux :**

1. **Télécharger le cross-compilateur**
   - Allez sur : https://gitlab.com/freepascal.org/fpc/binaries
   - Cherchez les binutils pour Linux

2. **Installer dans le bon répertoire**
   ```
   C:\freepascal\cross\
   ├── bin\
   │   └── x86_64-linux\
   │       ├── ld.exe
   │       ├── as.exe
   │       └── ...
   └── lib\
       └── x86_64-linux\
           └── (bibliothèques)
   ```

3. **Configurer fpc.cfg**
   Ajouter les chemins dans `C:\freepascal\bin\x86_64-win64\fpc.cfg`

**Cette méthode est complexe et source d'erreurs. Utilisez FPCUpDeluxe si possible !**

### Vérification de l'Installation

**Test en ligne de commande :**

```bash
# Windows (PowerShell ou cmd)
fpc -Px86_64 -Tlinux --version

# Si ça fonctionne, vous voyez :
# Free Pascal Compiler version 3.2.2
# Compiler for x86_64 processor
```

**Dans Lazarus :**
1. **Outils** → **Options...**
2. **Compilateur FreePascal** → **Compilateur**
3. Vérifiez que le chemin pointe vers le bon FPC
4. Testez avec un petit projet

---

## 5. Configuration dans Lazarus

### Étape 1 : Créer un Mode de Cross-Compilation

1. Ouvrez votre projet
2. **Projet** → **Options du projet**
3. **Options de compilation**
4. Créez un nouveau Build Mode : `Release-Linux-64` (si vous compilez depuis Windows)

### Étape 2 : Configurer la Cible

Dans le mode `Release-Linux-64` :

1. **Options de compilation** → **Cible**
2. **OS cible** : Sélectionnez `Linux`
3. **CPU cible** : Sélectionnez `x86_64`
4. **Famille CPU** : `x86_64` (généralement automatique)

### Étape 3 : Configurer les Chemins

**Chemins** → Section **Autres fichiers d'unités** :

```
Ajouter si nécessaire :
  $(ProjPath)/src
  $(ProjPath)/lib/common
  $(ProjPath)/lib/$(TargetOS)
```

**Répertoire de sortie :**
```
lib/$(TargetCPU)-$(TargetOS)
```

**Répertoire de sortie final :**
```
bin/$(BuildMode)
```

### Étape 4 : Options du Compilateur

**Options du compilateur** → **Autres** :

Pour Release :
```
Options supplémentaires :
  -O3         (Optimisation maximum)
  -XX         (Smart linking)
```

### Configuration Complète Exemple

**Mode : Release-Linux-64 (compilé depuis Windows)**

```
Cible :
  OS : Linux
  CPU : x86_64

Chemins :
  Autres unités : src;lib/common
  Sortie : lib/x86_64-linux
  Sortie finale : bin/Release-Linux-64

Options :
  Optimisation : Niveau 3
  Smart linking : Oui
  Strip symbols : Oui (optionnel)

Définitions personnalisées :
  RELEASE
```

---

## 6. Compilation Croisée en Pratique

### Exemple 1 : Application Console Simple

**Programme : HelloCross.lpr**

```pascal
program HelloCross;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('Hello from cross-compilation!');
  WriteLn('Compiled for: ', {$I %FPCTARGETOS%});  // {$I %VAR%} : macro compilateur, insère une info comme chaîne
  WriteLn('CPU: ', {$I %FPCTARGETCPU%});

  {$IFDEF WINDOWS}
  WriteLn('This is a Windows executable');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('This is a Linux executable');
  {$ENDIF}

  WriteLn('Press Enter to exit...');
  ReadLn;
end.
```

**Compilation depuis Windows vers Linux :**

1. Ouvrir le projet dans Lazarus
2. Sélectionner le mode **Release-Linux-64**
3. **Exécuter** → **Compiler** (ou F9)
4. L'exécutable est créé dans `bin/Release-Linux-64/HelloCross`

**Important :** Pas d'extension `.exe` ! C'est normal pour Linux.

**Test :**
- Copiez le fichier sur une machine Linux
- Rendez-le exécutable : `chmod +x HelloCross`
- Lancez : `./HelloCross`

### Exemple 2 : Application Lazarus Graphique

**Programme : CrossGUI.lpr**

```pascal
program CrossGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, MainForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

**MainForm.pas :**

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  Caption := 'Application Cross-Compilée';

  {$IFDEF WINDOWS}
  Label1.Caption := 'Exécutable Windows';
  {$ENDIF}

  {$IFDEF LINUX}
  Label1.Caption := 'Exécutable Linux';
  {$ENDIF}
end;

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ShowMessage('Bonjour depuis ' + {$I %FPCTARGETOS%} + ' !');
end;

end.
```

**Compilation :**
1. Créer les modes de compilation pour chaque plateforme
2. Compiler avec chaque mode
3. Tester sur les plateformes réelles

---

## 7. Cross-Compilation en Ligne de Commande

### Utilisation de fpc Directement

**Compiler depuis Windows vers Linux :**

```bash
fpc -Tlinux -Px86_64 -O3 MonProgramme.pas
```

**Options expliquées :**
- `-Tlinux` : Plateforme cible (Target) = Linux
- `-Px86_64` : Processeur = x86_64 (64 bits)
- `-O3` : Optimisation niveau 3

**Compiler depuis Linux vers Windows :**

```bash
fpc -Twin64 -Px86_64 -O3 MonProgramme.pas
```

### Utilisation de lazbuild

**Plus pratique pour les projets Lazarus :**

```bash
# Compiler le mode Linux depuis Windows
lazbuild --build-mode=Release-Linux-64 MonProjet.lpi

# Compiler le mode Windows depuis Linux
lazbuild --build-mode=Release-Windows-64 MonProjet.lpi
```

### Script de Cross-Compilation Complet

**build-all-platforms.bat (Windows) :**

```batch
@echo off
setlocal

set PROJECT=MonProjet.lpi  
set LAZBUILD=C:\lazarus\lazbuild.exe  

echo ========================================  
echo Cross-compilation multi-plateformes  
echo ========================================  
echo.  

echo [1/3] Compilation Windows 64 bits...
%LAZBUILD% --build-mode=Release-Windows-64 %PROJECT%
if errorlevel 1 goto error

echo.  
echo [2/3] Cross-compilation vers Linux 64 bits...  
%LAZBUILD% --build-mode=Release-Linux-64 %PROJECT%
if errorlevel 1 goto error

echo.  
echo [3/3] Cross-compilation vers macOS...  
%LAZBUILD% --build-mode=Release-Darwin-64 %PROJECT%
if errorlevel 1 goto error

echo.  
echo ========================================  
echo Compilation réussie pour toutes les plateformes !  
echo ========================================  
echo.  
echo Fichiers créés :  
dir /b bin\Release-Windows-64\*.exe  
dir /b bin\Release-Linux-64\  
dir /b bin\Release-Darwin-64\  
echo.  

goto end

:error
echo.  
echo ERREUR : La cross-compilation a échoué !  
exit /b 1  

:end
pause
```

---

## 8. Résolution des Problèmes Courants

### Problème 1 : "Cannot find -lc"

**Message d'erreur :**
```
Error: Cannot find -lc  
Fatal: There were 1 errors compiling module, stopping  
```

**Cause :** Les bibliothèques de la plateforme cible sont manquantes.

**Solution :**
1. Réinstallez le cross-compilateur avec FPCUpDeluxe
2. Ou téléchargez manuellement les binutils pour la cible
3. Vérifiez que les bibliothèques sont dans le bon répertoire

### Problème 2 : "Unknown target OS"

**Message d'erreur :**
```
Fatal: Unknown target OS: linux
```

**Cause :** FreePascal ne trouve pas le cross-compilateur.

**Solution :**
1. Vérifiez que le cross-compilateur est installé
2. Dans Lazarus : **Outils** → **Options** → vérifier le chemin FPC
3. Test en ligne de commande : `fpc -Tlinux --version`

### Problème 3 : L'Exécutable Linux ne Lance Pas

**Symptôme :** Le fichier existe mais "Permission denied"

**Cause :** L'exécutable n'a pas le bit d'exécution.

**Solution :**
```bash
chmod +x MonProgramme
./MonProgramme
```

### Problème 4 : "Undefined symbol" sur Linux

**Message :** Erreurs de symboles non définis au lancement

**Cause :** Dépendances manquantes sur la machine Linux cible.

**Solution :**
```bash
# Vérifier les dépendances
ldd MonProgramme

# Installer les bibliothèques manquantes (Ubuntu/Debian)
sudo apt install libgtk2.0-0 libglib2.0-0

# Pour les applications graphiques LCL
sudo apt install libgtk2.0-dev
```

### Problème 5 : Différences d'Apparence Graphique

**Symptôme :** L'interface graphique semble différente sous Linux

**Cause :** Thèmes et widgets natifs différents (GTK vs Windows)

**Solution :**
- C'est normal et attendu
- Testez sur la plateforme cible pour ajuster si nécessaire
- Utilisez des layouts flexibles (Anchors) plutôt que positions fixes

---

## 9. Bibliothèques Externes et Cross-Compilation

### Le Défi des Bibliothèques

**Problème :** Une DLL Windows ne fonctionne pas sur Linux (et vice-versa).

**Exemple :**
```pascal
// Votre code utilise une DLL
uses
  SQLite3;  // sqlite3.dll sous Windows

// Sur Linux, il faut libsqlite3.so
```

### Solution 1 : Bibliothèques Multi-Plateformes

Utilisez des bibliothèques qui ont des versions pour chaque plateforme :

**SQLite exemple :**

```pascal
unit DatabaseHelper;

{$mode objfpc}{$H+}

interface

uses
  SQLite3Conn, SQLDB;

// Votre code utilise les composants FreePascal
// qui gèrent automatiquement la bonne bibliothèque

implementation

end.
```

FreePascal charge automatiquement :
- `sqlite3.dll` sous Windows
- `libsqlite3.so` sous Linux
- `libsqlite3.dylib` sous macOS

### Solution 2 : Chargement Dynamique Conditionnel

```pascal
unit LibraryLoader;

{$mode objfpc}{$H+}

interface

uses
  DynLibs, SysUtils;

const
  {$IFDEF WINDOWS}
  LibraryName = 'mylib.dll';
  {$ENDIF}

  {$IFDEF LINUX}
  LibraryName = 'libmylib.so';
  {$ENDIF}

  {$IFDEF DARWIN}
  LibraryName = 'libmylib.dylib';
  {$ENDIF}

function LoadMyLibrary: Boolean;

implementation

var
  LibHandle: TLibHandle = 0;

function LoadMyLibrary: Boolean;  
begin  
  LibHandle := LoadLibrary(LibraryName);
  Result := LibHandle <> 0;

  if not Result then
    WriteLn('Erreur : Impossible de charger ', LibraryName);
end;

initialization
  if not LoadMyLibrary then
    raise Exception.Create('Bibliothèque requise non trouvée');

finalization
  if LibHandle <> 0 then
    UnloadLibrary(LibHandle);

end.
```

### Solution 3 : Inclure les Bibliothèques

**Structure de distribution :**

```
MonApplication/
├── MonApp.exe           (Windows)
├── MonApp               (Linux)
├── lib/
│   ├── windows/
│   │   └── sqlite3.dll
│   ├── linux/
│   │   └── libsqlite3.so
│   └── macos/
│       └── libsqlite3.dylib
└── README.txt
```

**Code pour chercher dans lib/ :**

```pascal
function GetLibraryPath: string;  
var  
  AppDir: string;
begin
  AppDir := ExtractFilePath(ParamStr(0));

  {$IFDEF WINDOWS}
  Result := AppDir + 'lib' + PathDelim + 'windows' + PathDelim + 'sqlite3.dll';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := AppDir + 'lib' + PathDelim + 'linux' + PathDelim + 'libsqlite3.so';
  {$ENDIF}
end;
```

---

## 10. Cas Pratiques Complets

### Cas 1 : Application de Gestion de Fichiers

**Objectif :** Créer un utilitaire qui fonctionne sous Windows et Linux

**Structure du projet :**

```pascal
program FileManager;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure ListFiles(const Directory: string);  
var  
  SearchRec: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(Directory);

  if FindFirst(Path + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        WriteLn(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

begin
  WriteLn('=== File Manager ===');
  WriteLn('Platform: ', {$I %FPCTARGETOS%});
  WriteLn;

  WriteLn('Files in current directory:');
  ListFiles(GetCurrentDir);

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
```

**Compilation :**

1. Mode `Release-Windows-64` : `FileManager.exe`
2. Mode `Release-Linux-64` : `FileManager` (pas d'extension)

**Résultat :** Le même code fonctionne sur les deux plateformes !

### Cas 2 : Serveur HTTP Simple Multi-Plateforme

**Utilisation de fphttpapp (inclus dans FreePascal) :**

```pascal
program SimpleHTTPServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, fphttpapp;

type
  TMyHTTPApp = class(THTTPApplication)
  protected
    procedure HandleRequest(Sender: TObject); override;
  end;

procedure TMyHTTPApp.HandleRequest(Sender: TObject);  
begin  
  Response.Content := '<html><body>' +
    '<h1>Serveur Multi-Plateforme</h1>' +
    '<p>Plateforme: ' + {$I %FPCTARGETOS%} + '</p>' +
    '<p>CPU: ' + {$I %FPCTARGETCPU%} + '</p>' +
    '</body></html>';
end;

begin
  Application.Title := 'Simple HTTP Server';
  Application.Port := 8080;
  Application.Threaded := True;
  Application.Initialize;

  WriteLn('Serveur démarré sur http://localhost:8080');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');

  Application.Run;
end.
```

**Test :**
- Compilez pour Windows et Linux
- Sur chaque plateforme, lancez et accédez à http://localhost:8080

---

## 11. Automatisation de la Cross-Compilation

### Script CI/CD (Intégration Continue)

**Exemple avec GitHub Actions :**

**Fichier : .github/workflows/build.yml**

```yaml
name: Build Multi-Platform

on: [push, pull_request]

jobs:
  build-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Lazarus
        uses: gcarreno/setup-lazarus@v3
        with:
          lazarus-version: stable
      - name: Build
        run: lazbuild --build-mode=Release-Windows-64 MonProjet.lpi
      - name: Upload
        uses: actions/upload-artifact@v2
        with:
          name: windows-build
          path: bin/Release-Windows-64/

  build-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Lazarus
        uses: gcarreno/setup-lazarus@v3
        with:
          lazarus-version: stable
      - name: Build
        run: lazbuild --build-mode=Release-Linux-64 MonProjet.lpi
      - name: Upload
        uses: actions/upload-artifact@v2
        with:
          name: linux-build
          path: bin/Release-Linux-64/
```

**Résultat :** À chaque commit, les versions Windows et Linux sont compilées automatiquement !

---

## 12. Bonnes Pratiques

### ✅ 1. Testez sur la Vraie Plateforme

**Ne JAMAIS livrer sans tester sur la plateforme cible !**

La cross-compilation peut réussir, mais :
- Des bugs peuvent apparaître uniquement sur la cible
- L'interface peut être différente
- Des bibliothèques peuvent manquer

**Solution :** Utilisez des machines virtuelles (VirtualBox, VMware) ou des vrais ordinateurs.

### ✅ 2. Utilisez des Bibliothèques Portables

**Privilégiez :**
- Les unités standard FreePascal (SysUtils, Classes)
- Les composants LCL (portables par design)
- Les bibliothèques multi-plateformes (SQLite, ZLib)

**Évitez :**
- Les API système spécifiques sans alternative
- Les bibliothèques propriétaires mono-plateforme

### ✅ 3. Gérez les Dépendances

**Documentez les prérequis sur chaque plateforme :**

**README.md :**

````markdown
## Dépendances

### Windows
- Aucune dépendance externe

### Linux
```bash
sudo apt install libgtk2.0-0 libglib2.0-0
```

### macOS
```bash
brew install gtk+
```
````

### ✅ 4. Structure de Projet Organisée

```
MonProjet/
├── src/              # Code source
├── lib/              # Bibliothèques externes
│   ├── common/
│   ├── windows/
│   └── linux/
├── bin/              # Exécutables finaux
│   ├── Release-Windows-64/
│   └── Release-Linux-64/
├── resources/        # Ressources
└── tests/            # Tests
```

### ✅ 5. Versionner Intelligemment

**.gitignore :**
```gitignore
# Fichiers compilés
bin/  
lib/  
*.exe
*.dll
*.so
*.o
*.ppu
*.compiled
*.lps

# Garder uniquement
!lib/common/
!lib/windows/*.dll
!lib/linux/*.so
```

### ✅ 6. Scripts de Build Reproductibles

**build.sh (Linux/macOS) :**
```bash
#!/bin/bash
set -e  # Arrêter en cas d'erreur

echo "=== Building for all platforms ==="

# Nettoyer
rm -rf bin/ lib/

# Windows
lazbuild --build-mode=Release-Windows-64 MonProjet.lpi

# Linux
lazbuild --build-mode=Release-Linux-64 MonProjet.lpi

# macOS
lazbuild --build-mode=Release-Darwin-64 MonProjet.lpi

echo "=== Build completed ==="  
ls -lh bin/*/  
```

---

## 13. Tableau Récapitulatif

| Aspect | Windows → Linux | Linux → Windows |
|--------|----------------|-----------------|
| **Difficulté** | Moyenne | Moyenne |
| **Outil recommandé** | FPCUpDeluxe | FPCUpDeluxe |
| **Applications console** | ✅ Facile | ✅ Facile |
| **Applications GUI** | ✅ Fonctionne bien | ✅ Fonctionne bien |
| **Principale limite** | Bibliothèques externes | Bibliothèques externes |
| **Test nécessaire** | ⚠️ Obligatoire | ⚠️ Obligatoire |

---

## 14. Pièges à Éviter

### ❌ Piège 1 : Ne Pas Tester

```
"J'ai cross-compilé, ça doit marcher !"
→ L'exécutable Linux ne démarre pas sur la machine cible
```

**Solution :** TOUJOURS tester sur la vraie plateforme.

### ❌ Piège 2 : Oublier les Dépendances

```
Compilation réussie sous Windows  
Transfert sur Linux  
→ "Error loading shared library: libgtk-x11-2.0.so.0"
```

**Solution :** Documentez et incluez les dépendances.

### ❌ Piège 3 : Chemins Codés en Dur

```pascal
// Code Windows
CheminConfig := 'C:\Users\MonApp\config.ini';  // ❌
// Ne fonctionnera PAS sous Linux !
```

**Solution :** Utilisez GetAppConfigDir, PathDelim, etc.

### ❌ Piège 4 : Supposer l'Architecture

```pascal
// "Je compile pour Linux, donc 64 bits"
// → Erreur si l'utilisateur a un système 32 bits
```

**Solution :** Créez des versions 32 et 64 bits si nécessaire.

---

## Conclusion

La cross-compilation avec FreePascal et Lazarus est un outil puissant qui vous permet de :

1. **Développer sur une plateforme**, compiler pour toutes
2. **Gagner du temps** en automatisant les builds
3. **Économiser des ressources** (pas besoin de multiples machines)
4. **Distribuer facilement** des versions multi-plateformes

**Points clés à retenir :**
- ✅ Utilisez **FPCUpDeluxe** pour installer les cross-compilateurs
- ✅ Configurez des **Build Modes** séparés pour chaque cible
- ✅ Utilisez du **code portable** (SysUtils, PathDelim, etc.)
- ✅ **Testez TOUJOURS** sur la plateforme réelle
- ✅ Documentez les **dépendances** pour chaque plateforme

**Limitation principale :** La cross-compilation produit l'exécutable, mais ne remplace pas les tests réels sur chaque plateforme.

Dans le chapitre suivant, nous verrons comment gérer les dépendances externes et créer des packages de distribution pour chaque plateforme.

⏭️ [Gestion des dépendances externes](/19-developpement-multi-plateforme-pratique/07-gestion-dependances-externes.md)
