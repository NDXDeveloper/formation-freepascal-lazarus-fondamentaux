🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.9 Empaquetage et distribution

## Introduction

Imaginez que vous avez cuisiné un délicieux plat que vous voulez offrir à vos amis. Vous ne le donnez pas simplement dans la casserole ! Vous le présentez joliment dans une belle assiette, avec une étiquette indiquant les ingrédients, et peut-être même un mode de réchauffage.

L'**empaquetage** (ou packaging) de votre application, c'est exactement ça : présenter votre logiciel de manière professionnelle, facile à installer et agréable à utiliser. La **distribution**, c'est choisir comment le livrer à vos utilisateurs.

Dans ce chapitre, nous allons apprendre à créer des packages professionnels pour Windows, Linux et macOS, et à les distribuer efficacement.

---

## 1. Types de Distribution

### Distribution par Archive Simple

**Format :** Fichier ZIP (Windows) ou TAR.GZ (Linux/macOS)

**Contenu :**
```
MonApp-v1.0-Windows.zip
├── MonApp.exe
├── lib/
│   ├── sqlite3.dll
│   └── libssl.dll
├── README.txt
├── LICENSE.txt
└── CHANGELOG.txt
```

**Avantages :**
- ✅ Simple à créer
- ✅ Portable (pas d'installation)
- ✅ Fonctionne sur clé USB
- ✅ Pas de droits administrateur nécessaires

**Inconvénients :**
- ❌ Pas d'icône dans le menu démarrer
- ❌ Pas de désinstalleur
- ❌ L'utilisateur doit extraire manuellement
- ❌ Moins professionnel

**Quand l'utiliser :**
- Applications portables
- Outils pour développeurs
- Versions "beta" ou tests

### Distribution par Installeur

**Windows :** Setup.exe  
**Linux :** .deb, .rpm, .pkg.tar.zst  
**macOS :** .dmg, .pkg  

**Avantages :**
- ✅ Installation guidée
- ✅ Icônes et raccourcis créés
- ✅ Désinstallation propre
- ✅ Professionnel
- ✅ Peut gérer les dépendances

**Inconvénients :**
- ❌ Plus complexe à créer
- ❌ Nécessite souvent droits admin
- ❌ Spécifique à chaque plateforme

**Quand l'utiliser :**
- Applications commerciales
- Logiciels grand public
- Versions finales/release

### Distribution par Store

**Windows Store**, **Snap Store** (Linux), **Mac App Store**

**Avantages :**
- ✅ Visibilité accrue
- ✅ Mises à jour automatiques
- ✅ Paiement intégré
- ✅ Confiance des utilisateurs

**Inconvénients :**
- ❌ Processus de validation
- ❌ Commission (30% généralement)
- ❌ Restrictions techniques
- ❌ Délais de publication

**Quand l'utiliser :**
- Applications grand public
- Applications payantes
- Si vous voulez la visibilité des stores

---

## 2. Empaquetage Windows

### Archive ZIP Portable

**Script de création (create-zip.bat) :**

```batch
@echo off
echo ========================================
echo Création Archive Portable Windows
echo ========================================
echo.

set VERSION=1.0.0
set APPNAME=MonApp
set BUILDDIR=bin\Release-Windows-64
set DISTDIR=dist\%APPNAME%-v%VERSION%-Windows-Portable

REM Nettoyer le répertoire de distribution
if exist dist rmdir /s /q dist
mkdir dist
mkdir "%DISTDIR%"
mkdir "%DISTDIR%\lib"

REM Copier l'exécutable
echo Copie de l'exécutable...
copy "%BUILDDIR%\%APPNAME%.exe" "%DISTDIR%\" >nul

REM Copier les bibliothèques
echo Copie des bibliothèques...
copy "lib\windows\*.dll" "%DISTDIR%\lib\" >nul

REM Copier la documentation
echo Copie de la documentation...
copy "README.txt" "%DISTDIR%\" >nul
copy "LICENSE.txt" "%DISTDIR%\" >nul
copy "CHANGELOG.txt" "%DISTDIR%\" >nul

REM Créer l'archive
echo Création de l'archive ZIP...
cd dist
7z a -tzip "%APPNAME%-v%VERSION%-Windows-Portable.zip" "%APPNAME%-v%VERSION%-Windows-Portable\*" >nul
cd ..

echo.
echo ========================================
echo Archive créée avec succès !
echo Fichier : dist\%APPNAME%-v%VERSION%-Windows-Portable.zip
echo ========================================
pause
```

### Installeur avec Inno Setup

**Inno Setup** est le créateur d'installeurs le plus populaire pour Windows.

**Installation :**
1. Télécharger depuis https://jrsoftware.org/isdl.php
2. Installer Inno Setup
3. Lancer Inno Setup Compiler

**Script d'installation (setup.iss) :**

```ini
; Script Inno Setup pour MonApp
; Documentation : https://jrsoftware.org/ishelp/

[Setup]
; Informations de base
AppName=Mon Application
AppVersion=1.0.0
AppPublisher=Votre Nom ou Société
AppPublisherURL=https://votre-site.com
AppSupportURL=https://votre-site.com/support
AppUpdatesURL=https://votre-site.com/downloads

; Répertoires d'installation
DefaultDirName={autopf}\MonApp
DefaultGroupName=Mon Application
DisableProgramGroupPage=yes

; Sortie
OutputDir=dist
OutputBaseFilename=MonApp-Setup-v1.0.0

; Compression
Compression=lzma2
SolidCompression=yes

; Interface
WizardStyle=modern
SetupIconFile=resources\app.ico

; Privilèges
PrivilegesRequired=admin
PrivilegesRequiredOverridesAllowed=dialog

; Architecture
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Files]
; Exécutable principal
Source: "bin\Release-Windows-64\MonApp.exe"; DestDir: "{app}"; Flags: ignoreversion

; Bibliothèques
Source: "lib\windows\*.dll"; DestDir: "{app}\lib"; Flags: ignoreversion recursesubdirs

; Documentation
Source: "README.txt"; DestDir: "{app}"; Flags: isreadme
Source: "LICENSE.txt"; DestDir: "{app}"
Source: "CHANGELOG.txt"; DestDir: "{app}"

; Fichiers de données (optionnel)
Source: "data\*"; DestDir: "{app}\data"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
; Menu Démarrer
Name: "{group}\Mon Application"; Filename: "{app}\MonApp.exe"
Name: "{group}\Lire le README"; Filename: "{app}\README.txt"
Name: "{group}\Désinstaller Mon Application"; Filename: "{uninstallexe}"

; Bureau (si demandé)
Name: "{autodesktop}\Mon Application"; Filename: "{app}\MonApp.exe"; Tasks: desktopicon

[Run]
; Lancer l'application après installation
Filename: "{app}\MonApp.exe"; Description: "{cm:LaunchProgram,Mon Application}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
; Supprimer les fichiers créés par l'application
Type: filesandordirs; Name: "{app}\data"
Type: filesandordirs; Name: "{userappdata}\MonApp"

[Code]
// Code Pascal personnalisé pour l'installeur

function InitializeSetup(): Boolean;
begin
  Result := True;
  // Vérifications avant installation
  if not RegKeyExists(HKLM, 'SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full') then
  begin
    MsgBox('Cette application nécessite .NET Framework 4.0 ou supérieur.', mbError, MB_OK);
    Result := False;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
  begin
    // Actions après installation
    // Par exemple : créer des fichiers de configuration
  end;
end;
```

**Compiler l'installeur :**
1. Ouvrir `setup.iss` dans Inno Setup Compiler
2. Menu **Build** → **Compile**
3. L'installeur `MonApp-Setup-v1.0.0.exe` est créé dans `dist/`

**Fonctionnalités avancées d'Inno Setup :**

```ini
[Setup]
; Vérifier la version de Windows
MinVersion=10.0

; Désinstaller l'ancienne version automatiquement
AppId={{GUID-UNIQUE-ICI}}

; Mode silencieux supporté
SilentInstall=yes

[Registry]
; Créer des entrées dans le registre
Root: HKLM; Subkey: "Software\MonApp"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\MonApp\Settings"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"

[InstallDelete]
; Supprimer des fichiers de versions précédentes
Type: filesandordirs; Name: "{app}\old_data"
```

### Installeur avec NSIS (Alternative)

**NSIS** (Nullsoft Scriptable Install System) est une alternative gratuite et puissante.

**Exemple de script NSIS (installer.nsi) :**

```nsis
; Script NSIS pour MonApp

;--------------------------------
; Includes

!include "MUI2.nsh"
!include "x64.nsh"

;--------------------------------
; Configuration générale

Name "Mon Application"
OutFile "dist\MonApp-Setup-v1.0.0.exe"
InstallDir "$PROGRAMFILES64\MonApp"
InstallDirRegKey HKLM "Software\MonApp" "InstallPath"
RequestExecutionLevel admin

;--------------------------------
; Interface

!define MUI_ABORTWARNING
!define MUI_ICON "resources\app.ico"
!define MUI_UNICON "resources\app.ico"

;--------------------------------
; Pages

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "LICENSE.txt"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
; Langues

!insertmacro MUI_LANGUAGE "French"

;--------------------------------
; Section Installation

Section "Installation" SecInstall

  SetOutPath "$INSTDIR"

  ; Fichiers
  File "bin\Release-Windows-64\MonApp.exe"
  File /r "lib\windows\*.dll"
  File "README.txt"
  File "LICENSE.txt"

  ; Créer les raccourcis
  CreateDirectory "$SMPROGRAMS\Mon Application"
  CreateShortcut "$SMPROGRAMS\Mon Application\Mon Application.lnk" "$INSTDIR\MonApp.exe"
  CreateShortcut "$SMPROGRAMS\Mon Application\Désinstaller.lnk" "$INSTDIR\Uninstall.exe"
  CreateShortcut "$DESKTOP\Mon Application.lnk" "$INSTDIR\MonApp.exe"

  ; Registre
  WriteRegStr HKLM "Software\MonApp" "InstallPath" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\MonApp" "DisplayName" "Mon Application"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\MonApp" "UninstallString" "$INSTDIR\Uninstall.exe"

  ; Créer le désinstalleur
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

;--------------------------------
; Section Désinstallation

Section "Uninstall"

  ; Supprimer les fichiers
  Delete "$INSTDIR\MonApp.exe"
  Delete "$INSTDIR\*.dll"
  Delete "$INSTDIR\README.txt"
  Delete "$INSTDIR\LICENSE.txt"
  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  ; Supprimer les raccourcis
  Delete "$SMPROGRAMS\Mon Application\*.*"
  Delete "$DESKTOP\Mon Application.lnk"
  RMDir "$SMPROGRAMS\Mon Application"

  ; Supprimer les entrées registre
  DeleteRegKey HKLM "Software\MonApp"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\MonApp"

SectionEnd
```

---

## 3. Empaquetage Linux

### Archive TAR.GZ Portable

**Script de création (create-tarball.sh) :**

```bash
#!/bin/bash

VERSION="1.0.0"
APPNAME="monapp"
BUILDDIR="bin/Release-Linux-64"
DISTDIR="dist/${APPNAME}-v${VERSION}-Linux-x64"

echo "========================================"
echo "Création Archive Portable Linux"
echo "========================================"
echo

# Nettoyer et créer les répertoires
rm -rf dist
mkdir -p "$DISTDIR"
mkdir -p "$DISTDIR/lib"

# Copier l'exécutable
echo "Copie de l'exécutable..."
cp "$BUILDDIR/$APPNAME" "$DISTDIR/"
chmod +x "$DISTDIR/$APPNAME"

# Copier les bibliothèques (si incluses)
if [ -d "lib/linux" ]; then
    echo "Copie des bibliothèques..."
    cp lib/linux/*.so* "$DISTDIR/lib/" 2>/dev/null || true
fi

# Copier la documentation
echo "Copie de la documentation..."
cp README.txt "$DISTDIR/"
cp LICENSE.txt "$DISTDIR/"
cp CHANGELOG.txt "$DISTDIR/"

# Créer un script de lancement
echo "Création du script de lancement..."
cat > "$DISTDIR/run.sh" << 'EOF'
#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export LD_LIBRARY_PATH="$SCRIPT_DIR/lib:$LD_LIBRARY_PATH"
cd "$SCRIPT_DIR"
exec ./monapp "$@"
EOF
chmod +x "$DISTDIR/run.sh"

# Créer l'archive
echo "Création de l'archive TAR.GZ..."
cd dist
tar -czf "${APPNAME}-v${VERSION}-Linux-x64.tar.gz" "${APPNAME}-v${VERSION}-Linux-x64"
cd ..

echo
echo "========================================"
echo "Archive créée avec succès !"
echo "Fichier : dist/${APPNAME}-v${VERSION}-Linux-x64.tar.gz"
echo "========================================"
```

### Package Debian (.deb)

**Structure du projet :**

```
monapp_1.0.0-1/
├── DEBIAN/
│   ├── control
│   ├── postinst
│   ├── prerm
│   └── copyright
└── usr/
    ├── bin/
    │   └── monapp
    ├── share/
    │   ├── applications/
    │   │   └── monapp.desktop
    │   ├── pixmaps/
    │   │   └── monapp.png
    │   └── doc/
    │       └── monapp/
    │           ├── README
    │           ├── LICENSE
    │           └── changelog.gz
    └── lib/
        └── monapp/
            └── (bibliothèques si nécessaire)
```

**Fichier DEBIAN/control :**

```
Package: monapp
Version: 1.0.0-1
Section: utils
Priority: optional
Architecture: amd64
Depends: libsqlite3-0 (>= 3.30), libgtk2.0-0, libssl1.1
Maintainer: Votre Nom <email@example.com>
Homepage: https://votre-site.com
Description: Mon Application - Description courte
 Description plus détaillée de votre application.
 Elle peut s'étendre sur plusieurs lignes.
 .
 Fonctionnalités principales :
  - Gestion de fichiers
  - Base de données intégrée
  - Interface graphique moderne
```

**Fichier DEBIAN/postinst :**

```bash
#!/bin/bash
set -e

# Actions après installation

# Créer le répertoire de configuration
mkdir -p /etc/monapp

# Définir les permissions
chmod 755 /usr/bin/monapp

echo "Mon Application installée avec succès !"

exit 0
```

**Fichier DEBIAN/prerm :**

```bash
#!/bin/bash
set -e

# Actions avant désinstallation

# Arrêter le service si en cours d'exécution
if pgrep -x "monapp" > /dev/null; then
    pkill -x "monapp"
fi

exit 0
```

**Fichier usr/share/applications/monapp.desktop :**

```ini
[Desktop Entry]
Version=1.0
Type=Application
Name=Mon Application
Name[fr]=Mon Application
Comment=Description de l'application
Comment[fr]=Description de l'application
Exec=/usr/bin/monapp %F
Icon=monapp
Terminal=false
Categories=Utility;Office;
Keywords=gestion;fichiers;
```

**Script de création du package (create-deb.sh) :**

```bash
#!/bin/bash

VERSION="1.0.0"
REVISION="1"
APPNAME="monapp"
PKGDIR="${APPNAME}_${VERSION}-${REVISION}"

echo "Création du package Debian..."

# Nettoyer
rm -rf "$PKGDIR" *.deb

# Créer la structure
mkdir -p "$PKGDIR/DEBIAN"
mkdir -p "$PKGDIR/usr/bin"
mkdir -p "$PKGDIR/usr/share/applications"
mkdir -p "$PKGDIR/usr/share/pixmaps"
mkdir -p "$PKGDIR/usr/share/doc/$APPNAME"

# Copier les fichiers
cp "bin/Release-Linux-64/$APPNAME" "$PKGDIR/usr/bin/"
chmod 755 "$PKGDIR/usr/bin/$APPNAME"

cp "resources/$APPNAME.desktop" "$PKGDIR/usr/share/applications/"
cp "resources/$APPNAME.png" "$PKGDIR/usr/share/pixmaps/"

cp README.txt "$PKGDIR/usr/share/doc/$APPNAME/README"
cp LICENSE.txt "$PKGDIR/usr/share/doc/$APPNAME/copyright"

# Compresser le changelog
gzip -9 -c CHANGELOG.txt > "$PKGDIR/usr/share/doc/$APPNAME/changelog.gz"

# Créer les fichiers DEBIAN
cat > "$PKGDIR/DEBIAN/control" << EOF
Package: $APPNAME
Version: ${VERSION}-${REVISION}
Section: utils
Priority: optional
Architecture: amd64
Depends: libsqlite3-0 (>= 3.30), libgtk2.0-0, libssl1.1
Maintainer: Votre Nom <email@example.com>
Description: Mon Application
 Description détaillée de votre application
EOF

cat > "$PKGDIR/DEBIAN/postinst" << 'EOF'
#!/bin/bash
set -e
echo "Mon Application installée avec succès !"
exit 0
EOF
chmod 755 "$PKGDIR/DEBIAN/postinst"

# Construire le package
dpkg-deb --build "$PKGDIR"

echo "Package créé : ${PKGDIR}.deb"
```

**Installation du package :**

```bash
# Installer
sudo dpkg -i monapp_1.0.0-1.deb

# Si des dépendances manquent
sudo apt-get install -f

# Désinstaller
sudo apt-get remove monapp
```

### AppImage (Portable Linux Universel)

**AppImage** est un format portable qui fonctionne sur toutes les distributions Linux.

**Avantages :**
- ✅ Un fichier unique exécutable
- ✅ Fonctionne sur toutes les distributions
- ✅ Pas besoin de droits admin
- ✅ Pas d'installation nécessaire

**Structure AppImage :**

```
MonApp.AppDir/
├── AppRun (script de lancement)
├── monapp.desktop
├── monapp.png
└── usr/
    ├── bin/
    │   └── monapp
    └── lib/
        └── (bibliothèques)
```

**Fichier AppRun :**

```bash
#!/bin/bash
APPDIR="$(dirname "$(readlink -f "$0")")"
export LD_LIBRARY_PATH="$APPDIR/usr/lib:$LD_LIBRARY_PATH"
exec "$APPDIR/usr/bin/monapp" "$@"
```

**Créer l'AppImage :**

```bash
#!/bin/bash

# Télécharger appimagetool
wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage
chmod +x appimagetool-x86_64.AppImage

# Créer la structure
mkdir -p MonApp.AppDir/usr/bin
mkdir -p MonApp.AppDir/usr/lib

# Copier les fichiers
cp bin/Release-Linux-64/monapp MonApp.AppDir/usr/bin/
cp lib/linux/*.so* MonApp.AppDir/usr/lib/ 2>/dev/null || true

# Copier les métadonnées
cp resources/monapp.desktop MonApp.AppDir/
cp resources/monapp.png MonApp.AppDir/

# Créer AppRun
cat > MonApp.AppDir/AppRun << 'EOF'
#!/bin/bash
APPDIR="$(dirname "$(readlink -f "$0")")"
export LD_LIBRARY_PATH="$APPDIR/usr/lib:$LD_LIBRARY_PATH"
exec "$APPDIR/usr/bin/monapp" "$@"
EOF
chmod +x MonApp.AppDir/AppRun

# Construire l'AppImage
./appimagetool-x86_64.AppImage MonApp.AppDir MonApp-x86_64.AppImage

echo "AppImage créé : MonApp-x86_64.AppImage"
```

---

## 4. Empaquetage macOS

### Application Bundle (.app)

**Structure d'un bundle macOS :**

```
MonApp.app/
└── Contents/
    ├── Info.plist
    ├── MacOS/
    │   └── monapp (exécutable)
    ├── Resources/
    │   ├── monapp.icns (icône)
    │   ├── README.txt
    │   └── LICENSE.txt
    └── Frameworks/
        └── (bibliothèques .dylib)
```

**Fichier Info.plist :**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>Mon Application</string>

    <key>CFBundleDisplayName</key>
    <string>Mon Application</string>

    <key>CFBundleIdentifier</key>
    <string>com.votresociete.monapp</string>

    <key>CFBundleVersion</key>
    <string>1.0.0</string>

    <key>CFBundleShortVersionString</key>
    <string>1.0.0</string>

    <key>CFBundleExecutable</key>
    <string>monapp</string>

    <key>CFBundleIconFile</key>
    <string>monapp.icns</string>

    <key>CFBundlePackageType</key>
    <string>APPL</string>

    <key>LSMinimumSystemVersion</key>
    <string>10.13</string>

    <key>NSHighResolutionCapable</key>
    <true/>

    <key>CFBundleDocumentTypes</key>
    <array>
        <dict>
            <key>CFBundleTypeName</key>
            <string>Fichier MonApp</string>
            <key>CFBundleTypeExtensions</key>
            <array>
                <string>monapp</string>
            </array>
            <key>CFBundleTypeRole</key>
            <string>Editor</string>
        </dict>
    </array>
</dict>
</plist>
```

**Script de création du bundle (create-app.sh) :**

```bash
#!/bin/bash

APPNAME="MonApp"
VERSION="1.0.0"
BUNDLE="${APPNAME}.app"

echo "Création du bundle macOS..."

# Nettoyer
rm -rf "$BUNDLE"

# Créer la structure
mkdir -p "$BUNDLE/Contents/MacOS"
mkdir -p "$BUNDLE/Contents/Resources"
mkdir -p "$BUNDLE/Contents/Frameworks"

# Copier l'exécutable
cp "bin/Release-Darwin-64/monapp" "$BUNDLE/Contents/MacOS/"
chmod +x "$BUNDLE/Contents/MacOS/monapp"

# Copier l'icône
cp "resources/monapp.icns" "$BUNDLE/Contents/Resources/"

# Copier la documentation
cp README.txt "$BUNDLE/Contents/Resources/"
cp LICENSE.txt "$BUNDLE/Contents/Resources/"

# Copier les bibliothèques
cp lib/macos/*.dylib "$BUNDLE/Contents/Frameworks/" 2>/dev/null || true

# Créer Info.plist
cat > "$BUNDLE/Contents/Info.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>$APPNAME</string>
    <key>CFBundleDisplayName</key>
    <string>$APPNAME</string>
    <key>CFBundleIdentifier</key>
    <string>com.example.$APPNAME</string>
    <key>CFBundleVersion</key>
    <string>$VERSION</string>
    <key>CFBundleExecutable</key>
    <string>monapp</string>
    <key>CFBundleIconFile</key>
    <string>monapp.icns</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.13</string>
</dict>
</plist>
EOF

echo "Bundle créé : $BUNDLE"
```

### Image Disque (.dmg)

**Créer un DMG attrayant :**

```bash
#!/bin/bash

APPNAME="MonApp"
VERSION="1.0.0"
DMG_NAME="${APPNAME}-${VERSION}.dmg"
VOLUME_NAME="${APPNAME} ${VERSION}"
SOURCE_DIR="dist"

echo "Création de l'image disque DMG..."

# Créer un répertoire temporaire
mkdir -p "$SOURCE_DIR"
cp -R "${APPNAME}.app" "$SOURCE_DIR/"

# Créer un lien vers Applications
ln -s /Applications "$SOURCE_DIR/Applications"

# Copier un fichier de fond (optionnel)
mkdir -p "$SOURCE_DIR/.background"
cp resources/dmg-background.png "$SOURCE_DIR/.background/"

# Créer le DMG temporaire
hdiutil create -volname "$VOLUME_NAME" \
               -srcfolder "$SOURCE_DIR" \
               -ov -format UDRW \
               -size 100m \
               temp.dmg

# Monter le DMG
device=$(hdiutil attach -readwrite -noverify temp.dmg | grep "/Volumes/$VOLUME_NAME" | awk '{print $1}')

# Personnaliser l'apparence
osascript << EOF
tell application "Finder"
    tell disk "$VOLUME_NAME"
        open
        set current view of container window to icon view
        set toolbar visible of container window to false
        set statusbar visible of container window to false
        set the bounds of container window to {100, 100, 800, 500}
        set viewOptions to the icon view options of container window
        set arrangement of viewOptions to not arranged
        set icon size of viewOptions to 128
        set position of item "${APPNAME}.app" of container window to {150, 200}
        set position of item "Applications" of container window to {550, 200}
        close
        open
        update without registering applications
        delay 2
    end tell
end tell
EOF

# Démonter
hdiutil detach "$device"

# Convertir en DMG compressé final
hdiutil convert temp.dmg -format UDZO -o "$DMG_NAME"

# Nettoyer
rm -f temp.dmg
rm -rf "$SOURCE_DIR"

echo "DMG créé : $DMG_NAME"
```

---

## 5. Signatures et Sécurité

### Pourquoi Signer Vos Applications ?

**Avantages :**
- ✅ Prouve l'authenticité
- ✅ Évite les avertissements de sécurité
- ✅ Requis pour les stores
- ✅ Confiance des utilisateurs

### Signature Windows (Code Signing)

**Obtenir un certificat :**
1. Acheter un certificat auprès d'une autorité (DigiCert, Sectigo, etc.)
2. Ou utiliser un certificat auto-signé (tests uniquement)

**Signer avec signtool.exe :**

```batch
REM Signer l'exécutable
signtool sign /f "certificat.pfx" /p "motdepasse" /t "http://timestamp.digicert.com" "MonApp.exe"

REM Vérifier la signature
signtool verify /pa "MonApp.exe"
```

**Signer l'installeur Inno Setup :**

```ini
[Setup]
SignTool=signtool sign /f "certificat.pfx" /p "motdepasse" /t "http://timestamp.digicert.com" $f
SignedUninstaller=yes
```

### Signature macOS (codesign)

**Requis pour éviter les avertissements Gatekeeper.**

```bash
# Signer l'application
codesign --force --deep --sign "Developer ID Application: Votre Nom" MonApp.app

# Vérifier la signature
codesign --verify --deep --verbose MonApp.app

# Notariser (obligatoire depuis macOS 10.15)
xcrun altool --notarize-app \
             --primary-bundle-id "com.example.monapp" \
             --username "votre@email.com" \
             --password "mot-de-passe-app-specific" \
             --file MonApp-1.0.0.dmg
```

### Checksums (Hash de Vérification)

**Générer des checksums pour vérifier l'intégrité :**

**Linux/macOS :**
```bash
# SHA256
sha256sum MonApp-1.0.0-Linux.tar.gz > MonApp-1.0.0-Linux.tar.gz.sha256

# MD5 (moins sécurisé, mais encore utilisé)
md5sum MonApp-1.0.0-Linux.tar.gz > MonApp-1.0.0-Linux.tar.gz.md5
```

**Windows (PowerShell) :**
```powershell
# SHA256
Get-FileHash MonApp-Setup-1.0.0.exe -Algorithm SHA256 | Format-List
```

**Fichier CHECKSUMS.txt :**
```
MonApp-1.0.0-Windows.zip
SHA256: a1b2c3d4e5f6...

MonApp-1.0.0-Linux.tar.gz
SHA256: 1a2b3c4d5e6f...

MonApp-1.0.0.dmg
SHA256: abc123def456...
```

---

## 6. Distribution

### Site Web Personnel

**Page de téléchargement type :**

```html
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Télécharger Mon Application</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            max-width: 800px;
            margin: 50px auto;
            padding: 20px;
        }
        .download-box {
            border: 2px solid #007bff;
            border-radius: 8px;
            padding: 20px;
            margin: 20px 0;
        }
        .download-btn {
            display: inline-block;
            background: #007bff;
            color: white;
            padding: 15px 30px;
            text-decoration: none;
            border-radius: 5px;
            margin: 10px;
        }
        .platform {
            font-weight: bold;
            color: #333;
        }
    </style>
</head>
<body>
    <h1>Télécharger Mon Application v1.0.0</h1>

    <div class="download-box">
        <span class="platform">🪟 Windows</span>
        <p>Compatible : Windows 10 et 11 (64 bits)</p>
        <a href="downloads/MonApp-Setup-v1.0.0.exe" class="download-btn">
            Télécharger l'installeur (25 MB)
        </a>
        <a href="downloads/MonApp-v1.0.0-Windows-Portable.zip" class="download-btn">
            Version portable (ZIP - 22 MB)
        </a>
        <p><small>SHA256: a1b2c3d4e5f6...</small></p>
    </div>

    <div class="download-box">
        <span class="platform">🐧 Linux</span>
        <p>Compatible : Ubuntu 20.04+, Debian 11+, Fedora 35+</p>
        <a href="downloads/monapp_1.0.0-1_amd64.deb" class="download-btn">
            .deb Ubuntu/Debian (18 MB)
        </a>
        <a href="downloads/MonApp-v1.0.0-Linux-x64.tar.gz" class="download-btn">
            Archive tar.gz (16 MB)
        </a>
        <a href="downloads/MonApp-x86_64.AppImage" class="download-btn">
            AppImage universel (20 MB)
        </a>
        <p><small>SHA256: 1a2b3c4d5e6f...</small></p>
    </div>

    <div class="download-box">
        <span class="platform">🍎 macOS</span>
        <p>Compatible : macOS 10.13+ (High Sierra et supérieur)</p>
        <a href="downloads/MonApp-1.0.0.dmg" class="download-btn">
            Télécharger le DMG (24 MB)
        </a>
        <p><small>SHA256: abc123def456...</small></p>
    </div>

    <h2>Installation</h2>
    <h3>Windows</h3>
    <ol>
        <li>Télécharger l'installeur</li>
        <li>Double-cliquer sur le fichier .exe</li>
        <li>Suivre les instructions à l'écran</li>
    </ol>

    <h3>Linux</h3>
    <h4>Debian/Ubuntu (.deb)</h4>
    <pre>sudo dpkg -i monapp_1.0.0-1_amd64.deb
sudo apt-get install -f</pre>

    <h4>AppImage</h4>
    <pre>chmod +x MonApp-x86_64.AppImage
./MonApp-x86_64.AppImage</pre>

    <h3>macOS</h3>
    <ol>
        <li>Télécharger le fichier .dmg</li>
        <li>Double-cliquer sur le .dmg</li>
        <li>Glisser MonApp.app vers Applications</li>
    </ol>

    <h2>Notes de Version</h2>
    <p>Voir le <a href="CHANGELOG.txt">CHANGELOG</a> complet.</p>

    <h2>Support</h2>
    <p>Besoin d'aide ? <a href="mailto:support@example.com">Contactez-nous</a></p>
</body>
</html>
```

### GitHub Releases

**Créer une release sur GitHub :**

1. Aller sur votre dépôt GitHub
2. **Releases** → **Create a new release**
3. Créer un tag : `v1.0.0`
4. Titre : "Version 1.0.0"
5. Description : Notes de version
6. Uploader les fichiers :
   - `MonApp-Setup-v1.0.0.exe`
   - `monapp_1.0.0-1_amd64.deb`
   - `MonApp-1.0.0.dmg`
   - `CHECKSUMS.txt`
7. Publier

**Avantages :**
- ✅ Gratuit
- ✅ Hébergement illimité
- ✅ Historique des versions
- ✅ Notifications aux abonnés

### Stores d'Applications

**Windows Store :**
1. Créer un compte développeur (20$ unique)
2. Packager avec MSIX
3. Soumettre pour validation
4. Attendre approbation (quelques jours)

**Snap Store (Linux) :**
1. Créer un compte snapcraft.io (gratuit)
2. Créer un snap
3. Soumettre
4. Publier

**Mac App Store :**
1. Compte développeur Apple (99$/an)
2. Respecter les guidelines strictes
3. Sandboxing obligatoire
4. Validation longue

---

## 7. Mises à Jour Automatiques

### Système de Vérification des Mises à Jour

**Fichier version.json sur votre serveur :**

```json
{
  "version": "1.0.1",
  "releaseDate": "2024-10-20",
  "downloadUrl": {
    "windows": "https://example.com/downloads/MonApp-Setup-v1.0.1.exe",
    "linux": "https://example.com/downloads/MonApp-v1.0.1-Linux.tar.gz",
    "macos": "https://example.com/downloads/MonApp-1.0.1.dmg"
  },
  "changelog": [
    "Correction du bug #45",
    "Amélioration des performances",
    "Nouvelle fonctionnalité X"
  ],
  "mandatory": false
}
```

**Code Pascal pour vérifier les mises à jour :**

```pascal
unit UpdateChecker;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, fphttpclient, fpjson, jsonparser;

const
  CURRENT_VERSION = '1.0.0';
  UPDATE_URL = 'https://example.com/version.json';

type
  TUpdateInfo = record
    Available: Boolean;
    NewVersion: string;
    DownloadURL: string;
    Changelog: TStringList;
    Mandatory: Boolean;
  end;

function CheckForUpdates(out UpdateInfo: TUpdateInfo): Boolean;

implementation

function CompareVersions(const V1, V2: string): Integer;
var
  Parts1, Parts2: TStringArray;
  i, N1, N2: Integer;
begin
  Parts1 := V1.Split('.');
  Parts2 := V2.Split('.');

  for i := 0 to Max(Length(Parts1), Length(Parts2)) - 1 do
  begin
    if i < Length(Parts1) then
      N1 := StrToIntDef(Parts1[i], 0)
    else
      N1 := 0;

    if i < Length(Parts2) then
      N2 := StrToIntDef(Parts2[i], 0)
    else
      N2 := 0;

    if N1 < N2 then
      Exit(-1)
    else if N1 > N2 then
      Exit(1);
  end;

  Result := 0;
end;

function CheckForUpdates(out UpdateInfo: TUpdateInfo): Boolean;
var
  HTTP: TFPHTTPClient;
  Response: string;
  JSON: TJSONData;
  DownloadURLs: TJSONObject;
  ChangelogArray: TJSONArray;
  i: Integer;
begin
  Result := False;
  UpdateInfo.Available := False;
  UpdateInfo.Changelog := TStringList.Create;

  HTTP := TFPHTTPClient.Create(nil);
  try
    try
      Response := HTTP.Get(UPDATE_URL);
      JSON := GetJSON(Response);

      if JSON is TJSONObject then
      begin
        UpdateInfo.NewVersion := TJSONObject(JSON).Get('version', '');

        // Comparer les versions
        if CompareVersions(CURRENT_VERSION, UpdateInfo.NewVersion) < 0 then
        begin
          UpdateInfo.Available := True;

          // Récupérer l'URL de téléchargement selon la plateforme
          DownloadURLs := TJSONObject(JSON).Objects['downloadUrl'];
          {$IFDEF WINDOWS}
          UpdateInfo.DownloadURL := DownloadURLs.Get('windows', '');
          {$ENDIF}
          {$IFDEF LINUX}
          UpdateInfo.DownloadURL := DownloadURLs.Get('linux', '');
          {$ENDIF}
          {$IFDEF DARWIN}
          UpdateInfo.DownloadURL := DownloadURLs.Get('macos', '');
          {$ENDIF}

          // Récupérer le changelog
          ChangelogArray := TJSONObject(JSON).Arrays['changelog'];
          for i := 0 to ChangelogArray.Count - 1 do
            UpdateInfo.Changelog.Add(ChangelogArray.Strings[i]);

          // Mise à jour obligatoire ?
          UpdateInfo.Mandatory := TJSONObject(JSON).Get('mandatory', False);

          Result := True;
        end;
      end;

      JSON.Free;
    except
      on E: Exception do
        WriteLn('Erreur vérification mise à jour : ', E.Message);
    end;
  finally
    HTTP.Free;
  end;
end;

end.
```

**Utilisation dans votre application :**

```pascal
uses
  UpdateChecker, Dialogs;

procedure TForm1.ButtonCheckUpdatesClick(Sender: TObject);
var
  UpdateInfo: TUpdateInfo;
  Msg: string;
begin
  if CheckForUpdates(UpdateInfo) then
  begin
    if UpdateInfo.Available then
    begin
      Msg := Format('Une nouvelle version %s est disponible !' + sLineBreak +
                    sLineBreak + 'Nouveautés :' + sLineBreak + '%s' + sLineBreak +
                    sLineBreak + 'Voulez-vous télécharger la mise à jour ?',
                    [UpdateInfo.NewVersion, UpdateInfo.Changelog.Text]);

      if MessageDlg('Mise à jour disponible', Msg, mtInformation, [mbYes, mbNo], 0) = mrYes then
        OpenURL(UpdateInfo.DownloadURL);
    end
    else
      ShowMessage('Vous utilisez déjà la dernière version.');
  end
  else
    ShowMessage('Impossible de vérifier les mises à jour.');
end;
```

---

## 8. Script de Build Complet

**build-all.sh (Multi-plateforme) :**

```bash
#!/bin/bash

VERSION="1.0.0"
APPNAME="MonApp"

echo "=========================================="
echo "Build complet multi-plateforme"
echo "Version : $VERSION"
echo "=========================================="
echo

# Nettoyer
echo "[1/6] Nettoyage..."
rm -rf dist bin lib/*.ppu

# Compiler Windows
echo "[2/6] Compilation Windows..."
lazbuild --build-mode=Release-Windows-64 MonProjet.lpi

# Compiler Linux
echo "[3/6] Compilation Linux..."
lazbuild --build-mode=Release-Linux-64 MonProjet.lpi

# Compiler macOS
echo "[4/6] Compilation macOS..."
lazbuild --build-mode=Release-Darwin-64 MonProjet.lpi

# Créer les packages
echo "[5/6] Création des packages..."
./scripts/create-windows-installer.bat
./scripts/create-linux-packages.sh
./scripts/create-macos-dmg.sh

# Générer les checksums
echo "[6/6] Génération des checksums..."
cd dist
sha256sum * > CHECKSUMS.txt
cd ..

echo
echo "=========================================="
echo "Build terminé !"
echo "Packages disponibles dans dist/"
echo "=========================================="
ls -lh dist/
```

---

## 9. Bonnes Pratiques

### ✅ 1. Versionning Sémantique

**Format : MAJEUR.MINEUR.CORRECTIF**

- **MAJEUR** : Changements incompatibles
- **MINEUR** : Nouvelles fonctionnalités compatibles
- **CORRECTIF** : Corrections de bugs

**Exemples :**
- `1.0.0` → Première version stable
- `1.1.0` → Ajout de fonctionnalités
- `1.1.1` → Correction de bugs
- `2.0.0` → Changements majeurs

### ✅ 2. Changelog Clair

**Format recommandé :**

```markdown
# Changelog

## [1.1.0] - 2024-10-20

### Ajouté
- Support du format PDF pour l'export
- Raccourcis clavier personnalisables
- Mode sombre

### Modifié
- Interface redesignée
- Performance d'import améliorée de 40%

### Corrigé
- Bug #45 : Plantage lors de l'ouverture de gros fichiers
- Bug #47 : Encodage incorrect sous Linux

## [1.0.0] - 2024-10-01

### Ajouté
- Version initiale
- Import/Export CSV
- Base de données SQLite
```

### ✅ 3. Documentation Complète

**Fichiers à inclure :**
- `README.txt` : Vue d'ensemble, installation
- `LICENSE.txt` : Licence logicielle
- `CHANGELOG.txt` : Historique des versions
- `DEPENDENCIES.txt` : Dépendances requises
- `BUILDING.txt` : Instructions de compilation

### ✅ 4. Tests Avant Distribution

**Checklist :**
- [ ] Testé sur toutes les plateformes cibles
- [ ] Installeurs testés
- [ ] Désinstallation testée
- [ ] Dépendances vérifiées
- [ ] Signatures valides
- [ ] Checksums générés
- [ ] Documentation à jour

### ✅ 5. Nommage Cohérent

**Convention recommandée :**
```
[NomApp]-[Version]-[Plateforme]-[Type].[Extension]

Exemples :
MonApp-v1.0.0-Windows-Setup.exe
MonApp-v1.0.0-Windows-Portable.zip
MonApp-v1.0.0-Linux-x64.tar.gz
MonApp-v1.0.0-Linux-amd64.deb
MonApp-v1.0.0-macOS.dmg
```

### ✅ 6. Communication

**Informer les utilisateurs :**
- Blog/News sur votre site
- Email aux utilisateurs enregistrés
- Réseaux sociaux
- Forums et communautés
- Release notes détaillées

---

## 10. Résumé

L'empaquetage et la distribution sont les **dernières étapes** mais non les moindres de votre projet. Une application excellente mal packagée ne sera pas adoptée.

**Points clés à retenir :**

1. **Choisir le bon format** : Installeur pour le grand public, archive pour les utilisateurs avancés
2. **Automatiser** : Scripts pour créer tous les packages d'un coup
3. **Signer** : Augmente la confiance et évite les avertissements
4. **Documenter** : README, changelog, instructions claires
5. **Tester** : Sur machines propres, comme un utilisateur lambda
6. **Distribuer** : GitHub Releases est gratuit et pratique
7. **Maintenir** : Système de mises à jour automatiques

**Effort nécessaire :**
- Configuration initiale : 2-3 jours
- Par version : 1-2 heures (si automatisé)

**Retour sur investissement :**
- Expérience utilisateur professionnelle
- Adoption facilitée
- Support simplifié
- Réputation améliorée

**Règle d'or :** Votre packaging doit être **aussi simple que possible** pour l'utilisateur. Si votre grand-mère ne peut pas l'installer, c'est trop compliqué !

Félicitations ! Vous avez maintenant toutes les connaissances pour développer, compiler, tester, packager et distribuer des applications FreePascal/Lazarus multi-plateformes de qualité professionnelle. 🎉

⏭️ [Débogage et Optimisation](/20-debogage-optimisation/README.md)
