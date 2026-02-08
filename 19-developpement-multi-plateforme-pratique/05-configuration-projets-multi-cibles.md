🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.5 Configuration de projets multi-cibles dans Lazarus

## Introduction

Imaginez que vous cuisinez un plat qui doit plaire à la fois à des végétariens et des carnivores. Plutôt que de faire deux repas complètement différents, vous préparez une base commune et ajoutez des ingrédients spécifiques selon les convives. C'est exactement ce que font les projets multi-cibles dans Lazarus !

Un **projet multi-cibles** est un projet unique qui peut être compilé pour différentes plateformes (Windows, Linux, macOS) et différentes configurations (Debug, Release, 32 bits, 64 bits) sans modifier le code source.

Dans ce chapitre, nous allons apprendre à configurer Lazarus pour gérer efficacement ces différentes cibles.

---

## 1. Comprendre les Options de Projet

### Accéder aux Options du Projet

Dans Lazarus, ouvrez votre projet et :
1. Menu **Projet** → **Options du projet...**
2. Ou raccourci clavier : **Shift+Ctrl+F11** (Windows/Linux)

Vous accédez à une fenêtre avec de nombreuses sections dans le panneau de gauche.

### Structure des Options

Les options de projet sont organisées en catégories :

| Catégorie | Description |
|-----------|-------------|
| **Options de compilation** | Modes de compilation, optimisations |
| **Chemins** | Où chercher les unités, les fichiers |
| **Options du compilateur** | Paramètres spécifiques au compilateur |
| **Options de l'éditeur de liens** | Configuration de la liaison |
| **Informations de version** | Métadonnées de l'application |
| **Fichiers de ressources** | Icônes, manifestes, etc. |

**Important :** Ces options sont sauvegardées dans le fichier `.lpi` (Lazarus Project Information) de votre projet.

---

## 2. Les Modes de Compilation (Build Modes)

### Qu'est-ce qu'un Mode de Compilation ?

Un **mode de compilation** (Build Mode) est un ensemble de paramètres de compilation prédéfinis. Par défaut, Lazarus en crée un seul (**Default**). Voici les modes typiques d'un projet :

1. **Default** : Configuration par défaut (créé automatiquement)
2. **Debug** : Pour le débogage (symboles de debug inclus)
3. **Release** : Pour la production (optimisé, sans debug)

Vous pouvez créer autant de modes que nécessaire !

### Accéder aux Modes de Compilation

1. **Projet** → **Options du projet**
2. Section **Options de compilation**
3. En haut, vous voyez la liste déroulante des modes

### Créer un Nouveau Mode

**Exemple : Créer un mode "Release-Windows-64"**

1. Dans **Options de compilation**, cliquez sur l'icône **engrenage** ou le bouton à côté de la liste déroulante
2. Cliquez sur **Créer Build Mode**
3. Nommez-le : `Release-Windows-64`
4. Choisissez éventuellement un mode existant comme base (par exemple "Release")
5. Cliquez sur **OK**

Votre nouveau mode apparaît dans la liste !

### Structure d'un Mode

Chaque mode peut avoir ses propres :
- **Niveau d'optimisation** (aucune, normale, maximum)
- **Symboles de debug** (activés ou non)
- **Définitions personnalisées** (symboles du compilateur)
- **Chemins** spécifiques
- **Options du compilateur**
- **Plateforme cible** (Windows, Linux, macOS, etc.)

---

## 3. Configuration par Plateforme

### Exemple Pratique : Projet Multi-Plateforme Complet

Créons un projet avec 4 modes de compilation :
1. **Debug-Windows-64** : Développement sous Windows 64 bits
2. **Debug-Linux-64** : Développement sous Linux 64 bits
3. **Release-Windows-64** : Production Windows 64 bits
4. **Release-Linux-64** : Production Linux 64 bits

### Étape 1 : Créer les Modes

Pour chaque mode :
1. **Projet** → **Options du projet** → **Options de compilation**
2. Créer le nouveau mode
3. Nommer selon la convention : `[Type]-[OS]-[Arch]`

### Étape 2 : Configurer les Options Communes

Pour **TOUS** les modes, dans **Chemins** :

```
Répertoire de sortie :
  lib/$(TargetCPU)-$(TargetOS)

Répertoire de sortie final :
  bin/$(BuildMode)
```

**Explications :**
- `$(TargetCPU)` : remplacé automatiquement par `x86_64`, `i386`, etc.
- `$(TargetOS)` : remplacé par `win64`, `linux`, `darwin`, etc.
- `$(BuildMode)` : nom du mode actuel

**Résultat :** Les fichiers intermédiaires et finaux sont séparés par plateforme !

**Structure de répertoire obtenue :**
```
MonProjet/
├── bin/
│   ├── Debug-Windows-64/
│   │   └── MonProjet.exe
│   ├── Debug-Linux-64/
│   │   └── MonProjet
│   ├── Release-Windows-64/
│   │   └── MonProjet.exe
│   └── Release-Linux-64/
│       └── MonProjet
├── lib/
│   ├── x86_64-win64/
│   └── x86_64-linux/
└── src/
```

### Étape 3 : Configuration Debug vs Release

**Pour les modes Debug :**
1. **Options de compilation** → **Débogage**
2. Cocher **Générer les informations de débogage** (`-g`)
3. **Optimisations** → Niveau : **1** ou aucune

**Pour les modes Release :**
1. **Options de compilation** → **Débogage**
2. DÉcocher **Générer les informations de débogage**
3. **Optimisations** → Niveau : **3** (maximum)
4. Cocher **Optimisation plus petite** (`-Os`) si vous voulez réduire la taille

### Étape 4 : Définir la Plateforme Cible

**Pour les modes Windows :**
1. **Options de compilation** → **Cible**
2. **OS cible** : `Win64` (ou `Win32` pour 32 bits)
3. **CPU cible** : `x86_64` (ou `i386` pour 32 bits)

**Pour les modes Linux :**
1. **Options de compilation** → **Cible**
2. **OS cible** : `Linux`
3. **CPU cible** : `x86_64`

**Important :** Sous Windows, pour compiler vers Linux, vous aurez besoin d'un compilateur croisé (cross-compiler). Nous verrons cela dans le chapitre suivant.

---

## 4. Définitions Personnalisées (Custom Defines)

### Ajouter des Symboles Spécifiques

Vous pouvez définir des symboles uniquement pour certains modes.

**Exemple : Ajouter un symbole VERSION_PRO**

1. **Options de compilation** → **Autres**
2. Section **Définitions personnalisées**
3. Ajouter : `VERSION_PRO`
4. Si plusieurs symboles : `VERSION_PRO;LOGS_ACTIVES;DEMO_MODE`

**Dans votre code :**
```pascal
{$IFDEF VERSION_PRO}
  WriteLn('Version professionnelle');
{$ELSE}
  WriteLn('Version gratuite');
{$ENDIF}
```

### Symboles Différents par Mode

**Mode Debug-Windows-64 :**
- Définitions : `DEBUG;LOGS_ACTIVES`

**Mode Release-Windows-64 :**
- Définitions : (aucune, ou `RELEASE`)

**Mode Debug-Linux-64 :**
- Définitions : `DEBUG;LOGS_ACTIVES;LINUX_SPECIFIC`

---

## 5. Chemins de Recherche Conditionnels

### Configuration des Chemins

**Projet** → **Options du projet** → **Chemins**

Vous pouvez configurer :

| Chemin | Description |
|--------|-------------|
| **Autres fichiers d'unités** | Où chercher les unités `.pas` |
| **Chemins Include** | Pour les fichiers `.inc` |
| **Bibliothèques** | Chemins des `.dll`, `.so` |
| **Répertoire de sortie** | Où mettre les fichiers compilés |

### Utiliser des Macros

Lazarus supporte plusieurs macros pour rendre les chemins dynamiques :

| Macro | Valeur |
|-------|--------|
| `$(ProjPath)` | Chemin du projet |
| `$(TargetOS)` | OS cible (win64, linux, darwin) |
| `$(TargetCPU)` | CPU cible (x86_64, i386, arm) |
| `$(BuildMode)` | Nom du mode actuel |
| `$(LCLWidgetType)` | Type de widget LCL (win32, gtk2, qt5) |

**Exemple de configuration :**
```
Autres fichiers d'unités :
  $(ProjPath)/src
  $(ProjPath)/lib/$(TargetOS)
  $(ProjPath)/lib/common

Répertoire de sortie :
  $(ProjPath)/lib/$(TargetCPU)-$(TargetOS)/$(BuildMode)

Répertoire de sortie final :
  $(ProjPath)/bin/$(BuildMode)
```

### Chemins Spécifiques par Plateforme

**Méthode 1 : Dans le code**
```pascal
{$IFDEF WINDOWS}
  {$I windows_specific.inc}
{$ENDIF}

{$IFDEF LINUX}
  {$I linux_specific.inc}
{$ENDIF}
```

**Méthode 2 : Fichiers de projet conditionnels**

Créez des fichiers `.lpi` différents :
- `MonProjet.lpi` (base)
- `MonProjet.Windows.lpi`
- `MonProjet.Linux.lpi`

**Moins recommandé**, car plus difficile à maintenir.

---

## 6. Options du Compilateur Avancées

### Accès aux Options

**Projet** → **Options du projet** → **Options du compilateur** → **Autres**

### Options Courantes par Mode

**Pour Debug :**
```
Options supplémentaires :
  -g        (Informations de debug)
  -gl       (Numéros de lignes pour debug)
  -gh       (Utiliser heaptrc pour détecter les fuites mémoire)
```

**Pour Release :**
```
Options supplémentaires :
  -O3       (Optimisation maximum)
  -XX       (Smart linking - enlève le code non utilisé)
  -Xs       (Strip symbols - enlève les symboles)
```

### Options Spécifiques Windows

```
-WG       (GUI application - pas de console)
-WC       (Console application - avec fenêtre console)
```

**Configuration dans Lazarus :**
1. **Projet** → **Options du projet**
2. **Options du compilateur** → **Configuration et cible**
3. **Type d'application Win32** : Choisir "Application graphique" ou "Application console"

---

## 7. Gestion des Ressources

### Fichiers de Ressources Windows

Les fichiers `.rc` contiennent les ressources Windows (icônes, manifestes, informations de version).

**Exemple : MonProjet.rc**
```rc
1 ICON "icone.ico"
1 24 "manifest.xml"

1 VERSIONINFO
FILEVERSION 1,0,0,0
PRODUCTVERSION 1,0,0,0
{
  BLOCK "StringFileInfo"
  {
    BLOCK "040C04B0"  // Français
    {
      VALUE "CompanyName", "Ma Société"
      VALUE "FileDescription", "Mon Application"
      VALUE "FileVersion", "1.0.0.0"
      VALUE "ProductName", "MonApp"
      VALUE "ProductVersion", "1.0.0.0"
    }
  }
}
```

**Configurer dans Lazarus :**
1. **Projet** → **Options du projet** → **Fichiers de ressources**
2. Ajouter votre fichier `.rc`

**Important :** Les fichiers de ressources sont spécifiques à Windows ! Sous Linux, utilisez d'autres méthodes.

### Icône Multi-Plateforme

**Windows :**
- Fichier `.ico` dans le `.rc`

**Linux :**
- Fichier `.png` ou `.svg`
- À installer séparément avec l'application (`.desktop` file)

**Configuration dans Lazarus :**
1. **Projet** → **Options du projet** → **Icône de l'application**
2. Choisir un fichier `.ico` (Lazarus le convertira pour Linux)

---

## 8. Macro et Scripts de Compilation

### Événements de Compilation

Lazarus permet d'exécuter des commandes avant et après la compilation.

**Accès :**
1. **Projet** → **Options du projet** → **Options de compilation**
2. Section **Exécuter avant** / **Exécuter après**

**Exemple : Copier des DLL après compilation (Windows)**

**Commande à exécuter après :**
```
cmd /c copy /Y "$(ProjPath)\lib\*.dll" "$(TargetFile)"
```

**Exemple : Définir les permissions sous Linux**

**Commande à exécuter après :**
```
chmod +x $(TargetFile)
```

### Scripts de Compilation Externes

Pour des projets complexes, créez des scripts :

**build.bat (Windows) :**
```batch
@echo off
echo Compilation Release Windows 64 bits...
lazbuild --build-mode=Release-Windows-64 MonProjet.lpi
echo.
echo Compilation terminée !
pause
```

**build.sh (Linux) :**
```bash
#!/bin/bash
echo "Compilation Release Linux 64 bits..."
lazbuild --build-mode=Release-Linux-64 MonProjet.lpi
echo ""
echo "Compilation terminée !"
```

**Utilisation :**
```bash
# Windows
build.bat

# Linux
chmod +x build.sh
./build.sh
```

---

## 9. Compilation en Ligne de Commande

### Utilisation de lazbuild

`lazbuild` est l'outil en ligne de commande de Lazarus pour compiler des projets.

**Syntaxe de base :**
```bash
lazbuild [options] projet.lpi
```

### Options Principales

| Option | Description |
|--------|-------------|
| `--build-mode=NomMode` | Compiler avec un mode spécifique |
| `--build-all` | Tout recompiler |
| `--recursive` | Compiler les dépendances |
| `--quiet` | Mode silencieux |
| `--verbose` | Mode verbeux |

### Exemples Pratiques

**Compiler en mode Debug :**
```bash
lazbuild --build-mode=Debug MonProjet.lpi
```

**Compiler tous les modes :**
```bash
lazbuild --build-mode=Debug-Windows-64 MonProjet.lpi
lazbuild --build-mode=Release-Windows-64 MonProjet.lpi
lazbuild --build-mode=Debug-Linux-64 MonProjet.lpi
lazbuild --build-mode=Release-Linux-64 MonProjet.lpi
```

**Script de compilation complet :**

**build-all.bat (Windows) :**
```batch
@echo off
setlocal

set PROJECT=MonProjet.lpi
set LAZBUILD=C:\lazarus\lazbuild.exe

echo ========================================
echo Compilation de tous les modes
echo ========================================
echo.

echo [1/4] Debug Windows 64...
%LAZBUILD% --build-mode=Debug-Windows-64 %PROJECT%
if errorlevel 1 goto error

echo.
echo [2/4] Release Windows 64...
%LAZBUILD% --build-mode=Release-Windows-64 %PROJECT%
if errorlevel 1 goto error

echo.
echo [3/4] Debug Linux 64...
%LAZBUILD% --build-mode=Debug-Linux-64 %PROJECT%
if errorlevel 1 goto error

echo.
echo [4/4] Release Linux 64...
%LAZBUILD% --build-mode=Release-Linux-64 %PROJECT%
if errorlevel 1 goto error

echo.
echo ========================================
echo Compilation réussie !
echo ========================================
goto end

:error
echo.
echo ERREUR : La compilation a échoué !
exit /b 1

:end
pause
```

---

## 10. Configuration IDE Multi-Plateformes

### Travailler sur Plusieurs Machines

Si vous développez sur Windows ET Linux, vous aurez deux installations de Lazarus.

**Bonne pratique :**
1. Utilisez le **contrôle de version** (Git, SVN)
2. **NE versionnez PAS** les dossiers `lib/` et `bin/`
3. **Versionnez** le fichier `.lpi` (options du projet)

**Fichier .gitignore :**
```gitignore
# Lazarus
lib/
bin/
backup/
*.compiled
*.lps

# Fichiers temporaires
*.bak
*.~*
*.tmp
```

### Synchronisation des Options

Le fichier `.lpi` contient TOUTES les options de projet, incluant les build modes. En versionnant ce fichier, vos configurations sont partagées entre machines.

---

## 11. Exemple Complet : Projet E-Commerce Multi-Plateforme

### Structure du Projet

```
ECommerce/
├── src/
│   ├── main.pas
│   ├── database.pas
│   ├── ui/
│   │   ├── forms.pas
│   │   └── dialogs.pas
│   └── platform/
│       ├── common.pas
│       ├── windows_specific.pas
│       └── linux_specific.pas
├── lib/
│   ├── common/
│   ├── windows/
│   └── linux/
├── resources/
│   ├── windows/
│   │   ├── app.ico
│   │   └── app.rc
│   └── linux/
│       └── app.png
├── bin/
├── ECommerce.lpi
└── ECommerce.lpr
```

### Modes de Compilation Créés

1. **Dev-Win64** : Développement quotidien Windows
2. **Dev-Linux64** : Développement quotidien Linux
3. **Release-Win64** : Production Windows
4. **Release-Linux64** : Production Linux
5. **Release-Win32** : Production Windows 32 bits (legacy)

### Configuration Dev-Win64

**Options de compilation :**
- OS cible : Win64
- CPU cible : x86_64
- Optimisation : Niveau 1
- Debug : Activé
- Définitions : `DEBUG;LOGS_ACTIVES;DEV_MODE`

**Chemins :**
- Autres unités : `src;src/ui;src/platform;lib/common;lib/windows`
- Sortie : `lib/x86_64-win64/dev`
- Sortie finale : `bin/Dev-Win64`

**Autres options :**
- Utiliser heaptrc : Oui (détection fuites mémoire)
- Type application : Graphique

### Configuration Release-Win64

**Options de compilation :**
- OS cible : Win64
- CPU cible : x86_64
- Optimisation : Niveau 3
- Debug : Désactivé
- Définitions : `RELEASE;VERSION_PRO`

**Chemins :**
- Autres unités : `src;src/ui;src/platform;lib/common;lib/windows`
- Sortie : `lib/x86_64-win64/release`
- Sortie finale : `bin/Release-Win64`

**Autres options :**
- Smart linking : Oui (`-XX`)
- Strip symbols : Oui (`-Xs`)
- Type application : Graphique

**Fichier de ressources :**
- `resources/windows/app.rc`

### Configuration Linux Similaire

Identique à Windows, mais :
- OS cible : Linux
- Chemins unités : remplacer `lib/windows` par `lib/linux`
- Pas de fichier `.rc`

### Script de Compilation

**build-release.bat :**
```batch
@echo off
echo Construction des versions de production...
lazbuild --build-mode=Release-Win64 ECommerce.lpi
lazbuild --build-mode=Release-Win32 ECommerce.lpi
echo.
echo Création des archives...
cd bin
7z a ECommerce-Win64.zip Release-Win64\*
7z a ECommerce-Win32.zip Release-Win32\*
cd ..
echo Terminé !
```

---

## 12. Bonnes Pratiques

### ✅ 1. Nommage Cohérent des Modes

**Bonne convention :**
```
[Type]-[OS]-[Arch]
Exemples :
  Debug-Windows-64
  Release-Linux-64
  Dev-Darwin-ARM64
```

### ✅ 2. Séparation des Fichiers Compilés

**Utilisez toujours :**
```
Sortie : lib/$(TargetCPU)-$(TargetOS)/$(BuildMode)
Sortie finale : bin/$(BuildMode)
```

**Avantages :**
- Pas de conflits entre plateformes
- Compilation plus rapide (pas de recompilation inutile)
- Tests multiples sans risque

### ✅ 3. Modes Debug Généreux

**Mode Debug doit inclure :**
- Symboles de debug (`-g`, `-gl`)
- HeapTrc pour détecter fuites (`-gh`)
- Vérifications de portée (`-Cr`, `-Co`, `-Ct`)
- Pas ou peu d'optimisation

**Pourquoi ?** Facilite le débogage et la détection de bugs.

### ✅ 4. Modes Release Optimisés

**Mode Release doit inclure :**
- Optimisation maximum (`-O3`)
- Smart linking (`-XX`)
- Strip symbols (`-Xs`)
- Pas de debug, pas de checks

**Résultat :** Exécutable plus petit, plus rapide.

### ✅ 5. Tester Régulièrement Tous les Modes

Ne compilez pas seulement en Debug ! Testez régulièrement :
- En Release (pour vérifier les performances)
- Sur Linux (si cible multi-plateforme)
- En 32 bits (si vous supportez cette architecture)

### ✅ 6. Versionner le .lpi

Le fichier `.lpi` contient toutes vos configurations. Versionnez-le avec Git/SVN pour partager les configurations entre développeurs.

### ✅ 7. Documenter les Modes

Ajoutez un fichier `BUILD.md` dans votre projet :

```markdown
# Modes de Compilation

## Dev-Win64
Mode développement quotidien Windows 64 bits.
Optimisation minimale, debug complet.

## Release-Win64
Version production Windows 64 bits.
Optimisation maximale, sans debug.

## Release-Win32
Version production Windows 32 bits (legacy).
Pour compatibilité avec anciens systèmes.
```

---

## 13. Pièges à Éviter

### ❌ Piège 1 : Oublier de Changer de Mode

```
Développeur : "Pourquoi mon exe Release est si lent ?"
Raison : Il a compilé avec le mode Debug !
```

**Solution :** Vérifiez toujours le mode actif en haut de la fenêtre Lazarus.

### ❌ Piège 2 : Chemins Absolus

```
Chemins des unités : C:\MonProjet\src
```

**Problème :** Ne fonctionnera pas sur un autre PC ou sous Linux !

**Solution :** Utilisez des chemins relatifs avec macros :
```
Chemins des unités : $(ProjPath)/src
```

### ❌ Piège 3 : Trop de Modes

```
Debug-Win64
Debug-Win32
Debug-Linux64
Debug-Linux32
Debug-Darwin64
Release-Win64
Release-Win32
... (30 modes au total)
```

**Problème :** Ingérable !

**Solution :** Créez seulement les modes que vous utilisez réellement. 4-6 modes suffisent généralement.

### ❌ Piège 4 : Ne Pas Tester en Release

```
Développeur : "Ça marche en Debug !"
En Release : CRASH !
```

**Raison :** Les optimisations peuvent révéler des bugs (variables non initialisées, etc.).

**Solution :** Testez régulièrement en mode Release.

---

## 14. Tableau Récapitulatif

| Aspect | Debug | Release |
|--------|-------|---------|
| **Optimisation** | Niveau 1 ou 0 | Niveau 3 |
| **Symboles Debug** | Oui (`-g`, `-gl`) | Non |
| **HeapTrc** | Oui (`-gh`) | Non |
| **Checks** | Oui (`-Cr`, `-Co`) | Non |
| **Smart Linking** | Non | Oui (`-XX`) |
| **Strip Symbols** | Non | Oui (`-Xs`) |
| **Taille exe** | Plus gros | Plus petit |
| **Vitesse** | Plus lent | Plus rapide |
| **Usage** | Développement | Production |

---

## Conclusion

La configuration de projets multi-cibles dans Lazarus vous permet de :

1. **Compiler pour plusieurs plateformes** sans changer le code
2. **Gérer différentes configurations** (Debug/Release) efficacement
3. **Optimiser** les exécutables pour chaque usage
4. **Automatiser** le processus de compilation
5. **Maintenir** facilement un projet complexe

**Les points clés :**
- Utilisez les **Build Modes** pour chaque configuration
- Utilisez les **macros** (`$(TargetOS)`, `$(BuildMode)`) pour les chemins
- Séparez les fichiers compilés par plateforme
- Testez régulièrement tous vos modes
- Automatisez avec `lazbuild` et des scripts

Avec ces configurations bien maîtrisées, vous êtes prêt pour le chapitre suivant sur la cross-compilation, qui vous permettra de compiler pour Linux depuis Windows (et vice-versa) !

⏭️ [Cross-compilation : théorie et pratique](/19-developpement-multi-plateforme-pratique/06-cross-compilation-theorie-pratique.md)
