🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.5 Installation sur Ubuntu/Linux

## Introduction

L'installation de Lazarus sur Linux est légèrement différente de celle sur Windows, mais ne vous inquiétez pas : elle reste tout à fait accessible aux débutants ! Linux offre même plusieurs méthodes d'installation, chacune avec ses avantages.

Cette section couvre principalement **Ubuntu** (la distribution Linux la plus populaire), mais les instructions sont aussi valables pour les distributions dérivées comme **Linux Mint**, **Pop!_OS**, **Elementary OS**, et partiellement pour **Debian**.

**Pour les autres distributions :** Les principes restent les mêmes, seuls les noms des commandes changent (apt → dnf, yum, pacman, zypper...).

## Prérequis système

### Configuration minimale

| Composant | Minimum | Recommandé |
|-----------|---------|------------|
| **Distribution** | Ubuntu 18.04 | Ubuntu 22.04 ou plus récent |
| **Architecture** | 32 bits | 64 bits |
| **RAM** | 1 Go | 4 Go ou plus |
| **Espace disque** | 500 Mo | 2 Go (avec marge pour vos projets) |
| **Écran** | 1024×768 | 1920×1080 |
| **Environnement** | GNOME, KDE, XFCE, etc. | N'importe lequel |

**Bonne nouvelle :** Lazarus fonctionne sur pratiquement toutes les variantes d'Ubuntu et sur la plupart des environnements de bureau Linux.

### Vérifier votre version d'Ubuntu

Si vous ne connaissez pas votre version exacte :

**Méthode graphique :**
1. Ouvrez **Paramètres système** (icône d'engrenage)
2. Allez dans **À propos** ou **Détails**
3. Vous verrez : "Ubuntu 22.04 LTS", "Ubuntu 23.10", etc.

**Méthode en ligne de commande :**
1. Ouvrez un **Terminal** (Ctrl+Alt+T)
2. Tapez : `lsb_release -a`
3. Appuyez sur **Entrée**
4. Vous verrez les détails de votre distribution

**Exemple de résultat :**
```
Distributor ID: Ubuntu  
Description:    Ubuntu 22.04.3 LTS  
Release:        22.04  
Codename:       jammy  
```

## Comprendre le Terminal (pour les débutants)

Sur Linux, beaucoup d'installations se font via le **Terminal** (aussi appelé console ou ligne de commande). Ne soyez pas intimidé !

### Qu'est-ce que le Terminal ?

C'est une fenêtre texte où vous tapez des commandes pour dire à l'ordinateur ce qu'il doit faire. C'est comme l'invite de commande de Windows, mais en plus puissant.

### Ouvrir le Terminal

**Méthode 1 : Raccourci clavier**
- Appuyez sur **Ctrl + Alt + T**
- Une fenêtre noire (ou colorée) s'ouvre

**Méthode 2 : Menu d'applications**
- Cliquez sur "Activités" (coin supérieur gauche)
- Tapez "Terminal"
- Cliquez sur l'icône Terminal

**Méthode 3 : Clic droit**
- Sur le bureau ou dans un dossier
- "Ouvrir un terminal ici" (selon votre environnement)

### Bases du Terminal

Quand vous ouvrez le Terminal, vous voyez quelque chose comme :
```
utilisateur@ordinateur:~$
```

Le **`$`** indique que vous pouvez taper une commande.

**Commandes de base à connaître :**
- **Entrée** : exécute la commande
- **Ctrl+C** : annule/arrête la commande en cours
- **Flèche haut** : rappelle la dernière commande
- **Ctrl+Shift+C** : copier
- **Ctrl+Shift+V** : coller
- **clear** : efface l'écran

**Note importante :** Linux distingue majuscules et minuscules ! `Lazarus` ≠ `lazarus`

### Le mot de passe sudo

Certaines commandes nécessitent des privilèges administrateur (comme "Exécuter en tant qu'administrateur" sur Windows). On utilise **sudo** devant ces commandes.

Quand vous tapez une commande avec `sudo`, Linux vous demande votre mot de passe :
- Tapez votre mot de passe (rien ne s'affiche à l'écran, c'est normal pour la sécurité)
- Appuyez sur Entrée
- La commande s'exécute avec les droits admin

**Exemple :**
```bash
sudo apt update
[sudo] Mot de passe de utilisateur : ▯
```
→ Tapez votre mot de passe et Entrée

## Trois méthodes d'installation

Linux vous offre plusieurs façons d'installer Lazarus. Choisissez celle qui vous convient !

### Comparaison rapide

| Méthode | Avantages | Inconvénients | Recommandé pour |
|---------|-----------|---------------|-----------------|
| **1. Dépôts Ubuntu (apt)** | Très simple, une commande | Version souvent ancienne | Débuter rapidement |
| **2. PPA officiel** | Simple, version récente | Nécessite ajout du PPA | **Usage recommandé** |
| **3. Téléchargement manuel** | Dernière version, contrôle total | Plus complexe | Utilisateurs avancés |

**👉 Nous recommandons la méthode 2 (PPA officiel) pour ce tutoriel : c'est le meilleur compromis !**

## Méthode 1 : Installation depuis les dépôts Ubuntu (simple mais ancienne)

C'est la méthode la plus simple, mais vous aurez probablement une version de Lazarus un peu ancienne.

### Étape 1 : Mettre à jour la liste des paquets

Ouvrez un Terminal et tapez :

```bash
sudo apt update
```

**Explication :** Cette commande met à jour la liste des logiciels disponibles. C'est comme "rafraîchir" le magasin d'applications.

### Étape 2 : Installer Lazarus

```bash
sudo apt install lazarus
```

**Ce qui se passe :**
- Linux télécharge Lazarus et FreePascal
- Il installe automatiquement toutes les dépendances nécessaires
- Durée : 5-10 minutes selon votre connexion

Quand on vous demande `Voulez-vous continuer ? [O/n]`, tapez **O** (ou juste Entrée) pour confirmer.

### Étape 3 : Installation terminée !

Une fois la commande terminée, Lazarus est installé.

**Vérifier l'installation :**
```bash
lazarus-ide --version
```

Vous devriez voir la version installée.

**Lancer Lazarus :**
- Menu Applications → Développement → Lazarus
- Ou tapez dans le Terminal : `lazarus-ide`

**⚠️ Limitation :** Cette méthode installe souvent une version ancienne (par exemple Lazarus 2.0 alors que la 3.0 existe). Pour avoir la dernière version, utilisez la méthode 2.

## Méthode 2 : Installation via le PPA officiel (RECOMMANDÉ)

Un **PPA** (Personal Package Archive) est un dépôt spécial qui contient des versions plus récentes de logiciels. Le PPA officiel de Lazarus est maintenu par l'équipe de développement.

### Étape 1 : Ajouter le PPA officiel Lazarus

Ouvrez un Terminal et tapez :

```bash
sudo add-apt-repository ppa:lazarus-team/lazarus
```

**Ce qui se passe :**
- On dit à Ubuntu : "Ajoute cette source de logiciels à ta liste"
- Une fenêtre peut vous demander confirmation
- Appuyez sur **Entrée** pour continuer

**Explication :** Le PPA `lazarus-team/lazarus` est le dépôt officiel maintenu par l'équipe Lazarus.

### Étape 2 : Mettre à jour la liste des paquets

```bash
sudo apt update
```

**Pourquoi ?** Pour que Ubuntu connaisse les nouveaux logiciels disponibles depuis le PPA qu'on vient d'ajouter.

### Étape 3 : Installer Lazarus et ses composants

```bash
sudo apt install lazarus lcl lcl-gtk2 lcl-qt5
```

**Détail des paquets :**
- **lazarus** : l'IDE principal
- **lcl** : la bibliothèque de composants Lazarus
- **lcl-gtk2** : support de l'interface graphique GTK2
- **lcl-qt5** : support de l'interface graphique Qt5 (optionnel mais recommandé)

**Durée :** 10-20 minutes selon votre connexion Internet

**Espace nécessaire :** ~300-400 Mo

Confirmez avec **O** (ou Entrée) quand demandé.

### Étape 4 : Installation terminée !

Une fois terminé, vous avez la dernière version stable de Lazarus !

**Vérifier :**
```bash
lazarus-ide --version
```

**Exemple de sortie :**
```
Lazarus 3.0 r[version]  
FPC 3.2.2  
```

**🎉 Félicitations ! Vous avez installé Lazarus avec la méthode recommandée !**

## Méthode 3 : Installation manuelle (téléchargement depuis le site)

Pour les utilisateurs qui veulent absolument la toute dernière version ou qui ne peuvent pas utiliser les PPA.

### Étape 1 : Télécharger les paquets .deb

1. Allez sur **https://www.lazarus-ide.org**
2. Cliquez sur **Download**
3. Section **Linux** → choisissez **Ubuntu**
4. Téléchargez les fichiers .deb nécessaires :
   - `fpc-laz_X.X.X-X_amd64.deb` (FreePascal)
   - `fpc-src_X.X.X-X_amd64.deb` (sources FreePascal)
   - `lazarus-project_X.X.X-X_amd64.deb` (Lazarus IDE)

**Note :** Remplacez les X.X.X par les numéros de version actuels.

### Étape 2 : Installer les dépendances

Avant d'installer les .deb, installez les dépendances :

```bash
sudo apt install build-essential gdb libgtk2.0-dev libcairo2-dev \
     libpango1.0-dev libgdk-pixbuf2.0-dev libatk1.0-dev libghc-x11-dev
```

### Étape 3 : Installer les paquets .deb

Ouvrez le Terminal dans le dossier où vous avez téléchargé les fichiers (généralement `~/Téléchargements`) :

```bash
cd ~/Téléchargements
```

Puis installez dans l'ordre :

```bash
sudo dpkg -i fpc-laz_*.deb  
sudo dpkg -i fpc-src_*.deb  
sudo dpkg -i lazarus-project_*.deb  
```

**Note :** Le `*` remplace automatiquement les numéros de version.

Si des erreurs de dépendances apparaissent :

```bash
sudo apt install -f
```

Cette commande corrige automatiquement les dépendances manquantes.

## Installation des outils de développement essentiels

Quel que soit la méthode choisie, installez ces outils utiles :

### Compilateur et outils de build

```bash
sudo apt install build-essential gdb
```

**Contient :**
- **gcc/g++** : compilateurs C/C++ (utiles pour certains packages)
- **make** : outil de build
- **gdb** : débogueur GNU (utilisé par Lazarus)

### Bibliothèques graphiques

Pour que les interfaces graphiques fonctionnent correctement :

```bash
sudo apt install libgtk2.0-dev libgtk-3-dev
```

### Bibliothèques optionnelles mais utiles

Pour des fonctionnalités avancées :

```bash
sudo apt install libcairo2-dev libpango1.0-dev libgdk-pixbuf2.0-dev
```

## Premier lancement de Lazarus

### Démarrer Lazarus

**Méthode 1 : Depuis le menu Applications**
1. Cliquez sur "Activités" ou ouvrez le menu Applications
2. Tapez "Lazarus"
3. Cliquez sur l'icône **Lazarus IDE**

**Méthode 2 : Depuis le Terminal**
```bash
lazarus-ide
```

**Note :** Sur certaines distributions, la commande peut être simplement `lazarus` au lieu de `lazarus-ide`.

### Premier chargement

Au premier lancement :
- Lazarus peut prendre 30-60 secondes pour démarrer
- Il configure l'environnement
- Crée des fichiers de configuration dans `~/.lazarus/`
- Peut demander à reconstruire l'IDE

**Message possible :** "Quick rebuild of Lazarus?"
- Cliquez **Yes** / **Oui**
- Attendez la fin de la compilation (1-3 minutes)
- C'est normal et ne se produit qu'une fois

### Configuration automatique initiale

**Fenêtre : "Lazarus Setup"**

Si cette fenêtre apparaît :

**1. "Default project directory"** (Répertoire de projets par défaut)
- Par défaut : `~/Documents/Lazarus` ou `~/Projets`
- **Recommandation :** Acceptez ou choisissez `~/MesProjetsLazarus`
- Cliquez **OK**

**2. "FPC Source Directory"** (Répertoire des sources FreePascal)
- Lazarus cherche automatiquement
- Si demandé, généralement : `/usr/share/fpcsrc/` ou `/usr/lib/fpc/src/`
- Cliquez **OK**

**3. "Scan packages?"**
- Cliquez **Yes** pour scanner les packages disponibles
- Durée : quelques secondes

### L'interface Lazarus s'ouvre

Vous devriez voir :
- La fenêtre principale avec menus et barres d'outils
- L'inspecteur d'objets (Object Inspector) à gauche
- Peut-être plusieurs fenêtres flottantes

**🎊 Lazarus fonctionne ! Vous êtes prêt à programmer !**

## Vérification de l'installation

### Test 1 : Vérifier les versions

1. Menu **Help** → **About Lazarus**
2. Vérifiez :
   - Version de Lazarus (ex: 3.0)
   - Version de FreePascal (ex: 3.2.2)
   - Plateforme cible (ex: x86_64-linux-gtk2)

**Exemple :**
```
Lazarus 3.0  
FPC 3.2.2  
Target: x86_64-linux-gtk2  
```

### Test 2 : Compiler un programme console

1. Menu **File** → **New** → **Simple Program**
2. Le code apparaît :

```pascal
program Project1;

begin  
end.  
```

3. Modifiez-le :

```pascal
program Project1;

begin
  WriteLn('Bonjour depuis Ubuntu !');
  ReadLn; // Pour que la fenêtre reste ouverte
end.
```

4. Appuyez sur **F9** (ou menu **Run** → **Run**)
5. Enregistrez le projet quand demandé
6. Une fenêtre Terminal s'ouvre avec : `Bonjour depuis Ubuntu !`

**✅ Si vous voyez ce message, la compilation fonctionne !**

### Test 3 : Créer une application graphique

1. Menu **File** → **New** → **Application**
2. Un formulaire vide apparaît
3. Palette de composants → onglet **Standard**
4. Double-cliquez sur **TButton**
5. Un bouton apparaît sur le formulaire
6. Appuyez sur **F9**
7. Une fenêtre GTK/Qt s'ouvre avec votre bouton

**✅ Si la fenêtre apparaît, les composants graphiques fonctionnent !**

### Test 4 : Vérifier le débogueur

```bash
which gdb
```

Devrait retourner : `/usr/bin/gdb`

Si ce n'est pas le cas, installez :
```bash
sudo apt install gdb
```

## Configuration de base

### 1. Langue de l'interface

Si Lazarus est en anglais :

1. Menu **Tools** → **Options**
2. **Environment** → **General**
3. **Language** → sélectionnez **Français**
4. Cliquez **OK**
5. Redémarrez Lazarus

### 2. Choisir la bibliothèque d'interface (widgetset)

Linux offre plusieurs types d'interfaces graphiques. Lazarus peut utiliser :
- **GTK2** (par défaut, stable)
- **GTK3** (moderne)
- **Qt5** (élégant)
- **Qt6** (très récent)

**Comment changer :**
1. Menu **Projet** → **Options du projet**
2. **Compiler Options** → **Additions and Overrides**
3. **LCL Widget Type** → choisissez (par exemple Qt5)
4. Recompilez le projet

**Pour débutant :** Gardez GTK2 par défaut, ça marche très bien !

### 3. Police de l'éditeur

1. Menu **Outils** → **Options**
2. **Éditeur** → **Options d'affichage**
3. **Taille** : ajustez (10-12 recommandé)
4. **Police** : "Monospace" ou "DejaVu Sans Mono"
5. Cliquez **OK**

### 4. Thème de couleurs

1. Menu **Outils** → **Options**
2. **Éditeur** → **Couleurs**
3. Choisissez un thème (Default, Twilight, Ocean...)
4. Cliquez **OK**

### 5. Répertoire de projets par défaut

1. Menu **Outils** → **Options**
2. **Environnement** → **Fichiers**
3. **Répertoire de projets par défaut** → Parcourir
4. Choisissez `~/Documents/MesProjetsLazarus`
5. Créez ce dossier s'il n'existe pas
6. Cliquez **OK**

## Problèmes courants et solutions

### Problème 1 : "Command not found: lazarus-ide"

**Symptômes :** Le Terminal dit que la commande n'existe pas.

**Solution :**
```bash
# Essayez simplement :
lazarus

# Ou trouvez où Lazarus est installé :
which lazarus  
which lazarus-ide  

# Si rien ne fonctionne, réinstallez :
sudo apt install --reinstall lazarus
```

### Problème 2 : "Cannot find FPC"

**Message :** Lazarus ne trouve pas le compilateur FreePascal.

**Solutions :**

**Vérifier que FPC est installé :**
```bash
fpc -v
```

Si "command not found", installez-le :
```bash
sudo apt install fpc fpc-source
```

**Configurer le chemin dans Lazarus :**
1. Menu **Tools** → **Options** → **Environment** → **Files**
2. **Compiler path** : doit pointer vers `/usr/bin/fpc`
3. **FPC source directory** : `/usr/share/fpcsrc/` ou `/usr/lib/fpc/src/`

### Problème 3 : Erreur "gtk2 not found" lors de la compilation

**Solution :** Installer les bibliothèques de développement GTK :

```bash
sudo apt install libgtk2.0-dev
```

Puis dans Lazarus :
1. Menu **Tools** → **Configure "Build Lazarus"**
2. Cliquez **Build**
3. Attendez la recompilation

### Problème 4 : "Permission denied" lors de l'enregistrement

**Cause :** Vous essayez d'enregistrer dans un dossier système protégé.

**Solution :** Enregistrez vos projets dans votre dossier personnel :
- `~/Documents/MesProjets/`
- `~/Projets/`
- Jamais dans `/opt/`, `/usr/`, ou `/etc/`

### Problème 5 : Interface graphique ne s'affiche pas

**Solutions :**

**Vérifier les bibliothèques graphiques :**
```bash
sudo apt install libgtk-3-0 libgtk2.0-0 libcairo2 libpango-1.0-0
```

**Tester avec un autre widgetset :**
1. Créez un nouveau projet
2. **Project** → **Project Options**
3. **Additions and Overrides** → **LCL Widget Type** → Qt5
4. Installez Qt5 si nécessaire :
```bash
sudo apt install libqt5pas-dev
```

### Problème 6 : Lazarus démarre très lentement

**Solutions :**

**Désactiver certains packages :**
1. Menu **Package** → **Install/Uninstall Packages**
2. Décochez les packages non essentiels
3. **Save and Rebuild IDE**

**Vérifier l'espace disque :**
```bash
df -h
```

Assurez-vous d'avoir au moins 2 Go libres.

### Problème 7 : "Error while linking"

**Cause :** Outils de build manquants.

**Solution :**
```bash
sudo apt install build-essential binutils ld-gold
```

### Problème 8 : Le débogueur ne fonctionne pas

**Solution :**

**Installer GDB :**
```bash
sudo apt install gdb
```

**Configurer dans Lazarus :**
1. Menu **Tools** → **Options** → **Debugger** → **General**
2. **Debugger type** : GNU debugger (gdb)
3. **Debugger path** : `/usr/bin/gdb`

### Problème 9 : Erreur de dépendances lors de l'installation .deb

**Message :** "Dependency problems"

**Solution :**
```bash
sudo apt --fix-broken install
```

Puis réessayez l'installation :
```bash
sudo dpkg -i lazarus*.deb
```

## Création d'un lanceur personnalisé (optionnel)

Si Lazarus n'apparaît pas dans votre menu d'applications, créez un lanceur manuel.

### Créer un fichier .desktop

```bash
nano ~/.local/share/applications/lazarus.desktop
```

Collez ce contenu :

```ini
[Desktop Entry]
Name=Lazarus IDE  
Comment=Environnement de développement FreePascal  
Exec=/usr/bin/lazarus-ide  
Icon=/usr/share/pixmaps/lazarus.png  
Terminal=false  
Type=Application  
Categories=Development;IDE;  
```

Sauvegardez : **Ctrl+O**, **Entrée**, **Ctrl+X**

Maintenant Lazarus apparaît dans votre menu Applications !

## Mise à jour de Lazarus

### Si installé via PPA (méthode 2)

Les mises à jour sont automatiques avec votre système :

```bash
sudo apt update  
sudo apt upgrade  
```

Lazarus se mettra à jour en même temps que vos autres logiciels.

### Si installé manuellement (méthode 3)

Téléchargez et installez les nouveaux paquets .deb comme expliqué dans la méthode 3.

## Installation sur d'autres distributions Linux

### Debian

Identique à Ubuntu :
```bash
sudo apt install lazarus
```

Ou utilisez le PPA avec quelques adaptations.

### Fedora / Red Hat / CentOS

```bash
sudo dnf install lazarus
```

### Arch Linux / Manjaro

```bash
sudo pacman -S lazarus
```

Ou via AUR pour la dernière version :
```bash
yay -S lazarus-gtk2
```

### openSUSE

```bash
sudo zypper install lazarus
```

### Linux Mint

Identique à Ubuntu (Linux Mint est basé sur Ubuntu).

## Avantages de Lazarus sur Linux

### 1. Performance native
Lazarus est développé en grande partie sur Linux. Les performances sont excellentes.

### 2. Pas de problèmes d'antivirus
Contrairement à Windows, pas de faux positifs ralentissant la compilation.

### 3. Mises à jour faciles
Via le gestionnaire de paquets, tout se met à jour automatiquement.

### 4. Open source sur open source
Philosophie cohérente : système libre + outils libres.

### 5. Développement multi-plateforme
Développez sur Linux, compilez pour Linux et Windows (cross-compilation).

## Où trouver de l'aide ?

**Documentation :**
- Wiki : https://wiki.lazarus.freepascal.org
- Dans Lazarus : Menu **Help** → **Online Help**

**Forums :**
- Forum officiel : https://forum.lazarus.freepascal.org
- Section française : https://forum.lazarus.freepascal.org/index.php/board,12.0.html

**Communauté Ubuntu :**
- Forum Ubuntu-fr : https://forum.ubuntu-fr.org
- Section "Programmation"

**IRC :**
- Canal #lazarus-ide sur Libera.Chat

## Conclusion

Vous avez maintenant Lazarus installé et configuré sur votre système Ubuntu/Linux ! L'installation sur Linux demande parfois un peu plus de commandes que sur Windows, mais elle offre aussi plus de flexibilité et de contrôle.

**Ce que vous avez accompli :**
- ✅ Compris les bases du Terminal Linux
- ✅ Installé Lazarus via la méthode de votre choix
- ✅ Installé les dépendances nécessaires
- ✅ Configuré l'environnement de base
- ✅ Vérifié que tout fonctionne
- ✅ Testé votre première compilation

**Avantages acquis :**
- Environnement de développement professionnel gratuit
- Performances natives Linux
- Outils open source cohérents
- Capacité de développement multi-plateforme

**Prochaines étapes :**
- Section 9.6 : Premier projet avec Lazarus
- Section 9.7 : Exploration approfondie de l'IDE
- Commencer à créer vos premières applications !

**N'oubliez pas :** La communauté Lazarus et Linux est très active et accueillante. Si vous rencontrez un problème, n'hésitez pas à demander de l'aide sur les forums !

**🐧 🚀 Vous êtes maintenant prêt à programmer en Pascal sur Linux avec Lazarus !**

---

**Points clés à retenir :**
- Trois méthodes d'installation : dépôts Ubuntu, PPA (recommandé), téléchargement manuel
- Le PPA officiel offre le meilleur compromis (facile + version récente)
- Le Terminal n'est pas compliqué : quelques commandes suffisent
- Installez les outils de développement (build-essential, gdb)
- Les bibliothèques GTK2/GTK3 sont nécessaires pour les interfaces graphiques
- La configuration est similaire à Windows mais avec des chemins Linux
- Les mises à jour sont automatiques via le gestionnaire de paquets
- Linux offre d'excellentes performances pour Lazarus
- La communauté est là pour vous aider en cas de problème

⏭️ [Premier projet avec Lazarus IDE](/09-introduction-freepascal-lazarus/06-premier-projet-lazarus-ide.md)
