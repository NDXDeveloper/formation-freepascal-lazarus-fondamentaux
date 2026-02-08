🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.8 Ligne de commande Windows et Linux basics

## Introduction

La ligne de commande (ou terminal, console, shell) est une interface textuelle qui permet d'interagir avec votre ordinateur en tapant des commandes. Bien qu'elle puisse sembler intimidante au début, c'est un outil extrêmement puissant et indispensable pour tout programmeur. Elle est particulièrement utile pour compiler et exécuter vos programmes FreePascal.

## Qu'est-ce que la ligne de commande ?

### Définition

La **ligne de commande** est une interface texte où vous tapez des commandes pour dire à l'ordinateur ce qu'il doit faire, au lieu de cliquer avec la souris.

**Analogie :**
- Interface graphique = conduire une voiture automatique (facile mais limité)
- Ligne de commande = conduire une voiture manuelle (plus complexe mais plus de contrôle)

### Pourquoi utiliser la ligne de commande ?

**Avantages :**
- **Plus rapide** : une commande au lieu de plusieurs clics
- **Plus puissant** : opérations impossibles en interface graphique
- **Automatisable** : créer des scripts pour répéter des tâches
- **Indispensable** : certains outils n'ont pas d'interface graphique
- **Serveurs** : les serveurs n'ont souvent pas d'interface graphique

**Pour la programmation :**
- Compiler des programmes
- Exécuter des tests
- Gérer les versions (Git)
- Automatiser la compilation
- Diagnostiquer des problèmes

### Terminologie

**Terminal / Console :**
- La fenêtre où vous tapez les commandes
- L'interface physique ou logicielle

**Shell :**
- Le programme qui interprète vos commandes
- Différents shells : Bash (Linux), PowerShell (Windows), CMD (Windows)

**Invite de commande / Prompt :**
- Le texte qui précède votre curseur
- Indique que le système attend une commande
- Exemple Windows : `C:\Users\Alice>`
- Exemple Linux : `alice@ubuntu:~$`

**Commande :**
- L'instruction que vous tapez
- Suivie de la touche Entrée pour l'exécuter

## Accéder à la ligne de commande

### Sous Windows

**Méthode 1 : Via le menu Démarrer**
1. Cliquer sur le menu Démarrer
2. Taper "cmd" ou "invite de commandes"
3. Appuyer sur Entrée

**Méthode 2 : Via Exécuter**
1. Appuyer sur `Windows + R`
2. Taper `cmd`
3. Appuyer sur Entrée

**Méthode 3 : PowerShell (recommandé)**
1. Clic droit sur le menu Démarrer
2. Sélectionner "Windows PowerShell" ou "Terminal Windows"

**Méthode 4 : Dans un dossier**
1. Ouvrir l'Explorateur de fichiers
2. Naviguer vers le dossier désiré
3. Taper `cmd` dans la barre d'adresse
4. Appuyer sur Entrée

**Apparence de l'invite Windows (CMD) :**
```
Microsoft Windows [Version 10.0.19044.1234]
(c) Microsoft Corporation. All rights reserved.

C:\Users\VotreNom>
```

**Apparence de PowerShell :**
```
Windows PowerShell
Copyright (C) Microsoft Corporation. All rights reserved.

PS C:\Users\VotreNom>
```

### Sous Linux (Ubuntu)

**Méthode 1 : Via le menu Applications**
1. Cliquer sur "Afficher les applications"
2. Chercher "Terminal"
3. Cliquer sur l'icône

**Méthode 2 : Raccourci clavier (le plus rapide)**
- Appuyer sur `Ctrl + Alt + T`

**Méthode 3 : Via le menu contextuel**
1. Ouvrir le gestionnaire de fichiers
2. Naviguer vers le dossier désiré
3. Clic droit dans le dossier
4. Sélectionner "Ouvrir dans un terminal"

**Apparence du terminal Ubuntu (Bash) :**
```
alice@ubuntu:~$
```

Décomposition :
- `alice` : nom de l'utilisateur
- `ubuntu` : nom de l'ordinateur
- `~` : dossier actuel (~ signifie le dossier personnel)
- `$` : vous êtes utilisateur normal (# pour root/administrateur)

## Structure de base d'une commande

### Syntaxe générale

```
commande [options] [arguments]
```

**Exemple :**
```
ls -l /home
│  │  │
│  │  └─ Argument (sur quoi agir)
│  └──── Option (comment agir)
└─────── Commande (quoi faire)
```

**Éléments :**
- **Commande** : l'action à effectuer (obligatoire)
- **Options** : modifient le comportement (facultatives)
  - Forme courte : `-l`, `-a`
  - Forme longue : `--list`, `--all`
  - Peuvent être combinées : `-la` = `-l -a`
- **Arguments** : cibles de la commande (facultatifs ou obligatoires selon la commande)

## Navigation dans le système de fichiers

### Comprendre les chemins

**Chemin absolu :**
- Commence depuis la racine du système
- Windows : `C:\Users\Alice\Documents\projet.pas`
- Linux : `/home/alice/Documents/projet.pas`

**Chemin relatif :**
- Relatif au dossier actuel
- `.` : dossier actuel
- `..` : dossier parent
- Exemple : `./sous-dossier/fichier.txt`

### Commandes de navigation

#### Windows (CMD / PowerShell)

**Afficher le dossier actuel :**
```cmd
cd
```
Ou sous PowerShell :
```powershell
pwd
```

**Changer de dossier :**
```cmd
cd C:\Users\Alice\Documents
cd Documents              REM relatif
cd ..                     REM dossier parent
cd \                      REM racine du lecteur
```

**Changer de lecteur :**
```cmd
D:                        REM passer au lecteur D:
C:                        REM revenir au lecteur C:
```

**Lister le contenu :**
```cmd
dir                       REM liste simple
dir /w                    REM affichage en colonnes
dir /p                    REM page par page
dir *.pas                 REM seulement fichiers .pas
```

#### Linux (Bash)

**Afficher le dossier actuel :**
```bash
pwd                       # Print Working Directory
```

**Changer de dossier :**
```bash
cd /home/alice/Documents  # absolu
cd Documents              # relatif
cd ..                     # dossier parent
cd ~                      # dossier personnel
cd -                      # dossier précédent
cd /                      # racine
```

**Lister le contenu :**
```bash
ls                        # liste simple
ls -l                     # liste détaillée
ls -a                     # afficher fichiers cachés
ls -la                    # combinaison
ls -lh                    # tailles lisibles (h = human)
ls *.pas                  # seulement fichiers .pas
```

**Comparaison ls -l :**
```
-rw-r--r-- 1 alice alice 1234 Oct 11 10:30 programme.pas
│││││││││  │ │     │     │    │           │
│││││││││  │ │     │     │    │           └─ Nom du fichier
│││││││││  │ │     │     │    └──────────── Date de modification
│││││││││  │ │     │     └───────────────── Taille en octets
│││││││││  │ │     └─────────────────────── Groupe propriétaire
│││││││││  │ └───────────────────────────── Utilisateur propriétaire
│││││││││  └─────────────────────────────── Nombre de liens
│││││││└──────────────────────────────────── Permissions autres
││││││└───────────────────────────────────── Permissions groupe
│││││└────────────────────────────────────── Permissions propriétaire
││││└─────────────────────────────────────── Exécutable
│││└──────────────────────────────────────── Écriture
││└───────────────────────────────────────── Lecture
│└────────────────────────────────────────── Type (- = fichier, d = dossier)
```

## Manipulation de fichiers et dossiers

### Windows (CMD)

**Créer un dossier :**
```cmd
mkdir MonProjet
md MonProjet              REM équivalent court
```

**Supprimer un dossier vide :**
```cmd
rmdir MonDossier
```

**Supprimer un dossier et son contenu :**
```cmd
rmdir /s MonDossier       REM /s = suppression récursive
rmdir /s /q MonDossier    REM /q = sans confirmation
```

**Copier un fichier :**
```cmd
copy fichier.pas copie.pas
copy fichier.pas C:\Backup\
```

**Déplacer un fichier :**
```cmd
move fichier.pas C:\Nouveau\
```

**Renommer un fichier :**
```cmd
ren ancien.pas nouveau.pas
rename ancien.pas nouveau.pas
```

**Supprimer un fichier :**
```cmd
del fichier.pas
del *.bak                 REM supprimer tous les .bak
```

**Afficher le contenu d'un fichier :**
```cmd
type fichier.txt
more fichier.txt          REM page par page
```

**Rechercher un fichier :**
```cmd
dir /s fichier.pas        REM recherche récursive
```

### Linux (Bash)

**Créer un dossier :**
```bash
mkdir MonProjet
mkdir -p parent/enfant/petit-enfant    # crée tous les dossiers intermédiaires
```

**Supprimer un dossier vide :**
```bash
rmdir MonDossier
```

**Supprimer un dossier et son contenu :**
```bash
rm -r MonDossier          # -r = récursif
rm -rf MonDossier         # -f = force, sans confirmation (ATTENTION !)
```

**Copier un fichier :**
```bash
cp fichier.pas copie.pas
cp fichier.pas /backup/
cp -r dossier/ copie/     # copier un dossier
```

**Déplacer/Renommer un fichier :**
```bash
mv fichier.pas nouveau.pas              # renommer
mv fichier.pas /nouveau/emplacement/    # déplacer
```

**Supprimer un fichier :**
```bash
rm fichier.pas
rm *.bak                  # supprimer tous les .bak
```

**Afficher le contenu d'un fichier :**
```bash
cat fichier.txt           # tout le contenu
less fichier.txt          # page par page (q pour quitter)
head fichier.txt          # 10 premières lignes
tail fichier.txt          # 10 dernières lignes
head -n 20 fichier.txt    # 20 premières lignes
```

**Rechercher un fichier :**
```bash
find . -name "fichier.pas"              # depuis le dossier actuel
find /home -name "*.pas"                # tous les .pas
locate fichier.pas                      # recherche dans une base de données
```

**Permissions (Linux uniquement) :**
```bash
chmod +x programme        # rendre exécutable
chmod 755 programme       # rwxr-xr-x
chmod 644 fichier.txt     # rw-r--r--
```

## Opérations courantes utiles

### Windows

**Effacer l'écran :**
```cmd
cls
```

**Obtenir de l'aide :**
```cmd
help                      REM liste des commandes
help dir                  REM aide sur la commande dir
dir /?                    REM aide sur dir
```

**Afficher des variables d'environnement :**
```cmd
echo %PATH%
echo %USERNAME%
set                       REM toutes les variables
```

**Créer une variable temporaire :**
```cmd
set MAVAR=valeur
echo %MAVAR%
```

**Historique des commandes :**
- Flèche ↑ : commande précédente
- Flèche ↓ : commande suivante
- F7 : afficher l'historique complet

**Complétion automatique :**
- Tab : compléter noms de fichiers/dossiers

### Linux

**Effacer l'écran :**
```bash
clear                     # ou Ctrl+L
```

**Obtenir de l'aide :**
```bash
man ls                    # manuel de la commande ls
ls --help                 # aide rapide
info ls                   # documentation info
```

**Afficher des variables d'environnement :**
```bash
echo $PATH
echo $USER
echo $HOME
env                       # toutes les variables
printenv                  # équivalent
```

**Créer une variable temporaire :**
```bash
MAVAR="valeur"
export MAVAR="valeur"     # pour la rendre disponible aux sous-processus
echo $MAVAR
```

**Historique des commandes :**
```bash
history                   # afficher l'historique
!123                      # exécuter la commande numéro 123
!!                        # répéter la dernière commande
!ls                       # répéter la dernière commande commençant par ls
```
- Flèche ↑ : commande précédente
- Flèche ↓ : commande suivante
- Ctrl+R : rechercher dans l'historique

**Complétion automatique :**
- Tab : compléter noms de fichiers/dossiers/commandes
- Tab Tab : afficher toutes les possibilités

**Rechercher dans un fichier :**
```bash
grep "mot" fichier.txt              # rechercher "mot"
grep -i "mot" fichier.txt           # insensible à la casse
grep -r "mot" /dossier              # recherche récursive
```

## Utilisation pour la programmation Pascal

### Compiler un programme FreePascal

**Windows :**
```cmd
REM Naviguer vers le dossier du projet
cd C:\MesProjets\Pascal

REM Compiler
fpc programme.pas

REM Exécuter
programme.exe

REM Compiler avec options
fpc -O3 programme.pas              REM Optimisation niveau 3
fpc -dDEBUG programme.pas          REM Définir symbole DEBUG
```

**Linux :**
```bash
# Naviguer vers le dossier du projet
cd ~/MesProjets/Pascal

# Compiler
fpc programme.pas

# Rendre exécutable (si nécessaire)
chmod +x programme

# Exécuter
./programme

# Compiler avec options
fpc -O3 programme.pas              # Optimisation niveau 3
fpc -dDEBUG programme.pas          # Définir symbole DEBUG
```

### Options de compilation utiles

```bash
fpc -h                              # Aide du compilateur
fpc -i                              # Informations sur le compilateur
fpc -l                              # Afficher le logo
fpc -Mobjfpc programme.pas          # Mode Object Pascal
fpc -gl programme.pas               # Infos de débogage
fpc -B programme.pas                # Tout recompiler
fpc -vewn programme.pas             # Mode verbeux (erreurs, warnings, notes)
```

### Organiser vos projets

**Structure recommandée :**
```
MonProjet/
├── src/                            # Code source
│   ├── main.pas
│   └── utils.pas
├── bin/                            # Exécutables compilés
├── lib/                            # Fichiers objets (.o, .ppu)
└── backup/                         # Sauvegardes
```

**Compiler dans un dossier spécifique :**
```bash
# Linux/Windows
fpc -FUlib -FEbin src/main.pas
#   │      │      │
#   │      │      └── Fichier source
#   │      └────────── Dossier pour l'exécutable
#   └───────────────── Dossier pour les fichiers objets
```

## Redirection et pipes

### Redirection de sortie

**Rediriger vers un fichier (écrase) :**
```bash
# Windows et Linux
dir > liste.txt           # Windows
ls > liste.txt            # Linux
```

**Rediriger vers un fichier (ajoute) :**
```bash
# Windows et Linux
dir >> liste.txt          # Windows
ls >> liste.txt           # Linux
```

**Rediriger les erreurs (Linux) :**
```bash
fpc programme.pas 2> erreurs.txt            # seulement les erreurs
fpc programme.pas > sortie.txt 2>&1         # tout dans un fichier
```

### Pipes (enchaîner des commandes)

**Linux :**
```bash
ls -l | grep ".pas"                         # lister seulement les .pas
cat fichier.txt | grep "erreur" | wc -l     # compter les lignes avec "erreur"
history | grep "fpc"                        # historique des compilations
```

**Windows (PowerShell) :**
```powershell
dir | Select-String ".pas"
Get-Content fichier.txt | Select-String "erreur"
```

## Scripts basiques

### Batch Windows (.bat)

**Créer un fichier `compiler.bat` :**
```batch
@echo off
REM Script de compilation pour Windows

echo Compilation en cours...
fpc -O3 -FUlib -FEbin src\main.pas

if errorlevel 1 (
    echo Erreur de compilation!
    pause
    exit /b 1
)

echo Compilation réussie!
echo Exécution du programme...
bin\main.exe
pause
```

**Exécuter :**
```cmd
compiler.bat
```

### Shell Linux (.sh)

**Créer un fichier `compiler.sh` :**
```bash
#!/bin/bash
# Script de compilation pour Linux

echo "Compilation en cours..."
fpc -O3 -FUlib -FEbin src/main.pas

if [ $? -ne 0 ]; then
    echo "Erreur de compilation!"
    exit 1
fi

echo "Compilation réussie!"
echo "Exécution du programme..."
./bin/main
```

**Rendre exécutable et exécuter :**
```bash
chmod +x compiler.sh
./compiler.sh
```

## Astuces et raccourcis

### Raccourcis clavier communs

**Windows (CMD/PowerShell) :**
- `Tab` : Complétion automatique
- `Ctrl+C` : Interrompre la commande en cours
- `Flèches ↑↓` : Naviguer dans l'historique
- `F7` : Afficher l'historique
- `Ctrl+V` : Coller (PowerShell)
- Clic droit : Coller (CMD)

**Linux (Bash) :**
- `Tab` : Complétion automatique
- `Ctrl+C` : Interrompre la commande en cours
- `Ctrl+D` : Fermer le terminal (EOF)
- `Ctrl+L` : Effacer l'écran (comme `clear`)
- `Ctrl+A` : Début de ligne
- `Ctrl+E` : Fin de ligne
- `Ctrl+U` : Effacer la ligne
- `Ctrl+R` : Recherche dans l'historique
- `Ctrl+Z` : Suspendre le processus
- `Flèches ↑↓` : Naviguer dans l'historique

### Caractères spéciaux et échappement

**Caractères à échapper :**

**Windows :**
- Espace dans un nom : utiliser des guillemets
  ```cmd
  cd "Program Files"
  copy "mon fichier.pas" backup\
  ```

**Linux :**
- Espace : utiliser `\` ou des guillemets
  ```bash
  cd /home/user/Mes\ Documents
  cd "/home/user/Mes Documents"
  ```

### Wildcards (jokers)

**Symboles communs :**
- `*` : remplace n'importe quelle suite de caractères
- `?` : remplace exactement un caractère

**Exemples :**
```bash
# Windows et Linux
dir *.pas                 # tous les fichiers .pas
ls *.pas

dir projet?.pas           # projet1.pas, projetA.pas, etc.
ls projet?.pas

dir *.p??                 # .pas, .ppu, etc. (p + 2 caractères exactement)
ls *.p??
```

## Commandes système utiles

### Informations système

**Windows :**
```cmd
systeminfo                # Infos système complètes
ver                       # Version de Windows
hostname                  # Nom de l'ordinateur
whoami                    # Utilisateur actuel
tasklist                  # Processus en cours
taskkill /PID 1234        # Tuer un processus
ipconfig                  # Configuration réseau
ping google.com           # Tester la connexion
```

**Linux :**
```bash
uname -a                  # Infos système
hostname                  # Nom de l'ordinateur
whoami                    # Utilisateur actuel
ps aux                    # Processus en cours
kill 1234                 # Tuer un processus
killall firefox           # Tuer tous les processus firefox
ifconfig                  # Configuration réseau (ancien)
ip addr                   # Configuration réseau (moderne)
ping google.com           # Tester la connexion
df -h                     # Espace disque
free -h                   # Mémoire disponible
top                       # Moniteur de processus
htop                      # Moniteur amélioré (si installé)
```

### Gestion des paquets (Linux)

**Ubuntu/Debian (APT) :**
```bash
sudo apt update                     # Mettre à jour la liste des paquets
sudo apt upgrade                    # Mettre à jour les paquets installés
sudo apt install fpc                # Installer FreePascal
sudo apt install lazarus            # Installer Lazarus
sudo apt remove nomdupaquet         # Désinstaller
sudo apt search motclé              # Rechercher un paquet
sudo apt autoremove                 # Nettoyer les paquets inutiles
```

## Bonnes pratiques

### 1. Attention aux commandes dangereuses

**Commandes destructives :**
```bash
# DANGER : supprime TOUT sans confirmation
rm -rf /                  # Linux (NE JAMAIS FAIRE!)
rd /s /q C:\              # Windows (NE JAMAIS FAIRE!)
```

**Toujours vérifier avant de supprimer :**
```bash
# Bon
ls *.tmp                  # vérifier ce qui sera supprimé
rm *.tmp                  # puis supprimer
```

### 2. Utilisez la complétion automatique

- Tapez les premières lettres
- Appuyez sur Tab
- Gagne du temps et évite les erreurs de frappe

### 3. Lisez les messages d'erreur

Les messages d'erreur contiennent des informations utiles :
```
fpc: Can't open file "programm.pas"
     └─ Vérifiez l'orthographe du nom de fichier
```

### 4. Sauvegardez avant de tester

Avant d'exécuter des commandes de suppression ou modification :
```bash
# Faire une copie de sécurité
cp important.pas important.pas.bak
```

### 5. Utilisez l'historique

Ne retapez pas les mêmes commandes :
- Flèche ↑ pour retrouver les commandes précédentes
- `Ctrl+R` (Linux) pour rechercher dans l'historique

### 6. Organisez vos fichiers

Utilisez une structure de dossiers claire :
```
MesProjets/
├── projet1/
│   ├── src/
│   ├── bin/
│   └── backup/
├── projet2/
│   ├── src/
│   └── bin/
└── tests/
```

### 7. Documentez vos scripts

Ajoutez des commentaires dans vos scripts :
```bash
# Windows
REM Ceci compile le projet principal

# Linux
# Ceci compile le projet principal
```

## Résolution de problèmes courants

### "Command not found" (Linux) ou "'xxx' n'est pas reconnu..." (Windows)

**Cause :** La commande n'est pas dans le PATH ou n'existe pas

**Solutions :**
1. Vérifier l'orthographe
2. Installer le logiciel si absent
3. Ajouter le dossier au PATH
4. Utiliser le chemin complet : `/usr/bin/fpc` ou `C:\lazarus\fpc.exe`

### "Permission denied" (Linux)

**Cause :** Vous n'avez pas les droits nécessaires

**Solutions :**
```bash
sudo commande             # exécuter en tant qu'administrateur
chmod +x fichier          # donner les droits d'exécution
```

### "Access denied" (Windows)

**Cause :** Vous n'avez pas les droits d'administrateur

**Solutions :**
- Exécuter CMD en tant qu'administrateur
- Vérifier les permissions du fichier

### Caractères bizarres ou accents mal affichés

**Cause :** Problème d'encodage

**Solutions Windows :**
```cmd
chcp 65001                # UTF-8
chcp 1252                 # Latin1 (défaut Windows occidental)
```

**Solutions Linux :**
```bash
export LANG=fr_FR.UTF-8
```

## Conclusion

La ligne de commande est un outil puissant qui peut sembler complexe au début, mais qui devient rapidement indispensable. Avec la pratique, vous serez plus efficace qu'avec une interface graphique pour de nombreuses tâches.

**Points clés à retenir :**
- La ligne de commande permet de contrôler l'ordinateur par des commandes textuelles
- Windows utilise CMD ou PowerShell, Linux utilise Bash
- Les commandes de base sont similaires mais avec des noms différents
- `cd` pour naviguer, `dir`/`ls` pour lister, `mkdir` pour créer
- FreePascal se compile avec `fpc nomfichier.pas`
- Les scripts permettent d'automatiser les tâches répétitives
- La complétion automatique (Tab) et l'historique vous font gagner du temps

**Commandes essentielles à retenir :**

| Action | Windows | Linux |
|--------|---------|-------|
| Lister | `dir` | `ls` |
| Changer dossier | `cd` | `cd` |
| Créer dossier | `mkdir` | `mkdir` |
| Copier | `copy` | `cp` |
| Déplacer | `move` | `mv` |
| Supprimer | `del` / `rmdir` | `rm` |
| Afficher fichier | `type` | `cat` |
| Effacer écran | `cls` | `clear` |
| Aide | `help` / `/?` | `man` / `--help` |

**Conseils pour progresser :**
1. Pratiquez régulièrement
2. Utilisez la complétion automatique
3. Consultez l'aide des commandes
4. Créez des scripts pour vos tâches courantes
5. N'ayez pas peur d'expérimenter (dans des dossiers de test)

Dans la prochaine section, nous découvrirons les éditeurs de texte et les environnements de développement, notamment Lazarus IDE que nous utiliserons pour programmer en Pascal.

⏭️ [Éditeurs de texte et environnements de développement](/01-prerequis-bases-programmation/09-editeurs-texte-environnements-developpement.md)
