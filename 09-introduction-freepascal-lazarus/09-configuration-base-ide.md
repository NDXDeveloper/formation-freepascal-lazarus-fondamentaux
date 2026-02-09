🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.9 Configuration de base de l'IDE

## Introduction

Lazarus est livré avec des paramètres par défaut qui fonctionnent bien, mais chaque développeur a ses préférences. Certains aiment un éditeur sombre, d'autres préfèrent des polices grandes, certains veulent des fenêtres ancrées, d'autres préfèrent des fenêtres flottantes...

Cette section vous guide dans la configuration de Lazarus pour qu'il corresponde à vos besoins et habitudes. Un IDE bien configuré, c'est un IDE dans lequel vous êtes à l'aise et donc plus productif !

**Rassurez-vous !** Toutes les modifications sont réversibles. Vous pouvez expérimenter sans risque.

## Accéder aux options de configuration

### Fenêtre principale des options

**Menu** : Tools (Outils) → Options

Une grande fenêtre s'ouvre avec une arborescence à gauche listant toutes les catégories de paramètres.

**Structure de la fenêtre :**
- **Panneau gauche** : arborescence des catégories
- **Panneau droit** : options de la catégorie sélectionnée
- **Boutons en bas** : OK, Cancel, Help

**Navigation :**
- Cliquez sur une catégorie dans l'arborescence pour voir ses options
- Les catégories avec un **+** peuvent être dépliées (contiennent des sous-catégories)
- Utilisez la recherche (si disponible) pour trouver une option spécifique

**Appliquer les changements :**
1. Modifiez les paramètres désirés
2. Cliquez **OK** pour sauvegarder
3. Certains changements nécessitent un redémarrage de Lazarus

### Autres fenêtres de configuration

**Options du projet :**
Menu : Project → Project Options
→ Configuration spécifique au projet actuel

**Options de l'éditeur (accès rapide) :**
Menu : Tools → Options → Editor
→ Raccourci vers les paramètres de l'éditeur

**Configuration des packages :**
Menu : Package → Configure Installed Packages
→ Gestion des extensions

## Configuration de l'environnement général

### Catégorie : Environment → General

**Chemin :** Tools → Options → Environment → General

C'est ici que se trouvent les paramètres globaux de l'IDE.

#### Langue de l'interface

**Option :** Language

**Choix disponibles :**
- English (Anglais)
- Français
- Deutsch (Allemand)
- Español (Espagnol)
- Italiano (Italien)
- Polski (Polonais)
- Русский (Russe)
- Et autres...

**Recommandation pour débutant :** Choisissez votre langue maternelle si disponible.

**Note importante :** Après changement de langue, vous devez **redémarrer Lazarus** pour que la modification prenne effet.

**Traduction partielle :** Certains termes techniques restent en anglais, c'est normal.

#### Thème de l'interface

**Option :** Desktop Theme (thème du bureau)

Certaines versions de Lazarus offrent des thèmes visuels :
- **Default** : thème par défaut
- **Dark** : thème sombre (reposant pour les yeux)
- **Light** : thème clair et lumineux

**Dépend de votre système :** Tous les thèmes ne sont pas disponibles partout.

#### Sauvegarde automatique

**Options disponibles :**

**Auto save editor files** (Sauvegarde auto des fichiers de l'éditeur)
- Intervalle en minutes
- Recommandation : **5-10 minutes**
- ✅ Activez cette option ! Protection contre les crashs

**Auto save project** (Sauvegarde auto du projet)
- Sauvegarde automatique du .lpi
- Recommandation : ✅ Activez
- Intervalle : **10-15 minutes**

**Auto save interval** (Intervalle de sauvegarde)
- Temps entre deux sauvegardes automatiques
- Recommandation : **10 minutes** (compromis entre sécurité et performance)

#### Fichiers récents

**Option :** Max recent open files

Nombre de projets récents affichés dans le menu File → Open Recent

**Recommandation :** **10-15 projets** (assez pour retrouver facilement vos projets sans encombrer le menu)

#### Sauvegarde de la position des fenêtres

**Option :** Desktop settings / Window positions

**Store window positions in** :
- **Project session file** : position sauvegardée dans le .lps (spécifique au projet)
- **IDE config file** : position globale pour tous les projets

**Recommandation pour débutant :** Laissez sur "IDE config file" pour avoir la même disposition partout.

### Catégorie : Environment → Files

**Chemin :** Tools → Options → Environment → Files

Définit les chemins importants pour Lazarus.

#### Répertoire des projets par défaut

**Option :** Default directory for projects

C'est le dossier proposé par défaut quand vous créez ou ouvrez un projet.

**Recommandation :**
- Windows : `C:\Users\VotreNom\Documents\MesProjetsLazarus`
- Linux : `~/Documents/MesProjetsLazarus` ou `~/Projets`

**Créez ce dossier s'il n'existe pas !**

#### Répertoire du compilateur

**Option :** Compiler path / FPC path

Chemin vers le compilateur FreePascal (fpc.exe ou fpc)

**Valeur typique :**
- Windows : `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe`
- Linux : `/usr/bin/fpc`

**Important :** Ne modifiez que si vous savez ce que vous faites !

#### Répertoire des sources FreePascal

**Option :** FPC source directory

Chemin vers le code source de FreePascal (utile pour comprendre le fonctionnement interne)

**Valeur typique :**
- Windows : `C:\lazarus\fpc\3.2.2\source`
- Linux : `/usr/share/fpcsrc/` ou `/usr/lib/fpc/src/`

#### Répertoire de la documentation

**Option :** Documentation directory

Où Lazarus cherche la documentation hors ligne.

**Généralement configuré automatiquement** lors de l'installation.

### Catégorie : Environment → Backup

**Chemin :** Tools → Options → Environment → Backup

Configuration des sauvegardes automatiques de fichiers.

#### Activer les backups

**Option :** Create backup of files

✅ **Cochez cette option !**

**Pourquoi ?** Protection contre :
- Les modifications accidentelles
- Les suppressions involontaires
- Les erreurs de manipulation

#### Sous-répertoire de backup

**Option :** Backup subdirectory

Nom du dossier où seront stockées les sauvegardes.

**Recommandation :** `backup` (valeur par défaut)

**Résultat :** Un dossier `backup/` sera créé dans votre projet avec les fichiers .bak

#### Extension de backup

**Option :** Backup file extension

Extension ajoutée aux fichiers sauvegardés.

**Par défaut :** `.bak`

**Exemples :**
- `unit1.pas` → `unit1.pas.bak`
- `MonAppli.lpr` → `MonAppli.lpr.bak`

#### Nombre de backups

**Option :** Maximum backup file counter

Nombre de versions à conserver par fichier.

**Recommandation :** **3-5 versions** (compromis entre sécurité et espace disque)

## Configuration de l'éditeur de code

### Catégorie : Editor → General

**Chemin :** Tools → Options → Editor → General

Paramètres généraux de l'éditeur de code.

#### Options d'édition

**Undo limit** (Limite d'annulation)
- Nombre d'actions qu'on peut annuler (Ctrl+Z)
- **Recommandation :** 100-500 (assez pour revenir en arrière sans problème)

**Tab indents blocks** (Tabulation indente les blocs)
- ✅ Cochez : la touche Tab indente le bloc de code sélectionné
- Pratique pour indenter plusieurs lignes d'un coup

**Tab width** (Largeur des tabulations)
- Nombre d'espaces pour une tabulation
- **Recommandation :** **2 ou 4 espaces**
- Standard Pascal : généralement 2

**Block indent** (Indentation de bloc)
- Nombre d'espaces ajoutés lors de l'indentation
- **Recommandation :** identique à Tab width (2 ou 4)

**Auto indent** (Indentation automatique)
- ✅ Cochez : après Entrée, le curseur se place au même niveau que la ligne précédente
- **Très utile !** Garde votre code bien indenté

#### Options de complétion

**Auto complete block** (Complétion auto des blocs)
- ✅ Cochez : quand vous tapez `begin`, Lazarus ajoute automatiquement `end;`

**Auto close parenthesis** (Fermeture auto des parenthèses)
- ✅ Cochez : quand vous tapez `(`, Lazarus ajoute automatiquement `)`
- Idem pour `[`, `{`, `'`, `"`

### Catégorie : Editor → Display

**Chemin :** Tools → Options → Editor → Display

Apparence visuelle de l'éditeur.

#### Police de caractères

**Option :** Font / Editor Font

**Cliquez sur le bouton [...] pour choisir :**

**Polices recommandées (monospace) :**
- **Windows :**
  - Consolas (moderne, claire)
  - Courier New (classique)
  - Lucida Console
- **Linux :**
  - DejaVu Sans Mono
  - Liberation Mono
  - Ubuntu Mono
- **Multi-plateforme :**
  - Source Code Pro
  - Fira Code (avec ligatures)
  - JetBrains Mono

**Taille recommandée :**
- Écran normal (15-17") : **10-12**
- Grand écran (>20") : **12-14**
- Tablette/petit écran : **14-16**
- Haute résolution (4K) : **14-18**

**Conseil :** Choisissez une taille confortable pour vos yeux. Vous devez pouvoir lire sans effort !

#### Afficher les numéros de ligne

**Option :** Show line numbers

✅ **Cochez absolument !**

**Pourquoi ?**
- Facilite le débogage (les erreurs indiquent le numéro de ligne)
- Permet de naviguer rapidement (Ctrl+G pour aller à la ligne X)
- Référence pour discuter de code avec d'autres

#### Afficher les marges

**Option :** Show gutter / Visible gutter

✅ Cochez : affiche la barre grise à gauche avec les numéros de ligne

**Right margin** (Marge droite)
- Position : **80 ou 120 caractères**
- Affiche une ligne verticale indiquant où couper les lignes longues
- ✅ Utile pour garder un code lisible

#### Espaces et tabulations visibles

**Option :** Visible tabs / Show special chars

Affiche des symboles pour les espaces et tabulations.

**Recommandation pour débutant :** ❌ Décochez (peut être distrayant)

**Utile pour :** Déboguer des problèmes d'indentation

### Catégorie : Editor → Colors

**Chemin :** Tools → Options → Editor → Colors

Configuration des couleurs de l'éditeur (coloration syntaxique).

#### Schémas de couleurs

**Option :** Color Scheme

**Schémas prédéfinis :**

**Thèmes clairs :**
- **Default** : couleurs classiques Lazarus (fond blanc)
- **Delphi Classic** : style Delphi (fond blanc, bleu et vert)
- **Twilight** : couleurs douces

**Thèmes sombres :**
- **Dark** : fond noir, texte clair
- **Monokai** : populaire, fond sombre
- **Solarized Dark** : scientifiquement optimisé pour les yeux

**Recommandation :**
- **Journée** : Default ou Delphi Classic
- **Soirée/nuit** : Dark ou Monokai (repose les yeux)

**Test :** Essayez plusieurs schémas et gardez celui dans lequel vous vous sentez bien !

#### Personnalisation des couleurs

Vous pouvez modifier individuellement les couleurs de chaque élément :

**Éléments configurables :**
- **Text** : texte normal
- **Reserved words** : mots-clés (begin, end, if, while...)
- **Strings** : chaînes de caractères ('texte')
- **Numbers** : nombres (42, 3.14)
- **Comments** : commentaires (// ou { })
- **Symbols** : opérateurs (+, -, :=, ;)
- **Directives** : directives de compilation ({$mode})

**Pour modifier :**
1. Sélectionnez l'élément dans la liste
2. Cliquez sur "Foreground" (couleur du texte)
3. Choisissez une couleur
4. Cochez "Bold" ou "Italic" si désiré

**Conseil :** Les schémas prédéfinis sont bien conçus. Personnalisez seulement si vous avez une raison précise.

### Catégorie : Editor → Code Tools

**Chemin :** Tools → Options → Editor → Code Tools

Outils d'aide à l'écriture de code.

#### Code completion (Auto-complétion)

**Option :** Automatic features / Code completion

✅ **Activez tout !**

**Auto invoke** : Délai avant déclenchement automatique
- **Recommandation :** 500-1000 ms (demi-seconde à 1 seconde)

**Show automatically** : Affiche automatiquement les suggestions
- ✅ Cochez

**Case sensitive** : Sensible à la casse
- ❌ Décochez (plus flexible pour les débutants)

#### Identifier completion

**Option :** Automatic identifier completion

Complète automatiquement le nom des variables, fonctions...

✅ Activez

**Min length** : Longueur minimum pour déclencher
- **Recommandation :** 3 caractères

**Exemple :**
- Tapez `Wri` + Ctrl+Espace → `WriteLn` proposé

#### Code templates (Modèles)

**Option :** Code Templates

Modèles de code réutilisables.

**Exemples de templates par défaut :**
- `beginend` + Tab → génère `begin end;`
- `ifb` + Tab → génère `if condition then begin end;`
- `forb` + Tab → génère `for i := 1 to 10 do begin end;`

**Personnalisation :**
Vous pouvez créer vos propres templates (nous verrons cela plus tard).

### Catégorie : Editor → Key Mappings

**Chemin :** Tools → Options → Editor → Key Mappings

Configuration des raccourcis clavier de l'éditeur.

#### Schémas de clavier

**Option :** Key mapping scheme

**Schémas prédéfinis :**
- **Lazarus** : raccourcis par défaut de Lazarus
- **Classic** : raccourcis classiques
- **Delphi** : comme Delphi/Turbo Pascal
- **Visual Studio** : comme VS Code
- **Emacs** : pour les fans d'Emacs

**Recommandation pour débutant :** Gardez **Lazarus** (c'est cohérent avec les menus)

#### Personnaliser les raccourcis

Vous pouvez modifier n'importe quel raccourci :

1. Cherchez la commande dans la liste (ex: "Save File")
2. Double-cliquez sur le raccourci actuel
3. Appuyez sur la nouvelle combinaison de touches
4. Cliquez OK

**Commandes utiles à connaître :**
- Save File : Ctrl+S
- Open File : Ctrl+O
- Find : Ctrl+F
- Find Next : F3
- Go to Line : Ctrl+G
- Code Completion : Ctrl+Space
- Comment Selection : Ctrl+/

**Conseil :** Ne changez les raccourcis que si vous avez une bonne raison (habitudes d'un autre éditeur).

## Configuration du concepteur de formulaires

### Catégorie : Object Inspector

**Chemin :** Tools → Options → Object Inspector

Paramètres de l'inspecteur d'objets.

#### Mode d'affichage

**Option :** Show property list / Component tree

**Choix :**
- **Show all properties** : Afficher toutes les propriétés
- **Show published properties only** : Seulement les propriétés publiées (recommandé)

**Recommandation :** "Published properties only" (évite la confusion avec trop de propriétés)

#### Grouper les propriétés

**Option :** Property name grouping

**Choix :**
- **None** : Ordre alphabétique simple
- **By category** : Groupées par catégorie (Action, Appearance, Layout...)

**Recommandation pour débutant :** **By category** (plus logique pour comprendre le rôle de chaque propriété)

#### Afficher les hints

**Option :** Show property hints

✅ Cochez : Affiche une info-bulle descriptive quand vous survolez une propriété

**Très utile pour apprendre !**

### Catégorie : Designer / Form Designer

**Chemin :** Tools → Options → Designer

Configuration du concepteur visuel de formulaires.

#### Grille de conception

**Option :** Grid options

**Show grid** (Afficher la grille)
- ✅ Cochez : Affiche des points de grille sur le formulaire
- Aide à aligner visuellement les composants

**Snap to grid** (Accrocher à la grille)
- ✅ Cochez : Les composants s'alignent automatiquement sur la grille
- Facilite un alignement propre

**Grid size X / Y** (Taille de la grille)
- **Recommandation :** 8×8 pixels (bon compromis)
- Plus petit = alignement précis mais parfois difficile
- Plus grand = alignement grossier mais rapide

#### Guides d'alignement

**Option :** Show guides

✅ Activez : Affiche des lignes d'aide quand vous déplacez des composants

**Très pratique** pour aligner plusieurs boutons, labels, etc.

#### Création de composants

**Option :** Double click in component palette creates component

**Choix :**
- ✅ Cochez : Double-clic sur un composant le place au centre du formulaire (rapide)
- ❌ Décochez : Un clic sur le composant, puis un clic sur le formulaire (contrôle précis)

**Recommandation pour débutant :** ✅ Cochez (plus rapide)

## Configuration de la compilation

### Catégorie : Project Options (spécifique au projet)

**Menu :** Project → Project Options

**Note :** Ces options sont sauvegardées dans le .lpi et sont donc spécifiques au projet actuel.

#### Application Settings

**Section :** Application

**Title** : Titre de l'application
- Apparaît dans la barre de titre, la barre des tâches

**Icon** : Icône de l'application
- Cliquez "Load Icon" pour choisir un fichier .ico (Windows) ou .png (Linux)

**Use Application Bundle** (macOS uniquement)
- Pour créer un .app sur macOS

#### Compiler Options

**Section :** Compiler Options

**Target** : Plateforme cible (OS + CPU)
- Généralement configuré automatiquement
- Ne changez que pour la cross-compilation

**Optimization Level** :
- Debug : -O1 ou -O0
- Release : -O2

**Debugging** :
- Generate debugging info : ✅ en Debug, ❌ en Release

Nous avons détaillé ces options dans la section 9.8 (Compilation).

## Configuration du débogueur

### Catégorie : Debugger

**Chemin :** Tools → Options → Debugger → General

Configuration du débogueur intégré.

#### Type de débogueur

**Option :** Debugger type

**Choix courants :**
- **GNU debugger (gdb)** : Débogueur standard (Linux, Windows)
- **LLDB** : Débogueur moderne (macOS)
- **FpDebug** : Débogueur intégré FreePascal

**Recommandation :**
- Windows/Linux : **GNU debugger (gdb)**
- macOS : **LLDB** ou **gdb**

#### Chemin du débogueur

**Option :** Debugger path

Chemin vers l'exécutable du débogueur.

**Valeur typique :**
- Windows : `C:\lazarus\mingw\x86_64-win64\bin\gdb.exe`
- Linux : `/usr/bin/gdb`

**Généralement configuré automatiquement.**

#### Options d'affichage

**Show line numbers in disassembler** : Afficher les numéros de ligne dans le désassembleur
- ✅ Utile pour le débogage avancé

**Show breakpoints in disassembler** : Afficher les points d'arrêt
- ✅ Recommandé

## Configuration des messages et verbosité

### Catégorie : Messages

**Chemin :** Tools → Options → Messages / Compiler Messages

Configuration des messages affichés par le compilateur.

#### Niveau de verbosité

**Option :** Show hints / Show warnings / Show notes

**Recommandation pour débutant :**
- ✅ **Show errors** : Toujours afficher (obligatoire)
- ✅ **Show warnings** : Afficher (problèmes potentiels)
- ✅ **Show hints** : Afficher (conseils d'amélioration)
- ❌ **Show notes** : Décochez si trop de messages (détails techniques)

**Pourquoi afficher hints et warnings ?**
- Apprendre les bonnes pratiques
- Détecter des bugs potentiels
- Améliorer la qualité du code

#### Filtrer les messages

**Option :** Message filters

Vous pouvez masquer certains types de messages spécifiques.

**Exemple :** Masquer "Parameter not used" si vous avez beaucoup de procédures avec des paramètres obligatoires mais non utilisés.

**Pour débutant :** Laissez tous les messages, ils sont instructifs !

## Configuration des fenêtres et disposition

### Layouts (dispositions)

Lazarus peut mémoriser plusieurs dispositions de fenêtres.

#### Sauvegarder une disposition

1. Organisez vos fenêtres comme vous le souhaitez
2. Menu **View** → **Layouts** → **Save current desktop layout as...**
3. Donnez un nom : "Ma disposition perso"

#### Charger une disposition

Menu **View** → **Layouts** → [Nom de la disposition]

#### Dispositions prédéfinies

**Default Layout** : Disposition par défaut de Lazarus  
**Debug Layout** : Optimisée pour le débogage (fenêtres de debug visibles)  

#### Réinitialiser les fenêtres

**Menu :** Window → Reset Layout

**Utilisation :** Quand vos fenêtres sont désorganisées ou hors de l'écran.

**Choix :**
- Reset to default positions
- Reset to factory default

**Recommandation :** Reset to default positions (garde vos paramètres)

### Ancrage des fenêtres (Docking)

**Option :** Tools → Options → Desktop → Docking

**Docking enabled** : Permet d'ancrer les fenêtres les unes aux autres
- Créer une disposition à onglets
- Fenêtres groupées

**Pour débutant :** Laissez désactivé au début (les fenêtres flottantes sont plus simples).

**Pour utilisateur avancé :** Activez pour créer un IDE à onglets comme VS Code.

## Configuration des packages

### Packages installés

**Menu :** Package → Configure Installed Packages

Liste des packages (extensions) installés dans Lazarus.

#### Activer/Désactiver un package

1. Cochez/décochez le package dans la liste
2. Cliquez **Save and rebuild IDE**
3. Lazarus se recompile (1-3 minutes)
4. Redémarre automatiquement

**Packages utiles pour débutant :**
- ✅ **LCL** : Bibliothèque de composants (obligatoire)
- ✅ **SynEdit** : Éditeur de code (obligatoire)
- ✅ **IDEIntf** : Interface IDE (obligatoire)
- ✅ **LazControls** : Contrôles supplémentaires (utile)

**Packages optionnels :**
- **LazDebuggerFp** : Débogueur FreePascal
- **TAChart** : Graphiques et diagrammes
- **Printer4Lazarus** : Support impression

#### Installer un nouveau package

1. Menu **Package** → **Install/Uninstall Packages**
2. Cliquez **Open Package File (.lpk)**
3. Sélectionnez le fichier .lpk du package
4. Cliquez **Compile** puis **Use** → **Install**
5. Lazarus se reconstruit

**Note :** Nous verrons l'installation de packages en détail plus tard.

## Conseils de configuration pour débutants

### Configuration recommandée minimale

Pour bien débuter avec Lazarus, configurez au minimum :

**1. Langue de l'interface**
- Tools → Options → Environment → General → Language : Français

**2. Numéros de ligne**
- Tools → Options → Editor → Display → Show line numbers : ✅

**3. Sauvegarde automatique**
- Tools → Options → Environment → General → Auto save : ✅ (10 min)

**4. Backup des fichiers**
- Tools → Options → Environment → Backup → Create backup : ✅

**5. Police lisible**
- Tools → Options → Editor → Display → Font : Consolas ou DejaVu Sans Mono, taille 11-12

**6. Auto-complétion**
- Tools → Options → Editor → Code Tools → Auto invoke : ✅

**7. Thème (optionnel)**
- Tools → Options → Editor → Colors → Schéma : Default ou Dark selon préférence

### Configuration pour grand écran

Si vous avez un grand écran (>24") :

**Police plus grande :**
- Editor Font : taille 14-16

**Fenêtres organisées :**
- Inspecteur d'objets à gauche
- Éditeur de code au centre (large)
- Messages en bas
- Project Inspector à droite

**Plus d'espace pour le code :**
- Masquez les barres d'outils peu utilisées
- View → Component Palette → Only visible at top

### Configuration pour petits écrans (laptop)

Si vous avez un petit écran (<15") :

**Police adaptée :**
- Editor Font : taille 9-10

**Fenêtres économes :**
- Utilisez les onglets plutôt que fenêtres multiples
- Masquez l'inspecteur d'objets quand vous codez (F11 pour basculer)
- Auto-hide pour les panneaux (si disponible)

**Maximisez l'espace code :**
- View → Toggle Form/Unit : F12 (passer du formulaire au code)
- Utilisez les raccourcis plutôt que les menus

### Configuration pour travail prolongé

Si vous passez beaucoup de temps à programmer :

**Reposez vos yeux :**
- Thème sombre (Editor → Colors → Dark)
- Police sans serif (Consolas, Source Code Pro)
- Taille confortable (pas trop petite)

**Activez les pauses :**
- Réglez des rappels (applications tierces)
- Règle 20-20-20 : toutes les 20 minutes, regardez à 20 pieds (6m) pendant 20 secondes

**Ergonomie :**
- Apprenez les raccourcis clavier (moins de souris)
- Positionnez bien votre écran (hauteur des yeux)

## Sauvegarder et restaurer la configuration

### Où est stockée la configuration ?

Lazarus sauvegarde ses paramètres dans des fichiers XML :

**Windows :**
```
C:\Users\VotreNom\AppData\Local\lazarus\
```

**Linux :**
```
~/.lazarus/
```

**Fichiers importants :**
- `environmentoptions.xml` : Options de l'environnement
- `editorsettings.xml` : Paramètres de l'éditeur
- `debuggerproperties.xml` : Configuration du débogueur

### Sauvegarder votre configuration

**Pour sauvegarder :**
1. Fermez Lazarus
2. Copiez le dossier de configuration complet
3. Conservez la copie en lieu sûr

**Windows :**
```
Copier C:\Users\VotreNom\AppData\Local\lazarus
Vers   C:\Backup\lazarus-config-2025-01-15
```

**Linux :**
```bash
cp -r ~/.lazarus ~/Backup/lazarus-config-2025-01-15
```

### Restaurer une configuration

**Pour restaurer :**
1. Fermez Lazarus
2. Supprimez le dossier de configuration actuel
3. Remplacez-le par votre sauvegarde
4. Relancez Lazarus

### Réinitialiser complètement Lazarus

**Si Lazarus ne démarre plus ou est instable :**

1. Fermez Lazarus
2. Supprimez le dossier de configuration
3. Relancez Lazarus → il recréera une configuration par défaut

**Windows :**
```
Supprimer : C:\Users\VotreNom\AppData\Local\lazarus
```

**Linux :**
```bash
rm -rf ~/.lazarus
```

**Note :** Vous perdez vos paramètres personnalisés, mais Lazarus redevient fonctionnel.

## Configuration exportable (pour équipe)

### Fichier de configuration partageable

Pour partager votre configuration avec d'autres développeurs :

**Pas de fonction intégrée directe**, mais vous pouvez :

1. Exporter certains paramètres manuellement
2. Documenter vos choix dans un README
3. Utiliser des scripts pour configurer automatiquement

### Bonnes pratiques en équipe

**Paramètres à standardiser :**
- Indentation (2 ou 4 espaces)
- Longueur de ligne (80 ou 120 caractères)
- Options de compilation (même configuration pour tous)

**Paramètres personnels :**
- Couleurs et thème (chacun son goût)
- Police et taille
- Disposition des fenêtres

**Dans un fichier d'équipe (README.md) :**
```markdown
# Configuration Lazarus pour notre projet

- Indentation : 2 espaces
- Largeur max : 120 caractères
- Mode compilation : {$mode objfpc}{$H+}
- Encodage : UTF-8
```

## Résolution de problèmes de configuration

### Problème : Lazarus ne démarre plus après changement de configuration

**Solution :**
1. Réinitialisez la configuration (supprimez le dossier de config)
2. Relancez Lazarus
3. Reconfigurez progressivement

### Problème : La configuration ne se sauvegarde pas

**Causes possibles :**
- Droits insuffisants sur le dossier de configuration
- Disque plein
- Fichier en lecture seule

**Solution :**
1. Vérifiez les permissions du dossier
2. Lancez Lazarus en administrateur (une fois)
3. Vérifiez l'espace disque disponible

### Problème : Certaines options ne sont pas disponibles

**Causes :**
- Version de Lazarus ancienne
- Package nécessaire non installé
- Plateforme non supportée

**Solution :**
- Mettez à jour Lazarus
- Installez les packages requis
- Consultez la documentation de votre version

### Problème : L'interface est trop petite/grande

**Solution :**
- Augmentez la taille de police (Editor → Display → Font)
- Changez la résolution de votre écran (paramètres système)
- Utilisez le zoom de Windows (loupe système)

## Conclusion

Vous savez maintenant configurer Lazarus selon vos préférences et besoins !

**Ce que vous avez appris dans cette section :**
- ✅ Accéder aux différentes options de configuration
- ✅ Configurer l'environnement général (langue, sauvegarde auto, chemins)
- ✅ Personnaliser l'éditeur (police, couleurs, indentation)
- ✅ Configurer les outils (complétion, templates, raccourcis)
- ✅ Ajuster le concepteur de formulaires (grille, alignement)
- ✅ Paramétrer le débogueur
- ✅ Gérer les fenêtres et dispositions
- ✅ Sauvegarder et restaurer la configuration

**Paramètres essentiels à configurer en priorité :**
1. Langue de l'interface (si nécessaire)
2. Numéros de ligne dans l'éditeur
3. Sauvegarde automatique
4. Police et taille confortables
5. Auto-complétion activée

**Paramètres optionnels selon préférence :**
- Thème de couleurs (clair ou sombre)
- Disposition des fenêtres
- Raccourcis clavier personnalisés
- Grille et guides du concepteur

**Conseils pratiques :**
- Ne changez pas tout d'un coup : configurez progressivement
- Testez chaque modification pour voir si elle vous convient
- Sauvegardez votre configuration quand vous êtes satisfait
- N'hésitez pas à réinitialiser si quelque chose ne va plus

**Prochaines étapes :**
- Section 9.10 : Utilisation de l'aide et documentation
- Puis : Partie II du tutoriel (Programmation Orientée Objet)

**Astuce finale :** Un IDE bien configuré, c'est un IDE dans lequel on aime travailler. Prenez le temps de trouver VOS paramètres idéaux !

---

**Points clés à retenir :**
- Configuration accessible via Tools → Options
- Les changements sont sauvegardés automatiquement
- Configuration globale (IDE) vs configuration projet (.lpi)
- Sauvegardez votre configuration dans un dossier séparé
- En cas de problème, réinitialisez la configuration
- Personnalisez selon vos besoins, mais gardez la simplicité au début
- Les paramètres par défaut sont déjà bien pensés

⏭️ [Utilisation de l'aide et documentation](/09-introduction-freepascal-lazarus/10-utilisation-aide-documentation.md)
