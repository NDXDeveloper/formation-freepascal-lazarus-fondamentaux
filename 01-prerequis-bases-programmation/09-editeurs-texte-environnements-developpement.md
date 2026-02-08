🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.9 Éditeurs de texte et environnements de développement

## Introduction

Pour écrire du code, vous avez besoin d'un outil adapté. Il existe deux grandes catégories d'outils : les éditeurs de texte simples et les environnements de développement intégrés (IDE). Cette section vous aidera à comprendre les différences et à choisir les outils adaptés à vos besoins.

## Éditeur de texte vs Traitement de texte

### Ne JAMAIS utiliser un traitement de texte

**Traitement de texte (Word, LibreOffice Writer) :**
- Conçu pour écrire des documents formatés
- Ajoute des caractères invisibles de formatage
- Sauvegarde dans des formats propriétaires (.docx, .odt)
- **Ne convient PAS pour programmer**

**Exemple de problème :**
Si vous écrivez du code dans Word :
```pascal
program Bonjour;  ← Word peut remplacer les guillemets droits " par des guillemets typographiques "
begin
    WriteLn('Hello'); ← Les apostrophes peuvent être modifiées
end.
```
Le compilateur ne comprendra pas ces caractères spéciaux et produira des erreurs.

### Éditeur de texte pour programmation

**Caractéristiques nécessaires :**
- Sauvegarde en texte brut (plain text)
- Préserve les caractères exacts
- Pas de formatage invisible
- Supporte différents encodages (UTF-8, ASCII)
- Affiche les numéros de lignes
- Gère l'indentation

## Les éditeurs de texte simples

### Windows

#### Notepad (Bloc-notes)

**Points positifs :**
- Préinstallé sur Windows
- Très léger et rapide
- Simple d'utilisation

**Points négatifs :**
- Aucune fonctionnalité avancée
- Pas de coloration syntaxique
- Pas d'indentation automatique
- Difficile pour de gros projets

**Quand l'utiliser :**
- Pour des fichiers de configuration simples
- Pour des scripts très courts
- En dépannage

#### Notepad++

**Points positifs :**
- Gratuit et open source
- Coloration syntaxique pour de nombreux langages
- Numéros de ligne
- Recherche et remplacement avancés
- Gestion des onglets (plusieurs fichiers ouverts)
- Complétion automatique basique
- Supporte les plugins

**Points négatifs :**
- Windows uniquement
- Pas de débogueur intégré
- Pas de gestion de projet

**Parfait pour :**
- Éditer rapidement des fichiers
- Scripts simples
- Apprendre à programmer

**Téléchargement :**
- Site officiel : notepad-plus-plus.org

### Linux

#### gedit

**Points positifs :**
- Préinstallé sur Ubuntu (GNOME)
- Interface simple et intuitive
- Coloration syntaxique
- Numéros de ligne
- Plugins disponibles

**Points négatifs :**
- Fonctionnalités limitées
- Pas de débogueur

**Parfait pour :**
- Édition rapide de fichiers
- Débutants sous Linux

#### nano

**Points positifs :**
- Éditeur en ligne de commande
- Préinstallé sur presque toutes les distributions
- Très léger
- Raccourcis affichés en bas de l'écran

**Points négatifs :**
- Interface textuelle uniquement
- Fonctionnalités basiques

**Quand l'utiliser :**
- Édition rapide en ligne de commande
- Sur des serveurs sans interface graphique
- Fichiers de configuration système

**Commandes de base :**
```bash
nano fichier.pas         # Ouvrir un fichier
Ctrl+O                   # Sauvegarder
Ctrl+X                   # Quitter
Ctrl+K                   # Couper une ligne
Ctrl+U                   # Coller
Ctrl+W                   # Rechercher
```

#### vim / vi

**Points positifs :**
- Très puissant et efficace
- Préinstallé sur toutes les distributions Unix/Linux
- Très rapide une fois maîtrisé
- Modes d'édition uniques

**Points négatifs :**
- Courbe d'apprentissage très difficile
- Interface déroutante pour les débutants
- Nécessite de mémoriser de nombreuses commandes

**Recommandation :**
- Réservé aux utilisateurs avancés
- Pas idéal pour débuter en programmation

### Multi-plateforme

#### Visual Studio Code (VS Code)

**Points positifs :**
- Gratuit et open source (base open source, téléchargement Microsoft)
- Très populaire dans la communauté
- Windows, Linux, macOS
- Coloration syntaxique excellente
- IntelliSense (complétion intelligente)
- Terminal intégré
- Gestion de projet
- Git intégré
- Milliers d'extensions disponibles
- Support FreePascal via extension

**Points négatifs :**
- Plus lourd qu'un éditeur simple
- Nécessite des extensions pour Pascal
- Pas un IDE complet pour Pascal

**Parfait pour :**
- Développeurs multi-langages
- Projets web et modernes
- Travail avec Git

**Extensions utiles pour Pascal :**
- OmniPascal
- Pascal (Alessandro Fragnani)
- Pascal Formatter

#### Sublime Text

**Points positifs :**
- Très rapide et réactif
- Interface élégante
- Multi-plateforme
- Recherche très puissante
- Mode multi-curseur

**Points négatifs :**
- Payant (version d'évaluation illimitée)
- Moins d'extensions que VS Code

#### Atom

**Points positifs :**
- Gratuit et open source
- Développé par GitHub
- Très personnalisable
- Multi-plateforme

**Points négatifs :**
- Plus lent que VS Code
- Développement arrêté depuis 2022
- Non recommandé pour de nouveaux projets

## Les Environnements de Développement Intégrés (IDE)

### Qu'est-ce qu'un IDE ?

Un **IDE (Integrated Development Environment)** est un logiciel complet qui intègre tous les outils nécessaires au développement :

**Composants d'un IDE :**
- Éditeur de code avec coloration syntaxique
- Compilateur intégré
- Débogueur (pour trouver les erreurs)
- Gestionnaire de projet
- Explorateur de fichiers
- Interface de conception graphique (pour les applications fenêtrées)
- Outils de refactoring
- Aide contextuelle

**Analogie :**
- Éditeur de texte = un tournevis
- IDE = une boîte à outils complète avec tous les outils nécessaires

### Avantages d'un IDE

**1. Tout en un seul endroit**
- Plus besoin de jongler entre plusieurs outils
- Workflow fluide et intégré

**2. Gain de temps**
- Compilation en un clic
- Détection d'erreurs en temps réel
- Navigation rapide dans le code

**3. Aide au développement**
- Complétion automatique du code
- Documentation intégrée
- Templates et assistants

**4. Débogage facilité**
- Points d'arrêt visuels
- Inspection des variables
- Exécution pas à pas

**5. Gestion de projet**
- Organisation des fichiers
- Dépendances gérées automatiquement
- Configuration de compilation

### Inconvénients d'un IDE

**1. Plus lourd**
- Consomme plus de ressources (RAM, CPU)
- Démarrage plus lent

**2. Courbe d'apprentissage**
- Interface complexe au début
- Nombreuses fonctionnalités à découvrir

**3. Moins flexible**
- Conçu pour un langage ou une famille de langages spécifique
- Difficile de changer de langage

## Lazarus : L'IDE pour FreePascal

### Présentation de Lazarus

**Lazarus** est l'IDE principal pour FreePascal, similaire à Delphi mais gratuit et open source.

**Caractéristiques :**
- Gratuit et open source
- Multi-plateforme (Windows, Linux, macOS, FreeBSD)
- Interface visuelle pour créer des applications graphiques
- Bibliothèque LCL (Lazarus Component Library)
- Compatible avec le code Delphi (migration facile)
- Éditeur de code avancé
- Débogueur intégré
- Gestionnaire de paquets

**Histoire :**
- Développé depuis 1999
- Vise à être un clone libre de Delphi
- Communauté active et contributive

### Interface de Lazarus

#### Fenêtre principale

**Zone de l'éditeur :**
- Zone centrale où vous écrivez votre code
- Onglets pour plusieurs fichiers ouverts
- Numéros de ligne à gauche
- Coloration syntaxique

**Palette de composants (en haut) :**
- Boutons, champs de texte, listes, etc.
- Pour créer des interfaces graphiques
- Glisser-déposer sur les formulaires

**Inspecteur d'objets (à gauche/droite) :**
- Propriétés des composants sélectionnés
- Événements associés
- Modification visuelle des paramètres

**Explorateur de projet :**
- Arborescence des fichiers du projet
- Unités, formulaires, ressources

**Barre d'outils :**
- Boutons d'action rapide
- Nouveau, Ouvrir, Sauvegarder
- Compiler, Exécuter, Déboguer

#### Organisation des fenêtres

Lazarus utilise des fenêtres flottantes ou ancrables :
- Personnalisable selon vos préférences
- Différentes dispositions (layouts) sauvegardables
- Menu : Outils → Options → Environnement → Bureau

### Fonctionnalités clés de Lazarus

#### 1. Éditeur de code intelligent

**Complétion de code (Code Completion) :**
- Appuyez sur `Ctrl+Espace` pour afficher les suggestions
- Liste des variables, fonctions, propriétés disponibles
- Gain de temps et évite les erreurs de frappe

**Hints (indices) :**
- Survol d'un identificateur avec la souris
- Affiche la déclaration ou la documentation

**Code Templates :**
- Raccourcis pour insérer du code fréquent
- Exemple : taper `begin` puis `Ctrl+J` → insère un bloc begin..end

**Refactoring :**
- Renommer une variable dans tout le projet
- Extraire une procédure
- Réorganiser le code

#### 2. Débogueur intégré

**Points d'arrêt (Breakpoints) :**
- Clic dans la marge gauche
- Le programme s'arrêtera à cette ligne
- Permet d'inspecter l'état du programme

**Exécution pas à pas :**
- F7 : Step Into (entrer dans une fonction)
- F8 : Step Over (passer par-dessus)
- Ctrl+F8 : Run to Cursor

**Inspection des variables :**
- Voir la valeur des variables en temps réel
- Surveiller (watch) des expressions
- Pile d'appels (call stack)

**Console de débogage :**
- Affiche les messages de débogage
- Évaluer des expressions

#### 3. Concepteur de formulaires (Form Designer)

**Mode visuel :**
- Glisser-déposer des composants
- Positionner visuellement les éléments
- Voir immédiatement le résultat

**Palette de composants :**
- Standard : TButton, TLabel, TEdit
- Additional : TImage, TShape, TBitBtn
- Dialogs : TOpenDialog, TSaveDialog
- Data Controls : composants liés aux bases de données

**Propriétés :**
- Modifier visuellement les propriétés (couleur, taille, texte)
- Génération automatique du code

#### 4. Gestionnaire de projet

**Types de projets :**
- Application (fenêtrée)
- Program (console)
- Library (bibliothèque)
- Package (extension Lazarus)

**Structure d'un projet :**
```
MonProjet.lpi            # Fichier projet principal
MonProjet.lpr            # Code source principal
Unit1.pas                # Unité Pascal
Unit1.lfm                # Formulaire (interface)
backup/                  # Sauvegardes automatiques
lib/                     # Fichiers de compilation
```

**Options de projet :**
- Chemins de compilation
- Définitions conditionnelles
- Options du compilateur
- Icône de l'application
- Informations de version

#### 5. Aide intégrée

**Aide contextuelle :**
- F1 sur un mot-clé → ouvre la documentation
- Documentation FreePascal et LCL intégrée

**Code Explorer :**
- Vue d'ensemble de votre unité
- Procédures, fonctions, types déclarés
- Navigation rapide

**Messages du compilateur :**
- Fenêtre dédiée aux erreurs et avertissements
- Double-clic pour aller à la ligne concernée
- Explications des erreurs

### Créer un projet dans Lazarus

**Nouveau projet console :**
1. Fichier → Nouveau → Projet
2. Choisir "Program"
3. Donner un nom au projet
4. Choisir le dossier de destination

**Nouveau projet application :**
1. Fichier → Nouveau → Projet
2. Choisir "Application"
3. Un formulaire vide est créé automatiquement
4. Ajouter des composants depuis la palette

**Ouvrir un projet existant :**
1. Fichier → Ouvrir un projet
2. Sélectionner le fichier `.lpi`
3. Le projet se charge avec tous ses fichiers

### Compiler et exécuter dans Lazarus

**Compilation :**
- F9 : Compiler et exécuter
- Ctrl+F9 : Compiler uniquement
- Shift+F9 : Compiler rapidement (sans nettoyer)
- Exécuter → Tout recompiler : recompilation complète

**Configuration de compilation :**
- Projet → Options du projet → Compilateur
- Mode de compilation : Debug / Release
- Optimisations : -O1, -O2, -O3
- Vérifications supplémentaires

**Modes de compilation :**

**Mode Debug :**
- Informations de débogage incluses
- Vérifications supplémentaires activées
- Exécution plus lente mais plus sûre
- Fichier exécutable plus gros

**Mode Release :**
- Optimisations activées
- Pas d'informations de débogage
- Exécution plus rapide
- Fichier exécutable plus petit

**Résultats de la compilation :**
- Fenêtre "Messages" affiche les erreurs et avertissements
- Onglet "Compiler" : détails de la compilation
- Statistiques : temps de compilation, taille du fichier

## Autres IDE pour Pascal

### Free Pascal IDE (fp)

**Caractéristiques :**
- IDE texte classique (interface à l'ancienne)
- Léger et rapide
- Préinstallé avec FreePascal
- Interface similaire à Turbo Pascal

**Avantages :**
- Très léger en ressources
- Fonctionne sans interface graphique
- Nostalgique pour les anciens de Turbo Pascal

**Inconvénients :**
- Interface textuelle dépassée
- Pas de concepteur visuel
- Moins de fonctionnalités que Lazarus

**Quand l'utiliser :**
- Sur de vieilles machines
- Pour la programmation console uniquement
- Par nostalgie

### Delphi (commercial)

**Caractéristiques :**
- IDE commercial professionnel de Embarcadero
- Ancêtre de Lazarus
- Très puissant et complet

**Avantages :**
- Support commercial
- Outils professionnels avancés
- Grande base d'utilisateurs en entreprise

**Inconvénients :**
- Très cher (plusieurs centaines/milliers d'euros)
- Windows principalement (multi-plateforme en version chère)
- Licence propriétaire

**Pour ce tutoriel :**
- Nous utilisons Lazarus (gratuit et multi-plateforme)
- Le code est compatible entre les deux

### Visual Studio (avec extensions)

**Possibilité :**
- Visual Studio peut supporter Pascal avec des extensions
- Principalement pour du développement mixte (C++/Pascal)

**Non recommandé pour débutants :**
- Configuration complexe
- Pas optimisé pour Pascal
- Mieux vaut utiliser Lazarus

## Comparaison des outils

### Tableau comparatif

| Outil | Type | Gratuit | Multi-plateforme | Niveau | Pascal |
|-------|------|---------|------------------|--------|--------|
| Notepad++ | Éditeur | Oui | Windows | Débutant | Support basique |
| gedit | Éditeur | Oui | Linux/Mac | Débutant | Support basique |
| VS Code | Éditeur+ | Oui | Oui | Inter./Avancé | Via extension |
| Sublime Text | Éditeur | Non* | Oui | Inter./Avancé | Via extension |
| Lazarus | IDE | Oui | Oui | Tous niveaux | Excellent |
| FP IDE | IDE | Oui | Oui | Débutant | Bon |
| Delphi | IDE | Non | Windows* | Pro | Excellent |

\* Version d'évaluation disponible / Multi-plateforme en version entreprise

### Recommandations selon votre situation

**Vous débutez en programmation :**
- **Lazarus** : le meilleur choix
- Tout intégré, facile à utiliser
- Documentation et tutoriels abondants

**Vous voulez quelque chose de léger :**
- **Notepad++** (Windows) ou **gedit** (Linux)
- Compilation en ligne de commande
- Bon pour apprendre les bases

**Vous développez en plusieurs langages :**
- **VS Code** avec extensions
- Polyvalent et moderne
- Communauté très active

**Vous travaillez uniquement en console :**
- **nano** ou **vim** (Linux)
- **Notepad++** (Windows)
- Compilation manuelle

**Vous avez une vieille machine :**
- **Free Pascal IDE** (fp)
- **gedit** ou **nano**
- Très léger en ressources

## Configuration de votre environnement

### Installation de Lazarus

**Windows :**
1. Télécharger depuis : www.lazarus-ide.org
2. Choisir la version pour votre architecture (32-bit ou 64-bit)
3. Exécuter l'installateur
4. Suivre l'assistant d'installation
5. Laisser les options par défaut

**Linux (Ubuntu) :**
```bash
# Via les dépôts officiels
sudo apt update
sudo apt install lazarus

# Ou depuis le site officiel pour la dernière version
# Télécharger le .deb et l'installer
sudo dpkg -i lazarus-project*.deb
sudo apt-get install -f  # Résoudre les dépendances
```

### Premier lancement de Lazarus

**Configuration initiale :**
1. Choisir la langue (français disponible)
2. Configurer la disposition des fenêtres
3. Parcourir les exemples fournis

**Vérifier l'installation :**
1. Fichier → Nouveau → Projet → Program
2. Écrire un simple WriteLn
3. F9 pour compiler et exécuter
4. Si ça fonctionne, tout est bon !

### Personnalisation de Lazarus

**Apparence :**
- Outils → Options → Environnement → Bureau
- Choisir un thème : Défaut, Sombre, etc.
- Taille des polices

**Éditeur :**
- Outils → Options → Éditeur → Affichage
- Coloration syntaxique personnalisable
- Taille des tabulations
- Affichage des espaces et fins de ligne

**Raccourcis clavier :**
- Outils → Options → Éditeur → Touches
- Personnaliser ou utiliser des schémas (Delphi, Visual Studio)

**Extensions :**
- Paquets → Ouvrir un paquet
- Installer des composants supplémentaires
- Exemples : SQLdb pour bases de données, Synapse pour réseau

## Conseils pour bien utiliser votre IDE

### 1. Apprenez les raccourcis clavier

**Raccourcis essentiels Lazarus :**
```
F9              → Compiler et exécuter
Ctrl+F9         → Compiler seulement
F12             → Basculer Form/Code
Ctrl+Espace     → Complétion de code
Ctrl+Shift+C    → Complétion de classe
Ctrl+Click      → Aller à la déclaration
F1              → Aide
Ctrl+S          → Sauvegarder
Ctrl+F          → Rechercher
Ctrl+H          → Remplacer
F7/F8           → Débogage pas à pas
```

### 2. Organisez vos projets

**Structure recommandée :**
```
MesProjetsLazarus/
├── Projet1/
│   ├── Projet1.lpi
│   ├── Projet1.lpr
│   └── units/
├── Projet2/
└── Bibliotheques/
```

### 3. Utilisez le contrôle de version

- Git intégré dans Lazarus
- Sauvegardez régulièrement
- Utilisez des noms de version explicites

### 4. Explorez les exemples

Lazarus inclut de nombreux exemples :
- Ouvrir un projet → Exemples
- Étudiez le code pour apprendre
- Modifiez et expérimentez

### 5. Consultez la documentation

- Documentation intégrée (F1)
- Wiki officiel : wiki.lazarus.freepascal.org
- Forum : forum.lazarus.freepascal.org

### 6. Configurez des sauvegardes automatiques

- Outils → Options → Environnement → Fichiers
- Activer la sauvegarde automatique
- Définir l'intervalle (ex: toutes les 5 minutes)

## Erreurs courantes des débutants

### 1. Confondre projet et fichier

**Erreur :**
Ouvrir un fichier `.pas` isolé au lieu du projet `.lpi`

**Solution :**
Toujours ouvrir le fichier projet `.lpi` qui contient toute la configuration

### 2. Ne pas sauvegarder avant de compiler

**Erreur :**
Compiler sans sauvegarder les modifications

**Solution :**
Lazarus demande généralement de sauvegarder, acceptez toujours

### 3. Oublier de reconstruire après modifications

**Erreur :**
Modifier une unité mais ne pas la recompiler

**Solution :**
Exécuter → Tout recompiler (ou Shift+F9) en cas de doute

### 4. Perdre ses fenêtres

**Erreur :**
Fermer accidentellement une fenêtre importante

**Solution :**
Affichage → [Nom de la fenêtre] pour la réafficher
Ou : Affichage → Bureau → Réinitialiser

### 5. Problèmes de chemins

**Erreur :**
Déplacer le projet et Lazarus ne trouve plus les fichiers

**Solution :**
Ouvrir le .lpi qui met à jour les chemins relatifs

## Passer d'un éditeur simple à un IDE

### Transition progressive

**Étape 1 : Commencez simple**
- Utilisez un éditeur de texte pour vos premiers programmes
- Compilez en ligne de commande
- Comprenez le processus de compilation

**Étape 2 : Découvrez l'IDE**
- Installez Lazarus
- Créez un projet simple
- Explorez l'interface sans pression

**Étape 3 : Apprenez progressivement**
- Utilisez d'abord juste l'éditeur et la compilation
- Découvrez le débogueur quand vous rencontrez des bugs
- Explorez les fonctionnalités avancées au fur et à mesure

**Étape 4 : Maîtrisez les outils**
- Apprenez les raccourcis
- Personnalisez selon vos besoins
- Devenez efficace

## Conclusion

Choisir le bon outil est important pour votre productivité et votre confort de programmation. Pour ce tutoriel, nous recommandons **Lazarus** car il offre le meilleur équilibre entre facilité d'utilisation et fonctionnalités complètes.

**Points clés à retenir :**
- N'utilisez JAMAIS un traitement de texte (Word) pour programmer
- Les éditeurs de texte simples sont parfaits pour débuter
- Les IDE intègrent tous les outils nécessaires au développement
- Lazarus est l'IDE recommandé pour FreePascal
- Un IDE peut sembler complexe au début, mais fait gagner du temps ensuite
- Apprenez les raccourcis clavier pour être plus efficace

**Pour la suite du tutoriel :**
Nous utiliserons principalement **Lazarus** pour :
- Ses capacités de développement rapide
- Son débogueur intégré
- Sa compatibilité multi-plateforme
- Sa facilité de création d'interfaces graphiques

**Votre choix selon votre objectif :**
- **Apprendre les bases** : Notepad++ ou gedit + ligne de commande
- **Développement sérieux** : Lazarus
- **Projets console simples** : VS Code ou éditeur favori
- **Applications graphiques** : Lazarus obligatoire

Dans la prochaine section, nous allons créer notre tout premier programme "Hello World" en Pascal, en utilisant à la fois la ligne de commande et Lazarus, pour bien comprendre les deux approches !

⏭️ [Premier programme "Hello World" en Pascal](/01-prerequis-bases-programmation/10-premier-programme-hello-world-pascal.md)
