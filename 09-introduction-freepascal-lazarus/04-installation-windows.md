🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.4 Installation sur Windows

## Introduction

L'installation de Lazarus sur Windows est un processus simple et direct. Contrairement à certains environnements de développement complexes, Lazarus peut être installé et prêt à l'emploi en quelques minutes. Cette section vous guide pas à pas pour une installation réussie.

**Ne vous inquiétez pas !** L'installation est conçue pour être accessible même aux débutants complets. Nous allons procéder étape par étape, en expliquant chaque choix.

## Prérequis système

Avant de commencer, vérifions que votre ordinateur répond aux exigences minimales.

### Configuration minimale

| Composant | Minimum | Recommandé |
|-----------|---------|------------|
| **Système d'exploitation** | Windows 7 | Windows 10 ou 11 |
| **Processeur** | 1 GHz | 2 GHz ou plus |
| **RAM** | 1 Go | 4 Go ou plus |
| **Espace disque** | 500 Mo | 2 Go (avec marge pour vos projets) |
| **Écran** | 1024×768 | 1920×1080 |
| **Droits** | Administrateur | Administrateur |

**Bonne nouvelle :** Lazarus est léger ! Même un ordinateur modeste de 5-10 ans peut le faire tourner correctement.

### Vérifier votre version de Windows

Si vous ne connaissez pas votre version de Windows :
1. Appuyez sur les touches **Windows + Pause** (ou **Windows + i**)
2. Allez dans **Système** → **Informations système**
3. Vous verrez : "Windows 10", "Windows 11", etc.
4. Notez aussi si c'est **32 bits** ou **64 bits**

**Important :** La grande majorité des ordinateurs modernes (depuis 2010) sont en **64 bits**. C'est cette version que nous recommandons.

## Choix de la version à télécharger

### Quelle version de Lazarus choisir ?

Rendez-vous sur le site officiel : **https://www.lazarus-ide.org**

Vous verrez plusieurs versions disponibles. Voici comment choisir :

#### 1. Version stable vs version de développement

**Version stable** (exemple : Lazarus 3.0)
- ✅ Testée et fiable
- ✅ Recommandée pour apprendre
- ✅ Documentation complète
- ✅ Moins de bugs

**Version de développement** (trunk/snapshot)
- ⚠️ Dernières fonctionnalités
- ⚠️ Peut contenir des bugs
- ⚠️ Pour utilisateurs avancés

**👉 Pour ce tutoriel : choisissez TOUJOURS la version stable la plus récente.**

#### 2. Architecture : 32 bits ou 64 bits ?

**64 bits** (win64)
- ✅ Recommandé pour les PC modernes
- ✅ Meilleure gestion de la mémoire
- ✅ Peut compiler des programmes 32 et 64 bits

**32 bits** (win32)
- Pour les très vieux PC uniquement
- Limitations de mémoire

**👉 Si votre Windows est 64 bits, téléchargez Lazarus 64 bits.**

#### 3. Type d'installateur

Vous verrez plusieurs fichiers disponibles :

**lazarus-X.X.X-fpc-Y.Y.Y-win64.exe** (RECOMMANDÉ)
- Installateur complet
- Inclut Lazarus + FreePascal
- Installation automatique
- Taille : ~200-300 Mo
- **C'est ce qu'il vous faut !**

**lazarus-X.X.X-win64.exe**
- Lazarus seul (sans FreePascal)
- Pour utilisateurs avancés qui ont déjà FreePascal
- Ne prenez pas celui-ci si c'est votre première installation

**Exemple de nom complet :**
`lazarus-3.0-fpc-3.2.2-win64.exe`
- Lazarus version 3.0
- FreePascal version 3.2.2
- Pour Windows 64 bits

## Téléchargement

### Méthode 1 : Depuis le site officiel (recommandé)

1. Allez sur **https://www.lazarus-ide.org**
2. Cliquez sur le gros bouton **"Download"** ou **"Télécharger"**
3. Dans la section **"Windows"**, cherchez la version stable
4. Cliquez sur le lien de téléchargement correspondant à votre système (64 bits généralement)
5. Le téléchargement commence (patientez, c'est un gros fichier !)

### Méthode 2 : Via SourceForge

Si le site principal est lent ou inaccessible :
1. Allez sur **https://sourceforge.net/projects/lazarus/**
2. Cliquez sur **"Download"**
3. Naviguez vers **Lazarus Windows 64 bits** ou **32 bits**
4. Téléchargez le fichier .exe complet

### Pendant le téléchargement

- **Durée** : 5-30 minutes selon votre connexion Internet
- **Taille** : environ 200-400 Mo
- **Emplacement** : généralement dans votre dossier "Téléchargements"
- Vous pouvez continuer à utiliser votre ordinateur pendant ce temps

**Note de sécurité :** Votre navigateur ou antivirus peut afficher un avertissement car il s'agit d'un fichier .exe. C'est normal. Le fichier provenant du site officiel est sûr.

## Installation pas à pas

Une fois le téléchargement terminé, lançons l'installation !

### Étape 1 : Lancement de l'installateur

1. Allez dans votre dossier "Téléchargements"
2. Double-cliquez sur le fichier `lazarus-X.X.X-fpc-Y.Y.Y-win64.exe`
3. Windows peut demander : **"Voulez-vous autoriser cette application à apporter des modifications ?"**
   - Cliquez sur **"Oui"**
   - (C'est normal, l'installateur doit créer des dossiers et des raccourcis)

### Étape 2 : Écran de bienvenue

Une fenêtre s'ouvre avec le logo Lazarus.

- Langue : choisissez **Français** si disponible, sinon **English**
- Cliquez sur **"Next"** ou **"Suivant"**

### Étape 3 : Licence

L'écran affiche la licence GPL.

- Lisez-la si vous le souhaitez (c'est une licence libre)
- Cochez **"I accept the agreement"** (J'accepte l'accord)
- Cliquez sur **"Next"**

**Note :** La licence GPL signifie que Lazarus est gratuit et vous pouvez l'utiliser sans restriction, même commercialement.

### Étape 4 : Choix du dossier d'installation

L'installateur propose un dossier par défaut :
- Généralement : `C:\lazarus`
- Ou parfois : `C:\Program Files\lazarus`

**Nos recommandations :**

**Option 1 : `C:\lazarus` (RECOMMANDÉ)**
- ✅ Plus simple (pas d'espaces dans le chemin)
- ✅ Évite les problèmes de permissions
- ✅ Plus facile à retrouver
- **👉 Gardez ce choix par défaut si proposé**

**Option 2 : `C:\Program Files\Lazarus`**
- Acceptable mais peut causer des soucis de permissions
- Si vous choisissez ce dossier, vous devrez parfois lancer Lazarus en administrateur

**À éviter :**
- ❌ Dossiers avec espaces ou caractères accentués : `C:\Mes Documents\Développement`
- ❌ Chemins trop longs
- ❌ OneDrive ou dossiers cloud synchronisés

**Action :** Laissez le dossier par défaut (`C:\lazarus`) et cliquez sur **"Next"**

### Étape 5 : Menu Démarrer

L'installateur demande le nom du dossier dans le menu Démarrer.

- Par défaut : "Lazarus"
- **Action :** Laissez tel quel et cliquez sur **"Next"**

### Étape 6 : Composants à installer

L'installateur liste les composants :

```
☑ Lazarus IDE
☑ FreePascal Compiler
☑ FreePascal Sources
☑ Documentation
☑ Examples
```

**Recommandation pour débutant :** Laissez **tout coché**. Tout est utile :
- **Lazarus IDE** : l'environnement de développement (obligatoire)
- **FreePascal Compiler** : le compilateur (obligatoire)
- **FreePascal Sources** : le code source de FreePascal (utile pour apprendre)
- **Documentation** : l'aide intégrée (très utile)
- **Examples** : des exemples de programmes (excellent pour apprendre)

Cliquez sur **"Next"**

### Étape 7 : Tâches supplémentaires

L'installateur propose des options additionnelles :

```
☑ Create a desktop icon (Créer une icône sur le bureau)
☐ Associate .pas files with Lazarus (Associer les fichiers .pas)
☐ Associate .lpi files with Lazarus (Associer les fichiers .lpi)
```

**Recommandations :**
- ✅ **Icône bureau** : Oui, c'est pratique pour démarrer rapidement
- ✅ **Associer .pas** : Oui, pour ouvrir vos fichiers Pascal directement
- ✅ **Associer .lpi** : Oui, pour ouvrir vos projets Lazarus en double-cliquant dessus

Cochez tout et cliquez sur **"Next"**

### Étape 8 : Récapitulatif

Un écran résume vos choix :
- Dossier d'installation
- Composants sélectionnés
- Options choisies

Vérifiez que tout vous convient, puis cliquez sur **"Install"** ou **"Installer"**

### Étape 9 : Installation en cours

L'installateur copie les fichiers. Vous voyez :
- Une barre de progression
- Les noms des fichiers qui s'installent
- Durée : 2-10 minutes selon la vitesse de votre ordinateur

**Soyez patient !** Ne fermez pas la fenêtre, même si ça semble long.

### Étape 10 : Fin de l'installation

L'installation est terminée !

Une fenêtre affiche :
```
☑ Launch Lazarus (Lancer Lazarus maintenant)
```

**Action :**
- Laissez la case cochée si vous voulez tester immédiatement
- Ou décochez si vous préférez lancer Lazarus plus tard
- Cliquez sur **"Finish"** ou **"Terminer"**

**🎉 Félicitations ! Lazarus est maintenant installé sur votre ordinateur !**

## Premier lancement de Lazarus

### Démarrage de Lazarus

**Méthode 1 : Depuis le bureau**
- Double-cliquez sur l'icône **Lazarus** sur votre bureau

**Méthode 2 : Depuis le menu Démarrer**
- Cliquez sur le bouton Windows
- Tapez "Lazarus"
- Cliquez sur l'application Lazarus

**Méthode 3 : Depuis le dossier d'installation**
- Ouvrez `C:\lazarus`
- Double-cliquez sur `lazarus.exe`

### Premier chargement (peut prendre 30 secondes)

Au premier lancement, Lazarus :
- Charge ses composants
- Configure l'environnement
- Crée des fichiers de configuration
- Peut sembler "figé" quelques secondes (c'est normal !)

**Message possible :** "Rebuilding Lazarus"
- Lazarus se compile lui-même
- Attendez tranquillement (1-5 minutes)
- Cela ne se produira qu'une seule fois

### Configuration initiale automatique

Une fenêtre de configuration peut apparaître : **"Lazarus Setup"**

**Questions possibles :**

**1. "Quel répertoire utiliser pour les projets ?"**
- Par défaut : `C:\Users\VotreNom\Documents\Lazarus`
- **Recommandation :** Acceptez ou choisissez un dossier simple sans espaces
- Cliquez **OK**

**2. "Scanner les packages FPC ?"**
- Lazarus demande s'il doit scanner les bibliothèques FreePascal
- **Recommandation :** Cliquez **Oui** ou **Yes**
- Durée : quelques secondes

**3. "Rebuild IDE ?"** (Reconstruire l'IDE)
- Parfois Lazarus demande à se recompiler
- **Recommandation :** Cliquez **Oui**
- Attendez que la compilation se termine

### L'interface Lazarus s'ouvre !

Vous devriez maintenant voir :
- La fenêtre principale de Lazarus
- Une barre de menus en haut
- Des barres d'outils avec des icônes
- Peut-être plusieurs fenêtres flottantes (inspecteur d'objets, etc.)

**🎊 Bravo ! Lazarus fonctionne correctement !**

## Vérification de l'installation

Assurons-nous que tout fonctionne bien.

### Test 1 : Vérifier les versions installées

1. Dans Lazarus, menu **Help** (Aide) → **About Lazarus** (À propos de Lazarus)
2. Une fenêtre affiche :
   - Version de Lazarus (ex: 3.0)
   - Version de FreePascal (ex: 3.2.2)
   - Date de compilation
   - Plateforme cible

**Exemple de ce que vous devriez voir :**
```
Lazarus 3.0  
FPC 3.2.2  
Date: 2023-10-15  
Target: x86_64-win64-win32  
```

Cliquez **OK** pour fermer.

### Test 2 : Créer un programme minimal

Testons en compilant un petit programme :

1. Menu **File** → **New...** → **Simple Program**
2. Une fenêtre de code s'ouvre avec un programme minimal :

```pascal
program Project1;

begin  
end.  
```

3. Ajoutez une ligne entre `begin` et `end.` :

```pascal
program Project1;

begin
  WriteLn('Bonjour de Lazarus !');
end.
```

4. Appuyez sur **F9** (ou menu **Run** → **Run**)
5. Lazarus demande où sauvegarder → choisissez votre dossier de projets
6. Une fenêtre console noire s'ouvre avec : `Bonjour de Lazarus !`

**✅ Si vous voyez ce message, tout fonctionne parfaitement !**

### Test 3 : Créer une application graphique simple

1. Menu **File** → **New...** → **Application**
2. Un formulaire vide apparaît
3. Dans la palette de composants (à gauche), onglet **Standard**
4. Double-cliquez sur **TButton** (bouton)
5. Un bouton apparaît sur le formulaire
6. Appuyez sur **F9**
7. Une fenêtre s'ouvre avec votre bouton

**✅ Si la fenêtre apparaît, les composants graphiques fonctionnent !**

## Configuration de base recommandée

Quelques réglages pour un confort optimal.

### 1. Langue de l'interface

Si Lazarus est en anglais et que vous préférez le français :

1. Menu **Tools** → **Options**
2. Section **Environment** → **General**
3. Trouvez **Language** (Langue)
4. Sélectionnez **Français** dans la liste
5. Cliquez **OK**
6. Redémarrez Lazarus pour appliquer

**Note :** La traduction française n'est pas toujours complète à 100%, certains termes techniques restent en anglais.

### 2. Taille de la police de l'éditeur

Si le texte est trop petit ou trop grand :

1. Menu **Outils** → **Options**
2. Section **Éditeur** → **Options d'affichage**
3. Cherchez **Taille de police**
4. Ajustez (recommandé : 10-12 pour un écran normal, 14-16 pour une tablette)
5. Cliquez **OK**

### 3. Thème de couleurs

Pour protéger vos yeux, surtout le soir :

1. Menu **Outils** → **Options**
2. Section **Éditeur** → **Couleurs**
3. Choisissez un thème :
   - **Default** : classique (fond blanc)
   - **Twilight** : sombre
   - **Delphi Classic** : style Delphi
4. Cliquez **OK**

### 4. Numérotation des lignes

Afficher les numéros de ligne (très utile) :

1. Menu **Outils** → **Options**
2. Section **Éditeur** → **Options générales**
3. Cochez **Afficher les numéros de ligne**
4. Cliquez **OK**

### 5. Dossier par défaut des projets

Définir où sauvegarder vos projets par défaut :

1. Menu **Outils** → **Options**
2. Section **Environnement** → **Fichiers**
3. **Répertoire des projets par défaut** → Parcourir
4. Choisissez `C:\Users\VotreNom\Documents\MesProjetsLazarus` (créez ce dossier)
5. Cliquez **OK**

## Problèmes courants et solutions

### Problème 1 : "Lazarus ne démarre pas"

**Symptômes :** Double-clic sur l'icône, rien ne se passe ou erreur immédiate.

**Solutions :**
1. **Lancer en administrateur**
   - Clic droit sur l'icône Lazarus → **Exécuter en tant qu'administrateur**
2. **Vérifier l'antivirus**
   - L'antivirus bloque peut-être Lazarus
   - Ajoutez `C:\lazarus` aux exceptions de l'antivirus
3. **Réinstaller**
   - Désinstaller complètement (Panneau de configuration → Programmes)
   - Supprimer le dossier `C:\lazarus` s'il reste
   - Réinstaller

### Problème 2 : "Cannot find fpc.exe"

**Message d'erreur :** Impossible de trouver le compilateur.

**Solutions :**
1. Menu **Outils** → **Options** → **Environnement** → **Fichiers**
2. Vérifiez **Répertoire du compilateur** : doit pointer vers `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\`
3. Si vide ou incorrect, cliquez sur **...** et naviguez manuellement
4. Cliquez **OK** et redémarrez Lazarus

### Problème 3 : "Erreur lors de la compilation"

**Message :** Erreurs mystérieuses lors de la compilation d'un projet.

**Solutions :**
1. **Nettoyer le projet**
   - Menu **Run** → **Clean up and Build** (Nettoyer et compiler)
2. **Vérifier les chemins**
   - Pas d'espaces, pas d'accents dans les chemins de fichiers
3. **Reconstruire Lazarus**
   - Menu **Tools** → **Configure "Build Lazarus"**
   - Cliquez **Build** puis attendez

### Problème 4 : "L'interface est en anglais"

**Solution :** Voir la section "Configuration de base" → "Langue de l'interface" ci-dessus.

### Problème 5 : "Les composants ne s'affichent pas"

**Symptômes :** La palette de composants est vide ou incomplète.

**Solutions :**
1. Menu **Packages** → **Configure installed packages**
2. Vérifiez que les packages sont cochés (LCL, FCL, etc.)
3. Cliquez **Save and rebuild IDE**
4. Attendez la recompilation

### Problème 6 : "Performances lentes"

**Solutions :**
1. **Désactiver l'antivirus pour le dossier Lazarus** (ajout d'exception)
2. **Fermer des fenêtres inutiles** dans l'IDE
3. **Augmenter la RAM** si votre PC a moins de 2 Go

### Problème 7 : Windows Defender bloque l'exécution

**Symptômes :** Message "Windows a protégé votre ordinateur"

**Solution :**
1. Cliquez sur **"Informations complémentaires"**
2. Cliquez sur **"Exécuter quand même"**
3. Ou ajoutez une exception dans Windows Defender :
   - Paramètres Windows → Sécurité Windows → Protection contre les virus
   - Gérer les paramètres → Exclusions → Ajouter une exclusion → Dossier
   - Sélectionnez `C:\lazarus`

## Mise à jour de Lazarus

Dans quelques mois, une nouvelle version de Lazarus sortira. Voici comment mettre à jour :

### Méthode recommandée : Installation propre

1. **Sauvegarder vos projets** (important !)
2. Désinstaller l'ancienne version :
   - Panneau de configuration → Programmes et fonctionnalités
   - Cherchez Lazarus → Désinstaller
3. Télécharger la nouvelle version depuis le site officiel
4. Installer comme expliqué dans cette section
5. Vos anciens projets restent compatibles !

### Paramètres préservés

Lazarus sauvegarde vos paramètres dans :
`C:\Users\VotreNom\AppData\Local\lazarus`

Ils sont généralement préservés lors d'une mise à jour.

## Où trouver de l'aide ?

Si vous rencontrez un problème non couvert ici :

**1. Documentation officielle**
- Dans Lazarus : Menu **Help** → **Online Help**
- Wiki : https://wiki.lazarus.freepascal.org

**2. Forum francophone**
- https://forum.lazarus.freepascal.org/index.php/board,12.0.html (section française)
- Créez un compte (gratuit) et posez votre question

**3. Forum anglophone (très actif)**
- https://forum.lazarus.freepascal.org
- Utilisez Google Translate si nécessaire

**4. Recherche sur Internet**
- "Lazarus [votre problème]" sur Google
- Beaucoup de solutions sont déjà documentées

## Conclusion

Vous avez maintenant Lazarus installé et configuré sur votre ordinateur Windows ! C'était la première étape technique, et vous l'avez franchie avec succès.

**Ce que vous avez accompli :**
- ✅ Téléchargé Lazarus + FreePascal
- ✅ Installé l'environnement complet
- ✅ Configuré les paramètres de base
- ✅ Vérifié que tout fonctionne
- ✅ Testé votre première compilation

**Prochaines étapes :**
Dans les sections suivantes, nous allons :
- Installer Lazarus sur Linux (section 9.5) si vous avez aussi Linux
- Créer votre premier vrai projet (section 9.6)
- Explorer l'IDE en profondeur (section 9.7)

**N'oubliez pas :** L'installation peut parfois présenter des petits soucis, c'est normal. Les solutions sont presque toujours simples. N'hésitez pas à consulter les forums si vous êtes bloqué.

**🚀 Vous êtes maintenant prêt à programmer en Pascal avec Lazarus !**

---

**Points clés à retenir :**
- Téléchargez toujours la version stable avec FreePascal inclus
- Choisissez l'installateur 64 bits pour les PC modernes
- Installez dans `C:\lazarus` de préférence
- Cochez toutes les options (documentation, exemples...)
- Le premier lancement peut être lent (configuration initiale)
- Testez avec un petit programme pour vérifier
- Les paramètres peuvent être personnalisés selon vos préférences
- En cas de problème, la communauté est là pour aider

⏭️ [Installation sur Ubuntu/Linux](/09-introduction-freepascal-lazarus/05-installation-ubuntu-linux.md)
