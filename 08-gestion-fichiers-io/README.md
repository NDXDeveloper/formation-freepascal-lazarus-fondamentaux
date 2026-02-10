🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Chapitre 8 : Gestion des Fichiers et I/O

## Introduction

Bienvenue dans le chapitre sur la gestion des fichiers et des entrées/sorties (I/O pour Input/Output) ! Jusqu'à présent, tous les programmes que vous avez écrits perdaient leurs données dès que le programme se terminait. Dans ce chapitre, vous allez apprendre à **sauvegarder** et **récupérer** des informations de manière permanente.

**Analogie :**
Imaginez que votre programme est comme votre cerveau :
- **Sans fichiers** : c'est comme avoir une mémoire à court terme uniquement. Vous vous souvenez des choses pendant que vous y pensez, mais dès que vous éteignez votre ordinateur (ou vous endormez), tout est oublié.
- **Avec fichiers** : c'est comme écrire dans un carnet. Vous pouvez fermer le carnet, partir, revenir le lendemain, et toutes vos notes sont toujours là !

---

## Pourquoi apprendre la gestion des fichiers ?

### La persistance des données

La **persistance** est la capacité de conserver des données au-delà de l'exécution d'un programme. C'est l'une des compétences les plus importantes en programmation.

**Sans persistance :**
```
Programme démarre → Utilisateur saisit des données → Programme se termine
                                                    ↓
                                            Tout est perdu !
```

**Avec persistance :**
```
Programme démarre → Charge les données du fichier → Utilisateur modifie
                                                              ↓
                    Programme sauvegarde ← Programme se termine
                            ↓
                    Données conservées !
```

### Applications concrètes

Pratiquement **toutes** les applications que vous utilisez au quotidien dépendent de la gestion des fichiers :

**🎮 Jeux vidéo**
- Sauvegardes de progression
- Paramètres graphiques et audio
- Scores et statistiques

**📝 Éditeurs de texte**
- Documents Word, PDF
- Code source de vos programmes
- Notes et listes de tâches

**💾 Bases de données**
- Contacts téléphoniques
- Historique bancaire
- Inventaire d'un magasin

**⚙️ Configuration d'applications**
- Thème sombre/clair
- Langue de l'interface
- Préférences utilisateur

**📊 Traitement de données**
- Rapports et statistiques
- Exportation de résultats
- Journaux d'événements (logs)

Sans la gestion des fichiers, aucune de ces applications ne pourrait fonctionner !

---

## Qu'est-ce qu'un fichier ?

### Définition simple

Un **fichier** est un ensemble de données stocké sur un support de stockage (disque dur, SSD, clé USB, etc.) avec un nom qui permet de le retrouver.

**Structure de base :**
```
┌─────────────────────────────────┐
│ Nom du fichier : rapport.txt    │
├─────────────────────────────────┤
│ Contenu :                       │
│ Ligne 1 : Introduction          │
│ Ligne 2 : Développement         │
│ Ligne 3 : Conclusion            │
└─────────────────────────────────┘
```

### Composants d'un fichier

**1. Nom** : identifie le fichier
- Exemple : `rapport.txt`, `photo.jpg`, `config.ini`

**2. Extension** : indique le type de fichier
- `.txt` = fichier texte
- `.dat` = fichier de données
- `.pdf` = document PDF
- `.jpg` = image

**3. Contenu** : les données réelles
- Peut être du texte, des nombres, des images, etc.

**4. Métadonnées** : informations sur le fichier
- Date de création
- Date de modification
- Taille
- Permissions (lecture, écriture)

---

## Qu'est-ce que l'I/O (Input/Output) ?

### Les deux flux de données

**Input (Entrée)** : données qui **entrent** dans votre programme
- ⌨️ Clavier (ReadLn)
- 📄 Fichiers sur disque
- 🌐 Données du réseau
- 🖱️ Souris et autres périphériques

**Output (Sortie)** : données qui **sortent** de votre programme
- 🖥️ Écran (WriteLn)
- 💾 Fichiers sur disque
- 🌐 Envoi sur le réseau
- 🖨️ Imprimante

```
          ┌─────────────────┐
Clavier → │                 │ → Écran  
Fichiers →│  VOTRE PROGRAMME│ → Fichiers  
Réseau → │                  │ → Réseau  
          └─────────────────┘
          Input          Output
```

### Les opérations I/O de base

Peu importe le type de fichier ou de données, vous effectuerez toujours les mêmes opérations de base :

1. **Ouvrir** : établir une connexion avec le fichier
2. **Lire** : récupérer des données depuis le fichier
3. **Écrire** : enregistrer des données dans le fichier
4. **Fermer** : terminer la connexion proprement

**Cycle typique :**
```
Ouvrir → Lire/Écrire → Fermer
```

**Important :** Toujours fermer un fichier après l'avoir utilisé !

---

## Ce que vous allez apprendre

Ce chapitre couvre tous les aspects essentiels de la gestion des fichiers en Pascal, du plus simple au plus avancé.

### 📚 Vue d'ensemble du chapitre

**8.1 Types de fichiers**
- Comprendre les trois types principaux : texte, binaire, typé
- Choisir le bon type selon vos besoins
- Avantages et inconvénients de chaque type

**8.2 Fichiers texte**
- Lire et écrire du texte ligne par ligne
- Manipuler des fichiers lisibles par l'humain
- Créer des logs et des rapports

**8.3 Fichiers binaires**
- Travailler avec des données brutes
- Accès direct aux données
- Copier des fichiers de tout type

**8.4 Fichiers typés**
- Stocker des structures de données (records)
- Créer des petites bases de données
- Accès rapide et direct

**8.5 Gestion des erreurs**
- Gérer les fichiers introuvables
- Traiter le disque plein
- Créer des programmes robustes

**8.6 Manipulation de répertoires**
- Créer et supprimer des dossiers
- Lister le contenu d'un répertoire
- Naviguer dans l'arborescence

**8.7 Chemins et noms de fichiers**
- Construire des chemins portables
- Extraire nom, extension, répertoire
- Gérer les chemins Windows et Linux

**8.8 Fichiers INI**
- Sauvegarder la configuration
- Format simple et lisible
- Préférences utilisateur

**8.9 Streams**
- Abstraction moderne des I/O
- Copier entre différentes sources
- Techniques avancées

---

## Concepts fondamentaux à comprendre

### 1. La persistance avant tout

**Question clé :** Comment mon application peut-elle se souvenir des informations entre deux exécutions ?

**Réponse :** En les sauvegardant dans des fichiers !

### 2. Toujours fermer les fichiers

**Règle d'or :** Un fichier ouvert doit **toujours** être fermé.

**Pourquoi ?**
- Évite la corruption des données
- Libère les ressources système
- Permet à d'autres programmes d'accéder au fichier

**Bonne pratique :**
```pascal
Ouvrir le fichier  
try  
  // Travailler avec le fichier
finally
  Fermer le fichier  // Toujours exécuté, même en cas d'erreur
end;
```

### 3. Gérer les erreurs

Les opérations sur les fichiers peuvent échouer pour de nombreuses raisons :
- 📂 Fichier introuvable
- 🔒 Permissions insuffisantes
- 💾 Disque plein
- 🔌 Périphérique déconnecté

**Vous devez toujours :**
- Vérifier si un fichier existe avant de le lire
- Gérer les erreurs avec try-except ou IOResult
- Informer l'utilisateur en cas de problème

### 4. Portabilité Windows/Linux

Les systèmes d'exploitation gèrent les fichiers différemment :

| Aspect | Windows | Linux |
|--------|---------|-------|
| Séparateur de chemin | `\` (backslash) | `/` (slash) |
| Casse | Insensible | Sensible |
| Lecteurs | `C:`, `D:`, etc. | Montage dans `/` |

**Solution :** Utiliser les fonctions portables de Pascal (PathDelim, etc.)

---

## Prérequis pour ce chapitre

Avant de commencer, assurez-vous de maîtriser :

✅ **Variables et types de données** (Integer, String, Boolean, Record)

✅ **Structures de contrôle** (if, while, for, case)

✅ **Procédures et fonctions** (paramètres, valeurs de retour)

✅ **Tableaux et chaînes** (manipulation de base)

✅ **Concepts de base de la POO** (pour les streams)

Si vous maîtrisez ces concepts, vous êtes prêt à apprendre la gestion des fichiers !

---

## Progression pédagogique

Ce chapitre suit une progression logique du plus simple au plus avancé :

```
Simple                                              Avancé
  │                                                    │
  ├─ Fichiers texte (lisibles, faciles)                │
  │                                                    │
  ├─ Fichiers typés (structures simples)               │
  │                                                    │
  ├─ Fichiers binaires (données brutes)                │
  │                                                    │
  ├─ Gestion d'erreurs (robustesse)                    │
  │                                                    │
  ├─ Répertoires et chemins (organisation)             │
  │                                                    │
  ├─ Fichiers INI (configuration)                      │
  │                                                    │
  └─ Streams (abstraction moderne)                     │
```

**Conseil :** Ne sautez pas d'étapes ! Chaque section s'appuie sur les précédentes.

---

## Exemple motivant : Un carnet d'adresses

Pour vous donner une idée concrète de ce que vous pourrez faire après ce chapitre, voici un aperçu d'un programme de carnet d'adresses :

**Fonctionnalités :**
- ✅ Ajouter des contacts (nom, téléphone, email)
- ✅ Rechercher un contact par nom
- ✅ Modifier les informations d'un contact
- ✅ Supprimer un contact
- ✅ Sauvegarder automatiquement les données
- ✅ Charger les données au démarrage
- ✅ Exporter vers un fichier texte
- ✅ Gérer les erreurs (fichier corrompu, disque plein)

**Technologies utilisées :**
- Fichiers typés pour stocker les contacts
- Fichiers INI pour les préférences
- Gestion des erreurs pour la robustesse
- Manipulation de chemins pour la portabilité

À la fin de ce chapitre, vous serez capable de créer ce type d'application et bien plus encore !

---

## Conseils pour réussir ce chapitre

### 💡 Pratique, pratique, pratique !

La gestion des fichiers s'apprend **en pratiquant**. Pour chaque section :
1. Lisez attentivement les explications
2. Étudiez les exemples fournis
3. Modifiez les exemples pour expérimenter
4. Créez vos propres petits programmes

### 🐛 N'ayez pas peur des erreurs

Les erreurs sont normales quand on apprend les fichiers :
- Fichier pas fermé → votre éditeur ne peut pas l'ouvrir
- Mauvais chemin → fichier introuvable
- Lecture au-delà de la fin → erreur de lecture

**Chaque erreur est une opportunité d'apprendre !**

### 📝 Testez avec de petits fichiers

Commencez toujours avec de petits fichiers de test :
- Créez un fichier `test.txt` avec 3 lignes
- Testez votre programme
- Vérifiez le résultat
- Augmentez progressivement la complexité

### 💾 Sauvegardez vos données importantes

Pendant vos tests, travaillez sur des **copies** de fichiers importants, jamais sur les originaux !

### 🔍 Utilisez un éditeur hexadécimal

Pour comprendre les fichiers binaires, téléchargez un éditeur hexadécimal gratuit (comme HxD pour Windows). Vous pourrez "voir" le contenu binaire des fichiers.

---

## Ressources et outils utiles

### Éditeurs de texte

- **Windows :** Notepad, Notepad++
- **Linux :** gedit, nano, vim
- **Multi-plateforme :** Visual Studio Code, Sublime Text

### Explorateurs de fichiers

- **Windows :** Explorateur Windows (Win+E)
- **Linux :** Nautilus, Dolphin, Thunar
- Apprenez à afficher les extensions de fichiers !

### Outils de développement

- **Lazarus IDE :** Votre environnement de développement principal
- **Débogueur :** Pour suivre l'exécution pas à pas
- **Éditeur hexadécimal :** Pour examiner les fichiers binaires

---

## Philosophie de ce chapitre

### Comprendre avant de mémoriser

Plutôt que de mémoriser des fonctions, nous allons **comprendre les concepts** :
- Pourquoi utiliser tel type de fichier ?
- Comment choisir la bonne approche ?
- Quelles sont les conséquences de chaque choix ?

### Du simple au complexe

Chaque section introduit progressivement des concepts nouveaux en s'appuyant sur les précédents. Ne vous découragez pas si une section semble difficile : revenez aux bases et avancez pas à pas.

### Pratique et théorie équilibrées

Ce chapitre contient :
- 📖 Explications théoriques claires
- 💻 Exemples de code commentés
- 🎯 Applications pratiques concrètes
- ⚠️ Pièges courants à éviter
- ✅ Bonnes pratiques professionnelles

---

## À retenir avant de commencer

1. **Les fichiers permettent la persistance** des données

2. **Toujours fermer** un fichier après l'avoir ouvert

3. **Gérer les erreurs** est essentiel pour des programmes robustes

4. **Tester avec de petits fichiers** avant de traiter de gros volumes

5. **La pratique** est la clé de la maîtrise

6. **Chaque type de fichier** a ses avantages et ses cas d'usage

7. **La portabilité** est importante (Windows/Linux)

---

## Prêt à démarrer ?

Vous avez maintenant une vue d'ensemble complète de ce qui vous attend dans ce chapitre. La gestion des fichiers est une compétence **fondamentale** en programmation, que vous utiliserez dans pratiquement tous vos projets futurs.

Prenez votre temps, pratiquez régulièrement, et surtout : **amusez-vous** ! La capacité de faire persister vos données ouvre un monde de possibilités pour créer des applications vraiment utiles.

Dans la prochaine section (8.1), nous commencerons par découvrir les trois types de fichiers disponibles en Pascal : texte, binaire et typé. Chacun a ses forces et ses usages spécifiques.

**Allons-y ! 🚀**

---

> **Citation inspirante :** "Les données sont le nouveau pétrole" - mais contrairement au pétrole, les données ne valent rien si vous ne savez pas les stocker et les récupérer ! Après ce chapitre, vous saurez exactement comment le faire.

⏭️ [Types de fichiers (texte, binaire, typé)](/08-gestion-fichiers-io/01-types-fichiers-texte-binaire-type.md)
