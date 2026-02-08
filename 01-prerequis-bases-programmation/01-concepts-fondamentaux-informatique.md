🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.1 Concepts fondamentaux de l'informatique

## Introduction

Avant de plonger dans la programmation avec FreePascal et Lazarus, il est essentiel de comprendre quelques concepts de base de l'informatique. Cette section vous donnera les fondations nécessaires pour bien démarrer votre apprentissage.

## Qu'est-ce qu'un ordinateur ?

Un ordinateur est une machine électronique capable de :
- **Recevoir** des informations (données d'entrée)
- **Traiter** ces informations selon des instructions précises
- **Stocker** des informations en mémoire
- **Restituer** des résultats (données de sortie)

Contrairement à une calculatrice simple, un ordinateur peut effectuer une grande variété de tâches différentes en changeant simplement les instructions qu'on lui donne.

## Les composants d'un ordinateur

Un ordinateur se compose de deux grandes catégories d'éléments :

### Le matériel (Hardware)

Ce sont les composants physiques que vous pouvez toucher :

#### Le processeur (CPU - Central Processing Unit)
- C'est le "cerveau" de l'ordinateur
- Il exécute les instructions des programmes
- Sa vitesse se mesure en GHz (Gigahertz)
- Plus il est rapide, plus il peut traiter d'opérations par seconde

#### La mémoire vive (RAM - Random Access Memory)
- C'est la mémoire de travail temporaire
- Elle stocke les programmes et données en cours d'utilisation
- Son contenu est effacé quand l'ordinateur s'éteint
- Plus vous avez de RAM, plus vous pouvez faire de choses simultanément

#### Le stockage (Disque dur, SSD)
- C'est la mémoire permanente
- Il conserve vos fichiers même quand l'ordinateur est éteint
- Les programmes y sont stockés avant d'être chargés en RAM pour être exécutés

#### Les périphériques d'entrée/sortie
- **Entrée** : clavier, souris, microphone, webcam
- **Sortie** : écran, imprimante, haut-parleurs
- **Entrée/Sortie** : écran tactile, clé USB, réseau

### Le logiciel (Software)

Ce sont les programmes, les instructions immatérielles :

#### Le système d'exploitation (OS)
- C'est le logiciel de base qui gère l'ordinateur
- Exemples : Windows, Linux (Ubuntu), macOS
- Il fait le lien entre le matériel et les autres programmes
- Il gère les fichiers, la mémoire, les périphériques

#### Les applications
- Ce sont les programmes que vous utilisez quotidiennement
- Exemples : navigateur web, traitement de texte, jeux
- Chaque application est conçue pour une tâche spécifique

#### Les pilotes (Drivers)
- Ce sont des petits programmes qui permettent au système d'exploitation de communiquer avec le matériel
- Chaque périphérique (imprimante, carte graphique, etc.) a besoin de son pilote

## Le langage binaire : la base de tout

### Pourquoi le binaire ?

Les ordinateurs ne comprennent qu'une seule chose : l'électricité. Un composant électronique peut avoir deux états :
- **Courant passe** : état "1" (vrai)
- **Courant ne passe pas** : état "0" (faux)

C'est pour cette raison que les ordinateurs utilisent le **système binaire**, qui n'utilise que deux chiffres : 0 et 1.

### Les bits et les octets

**Le bit** (binary digit)
- C'est l'unité la plus petite en informatique
- Un bit peut valoir 0 ou 1
- C'est comme un interrupteur : allumé ou éteint

**L'octet** (byte en anglais)
- Un octet = 8 bits
- Exemple : 10110011 est un octet
- Avec un octet, on peut représenter 256 valeurs différentes (de 0 à 255)

**Les unités de mesure**
- 1 kilo-octet (Ko) = 1 024 octets
- 1 méga-octet (Mo) = 1 024 Ko = environ 1 million d'octets
- 1 giga-octet (Go) = 1 024 Mo = environ 1 milliard d'octets
- 1 téra-octet (To) = 1 024 Go = environ 1 000 milliards d'octets

## Comment fonctionne un programme ?

### Du code source au programme exécutable

1. **Écriture du code source**
   - Le programmeur écrit des instructions dans un langage de programmation (comme Pascal)
   - Ce code est lisible par un humain
   - Il est stocké dans un fichier texte

2. **Compilation**
   - Un programme spécial appelé "compilateur" traduit le code source
   - Il le transforme en langage machine (binaire)
   - Le résultat est un fichier exécutable (.exe sur Windows, sans extension sur Linux)

3. **Exécution**
   - L'utilisateur lance le programme
   - Le système d'exploitation charge le programme en mémoire RAM
   - Le processeur exécute les instructions une par une

### Langage machine vs langage de haut niveau

**Langage machine**
- C'est le langage que le processeur comprend directement
- Suite de codes binaires illisibles pour un humain
- Exemple : `B8 05 00` (instruction binaire en hexadécimal)

**Langage assembleur**
- Représentation lisible du langage machine, utilisant des mnémoniques
- Chaque instruction correspond à une instruction machine
- Exemple : `MOV AX, 5` (déplace la valeur 5 dans un registre)

**Langage de haut niveau**
- Conçu pour être lu et écrit facilement par un humain
- Exemples : Pascal, C, Python, Java
- Doit être traduit en langage machine pour être exécuté
- Exemple en Pascal : `x := 5;` (attribue la valeur 5 à la variable x)

## Les données et leur représentation

### Les types de données de base

En programmation, les données peuvent être de différentes natures :

**Les nombres entiers**
- Exemples : -5, 0, 42, 1000
- Utilisés pour compter, numéroter, etc.

**Les nombres à virgule**
- Exemples : 3.14, -0.5, 2.71828
- Utilisés pour les calculs scientifiques, les prix, etc.

**Les caractères et textes**
- Un caractère : 'A', 'z', '5', '$'
- Un texte (chaîne) : "Bonjour", "Pascal", "123"

**Les booléens**
- Seulement deux valeurs possibles : VRAI ou FAUX
- Utilisés pour les décisions et les tests logiques

### Comment sont stockées les données ?

Toutes les données sont stockées en binaire dans la mémoire, mais selon des formats différents :

**Les entiers**
- Un entier est stocké directement en binaire
- Exemple : le nombre 5 en binaire = 00000101 (sur 8 bits)

**Les caractères**
- Chaque caractère a un code numérique
- Table ASCII : 'A' = 65, 'B' = 66, 'a' = 97, '0' = 48, etc.
- En Unicode (plus moderne) : possibilité de représenter tous les alphabets du monde

**Les nombres à virgule**
- Stockés en notation scientifique (mantisse et exposant)
- Format standardisé : IEEE 754

## L'importance de l'algorithme

### Qu'est-ce qu'un algorithme ?

Un algorithme est une suite d'instructions précises et ordonnées qui permettent de résoudre un problème ou d'effectuer une tâche.

**Caractéristiques d'un bon algorithme :**
- **Précis** : chaque étape doit être claire et sans ambiguïté
- **Fini** : il doit se terminer après un nombre fini d'étapes
- **Efficace** : il doit résoudre le problème en un temps raisonnable

### Exemple simple : faire du thé

Voici un algorithme pour faire du thé (en langage naturel) :

```
1. Remplir la bouilloire d'eau
2. Allumer la bouilloire
3. Attendre que l'eau bouille
4. Mettre un sachet de thé dans une tasse
5. Verser l'eau bouillante dans la tasse
6. Attendre 3 à 5 minutes
7. Retirer le sachet de thé
8. Ajouter du sucre si désiré
9. Fin
```

Un programme informatique suit le même principe : c'est une série d'instructions exécutées dans un ordre précis.

## La logique de programmation

### Les trois structures de base

Tout programme peut être construit avec seulement trois structures fondamentales :

**1. La séquence**
- Exécution d'instructions les unes après les autres
- Exemple : d'abord A, puis B, puis C

**2. La sélection (ou condition)**
- Choix entre différentes actions selon une condition
- Exemple : SI il pleut ALORS prendre un parapluie SINON ne rien prendre

**3. La répétition (ou boucle)**
- Répéter une action plusieurs fois
- Exemple : TANT QUE il reste des pages, lire la page suivante

Ces trois structures seront étudiées en détail dans les chapitres suivants.

## Abstraction et résolution de problèmes

### Le concept d'abstraction

L'abstraction consiste à simplifier un problème complexe en :
- Ignorant les détails non essentiels
- Se concentrant sur ce qui est important
- Divisant le problème en parties plus petites

**Exemple :**
Quand vous conduisez une voiture, vous n'avez pas besoin de comprendre comment fonctionne le moteur dans les moindres détails. Vous utilisez une interface simplifiée : volant, pédales, levier de vitesse.

### La décomposition de problèmes

Pour résoudre un problème complexe :
1. **Diviser** le problème en sous-problèmes plus petits
2. **Résoudre** chaque sous-problème séparément
3. **Assembler** les solutions pour obtenir la solution complète

Cette approche s'appelle "diviser pour régner" et est fondamentale en programmation.

## Conclusion

Vous avez maintenant une compréhension de base du fonctionnement d'un ordinateur et des concepts fondamentaux de l'informatique. Ces notions vous seront utiles tout au long de votre apprentissage de la programmation.

Points clés à retenir :
- Un ordinateur est composé de matériel (hardware) et de logiciel (software)
- Tout est stocké en binaire (0 et 1) dans la mémoire
- Un programme est une suite d'instructions traduites en langage machine
- Les algorithmes sont au cœur de la programmation
- La logique de programmation repose sur trois structures : séquence, sélection, répétition

Dans la prochaine section, nous découvrirons plus précisément ce qu'est un programme et comment il est structuré.

⏭️ [Qu'est-ce qu'un programme ?](/01-prerequis-bases-programmation/02-quest-ce-qu-un-programme.md)
