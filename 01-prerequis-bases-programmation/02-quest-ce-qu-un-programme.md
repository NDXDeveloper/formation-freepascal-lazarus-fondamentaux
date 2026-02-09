🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.2 Qu'est-ce qu'un programme ?

## Introduction

Maintenant que vous comprenez les concepts fondamentaux de l'informatique, il est temps de répondre à une question essentielle : qu'est-ce qu'un programme informatique ? Cette section vous aidera à comprendre la nature d'un programme et comment il fonctionne.

## Définition d'un programme

### En termes simples

Un programme informatique est une **suite d'instructions** écrites dans un langage que l'ordinateur peut comprendre et exécuter. C'est comme une recette de cuisine, mais pour un ordinateur.

Tout comme une recette indique étape par étape comment préparer un plat, un programme indique étape par étape ce que l'ordinateur doit faire pour accomplir une tâche.

### Les éléments constitutifs

Un programme est composé de :
- **Instructions** : les actions que l'ordinateur doit effectuer
- **Données** : les informations sur lesquelles le programme travaille
- **Logique** : l'ordre et les conditions d'exécution des instructions

## Analogie avec le monde réel

### La recette de cuisine

Prenons l'exemple d'une recette de crêpes :

```
Ingrédients (données) :
- 250g de farine
- 3 œufs
- 500ml de lait
- Une pincée de sel

Instructions :
1. Mélanger la farine et les œufs
2. Ajouter progressivement le lait
3. Ajouter le sel
4. Laisser reposer 1 heure
5. Faire chauffer une poêle
6. Pour chaque crêpe :
   - Verser une louche de pâte
   - Attendre 2 minutes
   - Retourner la crêpe
   - Attendre 2 minutes
7. Servir
```

Un programme fonctionne exactement de la même manière :
- Il prend des **données en entrée** (ingrédients)
- Il effectue des **traitements** (mélanger, chauffer)
- Il utilise des **boucles** (pour chaque crêpe)
- Il produit un **résultat** (crêpes prêtes)

### Le GPS

Un autre exemple : votre GPS est un programme qui :
1. **Reçoit** votre position actuelle et votre destination (entrées)
2. **Calcule** le meilleur itinéraire (traitement)
3. **Affiche** les directions à suivre (sortie)
4. **Met à jour** en temps réel selon votre position (boucle)

## Les différents types de programmes

### Programmes en ligne de commande (console)

Ce sont les programmes les plus simples :
- Ils s'exécutent dans une fenêtre de texte
- Pas d'interface graphique
- L'utilisateur tape des commandes au clavier
- Les résultats s'affichent sous forme de texte

**Exemple :** un programme qui calcule la moyenne de notes
```
Entrez la première note : 15
Entrez la deuxième note : 18
Entrez la troisième note : 12
Moyenne : 15
```

**Avantages :**
- Simples à créer
- Parfaits pour apprendre
- Efficaces pour des tâches automatisées

### Programmes avec interface graphique (GUI)

Ce sont les programmes que vous utilisez tous les jours :
- Fenêtres, boutons, menus
- Interaction à la souris
- Plus conviviaux pour l'utilisateur

**Exemples :** navigateur web, traitement de texte, lecteur vidéo

**Avantages :**
- Plus intuitifs
- Plus agréables à utiliser
- Permettent des interactions riches

### Services et programmes en arrière-plan

Ce sont des programmes qui fonctionnent sans interface visible :
- Ils tournent en permanence sur votre ordinateur
- Vous ne les voyez pas, mais ils sont actifs
- Ils réalisent des tâches spécifiques

**Exemples :** antivirus, serveur web, service de synchronisation cloud

### Applications web

Ce sont des programmes qui s'exécutent dans un navigateur :
- Le code s'exécute sur un serveur distant
- L'affichage se fait dans votre navigateur
- Pas besoin d'installation

**Exemples :** webmail, réseaux sociaux, Google Docs

## Le cycle de vie d'un programme

### 1. Conception

Avant d'écrire une seule ligne de code, il faut :
- **Définir le problème** à résoudre
- **Analyser les besoins** : que doit faire le programme ?
- **Concevoir l'algorithme** : comment le programme va-t-il fonctionner ?
- **Planifier la structure** : comment organiser le code ?

### 2. Écriture du code (programmation)

C'est la phase où le programmeur :
- Écrit le code source dans un langage de programmation
- Utilise un éditeur de texte ou un IDE (Environnement de Développement Intégré)
- Respecte les règles de syntaxe du langage

**Exemple de code source en Pascal :**
```pascal
program Bonjour;
begin
  WriteLn('Bonjour le monde !');
end.
```

### 3. Compilation

Le code source doit être traduit en langage machine :
- Le **compilateur** lit le code source
- Il vérifie les erreurs de syntaxe
- Il traduit le code en instructions binaires
- Il produit un **fichier exécutable**

**Sur Windows :** le fichier a l'extension `.exe`  
**Sur Linux :** le fichier n'a généralement pas d'extension  

### 4. Test et débogage

Une fois compilé, le programme doit être testé :
- **Tests fonctionnels** : le programme fait-il ce qu'il doit faire ?
- **Recherche de bugs** : y a-t-il des erreurs de logique ?
- **Débogage** : correction des erreurs trouvées
- **Tests limites** : que se passe-t-il dans des cas extrêmes ?

### 5. Déploiement

Quand le programme est prêt :
- Il est distribué aux utilisateurs
- Installation sur les ordinateurs cibles
- Documentation fournie

### 6. Maintenance

Après le déploiement :
- Correction de bugs découverts par les utilisateurs
- Ajout de nouvelles fonctionnalités
- Mises à jour pour rester compatible avec les nouveaux systèmes

## Comment un programme s'exécute

### Chargement en mémoire

Quand vous lancez un programme :
1. Le système d'exploitation **lit** le fichier exécutable sur le disque dur
2. Il **charge** le programme en mémoire RAM
3. Il **alloue** de l'espace mémoire pour les données du programme
4. Il **transfère** le contrôle au processeur

### Exécution séquentielle

Le processeur exécute les instructions **une par une**, dans l'ordre :

```
Instruction 1 → Instruction 2 → Instruction 3 → ...
```

Chaque instruction correspond à une opération élémentaire :
- Effectuer un calcul
- Lire ou écrire en mémoire
- Afficher quelque chose à l'écran
- Lire une entrée utilisateur
- Etc.

### Le pointeur d'instruction

Le processeur garde en mémoire **où il en est** dans le programme :
- Il utilise un "pointeur d'instruction"
- Ce pointeur indique quelle est la prochaine instruction à exécuter
- Après chaque instruction, le pointeur avance

### Branchements et boucles

Le programme n'est pas toujours strictement linéaire :
- **Conditions** : le programme peut sauter certaines instructions
- **Boucles** : le programme peut revenir en arrière et répéter des instructions
- **Appels de fonctions** : le programme peut sauter à un autre endroit, puis revenir

## Les langages de programmation

### Pourquoi différents langages ?

Il existe des centaines de langages de programmation, car :
- Chaque langage a ses points forts
- Certains sont adaptés à des domaines particuliers
- Certains sont plus faciles à apprendre
- Les technologies évoluent et de nouveaux langages apparaissent

### Classification des langages

**Langages bas niveau**
- Proches du langage machine
- Très performants
- Difficiles à lire et à écrire
- Exemples : Assembleur

**Langages de niveau intermédiaire**
- Bon compromis entre performance et lisibilité
- Contrôle fin de la mémoire
- Exemples : C, Pascal, Rust

**Langages de haut niveau**
- Très lisibles, proches du langage humain
- Gestion automatique de la mémoire
- Plus lents mais plus faciles à utiliser
- Exemples : Python, Java, JavaScript

### Langages compilés vs interprétés

**Langages compilés** (comme Pascal)
- Le code est traduit **une fois** en fichier exécutable
- L'exécution est rapide
- Le fichier peut être distribué sans le code source
- Exemples : Pascal, C, C++, Rust

**Langages interprétés**
- Le code est traduit **ligne par ligne** pendant l'exécution
- Plus lent à l'exécution
- Nécessite un interpréteur installé
- Exemples : Python, JavaScript, PHP

**Langages hybrides**
- Compilés dans un format intermédiaire
- Puis interprétés par une machine virtuelle
- Exemples : Java, C#

## Le langage Pascal : un excellent choix pour débuter

### Pourquoi apprendre Pascal ?

**1. Clarté et lisibilité**
- La syntaxe est très proche du langage naturel
- Le code est facile à lire et à comprendre
- Les instructions sont explicites

**2. Pédagogique**
- Conçu à l'origine pour l'enseignement
- Encourage les bonnes pratiques
- Structure claire et logique

**3. Complet**
- Permet d'apprendre tous les concepts fondamentaux
- De la programmation simple à la programmation orientée objet
- Du mode console aux interfaces graphiques

**4. Performant**
- Langage compilé, donc rapide à l'exécution
- Adapté aux applications professionnelles

**5. Multi-plateforme**
- FreePascal fonctionne sur Windows, Linux, macOS
- Un même code peut fonctionner sur différents systèmes

### Comparaison avec d'autres langages

**Pascal vs Python**
- Pascal : compilé, plus rapide, typage strict
- Python : interprété, plus facile pour débuter, typage dynamique

**Pascal vs C**
- Pascal : plus lisible, moins de pièges pour débutants
- C : plus proche du matériel, utilisé pour les systèmes

**Pascal vs Java**
- Pascal : plus simple, compilation native
- Java : orienté objet pur, machine virtuelle

## Structure générale d'un programme Pascal

Voici à quoi ressemble un programme Pascal simple :

```pascal
program NomDuProgramme;

{ Section des déclarations }
var
  variable1: Integer;
  variable2: String;

{ Corps du programme }
begin
  { Instructions }
  WriteLn('Début du programme');
  variable1 := 42;
  WriteLn('Valeur : ', variable1);
  WriteLn('Fin du programme');
end.
```

**Éléments clés :**
- `program` : définit le nom du programme
- `var` : section des déclarations de variables
- `begin` ... `end.` : bloc principal d'instructions
- `;` : termine chaque instruction
- `{ }` : commentaires

Nous étudierons tout cela en détail dans les chapitres suivants.

## Les erreurs de programmation

### Types d'erreurs

**1. Erreurs de syntaxe**
- Le code ne respecte pas les règles du langage
- Détectées lors de la compilation
- Le programme ne peut pas être compilé

Exemple : oublier un point-virgule
```pascal
WriteLn('Bonjour')  // Erreur : manque le ;
```

**2. Erreurs d'exécution**
- Le programme compile mais plante pendant l'exécution
- Causées par des opérations impossibles

Exemple : division par zéro
```pascal
resultat := 10 / 0;  // Erreur à l'exécution !
```

**3. Erreurs de logique**
- Le programme fonctionne mais ne fait pas ce qu'il devrait
- Les plus difficiles à détecter
- Nécessitent des tests approfondis

Exemple : mauvaise formule de calcul
```pascal
moyenne := (note1 + note2) * 2;  // Devrait être / 2
```

### L'importance du débogage

Le débogage (debugging) est l'art de trouver et corriger les erreurs :
- C'est une compétence essentielle du programmeur
- Il faut être patient et méthodique
- Utiliser des outils (débogueur, affichage de variables)
- Comprendre la logique du programme

**Conseil :** Ne vous découragez pas ! Tous les programmeurs, même les experts, passent beaucoup de temps à déboguer leur code.

## Bonnes pratiques de programmation

Même en tant que débutant, adoptez ces bonnes habitudes :

### 1. Commentez votre code
Expliquez ce que fait votre code avec des commentaires :
```pascal
{ Cette fonction calcule la moyenne de deux nombres }
```

### 2. Utilisez des noms significatifs
```pascal
// Mauvais
var x, y, z: Integer;

// Bon
var nombreEleves, totalPoints, moyenne: Integer;
```

### 3. Indentez correctement
```pascal
// Bon
if condition then
begin
  instruction1;
  instruction2;
end;
```

### 4. Testez régulièrement
Ne codez pas tout d'un coup. Testez fréquemment au fur et à mesure.

### 5. Gardez les choses simples
Un code simple est plus facile à comprendre et à maintenir.

## Conclusion

Vous savez maintenant ce qu'est un programme informatique :
- Une suite d'instructions exécutées par l'ordinateur
- Qui passe par plusieurs étapes : conception, écriture, compilation, test
- Qui peut prendre différentes formes : console, graphique, web
- Qui peut contenir différents types d'erreurs

Pascal est un excellent langage pour apprendre à programmer grâce à sa clarté et sa structure logique. Dans les sections suivantes, nous allons approfondir les concepts nécessaires avant d'écrire nos premiers programmes.

**Points clés à retenir :**
- Un programme est comme une recette que suit l'ordinateur
- Le code source est compilé en fichier exécutable
- Il existe différents types de langages de programmation
- Pascal est particulièrement adapté aux débutants
- Programmer implique aussi de tester et déboguer son code

⏭️ [Systèmes de numération et représentation des données](/01-prerequis-bases-programmation/03-systemes-numeration-representation-donnees.md)
