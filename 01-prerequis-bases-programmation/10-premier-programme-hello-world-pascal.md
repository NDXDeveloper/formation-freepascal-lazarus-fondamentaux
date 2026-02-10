🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.10 Premier programme "Hello World" en Pascal

## Introduction

Félicitations ! Vous êtes sur le point d'écrire votre tout premier programme. Le programme "Hello World" est une tradition en programmation : c'est le programme le plus simple possible qui affiche un message à l'écran. C'est le premier pas de tout programmeur, et aujourd'hui c'est le vôtre !

## La tradition du "Hello World"

### Origine et histoire

Le premier programme "Hello World" a été popularisé par Brian Kernighan et Dennis Ritchie dans leur livre "The C Programming Language" en 1978. Depuis, il est devenu une tradition d'apprendre un nouveau langage en écrivant ce programme simple.

**Pourquoi "Hello World" ?**
- Vérifier que votre environnement fonctionne correctement
- Comprendre la structure minimale d'un programme
- Voir le cycle complet : écrire → compiler → exécuter
- Obtenir un résultat immédiat et gratifiant
- Premier pas dans votre aventure de programmation !

### Ce que vous allez apprendre

Avec ce premier programme, vous allez :
- Écrire votre premier code Pascal
- Compiler un programme
- Exécuter votre création
- Comprendre la structure de base d'un programme
- Identifier et corriger vos premières erreurs

## Le code du programme "Hello World"

### Version minimale

Voici le code complet de votre premier programme :

```pascal
program HelloWorld;  
begin  
  WriteLn('Hello, World!');
end.
```

**C'est tout !** Ces 4 lignes constituent un programme complet et fonctionnel.

### Version commentée

Voici le même programme avec des commentaires explicatifs :

```pascal
program HelloWorld;              // Déclaration du nom du programme  
begin                            // Début du programme principal  
  WriteLn('Hello, World!');      // Afficher un message
end.                             // Fin du programme
```

## Explication ligne par ligne

### Ligne 1 : `program HelloWorld;`

```pascal
program HelloWorld;
```

**Analyse :**
- `program` : mot-clé qui indique le début d'un programme
- `HelloWorld` : le nom que vous donnez à votre programme
- `;` : point-virgule qui termine l'instruction

**Règles pour le nom du programme :**
- Commence par une lettre
- Peut contenir lettres, chiffres et underscores (_)
- Pas d'espaces ou de caractères spéciaux
- Pas sensible à la casse (HelloWorld = helloworld = HELLOWORLD)
- Convention : utiliser la notation PascalCase (majuscule à chaque mot)

**Exemples valides :**
```pascal
program MonPremierProgramme;  
program Test123;  
program Calcul_Simple;  
```

**Exemples invalides :**
```pascal
program Mon Programme;        // Espace interdit  
program 1erProgramme;         // Ne peut pas commencer par un chiffre  
program Programme-Test;       // Tiret interdit  
```

**Note importante :**
En FreePascal moderne, la ligne `program` est optionnelle pour les petits programmes, mais c'est une bonne pratique de toujours l'inclure.

### Ligne 2 : `begin`

```pascal
begin
```

**Analyse :**
- `begin` : mot-clé qui marque le début du bloc d'instructions principal
- C'est comme l'accolade ouvrante `{` en C ou Java
- Toutes les instructions du programme vont entre `begin` et `end`

**Concept :**
Le `begin` ouvre un bloc d'instructions. En Pascal, un bloc est un ensemble d'instructions regroupées. Le programme principal est contenu dans un grand bloc qui commence par `begin` et se termine par `end.`

### Ligne 3 : `WriteLn('Hello, World!');`

```pascal
WriteLn('Hello, World!');
```

**Analyse détaillée :**

**`WriteLn`** : procédure standard de Pascal
- Write + Ln (Line)
- Affiche un texte puis passe à la ligne suivante
- Équivalent de `println` en Java ou `print` avec `\n` en Python

**`(`** : parenthèse ouvrante
- Contient les paramètres de la procédure
- Ce qu'on veut afficher

**`'Hello, World!'`** : chaîne de caractères
- Délimitée par des apostrophes simples `'`
- Le texte exact qui sera affiché
- Peut contenir n'importe quel texte

**`)`** : parenthèse fermante
- Ferme la liste des paramètres

**`;`** : point-virgule
- Termine l'instruction
- TRÈS IMPORTANT : ne jamais l'oublier !
- Chaque instruction en Pascal se termine par un point-virgule

**Variante : `Write` vs `WriteLn`**
```pascal
Write('Hello');      // Affiche sans passer à la ligne  
WriteLn('World!');   // Affiche et passe à la ligne  
```

Résultat :
```
HelloWorld!
```

Avec deux `WriteLn` :
```pascal
WriteLn('Hello');  
WriteLn('World!');  
```

Résultat :
```
Hello  
World!  
```

### Ligne 4 : `end.`

```pascal
end.
```

**Analyse :**
- `end` : mot-clé qui ferme le bloc commencé par `begin`
- `.` : POINT (pas point-virgule !) qui marque la fin absolue du programme
- Le point final est OBLIGATOIRE pour le programme principal

**Distinction importante :**
- `end;` : ferme un bloc intermédiaire (if, while, procédure)
- `end.` : ferme le programme principal (avec un POINT)

**Exemple de structure complète :**
```pascal
program Test;  
begin  
  if True then
  begin
    WriteLn('Bloc interne');
  end;              // ← point-virgule pour un bloc interne
  WriteLn('Fin');
end.                // ← POINT pour la fin du programme
```

## Création du programme : Méthode 1 (Éditeur + Ligne de commande)

### Étape 1 : Créer le fichier source

**Sous Windows :**
1. Ouvrir Notepad++ (ou votre éditeur préféré)
2. Créer un nouveau fichier
3. Taper le code du programme
4. Enregistrer sous `hello.pas` dans un dossier de votre choix
   - Important : extension `.pas` obligatoire
   - Choisir l'encodage UTF-8

**Sous Linux :**
1. Ouvrir gedit ou votre éditeur préféré
2. Créer un nouveau fichier
3. Taper le code du programme
4. Enregistrer sous `hello.pas` dans votre dossier personnel

**Conseil :** Créez un dossier dédié à vos programmes Pascal
```
Windows : C:\MesProgrammesPascal\  
Linux :   ~/MesProgrammesPascal/  
```

### Étape 2 : Compiler le programme

**Sous Windows :**
1. Ouvrir l'invite de commandes (cmd)
2. Naviguer vers le dossier contenant votre fichier :
   ```cmd
   cd C:\MesProgrammesPascal
   ```
3. Compiler avec FreePascal :
   ```cmd
   fpc hello.pas
   ```

**Sous Linux :**
1. Ouvrir le terminal (Ctrl+Alt+T)
2. Naviguer vers le dossier :
   ```bash
   cd ~/MesProgrammesPascal
   ```
3. Compiler :
   ```bash
   fpc hello.pas
   ```

**Ce qui se passe pendant la compilation :**
```
Free Pascal Compiler version 3.2.2 [2024/05/21] for x86_64  
Copyright (c) 1993-2021 by Florian Klaempfl and others  
Target OS: Linux for x86-64  
Compiling hello.pas  
Linking hello  
9 lines compiled, 0.1 sec
```

**Fichiers créés :**
- `hello.exe` (Windows) ou `hello` (Linux) : le programme exécutable
- `hello.o` : fichier objet (intermédiaire)
- `hello.ppu` : unité précompilée

### Étape 3 : Exécuter le programme

**Sous Windows :**
```cmd
hello.exe
```
Ou simplement :
```cmd
hello
```

**Sous Linux :**
```bash
./hello
```
Note : le `./` indique qu'on exécute un fichier du dossier actuel

**Résultat attendu :**
```
Hello, World!
```

**Félicitations !** Vous venez d'écrire, compiler et exécuter votre premier programme !

## Création du programme : Méthode 2 (Lazarus)

### Étape 1 : Créer un nouveau projet

1. **Lancer Lazarus**

2. **Créer un projet Program :**
   - Menu : Projet → Nouveau projet
   - Sélectionner "Programme" (ou "Program")
   - Cliquer sur OK

3. **Choisir l'emplacement :**
   - Donner un nom au projet : `HelloWorld`
   - Choisir le dossier de destination
   - Cliquer sur Enregistrer

### Étape 2 : Écrire le code

Lazarus génère automatiquement un code de base :

```pascal
program HelloWorld;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
  { you can add units after this };

begin  
end.  
```

**Modifier le code :** Entre `begin` et `end.`, ajoutez :
```pascal
begin
  WriteLn('Hello, World!');
end.
```

**Code complet :**
```pascal
program HelloWorld;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes;

begin
  WriteLn('Hello, World!');
end.
```

**Notes sur le code généré :**
- `{$mode objfpc}{$H+}` : directives de compilation (on y reviendra)
- `uses` : section pour importer des bibliothèques
- Pour l'instant, ignorez ces lignes additionnelles

**Version simplifiée (fonctionne aussi) :**
Vous pouvez effacer tout et écrire simplement :
```pascal
program HelloWorld;  
begin  
  WriteLn('Hello, World!');
end.
```

### Étape 3 : Enregistrer

- Fichier → Tout enregistrer (Ctrl+Shift+S)
- Ou cliquer sur l'icône de disquette

### Étape 4 : Compiler et exécuter

**Option 1 : Tout en un (recommandé pour débuter)**
- Appuyer sur F9
- Ou Exécuter → Exécuter (F9)
- Lazarus compile puis exécute automatiquement

**Option 2 : Étape par étape**
- Compiler : Ctrl+F9 ou Exécuter → Compiler
- Exécuter : Exécuter → Exécuter (après compilation)

**Résultat :**
Une fenêtre console s'ouvre et affiche :
```
Hello, World!
```

La fenêtre peut se fermer immédiatement. Pour la garder ouverte, voir la section suivante.

### Étape 5 : Voir la fenêtre de console

**Problème :** Sous Windows, la console peut se fermer trop vite.

**Solution 1 : Ajouter une pause**
```pascal
program HelloWorld;  
begin  
  WriteLn('Hello, World!');
  WriteLn('Appuyez sur Entrée pour continuer...');
  ReadLn;    // Attend que l'utilisateur appuie sur Entrée
end.
```

**Solution 2 : Exécuter depuis la ligne de commande**
- Naviguer vers le dossier du projet
- Exécuter l'exe directement dans cmd/terminal

**Solution 3 : Configurer Lazarus**
- Exécuter → Options d'exécution
- Cocher "Utiliser une fenêtre de lancement pour les applications"

## Variantes du programme Hello World

### Afficher plusieurs lignes

```pascal
program HelloMultiline;  
begin  
  WriteLn('Bonjour !');
  WriteLn('Je suis votre premier programme.');
  WriteLn('Félicitations !');
end.
```

Résultat :
```
Bonjour !  
Je suis votre premier programme.  
Félicitations !  
```

### Utiliser Write au lieu de WriteLn

```pascal
program HelloWrite;  
begin  
  Write('Hello, ');
  Write('World!');
end.
```

Résultat :
```
Hello, World!
```

### Afficher des caractères spéciaux

```pascal
program HelloSpecial;  
begin  
  WriteLn('Hello, World!');
  WriteLn('-------------');
  WriteLn('* Bienvenue *');
  WriteLn('-------------');
end.
```

Résultat :
```
Hello, World!
-------------
* Bienvenue *
-------------
```

### Utiliser des séquences d'échappement

```pascal
program HelloEscape;  
begin  
  WriteLn('Ligne 1');
  WriteLn('Ligne 2');
  WriteLn('');                    // Ligne vide
  WriteLn('Guillemets : '' ');    // Apostrophe échappée
end.
```

**Pour afficher une apostrophe :**
Doubler l'apostrophe : `''`

Exemple :
```pascal
WriteLn('C''est super !');
```

Résultat :
```
C'est super !
```

### Avec des commentaires

```pascal
program HelloCommented;  
begin  
  // Ceci est un commentaire sur une ligne
  WriteLn('Hello, World!');

  { Ceci est un commentaire
    sur plusieurs lignes
    très utile ! }
  WriteLn('Programme terminé.');
end.
```

## Erreurs courantes et solutions

### Erreur 1 : Point-virgule manquant

**Code erroné :**
```pascal
program Hello;  
begin  
  WriteLn('Hello')     // ← Manque le point-virgule
end.
```

**Message d'erreur :**
```
Fatal: Syntax error, ";" expected but "END" found
```

**Solution :**
Ajouter le point-virgule après l'instruction :
```pascal
WriteLn('Hello');
```

### Erreur 2 : Apostrophes mal fermées

**Code erroné :**
```pascal
program Hello;  
begin  
  WriteLn('Hello, World!);     // ← Manque l'apostrophe finale
end.
```

**Message d'erreur :**
```
Fatal: Syntax error, ")" expected but "identifier WORLD" found
```

**Solution :**
Fermer correctement la chaîne :
```pascal
WriteLn('Hello, World!');
```

### Erreur 3 : Oubli du point final

**Code erroné :**
```pascal
program Hello;  
begin  
  WriteLn('Hello');
end;                  // ← Devrait être end. avec un point
```

**Message d'erreur :**
```
Fatal: Syntax error, "." expected but ";" found
```

**Solution :**
Utiliser un point après le end final :
```pascal
end.
```

### Erreur 4 : Parenthèses mal placées

**Code erroné :**
```pascal
program Hello;  
begin  
  WriteLn 'Hello';     // ← Manquent les parenthèses
end.
```

**Message d'erreur :**
```
Fatal: Syntax error, "(" expected but "string constant" found
```

**Solution :**
Ajouter les parenthèses :
```pascal
WriteLn('Hello');
```

### Erreur 5 : Nom de fichier et nom de programme différents

**Attention :** Ce n'est pas forcément une erreur, mais peut créer de la confusion.

**Bonne pratique :**
Si le fichier s'appelle `hello.pas`, le programme devrait s'appeler `Hello` :
```pascal
program Hello;
```

### Erreur 6 : Problème d'encodage (accents)

**Code :**
```pascal
program Bonjour;  
begin  
  WriteLn('Bonjour ! Comment ça va ?');
end.
```

**Problème possible :**
Les accents peuvent s'afficher incorrectement.

**Solutions :**
1. Enregistrer le fichier en UTF-8
2. Sous Windows, utiliser `chcp 65001` dans la console avant d'exécuter
3. Éviter les accents dans les premiers programmes

### Erreur 7 : Fichier non trouvé lors de la compilation

**Message :**
```
Fatal: Can't open file "helo.pas"
```

**Causes possibles :**
- Faute de frappe dans le nom du fichier
- Vous n'êtes pas dans le bon dossier
- Le fichier n'a pas été enregistré

**Solutions :**
- Vérifier l'orthographe : `hello.pas` pas `helo.pas`
- Utiliser `cd` pour aller dans le bon dossier
- Vérifier que le fichier existe avec `dir` (Windows) ou `ls` (Linux)

## Comprendre le processus de compilation

### Du code source à l'exécutable

**Étapes :**

1. **Code source** (`hello.pas`)
   - Fichier texte lisible par l'humain
   - Contient les instructions en Pascal

2. **Compilation** (`fpc hello.pas`)
   - Le compilateur lit le code
   - Vérifie la syntaxe
   - Traduit en langage machine

3. **Fichier objet** (`hello.o`)
   - Fichier intermédiaire en binaire
   - Pas encore exécutable

4. **Édition de liens (linking)**
   - Lie le code avec les bibliothèques
   - Crée l'exécutable final

5. **Exécutable** (`hello.exe` ou `hello`)
   - Programme final
   - Peut être exécuté directement

### Ce que fait le compilateur

**Analyse syntaxique :**
- Vérifie que le code respecte les règles de Pascal
- Détecte les erreurs de syntaxe (point-virgule manquant, etc.)

**Analyse sémantique :**
- Vérifie que le code a du sens
- Variables déclarées, types corrects, etc.

**Génération de code :**
- Traduit le code Pascal en instructions machine
- Optimise si demandé

**Messages du compilateur :**
- **Error (Erreur)** : le programme ne peut pas être compilé
- **Warning (Avertissement)** : potentiel problème mais compilation possible
- **Note (Note)** : information, pas de problème

## Options de compilation utiles

### Compiler avec optimisation

```bash
fpc -O3 hello.pas
```
- `-O1` : optimisation de base
- `-O2` : optimisation normale
- `-O3` : optimisation maximale

### Compiler en mode verbeux

```bash
fpc -vewn hello.pas
```
- `-v` : mode verbeux
- `-e` : afficher les erreurs
- `-w` : afficher les warnings
- `-n` : afficher les notes

### Spécifier le nom de l'exécutable

```bash
fpc hello.pas -o mon_programme
```

### Compiler pour une architecture spécifique

```bash
# Windows 64-bit
fpc -Twin64 hello.pas

# Windows 32-bit
fpc -Twin32 hello.pas

# Linux 64-bit
fpc -Tlinux hello.pas
```

## Tester votre compréhension

### Variations à essayer

**1. Modifier le message**
Changez "Hello, World!" en votre propre message.

**2. Plusieurs lignes**
Affichez votre nom, votre âge, votre ville sur des lignes séparées.

**3. Avec et sans WriteLn**
Expérimentez avec `Write` et `WriteLn` pour voir la différence.

**4. Ajouter des commentaires**
Documentez chaque ligne de votre programme.

**5. Créer une erreur volontaire**
Enlevez un point-virgule, compilez, observez l'erreur, corrigez.

## Au-delà du Hello World

### Ce que vous avez appris

**Félicitations !** Avec ce premier programme, vous avez appris :
- La structure de base d'un programme Pascal
- Comment écrire du code dans un fichier
- Comment compiler un programme
- Comment exécuter votre programme
- Comment identifier et corriger des erreurs basiques

### Ce qui vient ensuite

Dans les prochains chapitres, vous allez :
- Utiliser des variables pour stocker des données
- Faire des calculs
- Demander des informations à l'utilisateur
- Prendre des décisions avec des conditions
- Répéter des actions avec des boucles
- Créer vos propres fonctions et procédures

**Le Hello World est juste le début de votre voyage !**

## Bonnes pratiques dès le début

### 1. Nommage clair

```pascal
// Bon
program CalculateurDeMoyenne;

// Moins bon
program prog1;
```

### 2. Indentation correcte

```pascal
// Bon
program Test;  
begin  
  WriteLn('Ligne 1');
  WriteLn('Ligne 2');
end.

// Mauvais (même si ça compile)
program Test;  
begin  
WriteLn('Ligne 1');  
WriteLn('Ligne 2');  
end.  
```

### 3. Commentaires utiles

```pascal
// Bon commentaire : explique POURQUOI
// Affiche un message de bienvenue à l'utilisateur
WriteLn('Bienvenue !');

// Mauvais commentaire : répète ce que fait le code
// Affiche "Bienvenue !"
WriteLn('Bienvenue !');
```

### 4. Un programme = un fichier (au début)

Pour vos premiers programmes, gardez tout dans un seul fichier. Plus tard, vous apprendrez à organiser en plusieurs fichiers.

### 5. Testez immédiatement

Après chaque modification, compilez et testez. Ne changez pas 10 choses avant de tester !

### 6. Sauvegardez régulièrement

- Ctrl+S devient un réflexe
- Créez des sauvegardes de vos programmes importants
- Utilisez un système de contrôle de version (Git) plus tard

### 7. Lisez les messages d'erreur

Les erreurs ne sont pas vos ennemies ! Elles vous indiquent précisément où et quoi corriger.

## Célébrez votre réussite !

Vous venez de franchir une étape majeure : vous êtes maintenant officiellement un programmeur ! Votre premier programme fonctionne, et c'est un accomplissement dont vous pouvez être fier.

**Points importants à retenir :**
- Tout programme Pascal commence par `program` et finit par `end.` avec un POINT
- Les instructions sont entre `begin` et `end`
- Chaque instruction se termine par un point-virgule `;`
- `WriteLn` affiche un texte à l'écran
- Le processus est : écrire → compiler → exécuter
- Les erreurs sont normales et faciles à corriger

**Message final :**
N'ayez pas peur de faire des erreurs. Chaque erreur est une opportunité d'apprendre. Expérimentez, modifiez votre code, cassez des choses, et réparez-les. C'est comme ça qu'on devient un bon programmeur !

## Ressources pour aller plus loin

**Documentation officielle :**
- FreePascal Reference Guide
- Lazarus Wiki

**Communautés :**
- Forum Lazarus (forum.lazarus.freepascal.org)
- Stack Overflow (tag: pascal)
- Reddit : r/pascal

**Prochaine étape dans ce tutoriel :**
Dans le chapitre 2, nous allons découvrir le langage Pascal en profondeur : variables, types de données, opérateurs, et bien plus encore. Vous allez créer des programmes qui font vraiment quelque chose d'intéressant !

**Conseil final :**
Gardez ce premier programme précieusement. Dans quelques mois, quand vous écrirez des programmes complexes, vous pourrez le relire et mesurer tout le chemin parcouru. Tout le monde a commencé par un Hello World, même les meilleurs programmeurs du monde !

---

**Bravo et bienvenue dans le monde merveilleux de la programmation !** 🎉

⏭️ [Introduction au Langage Pascal](/02-introduction-langage-pascal/README.md)
