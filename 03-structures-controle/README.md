🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3. Structures de Contrôle

## Introduction au chapitre

Bienvenue dans le chapitre sur les **structures de contrôle** ! C'est ici que votre apprentissage de la programmation devient vraiment intéressant. Jusqu'à présent, vous avez appris les bases : variables, types de données, opérations simples. Maintenant, vous allez apprendre à donner de l'**intelligence** à vos programmes.

Les structures de contrôle sont les outils qui permettent à votre programme de **prendre des décisions** et de **répéter des actions**. Sans elles, vos programmes ne feraient qu'exécuter des instructions ligne par ligne, du début à la fin, sans aucune logique adaptative.

**Analogie de la vie quotidienne :**
- "**Si** il pleut, **alors** je prends un parapluie" → Structure conditionnelle
- "**Tant que** j'ai faim, je mange" → Boucle
- "**Pour** chaque jour de la semaine, je travaille" → Boucle avec compteur

## Qu'est-ce qu'une structure de contrôle ?

Une structure de contrôle est une **instruction spéciale** qui modifie l'ordre d'exécution normal de votre programme. Au lieu d'exécuter simplement les lignes les unes après les autres, elle permet de :

1. **Sauter** certaines lignes (conditions)
2. **Choisir** entre plusieurs chemins (alternatives)
3. **Répéter** des instructions plusieurs fois (boucles)

### Programme sans structure de contrôle

```pascal
program ProgrammeSimple;  
begin  
  WriteLn('Ligne 1');
  WriteLn('Ligne 2');
  WriteLn('Ligne 3');
  WriteLn('Ligne 4');
  ReadLn;
end.
```

**Flux d'exécution :**
```
Ligne 1 → Ligne 2 → Ligne 3 → Ligne 4 → Fin
```

Toujours le même déroulement, aucune adaptation.

### Programme avec structures de contrôle

```pascal
program ProgrammeIntelligent;  
var  
  age: Integer;
  i: Integer;
begin
  Write('Votre âge : ');
  ReadLn(age);

  // Structure conditionnelle
  if age >= 18 then
    WriteLn('Vous êtes majeur')
  else
    WriteLn('Vous êtes mineur');

  // Structure de boucle
  for i := 1 to 3 do
    WriteLn('Message numéro ', i);

  ReadLn;
end.
```

**Flux d'exécution :**
```
Si age >= 18 → Affiche "majeur"
       OU
Si age < 18 → Affiche "mineur"
       PUIS
Répète 3 fois → Affiche un message
```

Le programme s'adapte aux données et peut répéter des actions !

## Les trois familles de structures de contrôle

Les structures de contrôle se divisent en trois grandes catégories :

### 1. Les structures conditionnelles (Décisions)

Elles permettent à votre programme de **choisir** quoi faire selon une condition.

**Structures que vous allez apprendre :**
- `if-then-else` : "Si... alors... sinon..."
- `case-of` : "Dans le cas où..."

**Exemple :**
```pascal
if temperature < 0 then
  WriteLn('Il gèle')
else
  WriteLn('Il ne gèle pas');
```

**Quand les utiliser ?**
- Validation de données
- Choix entre plusieurs options
- Réactions différentes selon les situations

### 2. Les structures répétitives (Boucles)

Elles permettent de **répéter** des instructions plusieurs fois.

**Structures que vous allez apprendre :**
- `for-do` : Répéter un nombre connu de fois
- `while-do` : Répéter tant qu'une condition est vraie
- `repeat-until` : Répéter jusqu'à ce qu'une condition soit vraie

**Exemple :**
```pascal
for i := 1 to 10 do
  WriteLn('Nombre : ', i);
```

**Quand les utiliser ?**
- Traiter des collections de données
- Effectuer des calculs répétitifs
- Validation d'entrées utilisateur
- Parcourir des tableaux

### 3. Les structures de contrôle de flux

Elles permettent de **modifier** le comportement des boucles.

**Instructions que vous allez apprendre :**
- `break` : Sortir immédiatement d'une boucle
- `continue` : Passer à l'itération suivante

**Exemple :**
```pascal
for i := 1 to 100 do  
begin  
  if i = 50 then
    break;  // Arrête la boucle à 50
  WriteLn(i);
end;
```

## Pourquoi les structures de contrôle sont-elles importantes ?

### 1. Intelligence artificielle de base

Elles donnent à votre programme la capacité de **raisonner** :
```pascal
if score > 50 then
  WriteLn('Réussi')
else
  WriteLn('Échoué');
```

### 2. Efficacité

Au lieu d'écrire 100 fois la même instruction, vous écrivez une boucle :
```pascal
// Sans boucle : 100 lignes
WriteLn('Bonjour 1');  
WriteLn('Bonjour 2');  
// ... 98 lignes de plus

// Avec boucle : 2 lignes
for i := 1 to 100 do
  WriteLn('Bonjour ', i);
```

### 3. Interaction avec l'utilisateur

Elles permettent de **valider** les entrées et de réagir aux actions :
```pascal
repeat
  Write('Entrez un nombre positif : ');
  ReadLn(nombre);
until nombre > 0;
```

### 4. Traitement de données

Elles permettent de parcourir et analyser des collections de données :
```pascal
for i := 1 to tailleTableau do  
begin  
  if tableau[i] > maximum then
    maximum := tableau[i];
end;
```

## Vue d'ensemble du chapitre

Voici ce que vous allez apprendre dans ce chapitre :

### Section 3.1 : Instructions conditionnelles (if-then-else)
- Structure `if-then` simple
- Structure `if-then-else`
- Conditions imbriquées
- Opérateurs de comparaison et logiques

### Section 3.2 : Instructions de choix multiple (case-of)
- Structure `case-of`
- Intervalles de valeurs
- Clause `else`
- Quand utiliser case plutôt que if

### Section 3.3 : Boucles compteur (for-do)
- Boucle `for-to-do` (croissante)
- Boucle `for-downto-do` (décroissante)
- Boucles imbriquées
- Variables de boucle

### Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
- Boucle `while-do`
- Boucle `repeat-until`
- Différences entre while et repeat
- Boucles infinies et comment les éviter

### Section 3.5 : Instructions break et continue
- Sortir d'une boucle avec `break`
- Passer à l'itération suivante avec `continue`
- Contrôle de flux avancé

### Section 3.6 : Imbrication de structures
- Combiner plusieurs structures
- If dans des boucles
- Boucles dans des boucles
- Gestion de la complexité

### Section 3.7 : Gestion des erreurs simples
- Prévenir les erreurs
- Détecter les problèmes
- Messages d'erreur efficaces

### Section 3.8 : Validation des entrées utilisateur
- Techniques de validation
- Boucles de validation
- Messages clairs pour l'utilisateur

### Section 3.9 : Débogage pas à pas
- Utiliser le débogueur de Lazarus
- Points d'arrêt
- Inspecter les variables
- Trouver et corriger les bugs

## Progression pédagogique

Ce chapitre suit une progression logique :

```
Décisions simples (if)
    ↓
Décisions multiples (case)
    ↓
Répétitions avec compteur (for)
    ↓
Répétitions conditionnelles (while/repeat)
    ↓
Contrôle avancé (break/continue)
    ↓
Combinaison de structures
    ↓
Robustesse (erreurs et validation)
    ↓
Débogage
```

Chaque section s'appuie sur les précédentes. Il est important de les étudier dans l'ordre.

## Concepts clés à retenir

### Le flux d'exécution

Comprendre comment votre programme "coule" d'une instruction à l'autre est essentiel :

```
Programme linéaire :        Programme avec structures :
    Instruction 1                Instruction 1
         ↓                            ↓
    Instruction 2            [Condition] → Chemin A ou B
         ↓                            ↓
    Instruction 3                [Boucle] ← Retour
         ↓                            ↓
       Fin                          Fin
```

### Les conditions booléennes

Toutes les structures conditionnelles et de boucle reposent sur des **expressions booléennes** (vrai/faux) :
- `age >= 18` → vrai ou faux
- `note < 10` → vrai ou faux
- `(x > 0) and (x < 100)` → vrai ou faux

### L'indentation

Pour garder votre code lisible, **indentez** toujours les instructions à l'intérieur d'une structure :

```pascal
// BON
if condition then  
begin  
  instruction1;
  instruction2;
end;

// MAUVAIS
if condition then  
begin  
instruction1;  
instruction2;  
end;  
```

### BEGIN-END

Pour regrouper plusieurs instructions dans une structure, utilisez `begin` et `end` :

```pascal
if age >= 18 then  
begin                    // Début du bloc  
  WriteLn('Majeur');
  WriteLn('Autorisé');
end;                     // Fin du bloc
```

## Comment aborder ce chapitre ?

### 1. Lisez attentivement

Chaque concept s'appuie sur le précédent. Ne sautez pas de sections.

### 2. Expérimentez

Tapez les exemples dans Lazarus et exécutez-les. Modifiez-les pour voir ce qui se passe.

### 3. Faites des erreurs

Les erreurs sont normales et instructives. Utilisez le débogueur pour comprendre.

### 4. Pratiquez

La maîtrise vient avec la pratique. Écrivez vos propres petits programmes.

### 5. Prenez votre temps

N'essayez pas de tout apprendre en une journée. Prenez le temps d'assimiler chaque concept.

## Exemples de ce que vous pourrez faire

Après avoir terminé ce chapitre, vous serez capable de créer :

**Programme de calcul de moyenne avec validation :**
```pascal
repeat
  Write('Note (0-20) : ');
  ReadLn(note);
  if (note < 0) or (note > 20) then
    WriteLn('Note invalide !');
until (note >= 0) and (note <= 20);
```

**Jeu de devinette :**
```pascal
nombreSecret := Random(100) + 1;  
repeat  
  Write('Devinez : ');
  ReadLn(essai);
  if essai < nombreSecret then
    WriteLn('Plus grand !')
  else if essai > nombreSecret then
    WriteLn('Plus petit !');
until essai = nombreSecret;  
WriteLn('Gagné !');  
```

**Affichage de motifs :**
```pascal
for ligne := 1 to 5 do  
begin  
  for colonne := 1 to ligne do
    Write('*');
  WriteLn;
end;
```

**Menu interactif :**
```pascal
repeat
  WriteLn('1. Nouveau');
  WriteLn('2. Ouvrir');
  WriteLn('3. Quitter');
  ReadLn(choix);

  case choix of
    1: WriteLn('Nouveau fichier');
    2: WriteLn('Ouvrir fichier');
    3: WriteLn('Au revoir !');
  else
    WriteLn('Choix invalide');
  end;
until choix = 3;
```

## Conseils pour réussir

### ✓ À faire

- **Pratiquez** : La programmation s'apprend en programmant
- **Expérimentez** : Modifiez les exemples pour voir ce qui se passe
- **Déboguez** : Utilisez le débogueur pour comprendre le flux
- **Lisez du code** : Étudiez les exemples fournis
- **Commencez simple** : Maîtrisez les bases avant d'aller plus loin

### ✗ À éviter

- **Ne pas précipiter** : Prenez le temps de comprendre
- **Ne pas copier-coller** sans comprendre
- **Ne pas ignorer** les messages d'erreur
- **Ne pas avoir peur** de faire des erreurs
- **Ne pas sauter** des sections

## Terminologie importante

Voici les termes clés que vous rencontrerez :

- **Condition** : Expression qui est vraie ou fausse
- **Bloc** : Groupe d'instructions entre `begin` et `end`
- **Itération** : Un passage dans une boucle
- **Imbrication** : Structure à l'intérieur d'une autre structure
- **Flux d'exécution** : Ordre dans lequel les instructions s'exécutent
- **Boucle infinie** : Boucle qui ne se termine jamais
- **Validation** : Vérification de la validité des données
- **Point d'arrêt** : Marque pour arrêter le programme en mode débogage

## Ressources et aide

### Dans Lazarus

- **F1** : Aide contextuelle
- **Ctrl+Espace** : Complétion automatique
- **F9** : Exécuter avec débogueur
- **F5** : Placer/retirer un point d'arrêt

### Communauté

- Forums FreePascal
- Documentation officielle : https://www.freepascal.org/docs.html
- Wiki Lazarus : https://wiki.freepascal.org/

## Prêt à commencer ?

Vous avez maintenant une vue d'ensemble de ce qui vous attend dans ce chapitre. Les structures de contrôle sont le **cœur** de la programmation. Une fois que vous les maîtriserez, vous pourrez créer des programmes vraiment utiles et intéressants.

**Conseil final :** Ne vous découragez pas si certains concepts semblent difficiles au début. Ils deviendront naturels avec la pratique. Chaque programmeur est passé par là !

---

## Ce que vous avez déjà

Avant de commencer les structures de contrôle, vous disposez déjà de :  

✓ Variables et types de données  
✓ Opérateurs arithmétiques et logiques  
✓ Entrées/sorties de base (`ReadLn`, `WriteLn`)  
✓ Environnement Lazarus  

## Ce que vous allez acquérir  

Après ce chapitre, vous maîtriserez :  

✓ Prendre des décisions dans votre code  
✓ Répéter des actions efficacement  
✓ Valider les entrées utilisateur  
✓ Créer des programmes interactifs  
✓ Déboguer vos programmes  
✓ Écrire du code robuste et professionnel  

---

Commençons maintenant par la première structure : les **instructions conditionnelles** !

👉 **Section suivante : 3.1 Instructions conditionnelles (if-then-else)**

⏭️ [Instructions conditionnelles (if-then-else)](/03-structures-controle/01-instructions-conditionnelles-if-then-else.md)
