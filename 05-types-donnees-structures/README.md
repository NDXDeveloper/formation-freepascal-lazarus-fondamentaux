🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Chapitre 5 : Types de Données Structurés

## Introduction

Jusqu'à présent, vous avez travaillé avec des **types de données simples** : Integer, Real, Boolean, Char, String. Ces types permettent de stocker une seule valeur à la fois. Mais que faire quand vous devez gérer des informations plus complexes ?

Imaginez que vous devez créer un programme de gestion d'une classe de 30 élèves. Avec ce que vous connaissez actuellement, vous devriez écrire :

```pascal
var
  nom1, nom2, nom3, nom4, ..., nom30: String;
  note1, note2, note3, note4, ..., note30: Real;
  age1, age2, age3, age4, ..., age30: Integer;
```

C'est **impraticable** ! Il vous faudrait 90 variables, et impossible de les parcourir avec une boucle. De plus, comment savoir que `nom15`, `note15` et `age15` concernent la même personne ?

C'est là qu'interviennent les **types de données structurés**.

## Qu'est-ce qu'un type structuré ?

Un type de données structuré permet de **regrouper et organiser** plusieurs données, de même type ou de types différents, dans une seule structure cohérente.

### Analogie du monde réel

Pensez à votre maison :
- Vous n'avez pas 50 objets éparpillés au hasard
- Vous avez des **tiroirs, des étagères, des armoires** qui organisent vos affaires
- Chaque meuble a une **fonction spécifique** : le tiroir à couverts, l'étagère à livres, etc.

Les types structurés sont comme ces meubles : ils vous permettent d'**organiser vos données** de manière logique et efficace.

## Pourquoi les types structurés sont-ils importants ?

### 1. Organisation des données

Au lieu de gérer des dizaines de variables séparées, vous pouvez les regrouper logiquement :

```pascal
// Au lieu de ceci (désordonné) :
var
  nom: String;
  age: Integer;
  note1, note2, note3: Real;

// Vous pouvez faire cela (organisé) :
type
  TEleve = record
    nom: String;
    age: Integer;
    notes: array[1..3] of Real;
  end;
```

### 2. Manipulation de collections

Les types structurés permettent de traiter facilement de grandes quantités de données :

```pascal
// Au lieu de répéter le code 30 fois :
Write('Note élève 1 : ');
ReadLn(note1);
Write('Note élève 2 : ');
ReadLn(note2);
// ... 28 autres fois

// Vous pouvez utiliser une boucle :
for i := 1 to 30 do
begin
  Write('Note élève ', i, ' : ');
  ReadLn(notes[i]);
end;
```

### 3. Représentation du monde réel

Les structures permettent de modéliser des entités du monde réel de manière naturelle :

- Une **personne** a un nom, un prénom, un âge, une adresse
- Une **date** a un jour, un mois, une année
- Un **produit** a un code, une désignation, un prix, un stock

### 4. Code plus lisible et maintenable

```pascal
// Code difficile à comprendre :
if (var1 > 10) and (var2 = 'actif') and (var3 < 100) then ...

// Code clair et expressif :
if (personne.age > 10) and (personne.statut = 'actif') and
   (personne.points < 100) then ...
```

## Vue d'ensemble du chapitre

Ce chapitre vous présentera les différents types de données structurés disponibles en Pascal. Voici un aperçu de ce que vous allez découvrir :

### 5.1 Tableaux statiques unidimensionnels

Les **tableaux** permettent de stocker plusieurs valeurs **du même type** sous un seul nom :

```pascal
var
  notes: array[1..30] of Real;  // 30 notes
  prenoms: array[1..30] of String;  // 30 prénoms
```

**Utilité :** Gérer des listes d'éléments similaires (notes, noms, températures, etc.)

### 5.2 Tableaux multidimensionnels

Les tableaux peuvent avoir plusieurs dimensions, comme une **grille** ou un **tableau Excel** :

```pascal
var
  grille: array[1..10, 1..10] of Integer;  // Grille 10x10
```

**Utilité :** Jeux de plateau, matrices mathématiques, tableaux de données croisées

### 5.3 Chaînes de caractères (String, ShortString)

Les **chaînes** permettent de manipuler du texte :

```pascal
var
  message: String;
  phrase: String;
```

**Utilité :** Traitement de texte, saisies utilisateur, messages, noms

### 5.4 Enregistrements (Records)

Les **enregistrements** regroupent plusieurs champs **de types différents** :

```pascal
type
  TPersonne = record
    nom: String;
    age: Integer;
    salaire: Real;
  end;
```

**Utilité :** Représenter des entités complexes avec différentes propriétés

### 5.5 Enregistrements imbriqués

Les enregistrements peuvent **contenir d'autres enregistrements** pour créer des structures hiérarchiques :

```pascal
type
  TAdresse = record
    rue: String;
    ville: String;
  end;

  TPersonne = record
    nom: String;
    adresse: TAdresse;  // Enregistrement dans un enregistrement
  end;
```

**Utilité :** Modéliser des structures complexes du monde réel

### 5.6 Tableaux d'enregistrements

Combiner tableaux et enregistrements pour gérer des **collections d'entités** :

```pascal
var
  eleves: array[1..30] of TEleve;  // 30 élèves complets
```

**Utilité :** Bases de données simples, listes de contacts, catalogues

### 5.7 Types énumérés

Les **types énumérés** définissent un ensemble de valeurs **nommées** :

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
```

**Utilité :** Rendre le code plus lisible avec des noms significatifs au lieu de nombres

### 5.8 Types ensemble (Set)

Les **ensembles** représentent des collections d'éléments **uniques** :

```pascal
var
  joursTravail: set of TJour;
begin
  joursTravail := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];
end;
```

**Utilité :** Tests d'appartenance rapides, opérations ensemblistes

### 5.9 Types intervalle

Les **intervalles** restreignent les valeurs possibles à une plage :

```pascal
type
  TAge = 0..120;
  TNote = 0..20;
```

**Utilité :** Sécurité du code, validation automatique des valeurs

### 5.10 Définition de types personnalisés

Créer vos **propres types** adaptés à votre domaine :

```pascal
type
  TEleve = record
    nom: String;
    notes: array[1..5] of Real;
  end;
```

**Utilité :** Code organisé, réutilisable et maintenable

## Progression dans le chapitre

Ce chapitre suit une progression logique :

1. **D'abord les bases** : tableaux et chaînes (structures simples)
2. **Puis les structures** : enregistrements et combinaisons
3. **Enfin les types avancés** : énumérés, ensembles, intervalles
4. **Synthèse** : créer vos propres types personnalisés

Chaque concept s'appuie sur les précédents. Ne sautez pas d'étapes !

## Comment aborder ce chapitre ?

### 1. Prenez votre temps

Les types structurés sont **fondamentaux** en programmation. Ils représentent un grand saut par rapport aux types simples. Prenez le temps de bien comprendre chaque concept avant de passer au suivant.

### 2. Expérimentez

Chaque section contient des exemples. **Tapez-les vous-même**, modifiez-les, cassez-les, réparez-les. C'est en pratiquant qu'on apprend.

### 3. Visualisez

Dessinez sur papier la structure de vos données. Comment les tableaux sont organisés ? Comment les enregistrements regroupent les informations ? La visualisation aide énormément.

### 4. Pensez aux applications

Avant d'apprendre chaque type, demandez-vous : "À quoi pourrait me servir cette structure dans un vrai programme ?"

### 5. Construisez progressivement

- Commencez par des structures **simples** (tableaux de quelques éléments)
- Puis augmentez la **complexité** progressivement
- Enfin, **combinez** les différents types pour créer des structures sophistiquées

## À quoi servent vraiment les types structurés ?

Voici quelques exemples concrets de ce que vous pourrez créer après ce chapitre :

### Exemple 1 : Carnet d'adresses

```pascal
type
  TContact = record
    nom: String;
    telephone: String;
    email: String;
  end;

var
  carnet: array[1..100] of TContact;
```

### Exemple 2 : Gestion de notes

```pascal
type
  TEleve = record
    nom: String;
    prenom: String;
    notes: array[1..5] of Real;  // 5 matières
  end;

var
  classe: array[1..30] of TEleve;
```

### Exemple 3 : Jeu de morpion

```pascal
var
  grille: array[1..3, 1..3] of Char;  // Grille 3x3
```

### Exemple 4 : Planning hebdomadaire

```pascal
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);

var
  joursTravail: set of TJour;
  temperatures: array[TJour] of Real;
```

## Différence avec les chapitres précédents

### Avant ce chapitre

Vous manipuliez des **données isolées** :

```pascal
var
  nom: String;
  age: Integer;
  note: Real;
```

Chaque variable était **indépendante**.

### Après ce chapitre

Vous manipulerez des **structures de données** :

```pascal
type
  TPersonne = record
    nom: String;
    age: Integer;
    notes: array[1..3] of Real;
  end;

var
  personne: TPersonne;
  groupe: array[1..10] of TPersonne;
```

Les données sont **organisées et reliées logiquement**.

## Compétences que vous allez acquérir

À la fin de ce chapitre, vous saurez :

✓ Créer et manipuler des **tableaux** de toutes dimensions
✓ Travailler efficacement avec les **chaînes de caractères**
✓ Définir des **enregistrements** pour représenter des entités complexes
✓ Combiner différents types pour créer des **structures sophistiquées**
✓ Utiliser des **types énumérés** pour rendre le code plus clair
✓ Manipuler des **ensembles** pour des tests d'appartenance
✓ Restreindre les valeurs avec des **intervalles**
✓ Concevoir vos **propres types** adaptés à votre domaine

## État d'esprit pour ce chapitre

### Ce que ce n'est PAS

❌ Une simple liste de syntaxes à mémoriser
❌ Des concepts abstraits sans utilité pratique
❌ Réservé aux programmeurs avancés

### Ce que c'est VRAIMENT

✓ Les **fondations** pour créer de vrais programmes
✓ Des outils pour **organiser votre pensée** et vos données
✓ La **porte d'entrée** vers la programmation orientée objet
✓ Des concepts que vous utiliserez **tous les jours** en programmation

## Message important

Les types structurés peuvent sembler intimidants au début, surtout si vous venez de maîtriser les types simples. C'est **normal** !

Rappelez-vous :

- Vous n'avez pas besoin de tout comprendre immédiatement
- Chaque concept deviendra clair avec la pratique
- Les erreurs font partie de l'apprentissage
- Même les programmeurs expérimentés continuent d'apprendre

**Conseil :** Ne cherchez pas la perfection du premier coup. Créez d'abord des structures simples qui fonctionnent, puis améliorez-les progressivement.

## Prêt à commencer ?

Ce chapitre représente une étape majeure dans votre apprentissage du Pascal. Les types structurés transformeront votre façon de programmer et vous permettront de créer des applications réellement utiles.

Prenez votre temps, expérimentez beaucoup, et surtout : **amusez-vous !**

La programmation devient vraiment intéressante quand on peut modéliser des problèmes réels avec des structures de données adaptées. Vous êtes sur le point de découvrir cette puissance.

---

**Prochaine section :** 5.1 Tableaux statiques unidimensionnels

Nous commencerons par le type structuré le plus fondamental : le tableau. C'est la base de nombreuses autres structures et un outil que vous utiliserez constamment.

Allons-y ! 🚀

⏭️ [Tableaux statiques unidimensionnels](/05-types-donnees-structures/01-tableaux-statiques-unidimensionnels.md)
