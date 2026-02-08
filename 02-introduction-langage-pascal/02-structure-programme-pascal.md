🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.2 Structure d'un programme Pascal

## Vue d'ensemble

Un programme Pascal suit une structure claire et logique, divisée en plusieurs sections bien définies. Cette organisation rigoureuse est l'une des forces du langage : elle oblige le programmeur à penser de manière structurée et rend le code facile à lire.

Imaginez un programme Pascal comme une recette de cuisine : vous commencez par donner un nom à votre recette, vous listez les ingrédients nécessaires, puis vous détaillez les étapes de préparation.

## Le squelette de base

Voici la structure minimale d'un programme Pascal :

```pascal
program NomDuProgramme;

begin
  // Instructions du programme
end.
```

C'est tout ! Dans sa forme la plus simple, un programme Pascal nécessite seulement :
- Un **en-tête** avec le mot-clé `program`
- Un **bloc d'instructions** encadré par `begin` et `end`
- Un **point final** après le dernier `end`

## Les différentes sections en détail

### 1. L'en-tête du programme

```pascal
program MonPremierProgramme;
```

L'en-tête commence par le mot-clé `program` suivi du nom que vous donnez à votre programme.

**Règles importantes :**
- Le nom doit commencer par une lettre
- Il peut contenir des lettres, des chiffres et le caractère souligné (_)
- Pas d'espaces ni de caractères spéciaux (accents, tirets, etc.)
- La ligne se termine par un point-virgule (;)

**Exemples valides :**
- `program Calculatrice;`
- `program Jeu_de_Des;`
- `program Application2024;`

**Exemples invalides :**
- `program Mon Programme;` (espace interdit)
- `program 123Start;` (ne peut pas commencer par un chiffre)
- `program Calculatrice-Simple;` (tiret interdit)

**Note importante :** Dans Free Pascal moderne, l'en-tête `program` est optionnel pour les petits programmes, mais c'est une bonne habitude de toujours l'inclure pour la clarté.

### 2. La section des déclarations (optionnelle)

Entre l'en-tête et le bloc principal, vous pouvez déclarer différents éléments :

```pascal
program ExempleComplet;

// Section des constantes
const
  TauxTVA = 20;
  Message = 'Bonjour !';

// Section des types personnalisés
type
  TAge = 0..120;

// Section des variables
var
  nom: string;
  age: integer;
  prix: real;

// Section des procédures et fonctions
procedure Afficher;
begin
  writeln('Fonction personnalisée');
end;

// Le programme principal commence ici
begin
  // Instructions principales
end.
```

**L'ordre des sections est important :**
1. `const` - Constantes
2. `type` - Types personnalisés
3. `var` - Variables
4. Procédures et fonctions
5. Le bloc principal (`begin...end.`)

Vous n'êtes pas obligé d'utiliser toutes ces sections, seulement celles dont vous avez besoin.

### 3. Le bloc principal (corps du programme)

```pascal
begin
  writeln('Première instruction');
  writeln('Deuxième instruction');
  writeln('Troisième instruction');
end.
```

Le bloc principal contient les instructions qui seront exécutées lorsque vous lancez le programme. Il est encadré par les mots-clés `begin` et `end`.

**Points essentiels :**
- `begin` marque le début du bloc
- `end.` marque la fin (notez le point après `end`)
- Chaque instruction se termine par un point-virgule (;)
- Les instructions sont exécutées dans l'ordre, de haut en bas

## La ponctuation en Pascal

Pascal est très strict sur la ponctuation. Comprendre ces règles vous évitera beaucoup d'erreurs :

### Le point-virgule (;)

Le point-virgule sert de **séparateur** entre les instructions, pas de terminateur :

```pascal
begin
  writeln('Instruction 1');
  writeln('Instruction 2');
  writeln('Instruction 3')   // Pas de ; avant end
end.
```

**Règle pratique :** mettez un point-virgule après chaque instruction, sauf avant un `end`.

Cependant, mettre un point-virgule avant `end` n'est pas une erreur (c'est juste considéré comme une instruction vide) :

```pascal
begin
  writeln('Instruction');
  ;  // Instruction vide - pas d'erreur mais inutile
end.
```

### Le point final (.)

Le point final apparaît **une seule fois** dans tout le programme : après le dernier `end` du bloc principal.

```pascal
program Test;
begin
  writeln('Bonjour');
end.   // Le point est obligatoire ici !
```

Oublier ce point est une erreur fréquente chez les débutants !

## Exemples complets progressifs

### Exemple 1 : Le strict minimum

```pascal
program Minimum;
begin
  writeln('Ceci est un programme minimal');
end.
```

Ce programme affiche simplement un message. C'est la structure la plus simple possible.

### Exemple 2 : Avec des variables

```pascal
program AvecVariables;
var
  prenom: string;
  age: integer;
begin
  prenom := 'Alice';
  age := 25;
  writeln('Bonjour ', prenom);
  writeln('Vous avez ', age, ' ans');
end.
```

Ici, nous déclarons deux variables dans la section `var`, puis nous les utilisons dans le programme principal.

### Exemple 3 : Avec constante et calcul

```pascal
program Calcul;
const
  PI = 3.14159;
var
  rayon: real;
  surface: real;
begin
  rayon := 5.0;
  surface := PI * rayon * rayon;
  // :0:2 formate le nombre reel : 0 = largeur minimale, 2 = decimales
  writeln('Surface du cercle : ', surface:0:2);
end.
```

Ce programme calcule la surface d'un cercle. Notez l'utilisation d'une constante `PI` et de variables pour les calculs.

### Exemple 4 : Structure complète

```pascal
program ApplicationComplete;

const
  NomApplication = 'Ma Super App';
  Version = '1.0';

var
  choix: integer;
  continuer: boolean;

procedure AfficherMenu;
begin
  writeln('=== ', NomApplication, ' v', Version, ' ===');
  writeln('1. Option 1');
  writeln('2. Option 2');
  writeln('0. Quitter');
end;

begin
  // Programme principal
  continuer := true;

  AfficherMenu;
  write('Votre choix : ');
  readln(choix);

  // En Pascal, on double l'apostrophe ('') pour en afficher une dans un texte
  writeln('Vous avez choisi l''option : ', choix);

  writeln('Fin du programme');
end.
```

Cet exemple montre une structure complète avec constantes, variables et une procédure personnalisée.

## Les mots-clés réservés

Pascal possède des mots-clés qui ont une signification spéciale et ne peuvent pas être utilisés comme noms de variables ou de programmes :

**Mots-clés structurels principaux :**
- `program` - Déclare le nom du programme
- `begin` - Marque le début d'un bloc
- `end` - Marque la fin d'un bloc
- `const` - Section des constantes
- `type` - Section des types
- `var` - Section des variables

**Mots-clés que nous verrons plus tard :**
- `if`, `then`, `else` - Conditions
- `for`, `while`, `repeat`, `until` - Boucles
- `procedure`, `function` - Sous-programmes
- `and`, `or`, `not` - Opérateurs logiques

Ces mots sont en minuscules par convention, mais Pascal est **insensible à la casse** : `BEGIN`, `Begin` et `begin` sont identiques.

## Conventions de style (bonnes pratiques)

Bien que Pascal ne l'impose pas strictement, voici les conventions recommandées :

**Indentation :**
```pascal
program BonStyle;
var
  x: integer;
begin
  x := 10;
  if x > 5 then
  begin
    writeln('x est grand');
    writeln('x vaut : ', x);
  end;
end.
```

**Lisibilité :**
- Indentez le contenu des blocs `begin...end` (2 ou 4 espaces)
- Sautez une ligne entre les sections
- Alignez les déclarations de variables
- Utilisez des noms de variables explicites

**Mauvais style :**
```pascal
program MauvaisStyle;
var x:integer;y:integer;
begin x:=10;y:=20;writeln(x+y);end.
```

**Bon style :**
```pascal
program BonStyle;
var
  premierNombre: integer;
  deuxiemeNombre: integer;
  somme: integer;
begin
  premierNombre := 10;
  deuxiemeNombre := 20;
  somme := premierNombre + deuxiemeNombre;
  writeln('La somme est : ', somme);
end.
```

Les deux programmes font la même chose, mais le second est infiniment plus facile à lire et à maintenir !

## Erreurs fréquentes des débutants

### 1. Oublier le point final
```pascal
program Erreur;
begin
  writeln('Bonjour');
end     // ERREUR : manque le point !
```

### 2. Confondre = et :=
```pascal
var
  x: integer;
begin
  x = 10;    // ERREUR : utiliser := pour l'affectation
  x := 10;   // CORRECT
end.
```

### 3. Oublier le point-virgule
```pascal
begin
  writeln('Ligne 1')
  writeln('Ligne 2');   // ERREUR : manque ; après 'Ligne 1'
end.
```

### 4. Mal placer les déclarations
```pascal
program Erreur;
begin
  var x: integer;   // ERREUR : var doit être avant begin
  x := 10;
end.
```

## Récapitulatif

Un programme Pascal bien structuré suit ce modèle :

```pascal
program NomDuProgramme;

// 1. Déclarations (optionnelles)
const
  // Constantes

type
  // Types personnalisés

var
  // Variables

// 2. Procédures et fonctions (optionnelles)
procedure MaProcedure;
begin
  // Code de la procédure
end;

// 3. Programme principal (obligatoire)
begin
  // Instructions principales
end.  // Point final obligatoire !
```

---

**Retenez bien :** La structure d'un programme Pascal est comme une armoire bien rangée. Chaque chose a sa place, et cette organisation rend votre code clair, lisible et facile à maintenir. Au début, cela peut sembler contraignant, mais très vite, vous apprécierez cette rigueur qui vous évite de nombreuses erreurs !

⏭️ [Variables et constantes](/02-introduction-langage-pascal/03-variables-constantes.md)
