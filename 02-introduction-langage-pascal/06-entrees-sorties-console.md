🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.6 Entrées/Sorties console (Read, Write, ReadLn, WriteLn)

## Qu'est-ce que la console ?

La **console** (aussi appelée terminal, ligne de commande ou invite de commandes) est une fenêtre où votre programme peut :
- **Afficher** des informations (sorties)
- **Recevoir** des informations de l'utilisateur (entrées)

C'est le moyen le plus simple pour un programme de communiquer avec l'utilisateur. Imaginez la console comme une conversation écrite entre votre programme et l'utilisateur.

## Les sorties : afficher des informations

### Write - Affichage simple

La procédure `Write` affiche du texte ou des valeurs à l'écran, **sans retour à la ligne**.

```pascal
program ExempleWrite;  
begin  
  Write('Bonjour');
  Write(' ');
  Write('le monde');
  // Affiche : Bonjour le monde
end.
```

Le curseur reste sur la même ligne après chaque `Write`, donc tout s'affiche à la suite.

### WriteLn - Affichage avec retour à la ligne

La procédure `WriteLn` affiche du texte ou des valeurs, puis **passe à la ligne suivante**.

```pascal
program ExempleWriteLn;  
begin  
  WriteLn('Première ligne');
  WriteLn('Deuxième ligne');
  WriteLn('Troisième ligne');
end.
```

**Résultat :**
```
Première ligne  
Deuxième ligne  
Troisième ligne  
```

**Note :** `WriteLn` signifie "Write Line" (écrire une ligne). Le "Ln" est l'abréviation de "Line".

### Différence entre Write et WriteLn

```pascal
program DifferenceWriteWriteLn;  
begin  
  Write('A');
  Write('B');
  Write('C');
  WriteLn('D');
  Write('E');
  WriteLn('F');
end.
```

**Résultat :**
```
ABCD  
EF  
```

- Les trois premiers `Write` affichent sur la même ligne : ABC
- `WriteLn('D')` affiche D puis passe à la ligne
- `Write('E')` affiche E sur la nouvelle ligne
- `WriteLn('F')` affiche F puis passe à la ligne

### WriteLn sans paramètre

`WriteLn` peut être utilisé seul pour simplement passer à la ligne suivante :

```pascal
begin
  WriteLn('Ligne 1');
  WriteLn;              // Ligne vide
  WriteLn('Ligne 3');
end.
```

**Résultat :**
```
Ligne 1

Ligne 3
```

### Afficher des variables

Vous pouvez afficher le contenu de variables avec `Write` et `WriteLn` :

```pascal
program AfficherVariables;  
var  
  nom: string;
  age: integer;
  taille: real;
  estEtudiant: boolean;
begin
  nom := 'Alice';
  age := 20;
  taille := 1.65;
  estEtudiant := true;

  WriteLn('Nom : ', nom);
  WriteLn('Âge : ', age);
  WriteLn('Taille : ', taille);       // real sans formatage => notation scientifique
  WriteLn('Étudiant : ', estEtudiant); // boolean s'affiche TRUE ou FALSE
end.
```

**Résultat :**
```
Nom : Alice
Âge : 20
Taille :  1.6500000000E+00
Étudiant : TRUE
```

### Afficher plusieurs valeurs sur une ligne

Vous pouvez afficher plusieurs valeurs en les séparant par des virgules :

```pascal
program PlusieursSorties;  
var  
  prenom: string;
  nom: string;
  age: integer;
begin
  prenom := 'Marie';
  nom := 'Dupont';
  age := 25;

  // Les virgules séparent les arguments : chacun est affiché à la suite
  WriteLn('Bonjour ', prenom, ' ', nom, ', vous avez ', age, ' ans.');
end.
```

**Résultat :**
```
Bonjour Marie Dupont, vous avez 25 ans.
```

### Afficher des expressions

Vous pouvez afficher directement le résultat de calculs :

```pascal
program AfficherCalculs;  
var  
  a, b: integer;
begin
  a := 10;
  b := 5;

  WriteLn('Somme : ', a + b);
  WriteLn('Différence : ', a - b);
  WriteLn('Produit : ', a * b);
  WriteLn('Quotient : ', a / b);    // / entre entiers donne un real (notation scientifique)
end.
```

**Résultat :**
```
Somme : 15  
Différence : 5  
Produit : 50  
Quotient :  2.0000000000E+00  
```

## Formatage des sorties

### Le problème du formatage par défaut

Par défaut, Pascal affiche les nombres décimaux en notation scientifique, ce qui n'est pas toujours agréable :

```pascal
var
  prix: real;
begin
  prix := 19.99;
  WriteLn(prix);    // Affiche :  1.9990000000E+01
end.
```

### Formatage des nombres décimaux

Pour contrôler l'affichage des `real`, utilisez la notation `:largeur:decimales` :

```pascal
program FormatageReal;  
var  
  prix: real;
begin
  prix := 19.99;

  WriteLn(prix);          // Notation scientifique
  WriteLn(prix:0:2);      // 19.99 (2 décimales)
  WriteLn(prix:10:2);     //      19.99 (largeur 10, aligné à droite)
  WriteLn(prix:0:0);      // 20 (arrondi, pas de décimales)
  WriteLn(prix:0:5);      // 19.99000 (5 décimales)
end.
```

**Syntaxe :** `variable:largeurTotale:nombreDeDecimales`

- **largeurTotale** : nombre total de caractères (0 = minimum nécessaire)
- **nombreDeDecimales** : nombre de chiffres après la virgule

**Exemples :**

```pascal
var
  pi: real;
begin
  pi := 3.14159265;

  WriteLn(pi:0:2);     // 3.14
  WriteLn(pi:0:4);     // 3.1416 (arrondi)
  WriteLn(pi:10:2);    //       3.14 (largeur 10)
  WriteLn(pi:0:0);     // 3
end.
```

### Formatage des entiers

Pour les entiers, vous pouvez spécifier la largeur minimale :

```pascal
program FormatageInteger;  
var  
  nombre: integer;
begin
  nombre := 42;

  WriteLn(nombre);        // 42
  WriteLn(nombre:5);      //    42 (largeur 5, aligné à droite)
  WriteLn(nombre:10);     //         42 (largeur 10)
end.
```

**Syntaxe :** `variable:largeur`

C'est particulièrement utile pour aligner des colonnes :

```pascal
program TableauAligne;  
begin  
  // :N fonctionne aussi sur les littéraux (strings et entiers), pas seulement les variables
  WriteLn('Nom':15, 'Age':5, 'Ville':15);
  WriteLn('---------------':15, '-----':5, '---------------':15);
  WriteLn('Alice':15, 25:5, 'Paris':15);
  WriteLn('Bob':15, 30:5, 'Lyon':15);
  WriteLn('Charlie':15, 28:5, 'Marseille':15);
end.
```

**Résultat :**
```
            Nom  Age          Ville
-----------------------------------
          Alice   25          Paris
            Bob   30           Lyon
        Charlie   28      Marseille
```

### Formatage des chaînes de caractères

Les strings peuvent aussi utiliser un formatage pour l'alignement :

```pascal
var
  nom: string;
begin
  nom := 'Alice';

  WriteLn(nom);          // Alice
  WriteLn(nom:10);       //      Alice (aligné à droite)
  WriteLn(nom:-10);      // Alice      (aligné à gauche)
end.
```

- **Largeur positive** : alignement à droite
- **Largeur négative** : alignement à gauche

## Les entrées : recevoir des informations

### Read - Lire sans passer à la ligne

La procédure `Read` lit une valeur saisie par l'utilisateur, **sans consommer le retour à la ligne**.

```pascal
var
  nombre: integer;
begin
  Write('Entrez un nombre : ');
  Read(nombre);
  WriteLn('Vous avez saisi : ', nombre);
end.
```

**Important :** `Read` attend que l'utilisateur appuie sur Entrée, mais le curseur reste sur la même ligne pour la lecture suivante.

### ReadLn - Lire avec passage à la ligne

La procédure `ReadLn` lit une valeur et **consomme toute la ligne**, y compris le retour à la ligne.

```pascal
var
  nombre: integer;
begin
  Write('Entrez un nombre : ');
  ReadLn(nombre);
  WriteLn('Vous avez saisi : ', nombre);
end.
```

**En pratique :** Utilisez **toujours ReadLn** plutôt que Read pour éviter les problèmes. C'est plus prévisible et plus simple.

### Différence entre Read et ReadLn

```pascal
program DifferenceReadReadLn;  
var  
  a, b: integer;
begin
  WriteLn('=== Avec Read ===');
  Write('Entrez deux nombres : ');
  Read(a);
  Read(b);
  WriteLn('a = ', a, ', b = ', b);

  WriteLn;
  WriteLn('=== Avec ReadLn ===');
  Write('Entrez un nombre : ');
  ReadLn(a);
  Write('Entrez un autre nombre : ');
  ReadLn(b);
  WriteLn('a = ', a, ', b = ', b);
end.
```

**Avec Read :** L'utilisateur peut saisir "10 20" sur une seule ligne, les deux valeurs seront lues.

**Avec ReadLn :** Chaque valeur doit être saisie sur une ligne séparée.

### Lire différents types de données

#### Lire un entier

```pascal
var
  age: integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
  WriteLn('Vous avez ', age, ' ans.');
end.
```

#### Lire un nombre décimal

```pascal
var
  taille: real;
begin
  Write('Entrez votre taille (en m) : ');
  ReadLn(taille);
  WriteLn('Votre taille : ', taille:0:2, ' m');
end.
```

**Important :** L'utilisateur doit utiliser le **point** comme séparateur décimal, pas la virgule !
- Correct : `1.75`
- Incorrect : `1,75` (causera une erreur)

#### Lire une chaîne de caractères

```pascal
var
  nom: string;
begin
  Write('Entrez votre nom : ');
  ReadLn(nom);
  WriteLn('Bonjour ', nom, ' !');
end.
```

#### Lire un caractère

```pascal
var
  reponse: char;
begin
  Write('Continuer ? (O/N) : ');
  ReadLn(reponse);

  if (reponse = 'O') or (reponse = 'o') then
    WriteLn('Vous continuez...')
  else
    WriteLn('Vous arrêtez.');
end.
```

### Lire plusieurs valeurs sur une ligne

Vous pouvez lire plusieurs valeurs séparées par des espaces sur la même ligne :

```pascal
var
  a, b, c: integer;
begin
  Write('Entrez trois nombres : ');
  ReadLn(a, b, c);
  WriteLn('Vous avez saisi : ', a, ', ', b, ', ', c);
end.
```

L'utilisateur saisit par exemple : `10 20 30` puis appuie sur Entrée.

### ReadLn sans paramètre - Pause du programme

`ReadLn` sans paramètre attend simplement que l'utilisateur appuie sur Entrée :

```pascal
begin
  WriteLn('Calcul en cours...');
  WriteLn('Résultat : 42');
  WriteLn;
  Write('Appuyez sur Entrée pour continuer...');
  ReadLn;    // Pause : attend la touche Entrée
  WriteLn('Suite du programme...');
end.
```

C'est utile pour **empêcher la fenêtre de se fermer** immédiatement à la fin du programme.

## Gestion des erreurs de saisie

### Le problème

Si l'utilisateur saisit une valeur incompatible, le programme plante :

```pascal
var
  age: integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);    // Si l'utilisateur tape "vingt", ERREUR !
end.
```

### Solution basique : lire en string puis convertir

```pascal
var
  age: integer;
  saisie: string;
  code: integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(saisie);

  Val(saisie, age, code);

  if code = 0 then
    WriteLn('Âge : ', age, ' ans')
  else
    WriteLn('Erreur de saisie !');
end.
```

La fonction `Val` convertit une string en nombre et indique dans `code` si la conversion a réussi :
- `code = 0` : conversion réussie
- `code <> 0` : erreur (code indique la position de l'erreur)

**Version Free Pascal moderne :**

```pascal
uses SysUtils;

var
  age: integer;
  saisie: string;
begin
  Write('Entrez votre âge : ');
  ReadLn(saisie);

  try
    age := StrToInt(saisie);
    WriteLn('Âge : ', age, ' ans');
  except
    WriteLn('Erreur : veuillez saisir un nombre entier !');
  end;
end.
```

## Exemples pratiques complets

### Exemple 1 : Calculatrice interactive

```pascal
program CalculatriceInteractive;  
var  
  nombre1, nombre2: real;
  operateur: char;
  resultat: real;
begin
  WriteLn('=== CALCULATRICE ===');
  WriteLn;

  Write('Premier nombre : ');
  ReadLn(nombre1);

  Write('Opérateur (+, -, *, /) : ');
  ReadLn(operateur);

  Write('Deuxième nombre : ');
  ReadLn(nombre2);

  WriteLn;

  case operateur of
    '+': resultat := nombre1 + nombre2;
    '-': resultat := nombre1 - nombre2;
    '*': resultat := nombre1 * nombre2;
    '/':
      if nombre2 <> 0 then    // <> signifie "différent de" en Pascal
        resultat := nombre1 / nombre2
      else
      begin
        WriteLn('Erreur : division par zéro !');
        ReadLn;
        exit;
      end;
  else
    WriteLn('Opérateur invalide !');
    ReadLn;
    exit;
  end;

  WriteLn('Résultat : ', nombre1:0:2, ' ', operateur, ' ', nombre2:0:2, ' = ', resultat:0:2);

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Exemple 2 : Fiche d'identité

```pascal
program FicheIdentite;  
var  
  nom, prenom, ville: string;
  age: integer;
  taille: real;
begin
  // En Pascal, '' (deux apostrophes) insère une apostrophe littérale dans une chaîne
  WriteLn('=== FICHE D''IDENTITÉ ===');
  WriteLn;

  Write('Prénom : ');
  ReadLn(prenom);

  Write('Nom : ');
  ReadLn(nom);

  Write('Âge : ');
  ReadLn(age);

  Write('Taille (en m) : ');
  ReadLn(taille);

  Write('Ville : ');
  ReadLn(ville);

  WriteLn;
  WriteLn('========================');
  WriteLn('RÉCAPITULATIF');
  WriteLn('========================');
  WriteLn('Prénom      : ', prenom);
  WriteLn('Nom         : ', nom);
  WriteLn('Âge         : ', age, ' ans');
  WriteLn('Taille      : ', taille:0:2, ' m');
  WriteLn('Ville       : ', ville);
  WriteLn('========================');

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Exemple 3 : Calcul de moyenne

```pascal
program CalculMoyenneIO;  
var  
  note1, note2, note3: real;
  moyenne: real;
  appreciation: string;
begin
  WriteLn('=== CALCUL DE MOYENNE ===');
  WriteLn;

  Write('Note 1 : ');
  ReadLn(note1);

  Write('Note 2 : ');
  ReadLn(note2);

  Write('Note 3 : ');
  ReadLn(note3);

  moyenne := (note1 + note2 + note3) / 3;

  WriteLn;
  WriteLn('--- RÉSULTAT ---');
  WriteLn('Note 1   : ', note1:5:2);
  WriteLn('Note 2   : ', note2:5:2);
  WriteLn('Note 3   : ', note3:5:2);
  WriteLn('----------------');
  WriteLn('Moyenne  : ', moyenne:5:2);

  if moyenne >= 16 then
    appreciation := 'Très bien'
  else if moyenne >= 14 then
    appreciation := 'Bien'
  else if moyenne >= 12 then
    appreciation := 'Assez bien'
  else if moyenne >= 10 then
    appreciation := 'Passable'
  else
    appreciation := 'Insuffisant';

  WriteLn('Appréciation : ', appreciation);

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Exemple 4 : Conversion de température

```pascal
program ConversionTemperatureIO;  
var  
  celsius, fahrenheit: real;
  choix: integer;
begin
  WriteLn('=== CONVERSION DE TEMPÉRATURE ===');
  WriteLn;
  WriteLn('1. Celsius vers Fahrenheit');
  WriteLn('2. Fahrenheit vers Celsius');
  WriteLn;

  Write('Votre choix (1 ou 2) : ');
  ReadLn(choix);
  WriteLn;

  if choix = 1 then
  begin
    Write('Température en Celsius : ');
    ReadLn(celsius);

    fahrenheit := (celsius * 9 / 5) + 32;

    WriteLn;
    WriteLn(celsius:0:1, ' °C = ', fahrenheit:0:1, ' °F');
  end
  else if choix = 2 then
  begin
    Write('Température en Fahrenheit : ');
    ReadLn(fahrenheit);

    celsius := (fahrenheit - 32) * 5 / 9;

    WriteLn;
    WriteLn(fahrenheit:0:1, ' °F = ', celsius:0:1, ' °C');
  end
  else
    WriteLn('Choix invalide !');

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Exemple 5 : Menu interactif

```pascal
program MenuInteractif;  
var  
  choix: integer;
  continuer: boolean;
begin
  continuer := true;

  while continuer do
  begin
    WriteLn;
    WriteLn('===================');
    WriteLn('   MENU PRINCIPAL  ');
    WriteLn('===================');
    WriteLn('1. Option A');
    WriteLn('2. Option B');
    WriteLn('3. Option C');
    WriteLn('0. Quitter');
    WriteLn('===================');
    WriteLn;

    Write('Votre choix : ');
    ReadLn(choix);
    WriteLn;

    case choix of
      1: WriteLn('Vous avez choisi l''option A');
      2: WriteLn('Vous avez choisi l''option B');
      3: WriteLn('Vous avez choisi l''option C');
      0:
        begin
          WriteLn('Au revoir !');
          continuer := false;
        end;
    else
      WriteLn('Choix invalide !');
    end;

    if continuer then
    begin
      WriteLn;
      Write('Appuyez sur Entrée pour continuer...');
      ReadLn;
    end;
  end;
end.
```

## Bonnes pratiques

### 1. Toujours guider l'utilisateur

**Mauvais :**
```pascal
var
  age: integer;
begin
  ReadLn(age);    // L'utilisateur ne sait pas quoi saisir !
end.
```

**Bon :**
```pascal
var
  age: integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
end.
```

### 2. Valider les entrées

Vérifiez toujours que les données saisies sont valides :

```pascal
var
  age: integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);

  if (age < 0) or (age > 150) then
    WriteLn('Âge invalide !')
  else
    WriteLn('Âge : ', age, ' ans');
end.
```

### 3. Formater proprement les sorties

Utilisez l'alignement et le formatage pour rendre l'affichage professionnel :

```pascal
WriteLn('Nom':20, 'Prix':10, 'Quantité':10);  
WriteLn('--------------------':20, '----------':10, '----------':10);  
WriteLn('Article 1':20, 19.99:10:2, 5:10);  
WriteLn('Article 2':20, 25.50:10:2, 3:10);  
```

### 4. Empêcher la fermeture immédiate

Ajoutez toujours une pause à la fin pour que l'utilisateur puisse voir le résultat :

```pascal
begin
  // ... code du programme ...

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### 5. Séparer visuellement les sections

Utilisez des lignes vides et des séparateurs pour améliorer la lisibilité :

```pascal
WriteLn('=========================');  
WriteLn('   MON PROGRAMME');  
WriteLn('=========================');  
WriteLn;  

// ... contenu ...

WriteLn;  
WriteLn('=========================');  
```

## Erreurs courantes à éviter

### 1. Oublier le Write avant un ReadLn

```pascal
var
  nom: string;
begin
  ReadLn(nom);    // L'utilisateur ne sait pas quoi faire !

  // Mieux :
  Write('Entrez votre nom : ');
  ReadLn(nom);
end.
```

### 2. Utiliser la virgule au lieu du point

```pascal
// L'utilisateur tape : 19,99
var
  prix: real;
begin
  ReadLn(prix);    // ERREUR ! Utiliser 19.99 au lieu de 19,99
end.
```

### 3. Ne pas formater les nombres décimaux

```pascal
var
  prix: real;
begin
  prix := 19.99;
  WriteLn(prix);           // Notation scientifique peu lisible
  WriteLn(prix:0:2);       // Mieux : 19.99
end.
```

### 4. Mélanger Read et ReadLn

Évitez de mélanger `Read` et `ReadLn` dans le même programme, cela crée de la confusion. **Utilisez toujours ReadLn**.

### 5. Oublier ReadLn à la fin

```pascal
begin
  WriteLn('Hello World');
end.    // La fenêtre se ferme immédiatement !

// Mieux :
begin
  WriteLn('Hello World');
  ReadLn;    // Attend que l'utilisateur appuie sur Entrée
end.
```

## Récapitulatif

**Sorties (affichage) :**
- `Write(...)` : affiche sans retour à la ligne
- `WriteLn(...)` : affiche avec retour à la ligne
- `WriteLn` seul : retour à la ligne uniquement

**Entrées (saisie) :**
- `ReadLn(variable)` : lit une valeur et consomme la ligne (recommandé)
- `Read(variable)` : lit une valeur sans consommer la ligne (éviter)
- `ReadLn` seul : attend la touche Entrée (pause)

**Formatage :**
- Real : `variable:largeur:decimales` → `prix:10:2`
- Integer : `variable:largeur` → `age:5`
- String : `variable:largeur` (positif=droite, négatif=gauche)

**Conseils :**
- Guidez toujours l'utilisateur avec des messages clairs
- Validez les entrées pour éviter les erreurs
- Formatez proprement les sorties
- Ajoutez une pause avant la fin du programme

---

**Point clé :** Les entrées/sorties console sont la base de l'interaction avec l'utilisateur. Maîtriser `Write`, `WriteLn`, `Read` et `ReadLn` est essentiel pour créer des programmes utilisables et conviviaux. Prenez l'habitude d'utiliser ReadLn et de bien formater vos sorties !

⏭️ [Formatage de sortie](/02-introduction-langage-pascal/07-formatage-sortie.md)
