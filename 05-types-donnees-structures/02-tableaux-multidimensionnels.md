🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.2 Tableaux multidimensionnels

## Qu'est-ce qu'un tableau multidimensionnel ?

Un tableau multidimensionnel est un tableau qui contient plusieurs "dimensions" ou "niveaux". Alors qu'un tableau unidimensionnel ressemble à une simple liste, un tableau multidimensionnel peut représenter une grille, un cube, ou même des structures plus complexes.

### Analogies simples

- **Tableau 1D** : Une ligne de casiers
- **Tableau 2D** : Une grille comme un tableau Excel (lignes et colonnes)
- **Tableau 3D** : Un cube de Rubik avec plusieurs étages

## Tableaux à deux dimensions (2D)

Les tableaux 2D sont les plus couramment utilisés. Ils représentent une structure en lignes et colonnes.

### Déclaration d'un tableau 2D

```pascal
var
  nomTableau: array[ligne_début..ligne_fin, col_début..col_fin] of Type;
```

### Exemples de déclarations

```pascal
var
  grille: array[1..3, 1..4] of Integer;        // 3 lignes, 4 colonnes
  notes: array[1..5, 1..3] of Real;            // 5 élèves, 3 matières
  damier: array[1..8, 1..8] of Char;           // Échiquier 8x8
  planning: array[1..7, 1..24] of Boolean;     // 7 jours, 24 heures
```

### Visualisation d'un tableau 2D

Un tableau `array[1..3, 1..4]` ressemble à ceci :

```
       Colonne 1  Colonne 2  Colonne 3  Colonne 4
Ligne 1    [1,1]      [1,2]      [1,3]      [1,4]
Ligne 2    [2,1]      [2,2]      [2,3]      [2,4]
Ligne 3    [3,1]      [3,2]      [3,3]      [3,4]
```

## Accès aux éléments d'un tableau 2D

Pour accéder à un élément, on utilise **deux indices** : `[ligne, colonne]`

```pascal
program ExempleAcces2D;
var
  grille: array[1..3, 1..4] of Integer;
begin
  // Affectation
  grille[1, 1] := 10;    // Ligne 1, Colonne 1
  grille[1, 2] := 20;    // Ligne 1, Colonne 2
  grille[2, 3] := 30;    // Ligne 2, Colonne 3

  // Lecture
  WriteLn('Valeur en [1,1] : ', grille[1, 1]);
  WriteLn('Valeur en [2,3] : ', grille[2, 3]);

  // Modification
  grille[1, 1] := grille[1, 1] * 2;
  WriteLn('Nouvelle valeur en [1,1] : ', grille[1, 1]);
end.
```

**Sortie :**
```
Valeur en [1,1] : 10
Valeur en [2,3] : 30
Nouvelle valeur en [1,1] : 20
```

## Parcourir un tableau 2D avec des boucles imbriquées

Pour parcourir toutes les cases d'un tableau 2D, il faut **deux boucles imbriquées** :

```pascal
program ParcoursTableau2D;
var
  grille: array[1..3, 1..4] of Integer;
  ligne, colonne: Integer;
begin
  // Remplissage du tableau
  for ligne := 1 to 3 do
  begin
    for colonne := 1 to 4 do
    begin
      grille[ligne, colonne] := ligne * 10 + colonne;
    end;
  end;

  // Affichage du tableau
  WriteLn('Contenu de la grille :');
  for ligne := 1 to 3 do
  begin
    for colonne := 1 to 4 do
    begin
      Write(grille[ligne, colonne]:4);  // :4 pour aligner
    end;
    WriteLn;  // Retour à la ligne après chaque ligne
  end;
end.
```

**Sortie :**
```
Contenu de la grille :
  11  12  13  14
  21  22  23  24
  31  32  33  34
```

## Exemple pratique : Tableau de notes

```pascal
program TableauNotes;
var
  notes: array[1..4, 1..3] of Real;  // 4 élèves, 3 matières
  eleve, matiere: Integer;
  nomsMatieres: array[1..3] of String;
begin
  // Noms des matières
  nomsMatieres[1] := 'Maths';
  nomsMatieres[2] := 'Français';
  nomsMatieres[3] := 'Histoire';

  // Saisie des notes
  WriteLn('=== Saisie des notes ===');
  for eleve := 1 to 4 do
  begin
    WriteLn;
    WriteLn('Élève ', eleve, ' :');
    for matiere := 1 to 3 do
    begin
      Write('  ', nomsMatieres[matiere], ' : ');
      ReadLn(notes[eleve, matiere]);
    end;
  end;

  // Affichage sous forme de tableau
  WriteLn;
  WriteLn('=== Tableau récapitulatif ===');
  Write('Élève    ');
  for matiere := 1 to 3 do
    Write(nomsMatieres[matiere]:10);
  WriteLn;
  WriteLn('----------------------------------------');

  for eleve := 1 to 4 do
  begin
    Write('  ', eleve, '      ');
    for matiere := 1 to 3 do
      Write(notes[eleve, matiere]:10:1);
    WriteLn;
  end;
end.
```

## Calculs avec les tableaux 2D

### Exemple 1 : Somme par ligne

```pascal
program SommeLignes;
var
  tableau: array[1..3, 1..4] of Integer;
  ligne, colonne, somme: Integer;
begin
  // Remplissage (exemple)
  for ligne := 1 to 3 do
    for colonne := 1 to 4 do
      tableau[ligne, colonne] := ligne + colonne;

  // Calcul de la somme de chaque ligne
  for ligne := 1 to 3 do
  begin
    somme := 0;
    for colonne := 1 to 4 do
      somme := somme + tableau[ligne, colonne];
    WriteLn('Somme de la ligne ', ligne, ' : ', somme);
  end;
end.
```

### Exemple 2 : Moyenne par colonne

```pascal
program MoyenneColonnes;
var
  notes: array[1..5, 1..3] of Real;  // 5 élèves, 3 matières
  eleve, matiere: Integer;
  somme, moyenne: Real;
begin
  // Supposons que les notes sont déjà saisies...

  // Calcul de la moyenne de chaque matière
  WriteLn('Moyennes par matière :');
  for matiere := 1 to 3 do
  begin
    somme := 0;
    for eleve := 1 to 5 do
      somme := somme + notes[eleve, matiere];
    moyenne := somme / 5;
    WriteLn('Matière ', matiere, ' : ', moyenne:0:2);
  end;
end.
```

### Exemple 3 : Recherche dans un tableau 2D

```pascal
program RechercheValeur;
var
  grille: array[1..4, 1..5] of Integer;
  ligne, colonne, valeur: Integer;
  trouve: Boolean;
begin
  // Remplissage du tableau (exemple)
  for ligne := 1 to 4 do
    for colonne := 1 to 5 do
      grille[ligne, colonne] := ligne * colonne;

  // Recherche d'une valeur
  Write('Valeur à rechercher : ');
  ReadLn(valeur);

  trouve := False;
  for ligne := 1 to 4 do
  begin
    for colonne := 1 to 5 do
    begin
      if grille[ligne, colonne] = valeur then
      begin
        WriteLn('Trouvé en position [', ligne, ',', colonne, ']');
        trouve := True;
      end;
    end;
  end;

  if not trouve then
    WriteLn('Valeur non trouvée');
end.
```

## Initialisation des tableaux 2D

### Méthode 1 : Avec des boucles

```pascal
var
  matrice: array[1..3, 1..3] of Integer;
  i, j: Integer;
begin
  // Initialiser toutes les valeurs à zéro
  for i := 1 to 3 do
    for j := 1 to 3 do
      matrice[i, j] := 0;
end.
```

### Méthode 2 : Matrice identité

```pascal
program MatriceIdentite;
var
  matrice: array[1..3, 1..3] of Integer;
  i, j: Integer;
begin
  // Créer une matrice identité (1 sur la diagonale, 0 ailleurs)
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    begin
      if i = j then
        matrice[i, j] := 1
      else
        matrice[i, j] := 0;
    end;
  end;

  // Affichage
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
      Write(matrice[i, j]:3);
    WriteLn;
  end;
end.
```

**Sortie :**
```
  1  0  0
  0  1  0
  0  0  1
```

## Tableaux à trois dimensions (3D)

Les tableaux 3D ajoutent une troisième dimension. Ils sont moins courants mais utiles dans certains cas.

### Déclaration

```pascal
var
  cube: array[1..3, 1..3, 1..3] of Integer;
  immeuble: array[1..5, 1..10, 1..20] of Boolean;  // 5 étages, 10 couloirs, 20 portes
```

### Accès et parcours

```pascal
program Exemple3D;
var
  cube: array[1..2, 1..2, 1..2] of Integer;
  x, y, z, compteur: Integer;
begin
  // Remplissage
  compteur := 1;
  for x := 1 to 2 do
    for y := 1 to 2 do
      for z := 1 to 2 do
      begin
        cube[x, y, z] := compteur;
        compteur := compteur + 1;
      end;

  // Affichage par "couche"
  for x := 1 to 2 do
  begin
    WriteLn('Couche ', x, ' :');
    for y := 1 to 2 do
    begin
      for z := 1 to 2 do
        Write(cube[x, y, z]:3);
      WriteLn;
    end;
    WriteLn;
  end;
end.
```

## Utilisation de Low() et High() avec les tableaux multidimensionnels

Ces fonctions fonctionnent aussi avec les tableaux multidimensionnels :

```pascal
program LowHighMultiDim;
var
  tableau: array[1..3, 5..8] of Integer;
  i, j: Integer;
begin
  WriteLn('Lignes : de ', Low(tableau, 1), ' à ', High(tableau, 1));
  WriteLn('Colonnes : de ', Low(tableau, 2), ' à ', High(tableau, 2));

  // Parcours flexible
  for i := Low(tableau, 1) to High(tableau, 1) do
    for j := Low(tableau, 2) to High(tableau, 2) do
      tableau[i, j] := i * j;
end.
```

**Note :** Le deuxième paramètre (1 ou 2) indique la dimension.

## Exemple complet : Jeu du Morpion

```pascal
program Morpion;
var
  grille: array[1..3, 1..3] of Char;
  ligne, colonne: Integer;
begin
  // Initialisation de la grille
  for ligne := 1 to 3 do
    for colonne := 1 to 3 do
      grille[ligne, colonne] := '.';

  // Placement de quelques symboles
  grille[1, 1] := 'X';
  grille[1, 2] := 'O';
  grille[2, 2] := 'X';
  grille[3, 3] := 'O';

  // Affichage de la grille
  WriteLn('Grille de jeu :');
  WriteLn('  1 2 3');
  for ligne := 1 to 3 do
  begin
    Write(ligne, ' ');
    for colonne := 1 to 3 do
      Write(grille[ligne, colonne], ' ');
    WriteLn;
  end;
end.
```

**Sortie :**
```
Grille de jeu :
  1 2 3
1 X O .
2 . X .
3 . . O
```

## Applications pratiques des tableaux multidimensionnels

### 1. Gestion d'images

Un tableau 2D peut représenter une image en noir et blanc :

```pascal
var
  image: array[0..99, 0..99] of Byte;  // Image 100x100 pixels
```

### 2. Jeux de plateau

Échecs, dames, bataille navale, etc. :

```pascal
var
  echiquier: array[1..8, 1..8] of Char;
```

### 3. Calendriers et plannings

```pascal
var
  planning: array[1..12, 1..31] of String;  // 12 mois, 31 jours max
```

### 4. Données scientifiques

Matrices mathématiques, tableaux de mesures :

```pascal
var
  mesures: array[1..10, 1..24, 1..7] of Real;  // 10 capteurs, 24h, 7 jours
```

## Pièges courants

### 1. Confusion entre dimensions

```pascal
var
  tableau: array[1..3, 1..4] of Integer;
begin
  // ERREUR : ordre inversé
  tableau[4, 3] := 10;  // Devrait être [3, 4]
end.
```

### 2. Boucles imbriquées dans le mauvais ordre

```pascal
// Pour afficher ligne par ligne, la boucle externe doit être sur les lignes
for ligne := 1 to 3 do
begin
  for colonne := 1 to 4 do
    Write(tableau[ligne, colonne]);
  WriteLn;  // Nouvelle ligne après chaque ligne
end;
```

### 3. Oubli d'une dimension dans Low/High

```pascal
// INCORRECT
for i := Low(tableau) to High(tableau) do  // Quelle dimension ?

// CORRECT
for i := Low(tableau, 1) to High(tableau, 1) do  // Première dimension
```

## Comparaison : 1D vs 2D vs 3D

| Dimension | Analogie | Indices | Usage typique |
|-----------|----------|---------|---------------|
| 1D | Liste | `[i]` | Liste de valeurs |
| 2D | Grille | `[i, j]` | Tableaux, matrices, jeux |
| 3D | Cube | `[i, j, k]` | Volumes, données complexes |

## Conseils pour travailler avec les tableaux multidimensionnels

1. **Visualisez** : Dessinez la structure sur papier
2. **Nommez clairement** : Utilisez des noms de variables explicites (`ligne`, `colonne` plutôt que `i`, `j`)
3. **Testez avec de petites tailles** : Commencez par des tableaux 2x2 ou 3x3
4. **Commentez** : Indiquez ce que représente chaque dimension
5. **Utilisez Low/High** : Pour rendre le code plus flexible

## Résumé

Les tableaux multidimensionnels permettent de :
- Représenter des données structurées en plusieurs dimensions
- Modéliser des grilles, matrices et structures complexes
- Organiser des données selon plusieurs critères

**Points clés à retenir :**
- **Déclaration 2D** : `array[lignes_début..lignes_fin, col_début..col_fin] of Type`
- **Accès** : `tableau[ligne, colonne]`
- **Parcours** : Boucles imbriquées (une par dimension)
- **Low/High** : `Low(tableau, dimension)` et `High(tableau, dimension)`

Les tableaux 2D sont très courants en programmation. Une fois maîtrisés, ils ouvrent la porte à de nombreuses applications, des jeux aux calculs scientifiques.

⏭️ [Chaînes de caractères (String, ShortString)](/05-types-donnees-structures/03-chaines-caracteres-string.md)
