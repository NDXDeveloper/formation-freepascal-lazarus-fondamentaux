🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.1 Tableaux statiques unidimensionnels

## Qu'est-ce qu'un tableau ?

Un tableau est une structure de données qui permet de stocker **plusieurs valeurs du même type** sous un seul nom de variable. Au lieu de créer des dizaines de variables séparées, vous pouvez regrouper toutes ces valeurs dans un seul tableau.

### Analogie simple

Imaginez une étagère avec des casiers numérotés. Chaque casier peut contenir un objet du même type (des livres, par exemple). Au lieu de donner un nom différent à chaque livre, vous dites simplement "le livre dans le casier numéro 3".

## Pourquoi utiliser des tableaux ?

Sans tableaux, pour stocker 10 notes d'élèves, vous devriez écrire :

```pascal
var
  note1, note2, note3, note4, note5, note6, note7, note8, note9, note10: Integer;
```

Avec un tableau, c'est beaucoup plus simple :

```pascal
var
  notes: array[1..10] of Integer;
```

Les tableaux sont particulièrement utiles quand vous devez :
- Stocker une collection de données similaires
- Traiter ces données en boucle
- Organiser des informations de manière structurée

## Déclaration d'un tableau

### Syntaxe générale

```pascal
var
  nomTableau: array[indexDébut..indexFin] of TypeDonnées;
```

### Exemples de déclarations

```pascal
var
  notes: array[1..5] of Integer;        // 5 entiers, indices de 1 à 5
  temperatures: array[0..6] of Real;    // 7 réels, indices de 0 à 6
  voyelles: array[1..5] of Char;        // 5 caractères
  presents: array[1..20] of Boolean;    // 20 booléens
```

**Important :** En Pascal, vous pouvez choisir les indices de départ et de fin. Contrairement à d'autres langages où les tableaux commencent toujours à 0, Pascal vous laisse libre.

## Accès aux éléments du tableau

Pour accéder à un élément du tableau, on utilise son **indice** entre crochets :

```pascal
program ExempleAcces;  
var  
  notes: array[1..5] of Integer;
begin
  // Affectation de valeurs
  notes[1] := 15;
  notes[2] := 12;
  notes[3] := 18;
  notes[4] := 10;
  notes[5] := 16;

  // Lecture de valeurs
  WriteLn('La première note est : ', notes[1]);
  WriteLn('La troisième note est : ', notes[3]);

  // Modification d'une valeur
  notes[2] := notes[2] + 3;  // On augmente la deuxième note de 3
  WriteLn('La nouvelle deuxième note : ', notes[2]);
end.
```

**Sortie :**
```
La première note est : 15  
La troisième note est : 18  
La nouvelle deuxième note : 15  
```

## Parcourir un tableau avec une boucle

La puissance des tableaux se révèle quand on les combine avec des boucles :

```pascal
program ParcoursTableau;  
var  
  notes: array[1..5] of Integer;
  i: Integer;
begin
  // Saisie des notes
  for i := 1 to 5 do
  begin
    Write('Entrez la note ', i, ' : ');
    ReadLn(notes[i]);
  end;

  // Affichage des notes
  WriteLn;
  WriteLn('Vos notes :');
  for i := 1 to 5 do
  begin
    WriteLn('Note ', i, ' : ', notes[i]);
  end;
end.
```

## Calculs avec les tableaux

### Exemple : Calcul de la moyenne

```pascal
program CalculMoyenne;  
var  
  notes: array[1..5] of Integer;
  i, somme: Integer;
  moyenne: Real;
begin
  // Saisie des notes
  WriteLn('Entrez 5 notes :');
  for i := 1 to 5 do
  begin
    Write('Note ', i, ' : ');
    ReadLn(notes[i]);
  end;

  // Calcul de la somme
  somme := 0;
  for i := 1 to 5 do
  begin
    somme := somme + notes[i];
  end;

  // Calcul et affichage de la moyenne
  moyenne := somme / 5;
  WriteLn('La moyenne est : ', moyenne:0:2);
end.
```

### Exemple : Recherche du maximum

```pascal
program RechercheMax;  
var  
  temperatures: array[1..7] of Real;
  i: Integer;
  max: Real;
begin
  // Saisie des températures de la semaine
  WriteLn('Entrez les températures de la semaine :');
  for i := 1 to 7 do
  begin
    Write('Jour ', i, ' : ');
    ReadLn(temperatures[i]);
  end;

  // Recherche du maximum
  max := temperatures[1];  // On commence par la première valeur
  for i := 2 to 7 do
  begin
    if temperatures[i] > max then
      max := temperatures[i];
  end;

  WriteLn('La température maximale est : ', max:0:1, ' degrés');
end.
```

## Initialisation des tableaux

### Initialisation à zéro

```pascal
var
  notes: array[1..10] of Integer;
  i: Integer;
begin
  // Mettre toutes les notes à zéro
  for i := 1 to 10 do
    notes[i] := 0;
end.
```

### Initialisation avec des valeurs spécifiques

Pascal ne permet pas d'initialiser directement un tableau lors de sa déclaration (sauf avec des constantes), vous devez donc le faire élément par élément :

```pascal
program InitTableau;  
var  
  voyelles: array[1..5] of Char;
begin
  voyelles[1] := 'a';
  voyelles[2] := 'e';
  voyelles[3] := 'i';
  voyelles[4] := 'o';
  voyelles[5] := 'u';
end.
```

### Tableaux constants

Si vos valeurs ne changent jamais, vous pouvez utiliser un tableau constant :

```pascal
const
  JoursMois: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
```

## Taille d'un tableau

Pour connaître les limites d'un tableau, Pascal offre les fonctions `Low()` et `High()` :

```pascal
program TailleTableau;  
var  
  notes: array[1..5] of Integer;
  i: Integer;
begin
  WriteLn('Premier indice : ', Low(notes));   // Affiche 1
  WriteLn('Dernier indice : ', High(notes));  // Affiche 5
  WriteLn('Nombre d''éléments : ', High(notes) - Low(notes) + 1);

  // Parcours avec Low et High (plus flexible)
  for i := Low(notes) to High(notes) do
  begin
    notes[i] := i * 10;
  end;
end.
```

**Avantage :** Si vous changez la taille du tableau plus tard, votre boucle s'adaptera automatiquement.

## Pièges courants à éviter

### 1. Débordement d'indice

```pascal
var
  notes: array[1..5] of Integer;
begin
  notes[6] := 10;  // ERREUR ! L'indice 6 n'existe pas
end.
```

Le compilateur FreePascal détecte souvent ce genre d'erreur, mais soyez vigilant avec les indices calculés.

### 2. Oublier les indices de départ

```pascal
var
  notes: array[1..5] of Integer;
  i: Integer;
begin
  // ERREUR : la boucle commence à 0, mais le tableau à 1
  for i := 0 to 5 do
    notes[i] := 0;  // notes[0] n'existe pas !
end.
```

**Solution :** Toujours utiliser les bonnes bornes ou `Low()` et `High()`.

### 3. Indices négatifs

En Pascal, vous pouvez avoir des indices négatifs si vous le souhaitez :

```pascal
var
  temperatures: array[-3..3] of Real;  // Valide !
```

Cela peut être utile dans certains cas (par exemple, pour des coordonnées), mais assurez-vous de rester cohérent.

## Exemple complet : Gestion de statistiques

```pascal
program StatistiquesNotes;  
var  
  notes: array[1..10] of Integer;
  i, somme, nbSuperieur: Integer;
  moyenne: Real;
begin
  // Saisie
  WriteLn('=== Saisie de 10 notes ===');
  for i := 1 to 10 do
  begin
    Write('Note ', i, ' : ');
    ReadLn(notes[i]);
  end;

  // Calcul de la moyenne
  somme := 0;
  for i := 1 to 10 do
    somme := somme + notes[i];
  moyenne := somme / 10;

  // Comptage des notes supérieures à la moyenne
  nbSuperieur := 0;
  for i := 1 to 10 do
  begin
    if notes[i] > moyenne then
      nbSuperieur := nbSuperieur + 1;
  end;

  // Affichage des résultats
  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Moyenne : ', moyenne:0:2);
  WriteLn('Notes supérieures à la moyenne : ', nbSuperieur);
end.
```

## Résumé

Les tableaux statiques unidimensionnels permettent de :
- Stocker plusieurs valeurs du même type
- Accéder aux éléments via un indice
- Parcourir facilement les données avec des boucles
- Effectuer des calculs (somme, moyenne, recherche, etc.)

**Points clés à retenir :**
- Syntaxe : `array[début..fin] of Type`
- Accès : `tableau[indice]`
- Fonctions utiles : `Low()`, `High()`
- Toujours respecter les bornes du tableau

Les tableaux sont une structure fondamentale en programmation. Maîtriser les tableaux unidimensionnels est essentiel avant de passer aux tableaux multidimensionnels.

⏭️ [Tableaux multidimensionnels](/05-types-donnees-structures/02-tableaux-multidimensionnels.md)
