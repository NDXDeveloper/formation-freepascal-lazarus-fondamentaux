🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.6 Imbrication de structures

## Introduction

Jusqu'à présent, nous avons vu les structures de contrôle de manière isolée. Mais dans la réalité, la programmation nécessite souvent de **combiner** ces structures : placer une condition dans une boucle, une boucle dans une condition, ou même des boucles dans d'autres boucles.

C'est ce qu'on appelle l'**imbrication** : placer une structure à l'intérieur d'une autre.

**Analogie de la vie quotidienne :**
- "Pour chaque jour de la semaine, si c'est un jour de travail, alors pour chaque heure..."
- "Si tu as faim, alors parcours tous les restaurants jusqu'à en trouver un ouvert"
- "Pour chaque élève, si sa note est supérieure à 10, alors affiche son nom"

## Qu'est-ce que l'imbrication ?

L'imbrication consiste à placer une structure de contrôle (if, case, for, while, repeat) à l'intérieur d'une autre structure de contrôle.

### Visualisation de l'imbrication

```
Structure A
│
├── Instructions
│
├── Structure B (imbriquée dans A)
│   │
│   ├── Instructions
│   │
│   └── Structure C (imbriquée dans B)
│       │
│       └── Instructions
│
└── Instructions
```

### Les niveaux d'imbrication

- **Niveau 1** : Pas d'imbrication (structures simples)
- **Niveau 2** : Une structure dans une autre
- **Niveau 3** : Une structure dans une structure dans une autre
- **Niveau 4+** : À éviter si possible (code difficile à lire)

## IF imbriqués

### Concept

Placer une instruction `if` à l'intérieur d'une autre instruction `if`.

### Syntaxe de base

```pascal
if condition1 then
begin
  // Instructions

  if condition2 then
  begin
    // Instructions si condition1 ET condition2
  end;
end;
```

### Premier exemple simple

```pascal
program IfImbriques;
var
  age: Integer;
  permis: Boolean;
  reponse: String;
begin
  Write('Âge : ');
  ReadLn(age);
  Write('Avez-vous le permis ? (true/false) : ');
  ReadLn(reponse);
  permis := (reponse = 'true');

  if age >= 18 then
  begin
    WriteLn('Vous êtes majeur.');

    if permis then
      WriteLn('Vous pouvez conduire.')
    else
      WriteLn('Vous devez passer le permis.');
  end  // Pas de point-virgule avant else : en Pascal, ";" termine
  else // l'instruction if, donc le else n'aurait plus de if associe
    WriteLn('Vous êtes trop jeune pour conduire.');
end.
```

### Structure IF-ELSE IF imbriquée

```pascal
program NotesImbriquees;
var
  note: Integer;
  mention: String;
begin
  Write('Entrez la note (0-20) : ');
  ReadLn(note);

  if (note >= 0) and (note <= 20) then
  begin
    // Note valide, on détermine la mention
    // "else if" n'est pas un mot-cle unique en Pascal : c'est un else
    // dont la branche contient un nouveau if (pas besoin de begin..end
    // car chaque else contient une seule instruction : le if suivant)
    if note >= 16 then
      mention := 'Très bien'
    else if note >= 14 then
      mention := 'Bien'
    else if note >= 12 then
      mention := 'Assez bien'
    else if note >= 10 then
      mention := 'Passable'
    else
      mention := 'Insuffisant';

    WriteLn('Note : ', note, '/20');
    WriteLn('Mention : ', mention);
  end
  else
    WriteLn('Note invalide !');
end.
```

### IF à plusieurs niveaux

```pascal
program AccesSecurise;
var
  age: Integer;
  membre: Boolean;
  cotisationAJour: Boolean;
  reponse: String;
begin
  Write('Âge : ');
  ReadLn(age);
  Write('Membre ? (true/false) : ');
  ReadLn(reponse);
  membre := (reponse = 'true');

  if age >= 18 then
  begin
    WriteLn('✓ Âge valide');

    if membre then
    begin
      WriteLn('✓ Vous êtes membre');

      Write('Cotisation à jour ? (true/false) : ');
      ReadLn(reponse);
      cotisationAJour := (reponse = 'true');

      if cotisationAJour then
      begin
        WriteLn('✓ Cotisation à jour');
        WriteLn;
        WriteLn('=== ACCÈS AUTORISÉ ===');
      end
      else
      begin
        WriteLn('✗ Cotisation non payée');
        WriteLn('Veuillez régulariser votre situation.');
      end;
    end
    else
    begin
      WriteLn('✗ Vous n''êtes pas membre');
      WriteLn('Veuillez vous inscrire.');
    end;
  end
  else
  begin
    WriteLn('✗ Âge insuffisant');
    WriteLn('Accès réservé aux majeurs.');
  end;
end.
```

## Boucles FOR imbriquées

### Concept

Placer une boucle `for` à l'intérieur d'une autre boucle `for`.

### Syntaxe de base

```pascal
for i := debut1 to fin1 do
begin
  // Instructions de la boucle externe

  for j := debut2 to fin2 do
  begin
    // Instructions de la boucle interne
    // Utilisation de i et j
  end;

  // Autres instructions de la boucle externe
end;
```

### Table de multiplication

```pascal
program TableMultiplication2D;
var
  ligne, colonne: Integer;
begin
  WriteLn('TABLE DE MULTIPLICATION (1 à 10)');
  WriteLn;

  // En-tête
  Write('   ');
  for colonne := 1 to 10 do
    Write(colonne:4);
  WriteLn;
  WriteLn('   ', StringOfChar('-', 40));

  // Contenu
  for ligne := 1 to 10 do
  begin
    Write(ligne:2, ' |');

    for colonne := 1 to 10 do
      Write((ligne * colonne):4);

    WriteLn;
  end;
end.
```

### Dessin de motifs

```pascal
program MotifEtoiles;
var
  ligne, espace, etoile: Integer;
  hauteur: Integer;
begin
  Write('Hauteur du triangle : ');
  ReadLn(hauteur);
  WriteLn;

  for ligne := 1 to hauteur do
  begin
    // Espaces avant les étoiles
    for espace := 1 to (hauteur - ligne) do
      Write(' ');

    // Étoiles
    for etoile := 1 to (2 * ligne - 1) do
      Write('*');

    WriteLn;
  end;
end.
```

### Boucles imbriquées à 3 niveaux

```pascal
program Cube3D;
var
  x, y, z: Integer;
begin
  WriteLn('Génération d''un cube 3D (coordonnées) :');
  WriteLn;

  for x := 1 to 3 do
  begin
    WriteLn('Couche X = ', x);

    for y := 1 to 3 do
    begin
      Write('  Ligne Y = ', y, ' : ');

      for z := 1 to 3 do
        Write('(', x, ',', y, ',', z, ') ');

      WriteLn;
    end;
    WriteLn;
  end;
end.
```

## Boucles WHILE et REPEAT imbriquées

### WHILE dans WHILE

```pascal
program WhileImbrique;
var
  i, j: Integer;
begin
  i := 1;

  while i <= 3 do
  begin
    WriteLn('Groupe ', i, ' :');

    j := 1;
    while j <= 4 do
    begin
      Write(j, ' ');
      j := j + 1;
    end;

    WriteLn;
    i := i + 1;
  end;
end.
```

### REPEAT dans WHILE

```pascal
program MenuAvecValidation;
var
  continuer: Boolean;
  choix: Integer;
  valeur: Integer;
begin
  continuer := True;

  while continuer do
  begin
    WriteLn;
    WriteLn('=== MENU ===');
    WriteLn('1. Entrer un nombre');
    WriteLn('2. Quitter');
    Write('Choix : ');
    ReadLn(choix);
    WriteLn;

    case choix of
      1:
        begin
          // Validation avec REPEAT imbriqué
          repeat
            Write('Entrez un nombre entre 1 et 100 : ');
            ReadLn(valeur);

            if (valeur < 1) or (valeur > 100) then
              WriteLn('Invalide ! Réessayez.');
          until (valeur >= 1) and (valeur <= 100);

          WriteLn('Valeur acceptée : ', valeur);
        end;

      2:
        begin
          WriteLn('Au revoir !');
          continuer := False;
        end;

    else
      WriteLn('Choix invalide');
    end;
  end;
end.
```

## Mélange de structures

### IF dans boucle FOR

```pascal
program NombresPairs;
var
  i: Integer;
begin
  WriteLn('Nombres de 1 à 20 :');

  for i := 1 to 20 do
  begin
    Write(i:3);

    if (i mod 2) = 0 then
      Write(' (pair)')
    else
      Write(' (impair)');

    WriteLn;
  end;
end.
```

### CASE dans boucle FOR

```pascal
program JoursSemaine;
var
  jour: Integer;
begin
  WriteLn('Les jours de la semaine :');
  WriteLn;

  for jour := 1 to 7 do
  begin
    Write('Jour ', jour, ' : ');

    case jour of
      1: WriteLn('Lundi (travail)');
      2: WriteLn('Mardi (travail)');
      3: WriteLn('Mercredi (travail)');
      4: WriteLn('Jeudi (travail)');
      5: WriteLn('Vendredi (travail)');
      6: WriteLn('Samedi (week-end)');
      7: WriteLn('Dimanche (week-end)');
    end;
  end;
end.
```

### Boucle FOR dans IF

```pascal
program AffichageConditionnel;
var
  i, n: Integer;
  afficher: Boolean;
  reponse: String;
begin
  Write('Combien de nombres voulez-vous ? ');
  ReadLn(n);
  Write('Afficher les détails ? (true/false) : ');
  ReadLn(reponse);
  afficher := (reponse = 'true');
  WriteLn;

  if afficher then
  begin
    WriteLn('=== AFFICHAGE DÉTAILLÉ ===');
    for i := 1 to n do
      WriteLn('Nombre ', i, ' : carré = ', i*i, ', cube = ', i*i*i);
  end
  else
  begin
    WriteLn('=== AFFICHAGE SIMPLE ===');
    for i := 1 to n do
      Write(i, ' ');
    WriteLn;
  end;
end.
```

### CASE avec IF imbriqués

```pascal
program GestionNotes;
var
  note: Integer;
  mention: String;
begin
  Write('Note (0-20) : ');
  ReadLn(note);

  case note of
    0..9:
      begin
        mention := 'Insuffisant';
        if note < 5 then
          WriteLn('Note très faible - Soutien nécessaire')
        else
          WriteLn('Note faible - Travail à intensifier');
      end;

    10..11:
      begin
        mention := 'Passable';
        WriteLn('Note juste suffisante');
      end;

    12..13:
      begin
        mention := 'Assez bien';
        WriteLn('Bon travail');
      end;

    14..15:
      begin
        mention := 'Bien';
        if note = 15 then
          WriteLn('Très bon travail !')
        else
          WriteLn('Bon travail');
      end;

    16..20:
      begin
        mention := 'Très bien';
        if note >= 18 then
          WriteLn('Excellent ! Félicitations !')
        else
          WriteLn('Très bon résultat');
      end;

  else
    // Le else d'un case..of n'a pas besoin de begin..end :
    // toutes les instructions entre else et end font partie du bloc
    mention := 'Invalide';
    WriteLn('Note hors limites');
  end;

  if (note >= 0) and (note <= 20) then
    WriteLn('Mention : ', mention);
end.
```

## Structures complexes avec BREAK et CONTINUE

### BREAK dans boucles imbriquées

```pascal
program RechercheMatrice;
const
  LIGNES = 5;
  COLONNES = 5;
var
  matrice: array[1..LIGNES, 1..COLONNES] of Integer;
  i, j, recherche: Integer;
  trouve: Boolean;
begin
  // Remplissage
  Randomize;
  for i := 1 to LIGNES do
    for j := 1 to COLONNES do
      matrice[i, j] := Random(50) + 1;

  // Affichage
  WriteLn('Matrice :');
  for i := 1 to LIGNES do
  begin
    for j := 1 to COLONNES do
      Write(matrice[i, j]:4);
    WriteLn;
  end;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  // Recherche avec break
  trouve := False;
  for i := 1 to LIGNES do
  begin
    for j := 1 to COLONNES do
    begin
      if matrice[i, j] = recherche then
      begin
        WriteLn('Trouvé à la position [', i, ',', j, ']');
        trouve := True;
        break;  // Sort de la boucle interne
      end;
    end;

    if trouve then
      break;  // Sort de la boucle externe
  end;

  if not trouve then
    WriteLn('Non trouvé');
end.
```

### CONTINUE avec imbrication

```pascal
program FiltrageDonnees;
var
  i, j, valeur: Integer;
begin
  WriteLn('Tableau avec filtrage :');
  WriteLn;

  for i := 1 to 5 do
  begin
    Write('Ligne ', i, ' : ');

    for j := 1 to 10 do
    begin
      valeur := i * j;

      // Ignorer les multiples de 5
      if (valeur mod 5) = 0 then
        continue;

      Write(valeur:4);
    end;

    WriteLn;
  end;
end.
```

## Exemples pratiques

### Calculatrice de statistiques

```pascal
program StatistiquesMatrice;
const
  LIGNES = 3;
  COLS = 4;
var
  donnees: array[1..LIGNES, 1..COLS] of Real;
  i, j: Integer;
  sommeLigne, sommeColonne, sommeTotal: Real;
  moyenneLigne, moyenneColonne, moyenneGenerale: Real;
begin
  WriteLn('=== SAISIE DES DONNÉES ===');
  WriteLn;

  // Saisie
  for i := 1 to LIGNES do
  begin
    WriteLn('Ligne ', i, ' :');
    for j := 1 to COLS do
    begin
      Write('  Colonne ', j, ' : ');
      ReadLn(donnees[i, j]);
    end;
  end;

  WriteLn;
  WriteLn('=== TABLEAU ===');

  // Affichage avec statistiques par ligne
  sommeTotal := 0;

  for i := 1 to LIGNES do
  begin
    sommeLigne := 0;

    for j := 1 to COLS do
    begin
      Write(donnees[i, j]:8:2);
      sommeLigne := sommeLigne + donnees[i, j];
    end;

    moyenneLigne := sommeLigne / COLS;
    WriteLn('  | Moy: ', moyenneLigne:6:2);
    sommeTotal := sommeTotal + sommeLigne;
  end;

  WriteLn;

  // Statistiques par colonne
  Write('Moyennes colonnes : ');
  for j := 1 to COLS do
  begin
    sommeColonne := 0;

    for i := 1 to LIGNES do
      sommeColonne := sommeColonne + donnees[i, j];

    moyenneColonne := sommeColonne / LIGNES;
    Write(moyenneColonne:8:2);
  end;

  WriteLn;
  WriteLn;

  // Moyenne générale
  moyenneGenerale := sommeTotal / (LIGNES * COLS);
  WriteLn('Moyenne générale : ', moyenneGenerale:0:2);
end.
```

### Jeu du morpion (structure)

```pascal
program MorpionStructure;
type
  TGrille = array[1..3, 1..3] of Char;
var
  grille: TGrille;
  ligne, colonne: Integer;
  joueur: Char;
  continuer: Boolean;
  coups: Integer;
begin
  // Initialisation
  for ligne := 1 to 3 do
    for colonne := 1 to 3 do
      grille[ligne, colonne] := ' ';

  joueur := 'X';
  continuer := True;
  coups := 0;

  WriteLn('=== JEU DU MORPION ===');

  while continuer and (coups < 9) do
  begin
    // Affichage de la grille
    WriteLn;
    for ligne := 1 to 3 do
    begin
      for colonne := 1 to 3 do
      begin
        Write(' ', grille[ligne, colonne], ' ');
        if colonne < 3 then
          Write('|');
      end;
      WriteLn;

      if ligne < 3 then
        WriteLn('-----------');
    end;

    WriteLn;
    WriteLn('Joueur ', joueur);

    // Saisie avec validation
    // repeat..until False = boucle infinie volontaire, on en sort
    // uniquement par break. L'idiome simule une boucle avec sortie
    // au milieu du corps (ni au debut comme while, ni a la fin)
    repeat
      Write('Ligne (1-3) : ');
      ReadLn(ligne);
      Write('Colonne (1-3) : ');
      ReadLn(colonne);

      if (ligne < 1) or (ligne > 3) or (colonne < 1) or (colonne > 3) then
        WriteLn('Position invalide !')
      else if grille[ligne, colonne] <> ' ' then
        WriteLn('Case déjà occupée !')
      else
        break;  // Position valide
    until False;

    // Placer le symbole
    grille[ligne, colonne] := joueur;
    coups := coups + 1;

    // Vérification victoire (simplifié)
    // Dans un vrai jeu, il faudrait vérifier lignes, colonnes, diagonales

    // Changement de joueur
    if joueur = 'X' then
      joueur := 'O'
    else
      joueur := 'X';
  end;

  // Affichage final
  WriteLn;
  WriteLn('=== GRILLE FINALE ===');
  for ligne := 1 to 3 do
  begin
    for colonne := 1 to 3 do
    begin
      Write(' ', grille[ligne, colonne], ' ');
      if colonne < 3 then
        Write('|');
    end;
    WriteLn;

    if ligne < 3 then
      WriteLn('-----------');
  end;
end.
```

### Générateur d'emploi du temps

```pascal
program EmploiDuTemps;
const
  JOURS = 5;  // Lundi à Vendredi
  HEURES = 8;  // 8h à 15h
type
  TJour = 1..JOURS;
  THeure = 1..HEURES;
  TEmploi = array[TJour, THeure] of String;
var
  emploi: TEmploi;
  jour: TJour;
  heure: THeure;
  nomJour: String;
begin
  // Initialisation
  for jour := 1 to JOURS do
    for heure := 1 to HEURES do
      emploi[jour, heure] := 'Libre';

  // Ajout de cours
  emploi[1, 1] := 'Maths';
  emploi[1, 2] := 'Maths';
  emploi[1, 4] := 'Français';
  emploi[2, 1] := 'Anglais';
  emploi[2, 3] := 'Sport';
  emploi[3, 2] := 'Sciences';

  WriteLn('=== EMPLOI DU TEMPS ===');
  WriteLn;

  // Affichage
  for jour := 1 to JOURS do
  begin
    case jour of
      1: nomJour := 'LUNDI';
      2: nomJour := 'MARDI';
      3: nomJour := 'MERCREDI';
      4: nomJour := 'JEUDI';
      5: nomJour := 'VENDREDI';
    end;

    WriteLn('--- ', nomJour, ' ---');

    for heure := 1 to HEURES do
    begin
      Write((heure + 7), 'h-', (heure + 8), 'h : ');

      if emploi[jour, heure] <> 'Libre' then
        WriteLn(emploi[jour, heure], ' ★')
      else
        WriteLn(emploi[jour, heure]);
    end;

    WriteLn;
  end;
end.
```

### Tri à bulles avec affichage

```pascal
program TriBulles;
const
  TAILLE = 8;
var
  tableau: array[1..TAILLE] of Integer;
  i, j, temp: Integer;
  echange: Boolean;
  passe: Integer;
begin
  // Génération aléatoire
  Randomize;
  WriteLn('Tableau initial :');
  for i := 1 to TAILLE do
  begin
    tableau[i] := Random(100);
    Write(tableau[i]:4);
  end;
  WriteLn;
  WriteLn;

  WriteLn('=== TRI EN COURS ===');
  passe := 0;

  // Tri à bulles
  for i := 1 to TAILLE - 1 do
  begin
    echange := False;
    passe := passe + 1;
    WriteLn('Passe ', passe, ' :');

    for j := 1 to TAILLE - i do
    begin
      if tableau[j] > tableau[j + 1] then
      begin
        // Échange
        temp := tableau[j];
        tableau[j] := tableau[j + 1];
        tableau[j + 1] := temp;
        echange := True;

        Write('  Échange : ', temp, ' <-> ', tableau[j]);
        WriteLn;
      end;
    end;

    // Affichage après chaque passe
    Write('  Résultat : ');
    for j := 1 to TAILLE do
      Write(tableau[j]:4);
    WriteLn;
    WriteLn;

    // Optimisation : si pas d'échange, le tableau est trié
    if not echange then
    begin
      WriteLn('Tableau trié ! Arrêt anticipé.');
      break;
    end;
  end;

  WriteLn('=== TABLEAU FINAL ===');
  for i := 1 to TAILLE do
    Write(tableau[i]:4);
  WriteLn;
end.
```

## Gestion de la complexité

### Quand l'imbrication devient problématique

Plus vous imbriquez de structures, plus votre code devient difficile à lire et à maintenir.

**Règle générale :** Essayez de ne pas dépasser **3 niveaux d'imbrication**.

### Code trop imbriqué (MAUVAIS)

```pascal
// ❌ Trop complexe !
if condition1 then
begin
  for i := 1 to n do
  begin
    if condition2 then
    begin
      while condition3 do
      begin
        if condition4 then
        begin
          for j := 1 to m do
          begin
            // Code ici - niveau 6 !
          end;
        end;
      end;
    end;
  end;
end;
```

### Solution : Extraction en procédures

```pascal
// ✓ Meilleur : décomposition en procédures
procedure TraiterElement(valeur: Integer);
begin
  // Logique extraite
end;

procedure TraiterLigne(ligne: Integer);
var
  j: Integer;
begin
  for j := 1 to m do
    TraiterElement(tableau[ligne, j]);
end;

// Programme principal
begin
  if condition1 then
  begin
    for i := 1 to n do
    begin
      if condition2 then
        TraiterLigne(i);
    end;
  end;
end.
```

## Erreurs courantes

### 1. Oubli de BEGIN-END

```pascal
// ❌ ERREUR ! Seule la première instruction est dans le IF
for i := 1 to 10 do
  if i mod 2 = 0 then
    Write(i);
    Write(' pair');  // Toujours exécuté !

// ✓ CORRECT
for i := 1 to 10 do
begin
  if i mod 2 = 0 then
  begin
    Write(i);
    Write(' pair');
  end;
end;
```

### 2. Mauvaise indentation

```pascal
// ❌ Difficile à lire
for i:=1 to 10 do
begin
if i>5 then
begin
WriteLn(i);
for j:=1 to i do
Write(j);
end;
end;

// ✓ Correct et lisible
for i := 1 to 10 do
begin
  if i > 5 then
  begin
    WriteLn(i);
    for j := 1 to i do
      Write(j);
  end;
end;
```

### 3. Variables de boucle en conflit

```pascal
// ❌ ERREUR ! Même variable pour deux boucles
for i := 1 to 10 do
  for i := 1 to 5 do  // Conflit !
    WriteLn(i);

// ✓ CORRECT : variables différentes
for i := 1 to 10 do
  for j := 1 to 5 do
    WriteLn(i, ' - ', j);
```

### 4. Logique de condition inversée

```pascal
// ❌ Logique confuse
if not (age < 18) then
  if not (permis = False) then
    WriteLn('OK');

// ✓ Plus clair
if (age >= 18) and permis then
  WriteLn('OK');
```

### 5. Break/Continue dans la mauvaise boucle

```pascal
// ⚠️ Break sort seulement de la boucle J
for i := 1 to 10 do
begin
  for j := 1 to 10 do
  begin
    if condition then
      break;  // Sort de J, pas de I
  end;
  // Continue ici avec I
end;
```

## Bonnes pratiques

### 1. Indentation cohérente

```pascal
// Indentation de 2 espaces par niveau
if condition then
begin
  for i := 1 to n do
  begin
    if autre_condition then
    begin
      // Code
    end;
  end;
end;
```

### 2. Commentaires pour les structures complexes

```pascal
for ligne := 1 to hauteur do
begin
  // Affichage des espaces de gauche
  for espace := 1 to (hauteur - ligne) do
    Write(' ');

  // Affichage des étoiles
  for etoile := 1 to ligne do
    Write('*');

  WriteLn;
end;
```

### 3. Noms de variables significatifs

```pascal
// ❌ Moins bon
for i := 1 to n do
  for j := 1 to m do
    a[i, j] := b;

// ✓ Meilleur
for ligne := 1 to nombreLignes do
  for colonne := 1 to nombreColonnes do
    matrice[ligne, colonne] := valeurInitiale;
```

### 4. Limiter la profondeur d'imbrication

```pascal
// Si vous dépassez 3 niveaux, considérez :
// - Extraire des procédures/fonctions
// - Simplifier la logique
// - Utiliser des drapeaux (flags)
// - Inverser les conditions (return early)
```

### 5. Tester progressivement

```pascal
// Construisez progressivement :
// 1. Structure externe seule
for i := 1 to 10 do
  WriteLn(i);

// 2. Ajoutez la structure interne
for i := 1 to 10 do
begin
  for j := 1 to 5 do
    Write(j, ' ');
  WriteLn;
end;

// 3. Ajoutez la logique finale
// ...
```

### 6. Utiliser des constantes

```pascal
const
  MAX_LIGNES = 10;
  MAX_COLONNES = 10;
begin
  for ligne := 1 to MAX_LIGNES do
    for colonne := 1 to MAX_COLONNES do
      // Plus lisible et maintenable
end;
```

## Tableau récapitulatif

| Structure externe | Structure interne | Usage typique |
|-------------------|-------------------|---------------|
| FOR | FOR | Matrices, tableaux 2D, motifs |
| FOR | IF | Filtrage dans boucle |
| FOR | CASE | Traitement par catégorie |
| IF | FOR | Traitement conditionnel répété |
| IF | IF | Validation en cascade |
| WHILE | IF | Traitement avec conditions |
| WHILE | FOR | Traitement par lots |
| REPEAT | IF | Menu avec validation |

## Résumé

L'imbrication de structures permet de créer des programmes complexes en combinant les structures de contrôle :

### Principes clés
- **Toujours indenter** correctement (2 ou 4 espaces par niveau)
- **Limiter la profondeur** à 3 niveaux maximum
- **Utiliser BEGIN-END** pour la clarté
- **Variables différentes** pour les boucles imbriquées
- **Commentaires** pour les structures complexes

### Structures courantes
- **IF imbriqués** : Validations en cascade
- **FOR imbriqués** : Tableaux 2D, matrices, motifs
- **IF dans FOR** : Filtrage de données
- **CASE dans FOR** : Traitement par catégorie

### Gestion de la complexité
- Extraire des procédures/fonctions
- Utiliser des drapeaux (flags)
- Simplifier la logique
- Tester progressivement

### BREAK et CONTINUE
- N'affectent que la boucle la plus proche
- Utiliser des drapeaux pour sortir de plusieurs boucles
- Documenter l'intention

L'imbrication est un outil puissant, mais utilisez-la avec modération. Un code trop imbriqué devient difficile à comprendre et à maintenir. Privilégiez toujours la **clarté** et la **lisibilité** !

⏭️ [Gestion des erreurs simples](/03-structures-controle/07-gestion-erreurs-simples.md)
