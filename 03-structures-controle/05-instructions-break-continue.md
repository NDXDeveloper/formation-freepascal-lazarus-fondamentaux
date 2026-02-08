🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.5 Instructions break et continue

## Introduction

Jusqu'à présent, nous avons vu que les boucles s'exécutent normalement jusqu'à ce que leur condition soit remplie. Mais parfois, vous avez besoin de **contrôler le flux** de la boucle de manière plus fine :
- "Arrête la boucle dès que tu trouves ce que tu cherches"
- "Saute cette itération et passe à la suivante"

Les instructions `break` et `continue` permettent ce contrôle avancé.

## L'instruction BREAK

### Concept

`Break` signifie "**casser**" ou "**sortir**". Cette instruction permet de **sortir immédiatement** d'une boucle, peu importe où vous en êtes.

**Analogie de la vie quotidienne :**
- "Cherche dans les tiroirs jusqu'à trouver les clés, puis arrête"
- "Interroge les témoins jusqu'à ce que l'un d'eux sache quelque chose"
- "Essaie les codes jusqu'à ce que la porte s'ouvre"

### Syntaxe

```pascal
break;
```

C'est tout ! Un simple mot-clé sans parenthèses ni paramètres.

### Premier exemple avec FOR

```pascal
program PremierBreak;
var
  i: Integer;
begin
  WriteLn('Comptage de 1 à 10, mais arrêt à 5 :');

  for i := 1 to 10 do
  begin
    if i = 5 then
    begin
      WriteLn('Arrêt à ', i);
      break;  // Sort de la boucle immédiatement
    end;
    WriteLn(i);
  end;

  WriteLn('Après la boucle');
end.
```

**Résultat :**
```
Comptage de 1 à 10, mais arrêt à 5 :
1
2
3
4
Arrêt à 5
Après la boucle
```

Notez que le `WriteLn(i)` après le `break` ne s'est jamais exécuté pour i=5, et que les valeurs 6 à 10 n'ont jamais été traitées.

### Comment fonctionne BREAK

```
┌─────────────────┐
│  Début boucle   │
└────────┬────────┘
         │
         ▼
    ┌─────────┐
    │Condition│─────► Continue normalement
    │  break  │
    └────┬────┘
         │ Break !
         ▼
    ┌─────────┐
    │Sort de  │
    │la boucle│
    └─────────┘
```

### Exemple avec WHILE

```pascal
program BreakAvecWhile;
var
  nombre, tentatives: Integer;
begin
  tentatives := 0;

  WriteLn('Entrez des nombres positifs (nombre négatif pour arrêter) :');

  while True do  // Boucle infinie intentionnelle
  begin
    tentatives := tentatives + 1;
    Write('Nombre ', tentatives, ' : ');
    ReadLn(nombre);

    if nombre < 0 then
    begin
      WriteLn('Nombre négatif détecté. Arrêt.');
      break;  // Sort de la boucle infinie
    end;

    WriteLn('Vous avez entré : ', nombre);
  end;

  WriteLn('Total de tentatives : ', tentatives);
end.
```

### Exemple avec REPEAT

```pascal
program BreakAvecRepeat;
var
  choix: Integer;
begin
  WriteLn('=== MENU ===');

  repeat
    WriteLn;
    WriteLn('1. Option A');
    WriteLn('2. Option B');
    WriteLn('3. Quitter');
    Write('Votre choix : ');
    ReadLn(choix);

    case choix of
      1: WriteLn('Option A sélectionnée');
      2: WriteLn('Option B sélectionnée');
      3:
        begin
          WriteLn('Au revoir !');
          break;  // Sort du repeat
        end;
    else
      WriteLn('Choix invalide');
    end;
  until False;  // Normalement boucle infinie

  WriteLn('Programme terminé');
end.
```

### Recherche avec BREAK

Un usage très courant : arrêter une recherche dès qu'on trouve ce qu'on cherche.

```pascal
program RechercheAvecBreak;
const
  TAILLE = 10;
var
  nombres: array[1..TAILLE] of Integer;
  i, recherche: Integer;
  position: Integer;
begin
  // Remplir le tableau
  WriteLn('Entrez ', TAILLE, ' nombres :');
  for i := 1 to TAILLE do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombres[i]);
  end;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  // Recherche
  position := -1;  // -1 signifie "non trouvé"

  for i := 1 to TAILLE do
  begin
    if nombres[i] = recherche then
    begin
      position := i;
      break;  // Trouvé ! Inutile de continuer
    end;
  end;

  WriteLn;
  if position <> -1 then
    WriteLn('Nombre trouvé à la position ', position)
  else
    WriteLn('Nombre non trouvé');
end.
```

## L'instruction CONTINUE

### Concept

`Continue` signifie "**passer au suivant**". Cette instruction saute le reste de l'itération actuelle et passe directement à l'itération suivante de la boucle.

**Analogie de la vie quotidienne :**
- "Vérifie tous les dossiers, mais ignore ceux qui sont vides"
- "Interroge tous les élèves, mais saute ceux qui sont absents"
- "Traite tous les fichiers, sauf ceux qui sont corrompus"

### Syntaxe

```pascal
continue;
```

### Premier exemple avec FOR

```pascal
program PremierContinue;
var
  i: Integer;
begin
  WriteLn('Nombres de 1 à 10, sauf les multiples de 3 :');

  for i := 1 to 10 do
  begin
    if (i mod 3) = 0 then
      continue;  // Saute les multiples de 3

    WriteLn(i);
  end;
end.
```

**Résultat :**
```
Nombres de 1 à 10, sauf les multiples de 3 :
1
2
4
5
7
8
10
```

Les nombres 3, 6 et 9 ont été sautés.

### Comment fonctionne CONTINUE

```
┌─────────────────┐
│  Itération N    │
└────────┬────────┘
         │
         ▼
    ┌─────────┐
    │Condition│─────► Continue normalement
    │continue │       l'itération
    └────┬────┘
         │ Continue !
         ▼
    ┌─────────────┐
    │ Saute le    │
    │ reste et    │
    │ passe à N+1 │
    └─────────────┘
```

### Exemple avec WHILE

```pascal
program ContinueAvecWhile;
var
  i: Integer;
begin
  WriteLn('Nombres pairs de 1 à 20 :');

  i := 0;
  while i < 20 do
  begin
    // Important : incrémenter AVANT le Continue, sinon boucle infinie
    // (dans un for, l'incrémentation est automatique, pas dans un while)
    i := i + 1;

    if (i mod 2) = 1 then  // Si impair
      continue;  // Passe au suivant

    WriteLn(i);
  end;
end.
```

**Résultat :**
```
Nombres pairs de 1 à 20 :
2
4
6
8
10
12
14
16
18
20
```

### Exemple avec REPEAT

```pascal
program ContinueAvecRepeat;
var
  nombre: Integer;
  compteur: Integer;
begin
  compteur := 0;

  WriteLn('Entrez 5 nombres positifs :');

  repeat
    compteur := compteur + 1;
    Write('Nombre ', compteur, ' : ');
    ReadLn(nombre);

    if nombre <= 0 then
    begin
      WriteLn('Nombre invalide, ignoré');
      compteur := compteur - 1;  // Ne compte pas cette tentative
      // Continue saute directement au test 'until' (pas au début du repeat)
      continue;
    end;

    WriteLn('Nombre ', nombre, ' accepté');
  until compteur >= 5;

  WriteLn('Merci !');
end.
```

### Filtrage de données avec CONTINUE

```pascal
program FiltrageNombres;
var
  i, nombre: Integer;
  somme, compteur: Integer;
begin
  somme := 0;
  compteur := 0;

  WriteLn('Entrez 10 nombres (négatifs ignorés) :');

  for i := 1 to 10 do
  begin
    Write('Nombre ', i, ' : ');
    ReadLn(nombre);

    if nombre < 0 then
    begin
      WriteLn('→ Nombre négatif ignoré');
      continue;  // Passe au nombre suivant
    end;

    somme := somme + nombre;
    compteur := compteur + 1;
    WriteLn('→ Nombre ajouté');
  end;

  WriteLn;
  WriteLn('Nombres positifs entrés : ', compteur);
  if compteur > 0 then
    WriteLn('Somme : ', somme)
  else
    WriteLn('Aucun nombre positif');
end.
```

## BREAK vs CONTINUE : Les différences

| Caractéristique | BREAK | CONTINUE |
|----------------|-------|----------|
| **Action** | Sort de la boucle | Passe à l'itération suivante |
| **Après l'instruction** | Continue après la boucle | Retourne au début de la boucle |
| **Itérations restantes** | Annulées | Continuent |
| **Usage typique** | Recherche trouvée | Filtrage de données |

### Exemple comparatif

```pascal
program ComparaisonBreakContinue;
var
  i: Integer;
begin
  WriteLn('=== Avec BREAK ===');
  for i := 1 to 10 do
  begin
    if i = 5 then
      break;  // Arrête tout
    WriteLn(i);
  end;
  WriteLn('Boucle terminée');
  WriteLn;

  WriteLn('=== Avec CONTINUE ===');
  for i := 1 to 10 do
  begin
    if i = 5 then
      continue;  // Saute juste le 5
    WriteLn(i);
  end;
  WriteLn('Boucle terminée');
end.
```

**Résultat :**
```
=== Avec BREAK ===
1
2
3
4
Boucle terminée

=== Avec CONTINUE ===
1
2
3
4
6
7
8
9
10
Boucle terminée
```

## Boucles imbriquées

Avec des boucles imbriquées, `break` et `continue` n'affectent que la boucle **la plus proche**.

### BREAK dans boucles imbriquées

```pascal
program BreakImbrique;
var
  i, j: Integer;
begin
  WriteLn('Boucles imbriquées avec break :');

  for i := 1 to 3 do
  begin
    WriteLn('Ligne ', i, ' :');

    for j := 1 to 5 do
    begin
      if j = 4 then
        break;  // Sort seulement de la boucle J
      Write(j, ' ');
    end;

    WriteLn;  // Ceci s'exécute car on sort juste de la boucle J
  end;
end.
```

**Résultat :**
```
Boucles imbriquées avec break :
Ligne 1 :
1 2 3
Ligne 2 :
1 2 3
Ligne 3 :
1 2 3
```

Le `break` sort de la boucle `j`, mais la boucle `i` continue normalement.

### CONTINUE dans boucles imbriquées

```pascal
program ContinueImbrique;
var
  i, j: Integer;
begin
  WriteLn('Boucles imbriquées avec continue :');

  for i := 1 to 3 do
  begin
    WriteLn('Ligne ', i, ' :');

    for j := 1 to 5 do
    begin
      if j = 3 then
        continue;  // Saute le 3 dans la boucle J
      Write(j, ' ');
    end;

    WriteLn;
  end;
end.
```

**Résultat :**
```
Boucles imbriquées avec continue :
Ligne 1 :
1 2 4 5
Ligne 2 :
1 2 4 5
Ligne 3 :
1 2 4 5
```

### Sortir de plusieurs boucles

Pour sortir de plusieurs boucles imbriquées, utilisez un drapeau (flag) :

```pascal
program SortieDoubleBreak;
var
  i, j: Integer;
  // En Pascal, Break ne sort que de la boucle la plus proche.
  // Un drapeau (flag) booléen permet de propager la sortie aux boucles externes.
  trouve: Boolean;
begin
  trouve := False;

  for i := 1 to 5 do
  begin
    for j := 1 to 5 do
    begin
      WriteLn('i=', i, ', j=', j);

      if (i = 3) and (j = 3) then
      begin
        trouve := True;
        break;  // Sort de la boucle J
      end;
    end;

    if trouve then
      break;  // Sort de la boucle I
  end;

  WriteLn('Fin des boucles');
end.
```

## Exemples pratiques

### Validation de mot de passe avec tentatives

```pascal
program ValidationMotDePasse;
const
  MAX_TENTATIVES = 3;
  MOT_DE_PASSE = 'secret123';
var
  tentative: Integer;
  saisie: String;
  succes: Boolean;
begin
  succes := False;

  WriteLn('=== AUTHENTIFICATION ===');

  for tentative := 1 to MAX_TENTATIVES do
  begin
    Write('Tentative ', tentative, '/', MAX_TENTATIVES, ' - Mot de passe : ');
    ReadLn(saisie);

    if saisie = MOT_DE_PASSE then
    begin
      WriteLn('Authentification réussie !');
      succes := True;
      break;  // Plus besoin de continuer
    end
    else
    begin
      if tentative < MAX_TENTATIVES then
        WriteLn('Incorrect. Il reste ', MAX_TENTATIVES - tentative, ' tentative(s)')
      else
        WriteLn('Accès refusé. Nombre maximum de tentatives atteint.');
    end;
  end;

  if succes then
    WriteLn('Bienvenue dans le système !');
end.
```

### Traitement de commandes avec filtrage

```pascal
program TraitementCommandes;
type
  TCommande = record
    numero: Integer;
    montant: Real;
    statut: String;
  end;
var
  commandes: array[1..5] of TCommande;
  i: Integer;
  totalValide: Real;
begin
  // Initialisation des commandes
  commandes[1].numero := 101; commandes[1].montant := 150.50; commandes[1].statut := 'valide';
  commandes[2].numero := 102; commandes[2].montant := 75.00; commandes[2].statut := 'annulee';
  commandes[3].numero := 103; commandes[3].montant := 200.00; commandes[3].statut := 'valide';
  commandes[4].numero := 104; commandes[4].montant := 50.00; commandes[4].statut := 'annulee';
  commandes[5].numero := 105; commandes[5].montant := 300.00; commandes[5].statut := 'valide';

  totalValide := 0;

  WriteLn('=== TRAITEMENT DES COMMANDES ===');
  WriteLn;

  for i := 1 to 5 do
  begin
    Write('Commande #', commandes[i].numero, ' - ');

    // Ignorer les commandes annulées
    if commandes[i].statut = 'annulee' then
    begin
      WriteLn('ANNULÉE (ignorée)');
      continue;  // Passe à la commande suivante
    end;

    // Traiter les commandes valides
    WriteLn('Montant : ', commandes[i].montant:0:2, ' € - TRAITÉE');
    totalValide := totalValide + commandes[i].montant;
  end;

  WriteLn;
  WriteLn('Total des commandes valides : ', totalValide:0:2, ' €');
end.
```

### Recherche avec conditions multiples

```pascal
program RechercheMulticriteres;
const
  TAILLE = 10;
var
  nombres: array[1..TAILLE] of Integer;
  i, premierPair, premierMultiple5: Integer;
begin
  // Génération de nombres
  Randomize;
  WriteLn('Nombres générés :');
  for i := 1 to TAILLE do
  begin
    nombres[i] := Random(50) + 1;
    Write(nombres[i], ' ');
  end;
  WriteLn;
  WriteLn;

  // Recherche du premier nombre pair
  premierPair := -1;
  for i := 1 to TAILLE do
  begin
    if (nombres[i] mod 2) = 0 then
    begin
      premierPair := nombres[i];
      break;  // Trouvé !
    end;
  end;

  // Recherche du premier multiple de 5
  premierMultiple5 := -1;
  for i := 1 to TAILLE do
  begin
    if (nombres[i] mod 5) = 0 then
    begin
      premierMultiple5 := nombres[i];
      break;  // Trouvé !
    end;
  end;

  WriteLn('Premier nombre pair : ', premierPair);
  WriteLn('Premier multiple de 5 : ', premierMultiple5);
end.
```

### Menu interactif avancé

```pascal
program MenuInteractifAvance;
var
  choixPrincipal, sousChoix: Integer;
  continuer: Boolean;
begin
  continuer := True;

  WriteLn('=============================');
  WriteLn('   APPLICATION DE GESTION   ');
  WriteLn('=============================');

  while continuer do
  begin
    WriteLn;
    WriteLn('--- MENU PRINCIPAL ---');
    WriteLn('1. Gestion des fichiers');
    WriteLn('2. Gestion des utilisateurs');
    WriteLn('3. Paramètres');
    WriteLn('4. Quitter');
    Write('Choix : ');
    ReadLn(choixPrincipal);
    WriteLn;

    case choixPrincipal of
      1:  // Gestion fichiers
        begin
          WriteLn('--- FICHIERS ---');
          WriteLn('1. Nouveau');
          WriteLn('2. Ouvrir');
          WriteLn('3. Retour');
          Write('Choix : ');
          ReadLn(sousChoix);

          if sousChoix = 3 then
            continue;  // Retour au menu principal

          case sousChoix of
            1: WriteLn('Création d''un nouveau fichier...');
            2: WriteLn('Ouverture d''un fichier...');
          else
            WriteLn('Option invalide');
          end;
        end;

      2:  // Gestion utilisateurs
        begin
          WriteLn('--- UTILISATEURS ---');
          WriteLn('1. Ajouter');
          WriteLn('2. Modifier');
          WriteLn('3. Retour');
          Write('Choix : ');
          ReadLn(sousChoix);

          if sousChoix = 3 then
            continue;  // Retour au menu principal

          case sousChoix of
            1: WriteLn('Ajout d''un utilisateur...');
            2: WriteLn('Modification d''un utilisateur...');
          else
            WriteLn('Option invalide');
          end;
        end;

      3:  // Paramètres
        WriteLn('Accès aux paramètres...');

      4:  // Quitter
        begin
          WriteLn('Au revoir !');
          continuer := False;
          break;  // Sort de la boucle while
        end;

    else
      WriteLn('Choix invalide');
    end;
  end;
end.
```

### Analyse de texte avec filtrage

```pascal
program AnalyseTexteFiltre;
var
  texte: String;
  i: Integer;
  caractere: Char;
  compteurLettres: Integer;
begin
  compteurLettres := 0;

  Write('Entrez un texte : ');
  ReadLn(texte);
  WriteLn;
  WriteLn('Lettres trouvées :');

  for i := 1 to Length(texte) do
  begin
    caractere := texte[i];

    // Ignorer les espaces et la ponctuation
    if not (caractere in ['A'..'Z', 'a'..'z']) then
      continue;  // Passe au caractère suivant

    // Afficher les lettres
    Write(caractere, ' ');
    compteurLettres := compteurLettres + 1;

    // Arrêter après 50 lettres
    if compteurLettres >= 50 then
    begin
      WriteLn('...');
      break;
    end;
  end;

  WriteLn;
  WriteLn('Total de lettres : ', compteurLettres);
end.
```

## Cas d'usage avancés

### Recherche avec optimisation

```pascal
program RechercheOptimisee;
const
  TAILLE = 100;
var
  tableau: array[1..TAILLE] of Integer;
  i, recherche, position: Integer;
begin
  // Initialisation
  for i := 1 to TAILLE do
    tableau[i] := i * 2;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  position := -1;

  for i := 1 to TAILLE do
  begin
    // Optimisation : si le tableau est trié et qu'on dépasse
    // la valeur recherchée, inutile de continuer
    if tableau[i] > recherche then
    begin
      WriteLn('Valeur dépassée, arrêt de la recherche');
      break;
    end;

    if tableau[i] = recherche then
    begin
      position := i;
      break;
    end;
  end;

  if position <> -1 then
    WriteLn('Trouvé à la position ', position)
  else
    WriteLn('Non trouvé');
end.
```

### Traitement par lots

```pascal
program TraitementParLots;
const
  TAILLE_LOT = 5;
  MAX_ITEMS = 20;
var
  i, lot, itemsDansLot: Integer;
begin
  WriteLn('=== TRAITEMENT PAR LOTS ===');
  WriteLn('Taille du lot : ', TAILLE_LOT);
  WriteLn;

  lot := 1;
  itemsDansLot := 0;

  for i := 1 to MAX_ITEMS do
  begin
    // Début d'un nouveau lot
    if itemsDansLot = 0 then
      WriteLn('Lot #', lot, ' :');

    Write('  Item #', i);

    // Simulation : ignorer les items pairs
    if (i mod 2) = 0 then
    begin
      WriteLn(' - IGNORÉ');
      continue;  // Ne compte pas dans le lot
    end;

    WriteLn(' - TRAITÉ');
    itemsDansLot := itemsDansLot + 1;

    // Lot complet
    if itemsDansLot >= TAILLE_LOT then
    begin
      WriteLn('  → Lot complet !');
      WriteLn;
      lot := lot + 1;
      itemsDansLot := 0;
    end;
  end;
end.
```

## Erreurs courantes

### 1. Utiliser BREAK en dehors d'une boucle

```pascal
// ERREUR ! Break n'a de sens que dans une boucle
if age >= 18 then
  break;  // ❌ ERREUR DE COMPILATION
```

### 2. Confusion entre BREAK et EXIT

```pascal
// BREAK : sort de la boucle
for i := 1 to 10 do
begin
  if i = 5 then
    break;  // Continue après la boucle
end;

// EXIT : sort de la procédure/fonction/programme entier
procedure MaProcedure;
begin
  for i := 1 to 10 do
  begin
    if i = 5 then
      exit;  // Sort de MaProcedure complètement
  end;
end;
```

### 3. Oublier que CONTINUE saute le reste

```pascal
// ERREUR ! Le compteur n'est pas incrémenté pour i=5
var
  compteur: Integer;
begin
  compteur := 0;
  for i := 1 to 10 do
  begin
    if i = 5 then
      continue;
    compteur := compteur + 1;  // ❌ Jamais exécuté pour i=5
  end;
  WriteLn(compteur);  // Affiche 9, pas 10
end.
```

### 4. BREAK dans la mauvaise boucle

```pascal
// Attention avec les boucles imbriquées
for i := 1 to 10 do
begin
  for j := 1 to 10 do
  begin
    if condition then
      break;  // Sort seulement de la boucle J, pas de I !
  end;
end;
```

### 5. CONTINUE dans REPEAT avec condition finale

```pascal
// ATTENTION ! Continue passe directement au test UNTIL
var
  i: Integer;
begin
  i := 0;
  repeat
    i := i + 1;
    if i = 5 then
      continue;  // Saute WriteLn mais teste quand même la condition
    WriteLn(i);
  until i >= 10;
end.
```

## Bonnes pratiques

### 1. Préférer des conditions claires

```pascal
// MOINS BON : utilisation de break pour sortir
while True do
begin
  ReadLn(valeur);
  if valeur = 0 then
    break;
  // traitement
end;

// MEILLEUR : condition explicite
valeur := -1;
while valeur <> 0 do
begin
  ReadLn(valeur);
  if valeur <> 0 then
    // traitement
end;
```

### 2. Utiliser des drapeaux pour plus de clarté

```pascal
// Avec drapeau (plus lisible)
var
  trouve: Boolean;
begin
  trouve := False;
  for i := 1 to n do
  begin
    if tableau[i] = recherche then
    begin
      trouve := True;
      break;
    end;
  end;

  if trouve then
    WriteLn('Trouvé');
end;
```

### 3. Commenter l'usage de BREAK et CONTINUE

```pascal
for i := 1 to 100 do
begin
  if condition_speciale then
    continue;  // Ignorer les cas spéciaux

  // Traitement normal

  if objectif_atteint then
    break;  // Optimisation : arrêt anticipé
end;
```

### 4. Éviter l'abus de BREAK et CONTINUE

Si vous utilisez trop de `break` et `continue`, votre code devient difficile à suivre. Envisagez de restructurer.

### 5. Attention avec WHILE et REPEAT

```pascal
// WHILE : Continue incrémente automatiquement dans FOR,
// mais pas dans WHILE !
i := 0;
while i < 10 do
begin
  i := i + 1;  // Important !
  if condition then
    continue;  // i a déjà été incrémenté
  WriteLn(i);
end;
```

## Résumé

Les instructions `break` et `continue` offrent un contrôle fin sur les boucles :

### BREAK
- **Action** : Sort immédiatement de la boucle
- **Utilisation** : Recherches, conditions d'arrêt anticipées
- **Effet** : Les itérations restantes sont abandonnées
- **Portée** : Affecte seulement la boucle la plus proche

### CONTINUE
- **Action** : Passe immédiatement à l'itération suivante
- **Utilisation** : Filtrage de données, cas à ignorer
- **Effet** : Le reste de l'itération actuelle est sauté
- **Portée** : Affecte seulement la boucle la plus proche

### Quand les utiliser ?

| Situation | Instruction |
|-----------|-------------|
| Recherche trouvée | BREAK |
| Condition d'arrêt critique | BREAK |
| Ignorer certains cas | CONTINUE |
| Filtrer des données | CONTINUE |
| Sortir de boucle infinie contrôlée | BREAK |

### Points importants
- N'affectent que la boucle la plus proche (imbrication)
- À utiliser avec modération pour garder le code lisible
- Peuvent améliorer les performances en évitant des itérations inutiles
- Attention à bien comprendre le flux d'exécution

Ces instructions sont des outils puissants qui, utilisés judicieusement, rendent votre code plus efficace et plus lisible !

⏭️ [Imbrication de structures](/03-structures-controle/06-imbrication-structures.md)
