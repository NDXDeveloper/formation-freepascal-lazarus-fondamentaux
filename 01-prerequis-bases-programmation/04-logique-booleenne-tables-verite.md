🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.4 Logique booléenne et tables de vérité

## Introduction

La logique booléenne est le fondement de toute la programmation. Elle permet de prendre des décisions, de contrôler le flux d'exécution d'un programme et de tester des conditions. Cette section vous donnera les outils pour comprendre et maîtriser la logique dans vos programmes.

## Qu'est-ce que la logique booléenne ?

### Histoire et origines

La logique booléenne porte le nom du mathématicien britannique **George Boole** (1815-1864) qui a développé un système mathématique pour représenter la logique.

**Principe fondamental :** En logique booléenne, toute affirmation ne peut être que **vraie** ou **fausse**, sans état intermédiaire.

### Les valeurs booléennes

Il n'existe que deux valeurs possibles :
- **VRAI** (True, 1, Oui)
- **FAUX** (False, 0, Non)

**Exemples d'affirmations booléennes :**
```
"Il pleut" → VRAI ou FAUX
"5 est plus grand que 3" → VRAI
"10 est égal à 20" → FAUX
"La porte est ouverte" → VRAI ou FAUX
```

**En Pascal :**
```pascal
var
  estVrai: Boolean;

begin
  estVrai := True;   // Affectation de VRAI
  estVrai := False;  // Affectation de FAUX
end.
```

## Les opérateurs logiques de base

### L'opérateur NOT (NON)

**Symbole en Pascal :** `not`

**Fonction :** Inverse une valeur booléenne.

**Table de vérité :**
```
| A     | NOT A |
|-------|-------|
| VRAI  | FAUX  |
| FAUX  | VRAI  |
```

**Exemples concrets :**
```
NOT (Il pleut) = Il ne pleut pas
NOT (La porte est ouverte) = La porte est fermée
NOT (VRAI) = FAUX
NOT (FAUX) = VRAI
```

**En Pascal :**
```pascal
var
  aFaim: Boolean;
  nAPasFaim: Boolean;

begin
  aFaim := True;
  nAPasFaim := not aFaim;  // nAPasFaim vaut False
end.
```

### L'opérateur AND (ET)

**Symbole en Pascal :** `and`

**Fonction :** Retourne VRAI seulement si **toutes** les conditions sont vraies.

**Table de vérité :**
```
| A     | B     | A AND B |
|-------|-------|---------|
| FAUX  | FAUX  | FAUX    |
| FAUX  | VRAI  | FAUX    |
| VRAI  | FAUX  | FAUX    |
| VRAI  | VRAI  | VRAI    |
```

**Mémorisation :** Le résultat n'est VRAI que si A **ET** B sont vrais tous les deux.

**Exemples concrets :**
```
(J'ai faim) AND (Il y a de la nourriture) → Je peux manger
(Il pleut) AND (J'ai un parapluie) → Je peux sortir sans me mouiller
(Age >= 18) AND (A le permis) → Peut conduire
```

**En Pascal :**
```pascal
var
  aFaim, aNourriture, peutManger: Boolean;

begin
  aFaim := True;
  aNourriture := True;
  peutManger := aFaim and aNourriture;  // VRAI

  // Exemple avec des conditions
  if (age >= 18) and (aPermis) then
    WriteLn('Vous pouvez conduire');
end.
```

### L'opérateur OR (OU)

**Symbole en Pascal :** `or`

**Fonction :** Retourne VRAI si **au moins une** des conditions est vraie.

**Table de vérité :**
```
| A     | B     | A OR B |
|-------|-------|--------|
| FAUX  | FAUX  | FAUX   |
| FAUX  | VRAI  | VRAI   |
| VRAI  | FAUX  | VRAI   |
| VRAI  | VRAI  | VRAI   |
```

**Mémorisation :** Le résultat est VRAI si A **OU** B (ou les deux) est vrai.

**Exemples concrets :**
```
(C'est samedi) OR (C'est dimanche) → C'est le week-end
(Il fait chaud) OR (J'ai soif) → Je veux boire
(Note >= 10) OR (Rattrapages réussis) → Examen validé
```

**En Pascal :**
```pascal
var
  estSamedi, estDimanche, estWeekend: Boolean;

begin
  estSamedi := False;
  estDimanche := True;
  estWeekend := estSamedi or estDimanche;  // VRAI

  // Exemple avec des conditions
  if (temperature > 30) or (soleilBrillant) then
    WriteLn('Portez de la crème solaire');
end.
```

## Les opérateurs logiques composés

### L'opérateur XOR (OU exclusif)

**Symbole en Pascal :** `xor`

**Fonction :** Retourne VRAI si **une seule** des deux conditions est vraie (mais pas les deux).

**Table de vérité :**
```
| A     | B     | A XOR B |
|-------|-------|---------|
| FAUX  | FAUX  | FAUX    |
| FAUX  | VRAI  | VRAI    |
| VRAI  | FAUX  | VRAI    |
| VRAI  | VRAI  | FAUX    |
```

**Mémorisation :** Le résultat est VRAI si A et B sont **différents**.

**Exemples concrets :**
```
(Thé) XOR (Café) → Je prends l'un OU l'autre, mais pas les deux
(Pile) XOR (Face) → Une pièce ne peut être que pile ou face
```

**En Pascal :**
```pascal
var
  choixThe, choixCafe: Boolean;

begin
  choixThe := True;
  choixCafe := False;

  if choixThe xor choixCafe then
    WriteLn('Vous avez fait un choix exclusif');
  // Résultat : VRAI (un seul est vrai)
end.
```

### Les opérateurs NAND et NOR

Ces opérateurs sont moins courants en programmation mais importants en électronique.

**NAND (NOT AND) :**
```
| A     | B     | A NAND B |
|-------|-------|----------|
| FAUX  | FAUX  | VRAI     |
| FAUX  | VRAI  | VRAI     |
| VRAI  | FAUX  | VRAI     |
| VRAI  | VRAI  | FAUX     |
```

NAND = NOT (A AND B)

**NOR (NOT OR) :**
```
| A     | B     | A NOR B |
|-------|-------|---------|
| FAUX  | FAUX  | VRAI    |
| FAUX  | VRAI  | FAUX    |
| VRAI  | FAUX  | FAUX    |
| VRAI  | VRAI  | FAUX    |
```

NOR = NOT (A OR B)

**En Pascal :**
```pascal
// NAND s'écrit :
resultat := not (A and B);

// NOR s'écrit :
resultat := not (A or B);
```

## Les opérateurs de comparaison

Ces opérateurs retournent des valeurs booléennes.

### Opérateurs disponibles en Pascal

```
=   : Égal à
<>  : Différent de
<   : Inférieur à
>   : Supérieur à
<=  : Inférieur ou égal à
>=  : Supérieur ou égal à
```

**Exemples :**
```pascal
5 = 5      // VRAI
5 <> 3     // VRAI (5 est différent de 3)
10 > 7     // VRAI
10 < 7     // FAUX
10 >= 10   // VRAI
8 <= 5     // FAUX
```

### Utilisation dans les expressions

```pascal
var
  age: Integer;
  estMajeur, estEnfant: Boolean;

begin
  age := 20;

  estMajeur := (age >= 18);     // VRAI
  estEnfant := (age < 12);      // FAUX

  if (age >= 18) and (age < 65) then
    WriteLn('Vous êtes un adulte actif');
end.
```

## Les expressions booléennes complexes

### Combinaison d'opérateurs

On peut combiner plusieurs opérateurs logiques dans une même expression.

**Exemple 1 :** Pour entrer dans un parc d'attractions
```pascal
var
  age, taille: Integer;
  avecAdulte, peutEntrer: Boolean;

begin
  age := 10;
  taille := 140;
  avecAdulte := True;

  // Conditions : (plus de 12 ans) OU (plus de 130cm ET accompagné)
  peutEntrer := (age >= 12) or ((taille >= 130) and avecAdulte);
  // Résultat : VRAI
end.
```

**Exemple 2 :** Conditions d'accès à une réduction
```pascal
// Réduction si : (Étudiant OU Retraité) ET pas déjà client
reductionApplicable := (estEtudiant or estRetraite) and (not estDejaClient);
```

### Ordre de priorité des opérateurs

Comme en mathématiques, il existe un ordre de priorité :

**1. Parenthèses** : `( )`
**2. NOT** (négation)
**3. AND** (multiplication logique)
**4. OR, XOR** (addition logique)
**5. Comparaisons** : `=`, `<>`, `<`, `>`, `<=`, `>=`

**Exemple sans parenthèses :**
```pascal
A or B and C
// Équivaut à : A or (B and C)
// AND est évalué avant OR
```

**Conseil :** Utilisez toujours des parenthèses pour clarifier vos intentions, même si ce n'est pas strictement nécessaire.

```pascal
// Ambigu
if age >= 18 and age <= 65 or estRetraite then ...

// Clair
if ((age >= 18) and (age <= 65)) or estRetraite then ...
```

## Les lois de la logique booléenne

### Lois fondamentales

**Loi de commutativité :**
```
A AND B = B AND A
A OR B = B OR A
```

**Loi d'associativité :**
```
(A AND B) AND C = A AND (B AND C)
(A OR B) OR C = A OR (B OR C)
```

**Loi de distributivité :**
```
A AND (B OR C) = (A AND B) OR (A AND C)
A OR (B AND C) = (A OR B) AND (A OR C)
```

**Loi de l'identité :**
```
A AND VRAI = A
A OR FAUX = A
```

**Loi de l'élément absorbant :**
```
A AND FAUX = FAUX
A OR VRAI = VRAI
```

**Loi de complémentarité :**
```
A AND (NOT A) = FAUX
A OR (NOT A) = VRAI
```

**Loi de la double négation :**
```
NOT (NOT A) = A
```

### Les lois de De Morgan

Ces lois sont très importantes pour simplifier les expressions :

**Première loi :**
```
NOT (A AND B) = (NOT A) OR (NOT B)
```

**Deuxième loi :**
```
NOT (A OR B) = (NOT A) AND (NOT B)
```

**Exemple pratique :**
```pascal
// Expression originale
if not ((age < 18) or (age > 65)) then
  WriteLn('Adulte actif');

// Équivalent selon De Morgan
if (age >= 18) and (age <= 65) then
  WriteLn('Adulte actif');
```

## Tables de vérité pour expressions complexes

### Construire une table de vérité

Pour analyser une expression complexe, on construit une table avec toutes les combinaisons possibles.

**Exemple : (A AND B) OR (NOT A AND C)**

```
| A     | B     | C     | A AND B | NOT A | NOT A AND C | Résultat |
|-------|-------|-------|---------|-------|-------------|----------|
| FAUX  | FAUX  | FAUX  | FAUX    | VRAI  | FAUX        | FAUX     |
| FAUX  | FAUX  | VRAI  | FAUX    | VRAI  | VRAI        | VRAI     |
| FAUX  | VRAI  | FAUX  | FAUX    | VRAI  | FAUX        | FAUX     |
| FAUX  | VRAI  | VRAI  | FAUX    | VRAI  | VRAI        | VRAI     |
| VRAI  | FAUX  | FAUX  | FAUX    | FAUX  | FAUX        | FAUX     |
| VRAI  | FAUX  | VRAI  | FAUX    | FAUX  | FAUX        | FAUX     |
| VRAI  | VRAI  | FAUX  | VRAI    | FAUX  | FAUX        | VRAI     |
| VRAI  | VRAI  | VRAI  | VRAI    | FAUX  | FAUX        | VRAI     |
```

**Méthode :**
1. Lister toutes les combinaisons possibles des variables (2ⁿ lignes pour n variables)
2. Calculer les expressions intermédiaires
3. Calculer le résultat final

## Applications en programmation

### Les conditions IF

Les expressions booléennes sont au cœur des instructions conditionnelles.

```pascal
var
  temperature: Integer;
  estEte, faitChaud: Boolean;

begin
  temperature := 28;
  estEte := True;

  // Condition simple
  if temperature > 25 then
    WriteLn('Il fait chaud');

  // Condition composée
  if (temperature > 25) and estEte then
    WriteLn('C''est l''été et il fait chaud');

  // Condition avec ELSE
  if (temperature < 10) or (not estEte) then
    WriteLn('Prenez un manteau')
  else
    WriteLn('Vêtements légers suffisent');
end.
```

### Les boucles WHILE

Les boucles utilisent aussi des expressions booléennes.

```pascal
var
  compteur, somme: Integer;
  continuer: Boolean;

begin
  compteur := 1;
  somme := 0;
  continuer := True;

  // La boucle continue tant que la condition est VRAIE
  while (compteur <= 10) and continuer do
  begin
    somme := somme + compteur;
    compteur := compteur + 1;

    // On peut arrêter la boucle si une condition est remplie
    if somme > 30 then
      continuer := False;
  end;
end.
```

### Les drapeaux (flags)

Les variables booléennes servent souvent de drapeaux pour contrôler l'état du programme.

```pascal
var
  trouve, erreur, fini: Boolean;
  i: Integer;

begin
  trouve := False;
  erreur := False;
  i := 1;

  while (i <= 100) and (not trouve) and (not erreur) do
  begin
    // Recherche d'un élément
    if tableau[i] = valeurRecherchee then
      trouve := True
    else if tableau[i] < 0 then
      erreur := True;

    i := i + 1;
  end;

  if trouve then
    WriteLn('Élément trouvé')
  else if erreur then
    WriteLn('Erreur détectée')
  else
    WriteLn('Élément non trouvé');
end.
```

## Court-circuit d'évaluation

### Qu'est-ce que le court-circuit ?

En Pascal (et beaucoup d'autres langages), les expressions booléennes sont évaluées de gauche à droite, et l'évaluation s'arrête dès que le résultat est connu.

**Pour AND :**
- Si la première condition est FAUX, le résultat est forcément FAUX
- La deuxième condition n'est pas évaluée

**Pour OR :**
- Si la première condition est VRAI, le résultat est forcément VRAI
- La deuxième condition n'est pas évaluée

### Exemple pratique

```pascal
// Éviter une division par zéro
if (denominateur <> 0) and (numerateur / denominateur > 10) then
  WriteLn('Résultat supérieur à 10');
// Si denominateur = 0, la division n'est jamais effectuée

// Vérifier qu'un tableau n'est pas vide avant d'accéder à un élément
if (Length(tableau) > 0) and (tableau[0] = valeur) then
  WriteLn('Premier élément correspond');
```

**Attention :** En FreePascal, le court-circuit est activé par défaut (directive `{$B-}`). Si vous voulez être sûr qu'il est actif, ajoutez cette directive en début de fichier :

```pascal
{$B-}  // Active l'évaluation en court-circuit (comportement par défaut)

// Avec le court-circuit actif, la division n'est jamais effectuée si x = 0
if (x <> 0) and (y / x > 5) then
  WriteLn('OK');
```

**Note :** La directive `{$B+}` force l'évaluation complète de toutes les conditions (utile dans de rares cas). Par défaut, FreePascal utilise `{$B-}` (court-circuit), donc vous n'avez normalement rien à ajouter.

## Simplification d'expressions booléennes

### Pourquoi simplifier ?

Une expression simplifiée est :
- Plus facile à comprendre
- Plus rapide à exécuter
- Moins sujette aux erreurs

### Exemples de simplification

**Exemple 1 : Utilisation des lois**
```pascal
// Expression originale
if (age >= 18) and (True) then
  ...

// Simplifié (loi de l'identité : A AND VRAI = A)
if age >= 18 then
  ...
```

**Exemple 2 : Élimination des négations doubles**
```pascal
// Expression originale
if not (not estActif) then
  ...

// Simplifié (double négation)
if estActif then
  ...
```

**Exemple 3 : Application de De Morgan**
```pascal
// Expression originale
if not (estFermé or estComplet) then
  ...

// Simplifié (De Morgan)
if (not estFermé) and (not estComplet) then
  ...

// Ou mieux, avec des noms positifs
if estOuvert and aPlacesDisponibles then
  ...
```

## Bonnes pratiques

### 1. Utilisez des noms de variables explicites

```pascal
// Mauvais
if x and y or not z then ...

// Bon
if estConnecté and aLesPermissions or not estVerrouillé then ...
```

### 2. Évitez les comparaisons inutiles avec True/False

```pascal
// Mauvais
if estActif = True then ...
if estFermé = False then ...

// Bon
if estActif then ...
if not estFermé then ...
```

### 3. Utilisez des parenthèses pour la clarté

```pascal
// Moins clair
if age >= 18 and age <= 65 or estRetraité then ...

// Plus clair
if ((age >= 18) and (age <= 65)) or estRetraité then ...
```

### 4. Préférez les expressions positives

```pascal
// Moins clair
if not (not estOuvert) then ...

// Plus clair
if estOuvert then ...
```

### 5. Décomposez les expressions complexes

```pascal
// Complexe
if (age >= 18) and (age <= 65) and (aPermis) and (not estSuspendu) then ...

// Plus lisible
var
  estAdulte, peutConduire: Boolean;
begin
  estAdulte := (age >= 18) and (age <= 65);
  peutConduire := aPermis and (not estSuspendu);

  if estAdulte and peutConduire then ...
end.
```

## Pièges courants à éviter

### 1. Confusion entre = et :=

```pascal
// Erreur : = est pour la comparaison, pas l'affectation
if x = 5 then
  x = 10;  // ERREUR !

// Correct
if x = 5 then
  x := 10;  // Affectation
```

### 2. Oublier les parenthèses

```pascal
// Ambigu à cause de la priorité des opérateurs
if age >= 18 and aPermis then ...

// Mieux
if (age >= 18) and aPermis then ...
```

### 3. Négations complexes

```pascal
// Difficile à comprendre
if not (not A and not B) then ...

// Appliquer De Morgan pour simplifier
if A or B then ...
```

### 4. Court-circuit involontaire

```pascal
// Peut causer des problèmes si la fonction a des effets de bord
if (a > 0) and FonctionQuiModifie() then ...
// Si a <= 0, FonctionQuiModifie() n'est jamais appelée !
```

## Conclusion

La logique booléenne est un outil fondamental en programmation qui vous permettra de créer des programmes intelligents capables de prendre des décisions.

**Points clés à retenir :**
- Il n'existe que deux valeurs booléennes : VRAI et FAUX
- Les opérateurs de base sont NOT, AND, OR
- Les tables de vérité permettent d'analyser les expressions
- Les lois booléennes permettent de simplifier les expressions
- Les expressions booléennes sont au cœur des structures de contrôle (if, while)
- Utilisez des noms explicites et des parenthèses pour la clarté
- Attention à l'évaluation en court-circuit

**Ce que vous devez maîtriser :**
- Construire et comprendre des tables de vérité
- Combiner des conditions avec AND, OR, NOT
- Simplifier des expressions booléennes
- Utiliser correctement les opérateurs de comparaison
- Écrire des conditions claires et maintenables

Dans la prochaine section, nous mettrons ces connaissances en pratique avec les algorithmes et le pseudo-code, avant de commencer à programmer réellement en Pascal.

⏭️ [Algorithmes et pseudo-code](/01-prerequis-bases-programmation/05-algorithmes-pseudo-code.md)
