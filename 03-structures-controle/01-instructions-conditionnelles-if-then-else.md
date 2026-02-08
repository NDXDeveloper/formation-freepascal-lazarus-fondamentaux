🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.1 Instructions conditionnelles (if-then-else)

## Introduction

Dans la vie quotidienne, nous prenons constamment des décisions basées sur des conditions : "S'il pleut, je prends un parapluie", "Si j'ai faim, je mange". En programmation, les instructions conditionnelles permettent à votre programme de prendre des décisions et d'exécuter différentes actions selon les situations.

## Qu'est-ce qu'une instruction conditionnelle ?

Une instruction conditionnelle permet d'exécuter un bloc de code uniquement si une certaine condition est vraie (true). C'est comme un aiguillage sur une voie ferrée : selon la condition, le programme empruntera un chemin ou un autre.

## La structure IF-THEN simple

### Syntaxe de base

```pascal
if condition then
  instruction;
```

Cette structure est la plus simple. Elle signifie : "Si la condition est vraie, alors exécute l'instruction".

### Exemple concret

```pascal
program ExempleIfSimple;
var
  age: Integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
  if age >= 18 then
    WriteLn('Vous êtes majeur.');
  WriteLn('Programme terminé.');
end.
```

Dans cet exemple, le message "Vous êtes majeur." ne s'affiche que si l'âge est supérieur ou égal à 18.

### Plusieurs instructions avec BEGIN-END

Si vous voulez exécuter plusieurs instructions quand la condition est vraie, vous devez les regrouper entre `begin` et `end` :

```pascal
if age >= 18 then
begin
  WriteLn('Vous êtes majeur.');
  WriteLn('Vous pouvez voter.');
  WriteLn('Vous pouvez passer le permis de conduire.');
end;
```

**Important** : Notez qu'il n'y a pas de point-virgule après le `begin`, mais il y en a un avant le `end`.

## La structure IF-THEN-ELSE

### Syntaxe

```pascal
if condition then
  instruction1
else
  instruction2;
```

Cette structure permet de dire : "Si la condition est vraie, fais ceci, sinon fais cela".

### Exemple

```pascal
program ExempleIfElse;
var
  age: Integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
  if age >= 18 then
    WriteLn('Vous êtes majeur.')  { Pas de point-virgule ici : en Pascal, un ; avant else provoque une erreur }
  else
    WriteLn('Vous êtes mineur.');
end.
```

**Attention** : Il ne faut JAMAIS mettre de point-virgule avant le `else` ! C'est une erreur très courante chez les débutants.

```pascal
// INCORRECT - Ne faites pas ceci !
if age >= 18 then
  WriteLn('Majeur.');  // PAS DE POINT-VIRGULE ICI !
else
  WriteLn('Mineur.');

// CORRECT
if age >= 18 then
  WriteLn('Majeur.')
else
  WriteLn('Mineur.');
```

### Avec plusieurs instructions

```pascal
if age >= 18 then
begin
  WriteLn('Vous êtes majeur.');
  WriteLn('Vous avez tous les droits civiques.');
end
else
begin
  WriteLn('Vous êtes mineur.');
  WriteLn('Vous êtes sous la responsabilité de vos parents.');
end;
```

## Conditions imbriquées (IF dans IF)

Vous pouvez placer une instruction `if` à l'intérieur d'une autre instruction `if`. C'est ce qu'on appelle l'imbrication.

### Exemple

```pascal
program ExempleImbrique;
var
  age: Integer;
  permis: Boolean;
  reponse: String;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
  if age >= 18 then
  begin
    WriteLn('Vous êtes majeur.');
    Write('Avez-vous le permis de conduire ? (true/false) : ');
    ReadLn(reponse);
    permis := (reponse = 'true');
    if permis then
      WriteLn('Vous pouvez conduire.')
    else
      WriteLn('Vous devez passer le permis.');
  end  { Pas de ; après end quand un else suit : même règle que pour une instruction simple }
  else
    WriteLn('Vous êtes trop jeune pour conduire.');
end.
```

## Structure IF-THEN-ELSE IF

Pour tester plusieurs conditions successives, on utilise la structure `else if` :

```pascal
program ExempleElseIf;
var
  note: Integer;
begin
  Write('Entrez votre note (0-20) : ');
  ReadLn(note);
  if note >= 16 then
    WriteLn('Excellent !')
  else if note >= 14 then
    WriteLn('Très bien')
  else if note >= 12 then
    WriteLn('Bien')
  else if note >= 10 then
    WriteLn('Assez bien')
  else
    WriteLn('Insuffisant');
end.
```

Le programme teste les conditions dans l'ordre et s'arrête à la première qui est vraie.

## Les opérateurs de comparaison

Pour construire des conditions, vous utilisez les opérateurs de comparaison :

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `=` | Égal à | `age = 18` |
| `<>` | Différent de | `age <> 18` |
| `<` | Inférieur à | `age < 18` |
| `>` | Supérieur à | `age > 18` |
| `<=` | Inférieur ou égal | `age <= 18` |
| `>=` | Supérieur ou égal | `age >= 18` |

## Les opérateurs logiques

Pour combiner plusieurs conditions, utilisez les opérateurs logiques :

### L'opérateur AND (ET)

Les deux conditions doivent être vraies :

```pascal
if (age >= 18) and (permis = true) then
  WriteLn('Vous pouvez conduire.');
```

### L'opérateur OR (OU)

Au moins une des conditions doit être vraie :

```pascal
if (age < 12) or (age > 65) then
  WriteLn('Tarif réduit disponible.');
```

### L'opérateur NOT (NON)

Inverse une condition :

```pascal
if not (age >= 18) then
  WriteLn('Vous êtes mineur.');
// Équivalent à : if age < 18 then
```

### Exemple complet avec plusieurs opérateurs

```pascal
program ExempleOperateurs;
var
  age: Integer;
  etudiant: Boolean;
  revenus: Real;
  reponse: String;
begin
  Write('Âge : ');
  ReadLn(age);
  Write('Êtes-vous étudiant ? (true/false) : ');
  ReadLn(reponse);
  etudiant := (reponse = 'true');
  Write('Revenus annuels : ');
  ReadLn(revenus);
  { En Pascal, and/or ont une priorité PLUS HAUTE que < > = : les
    parenthèses autour de chaque comparaison sont donc obligatoires,
    sinon le compilateur interprète « age < (26 and etudiant) » }
  if ((age < 26) and etudiant) or (revenus < 10000) then
    WriteLn('Vous avez droit à une aide financière.')
  else
    WriteLn('Vous n''avez pas droit à une aide financière.');
    { '' (deux apostrophes) est la façon d'écrire une apostrophe dans une chaîne Pascal }
end.
```

**Important** : En Pascal, les opérateurs `and`/`or` ont une priorité **plus haute** que les opérateurs de comparaison (`<`, `>`, `=`, etc.). Les parenthèses autour de chaque comparaison sont donc **obligatoires**, pas simplement recommandées.

## Bonnes pratiques

### 1. Indentation

Indentez toujours votre code pour le rendre lisible :

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

### 2. Clarté des conditions

Utilisez des noms de variables explicites :

```pascal
// BON
if estMajeur then

// MOINS BON
if x then
```

### 3. Éviter les conditions trop complexes

Si une condition devient trop longue, utilisez des variables booléennes intermédiaires :

```pascal
// Complexe et difficile à lire
if ((age >= 18) and (permis = true) and (annees_experience > 2)) or ((age >= 25) and (permis = true)) then

// Plus clair
var
  conducteurExperimente: Boolean;
  jeuneConducteurQualifie: Boolean;
begin
  jeuneConducteurQualifie := (age >= 18) and (permis = true) and (annees_experience > 2);
  conducteurExperimente := (age >= 25) and (permis = true);

  if jeuneConducteurQualifie or conducteurExperimente then
    WriteLn('Vous pouvez louer une voiture.');
end;
```

### 4. L'ordre des conditions

Placez les conditions les plus probables en premier pour optimiser l'exécution :

```pascal
// Si la plupart des utilisateurs sont majeurs
if age >= 18 then
  // cas le plus fréquent en premier
else
  // cas moins fréquent
```

## Erreurs courantes à éviter

### 1. Point-virgule avant else

```pascal
// ERREUR !
if condition then
  instruction;  // Ce point-virgule est une erreur !
else
  instruction;

// CORRECT
if condition then
  instruction
else
  instruction;
```

### 2. Confusion entre = et :=

```pascal
// ERREUR ! = est pour la comparaison, pas pour l'affectation
if age = 18 then
  age = 19;  // ERREUR !

// CORRECT
if age = 18 then
  age := 19;  // Affectation avec :=
```

### 3. Oublier begin-end pour plusieurs instructions

```pascal
// ERREUR ! Seule la première instruction est conditionnelle
if age >= 18 then
  WriteLn('Majeur');
  WriteLn('Accès autorisé');  // Cette ligne s'exécute toujours !

// CORRECT
if age >= 18 then
begin
  WriteLn('Majeur');
  WriteLn('Accès autorisé');
end;
```

## Exemple récapitulatif

```pascal
program GestionAcces;
var
  age: Integer;
  membre: Boolean;
  solde: Real;
  acces: Boolean;
  reponse: String;
begin
  WriteLn('=== Système de gestion d''accès ===');
  WriteLn;
  Write('Entrez votre âge : ');
  ReadLn(age);
  Write('Êtes-vous membre ? (true/false) : ');
  ReadLn(reponse);
  membre := (reponse = 'true');
  Write('Solde du compte : ');
  ReadLn(solde);
  WriteLn;
  WriteLn('--- Analyse ---');
  if age < 18 then
  begin
    WriteLn('Vous êtes mineur.');
    WriteLn('Accès refusé pour les mineurs.');
    acces := false;
  end
  else
  begin
    WriteLn('Vous êtes majeur.');
    if membre then
    begin
      WriteLn('Vous êtes membre.');
      if solde >= 0 then
      begin
        WriteLn('Votre compte est créditeur.');
        acces := true;
      end
      else
      begin
        WriteLn('Votre compte est débiteur.');
        acces := false;
      end;
    end
    else
    begin
      WriteLn('Vous n''êtes pas membre.');
      acces := false;
    end;
  end;
  WriteLn;
  WriteLn('--- Résultat ---');
  if acces then
    WriteLn('ACCÈS AUTORISÉ')
  else
    WriteLn('ACCÈS REFUSÉ');
end.
```

## Résumé

Les instructions conditionnelles permettent à votre programme de prendre des décisions :

- `if condition then` exécute une instruction si la condition est vraie
- `if condition then ... else ...` exécute une instruction ou une autre selon la condition
- Utilisez `begin-end` pour regrouper plusieurs instructions
- **Ne mettez jamais de point-virgule avant `else`**
- Les opérateurs de comparaison : `=`, `<>`, `<`, `>`, `<=`, `>=`
- Les opérateurs logiques : `and`, `or`, `not`
- Indentez votre code pour le rendre lisible
- Utilisez des noms de variables clairs

Les instructions conditionnelles sont fondamentales en programmation. Maîtriser leur utilisation est essentiel pour créer des programmes capables de réagir intelligemment à différentes situations.

⏭️ [Instructions de choix multiple (case-of)](/03-structures-controle/02-instructions-choix-multiple-case-of.md)
