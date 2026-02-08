🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.2 Instructions de choix multiple (case-of)

## Introduction

Imaginez que vous devez écrire un programme qui affiche le nom d'un jour de la semaine selon un numéro (1 pour lundi, 2 pour mardi, etc.). Avec des `if-else`, vous devriez écrire 7 conditions différentes ! L'instruction `case-of` est conçue pour simplifier ce type de situation où vous devez choisir parmi plusieurs possibilités basées sur une seule valeur.

## Pourquoi utiliser CASE-OF ?

### Le problème avec IF-ELSE multiples

Regardez ce code avec des `if-else` :

```pascal
if jour = 1 then
  WriteLn('Lundi')
else if jour = 2 then
  WriteLn('Mardi')
else if jour = 3 then
  WriteLn('Mercredi')
else if jour = 4 then
  WriteLn('Jeudi')
else if jour = 5 then
  WriteLn('Vendredi')
else if jour = 6 then
  WriteLn('Samedi')
else if jour = 7 then
  WriteLn('Dimanche');
```

C'est long, répétitif et difficile à lire !

### La solution avec CASE-OF

Le même code avec `case-of` :

```pascal
case jour of
  1: WriteLn('Lundi');
  2: WriteLn('Mardi');
  3: WriteLn('Mercredi');
  4: WriteLn('Jeudi');
  5: WriteLn('Vendredi');
  6: WriteLn('Samedi');
  7: WriteLn('Dimanche');
end;
```

Beaucoup plus clair et concis !

## Syntaxe de base

### Structure générale

```pascal
case expression of
  valeur1: instruction1;
  valeur2: instruction2;
  valeur3: instruction3;
  // ... autres valeurs
end;
```

**Éléments importants :**
- `case` : mot-clé de début
- `expression` : la variable ou expression à tester
- `of` : mot-clé qui suit l'expression
- `valeur:` : chaque cas possible suivi de deux-points
- `end;` : mot-clé de fin (avec point-virgule)

### Premier exemple complet

```pascal
program ExempleNoteLettre;
var
  note: Integer;
begin
  Write('Entrez votre note (0-20) : ');
  ReadLn(note);
  { Virgule = plusieurs valeurs pour un meme cas (pas besoin de repeter l'instruction) }
  case note of
    18, 19, 20: WriteLn('Mention Excellent');
    16, 17: WriteLn('Mention Tres Bien');
    14, 15: WriteLn('Mention Bien');
    12, 13: WriteLn('Mention Assez Bien');
    10, 11: WriteLn('Passable');
  end;
end.
```

Notez que vous pouvez regrouper plusieurs valeurs avec des virgules : `18, 19, 20`

## Types compatibles avec CASE-OF

L'instruction `case-of` ne fonctionne **qu'avec des types ordinaux** :

### Types autorisés

- **Integer** (entiers) : le plus courant
- **Char** (caractères)
- **Boolean** (booléens)
- **Types énumérés** (que nous verrons plus tard)

### Exemple avec Integer

```pascal
var
  choix: Integer;
begin
  case choix of
    1: WriteLn('Option 1');
    2: WriteLn('Option 2');
    3: WriteLn('Option 3');
  end;
end.
```

### Exemple avec Char

```pascal
var
  reponse: Char;
begin
  Write('Votre choix (A/B/C) : ');
  ReadLn(reponse);

  case reponse of
    'A', 'a': WriteLn('Vous avez choisi A');
    'B', 'b': WriteLn('Vous avez choisi B');
    'C', 'c': WriteLn('Vous avez choisi C');
  end;
end.
```

### Exemple avec Boolean

```pascal
var
  actif: Boolean;
begin
  case actif of
    True: WriteLn('Le système est actif');
    False: WriteLn('Le système est inactif');
  end;
end.
```

### Types NON compatibles

**Attention !** Vous ne pouvez PAS utiliser `case-of` avec :
- **String** (chaînes de caractères)
- **Real** (nombres à virgule)

```pascal
// CECI NE FONCTIONNE PAS !
var
  nom: String;
begin
  case nom of  // ERREUR !
    'Pierre': WriteLn('Bonjour Pierre');
  end;
end.
```

Pour ces types, vous devez utiliser `if-else`.

## Intervalles de valeurs

Une fonctionnalité très pratique : vous pouvez spécifier des intervalles avec `..`

### Syntaxe des intervalles

```pascal
case valeur of
  1..5: instruction;      // de 1 à 5 inclus
  10..20: instruction;    // de 10 à 20 inclus
end;
```

### Exemple pratique

```pascal
program ExempleIntervalle;
var
  age: Integer;
begin
  Write('Entrez votre age : ');
  ReadLn(age);
  { La syntaxe 0..2 signifie "de 0 a 2 inclus" (intervalle) }
  case age of
    0..2: WriteLn('Bebe');
    3..5: WriteLn('Petite enfance');
    6..11: WriteLn('Enfance');
    12..17: WriteLn('Adolescence');
    18..64: WriteLn('Adulte');
    65..120: WriteLn('Senior');
  end;
end.
```

### Mélanger valeurs et intervalles

Vous pouvez combiner des valeurs individuelles et des intervalles :

```pascal
case note of
  0..9: WriteLn('Insuffisant');
  10, 11: WriteLn('Passable');
  12..13: WriteLn('Assez bien');
  14..15: WriteLn('Bien');
  16..17: WriteLn('Très bien');
  18..20: WriteLn('Excellent');
end;
```

## La clause ELSE

Que se passe-t-il si la valeur ne correspond à aucun cas ? Par défaut, rien ne se passe. Pour gérer ce cas, utilisez `else` :

### Syntaxe avec ELSE

```pascal
case expression of
  valeur1: instruction1;
  valeur2: instruction2;
else
  instruction_par_defaut;
end;
```

### Exemple

```pascal
program ExempleAvecElse;
var
  jour: Integer;
begin
  Write('Entrez un numero de jour (1-7) : ');
  ReadLn(jour);
  case jour of
    1: WriteLn('Lundi');
    2: WriteLn('Mardi');
    3: WriteLn('Mercredi');
    4: WriteLn('Jeudi');
    5: WriteLn('Vendredi');
    6: WriteLn('Samedi');
    7: WriteLn('Dimanche');
  else  { Dans case..of, le point-virgule avant else est autorise (contrairement a if..then..else) }
    WriteLn('Numero de jour invalide !');
  end;
end.
```

**Important :** Contrairement au `if-then-else`, vous **pouvez** mettre un point-virgule avant le `else` dans un `case-of`. Les deux formes (avec ou sans point-virgule) sont acceptées par FreePascal.

## Plusieurs instructions par cas

Si vous voulez exécuter plusieurs instructions pour un cas, utilisez `begin-end` :

### Syntaxe

```pascal
case expression of
  valeur1:
    begin
      instruction1;
      instruction2;
      instruction3;
    end;
  valeur2: instruction;
end;
```

### Exemple

```pascal
program MenuApplication;
var
  choix: Integer;
begin
  WriteLn('=== MENU PRINCIPAL ===');
  WriteLn('1. Nouveau document');
  WriteLn('2. Ouvrir un document');
  WriteLn('3. Quitter');
  WriteLn;
  Write('Votre choix : ');
  ReadLn(choix);
  WriteLn;
  case choix of
    1:
      begin
        WriteLn('Creation d''un nouveau document...');
        WriteLn('Document cree avec succes !');
        WriteLn('Vous pouvez maintenant travailler.');
      end;
    2:
      begin
        WriteLn('Ouverture d''un document existant...');
        WriteLn('Veuillez selectionner un fichier.');
      end;
    3:
      begin
        WriteLn('Fermeture de l''application...');
        WriteLn('Au revoir !');
      end;
  else
    WriteLn('Choix invalide. Veuillez reessayer.');
  end;
end.
```

## CASE-OF imbriqués

Vous pouvez placer un `case-of` à l'intérieur d'un autre `case-of` :

```pascal
program MenuImbriques;
var
  categorie, choix: Integer;
begin
  WriteLn('Categories :');
  WriteLn('1. Fichier');
  WriteLn('2. Edition');
  Write('Choisissez une categorie : ');
  ReadLn(categorie);
  case categorie of
    1:
      begin
        WriteLn('1. Nouveau');
        WriteLn('2. Ouvrir');
        WriteLn('3. Enregistrer');
        Write('Votre choix : ');
        ReadLn(choix);
        case choix of
          1: WriteLn('Nouveau fichier cree');
          2: WriteLn('Ouvrir un fichier');
          3: WriteLn('Enregistrer le fichier');
        else
          WriteLn('Choix invalide');
        end;
      end;
    2:
      begin
        WriteLn('1. Copier');
        WriteLn('2. Coller');
        WriteLn('3. Couper');
        Write('Votre choix : ');
        ReadLn(choix);
        case choix of
          1: WriteLn('Texte copie');
          2: WriteLn('Texte colle');
          3: WriteLn('Texte coupe');
        else
          WriteLn('Choix invalide');
        end;
      end;
  else
    WriteLn('Categorie invalide');
  end;
end.
```

## Exemples pratiques

### Calculatrice simple

```pascal
program CalculatriceCaseOf;
var
  nombre1, nombre2: Real;
  operation: Char;
  resultat: Real;
begin
  WriteLn('=== CALCULATRICE SIMPLE ===');
  Write('Premier nombre : ');
  ReadLn(nombre1);
  Write('Operation (+, -, *, /) : ');
  ReadLn(operation);
  Write('Deuxieme nombre : ');
  ReadLn(nombre2);
  { case..of fonctionne avec Char car c'est un type ordinal (comme Integer et Boolean) }
  case operation of
    '+':
      begin
        resultat := nombre1 + nombre2;
        WriteLn('Resultat : ', resultat:0:2);
      end;
    '-':
      begin
        resultat := nombre1 - nombre2;
        WriteLn('Resultat : ', resultat:0:2);
      end;
    '*':
      begin
        resultat := nombre1 * nombre2;
        WriteLn('Resultat : ', resultat:0:2);
      end;
    '/':
      begin
        if nombre2 <> 0 then
        begin
          resultat := nombre1 / nombre2;
          WriteLn('Resultat : ', resultat:0:2);
        end
        else
          WriteLn('Erreur : division par zero !');
      end;
  else
    WriteLn('Operation non reconnue !');
  end;
end.
```

### Conversion de notes

```pascal
program ConversionNotes;
var
  noteLettre: Char;
begin
  Write('Entrez votre note (A, B, C, D, F) : ');
  ReadLn(noteLettre);
  Write('Equivalent francais : ');
  case noteLettre of
    'A', 'a': WriteLn('Excellent (16-20)');
    'B', 'b': WriteLn('Bien (14-15)');
    'C', 'c': WriteLn('Assez Bien (12-13)');
    'D', 'd': WriteLn('Passable (10-11)');
    'F', 'f': WriteLn('Insuffisant (0-9)');
  else
    WriteLn('Note invalide !');
  end;
end.
```

### Système de tarification

```pascal
program SystemeTarification;
var
  age: Integer;
  tarif: Real;
begin
  Write('Entrez votre age : ');
  ReadLn(age);
  case age of
    0..4:
      begin
        tarif := 0;
        WriteLn('Categorie : Gratuit (moins de 5 ans)');
      end;
    5..12:
      begin
        tarif := 5.50;
        WriteLn('Categorie : Enfant');
      end;
    13..17:
      begin
        tarif := 8.00;
        WriteLn('Categorie : Adolescent');
      end;
    18..64:
      begin
        tarif := 12.50;
        WriteLn('Categorie : Adulte');
      end;
    65..120:
      begin
        tarif := 9.00;
        WriteLn('Categorie : Senior');
      end;
  else
    begin
      tarif := 0;
      WriteLn('Age invalide !');
    end;
  end;
  if (age >= 0) and (age <= 120) then
    WriteLn('Tarif : ', tarif:0:2, ' euros');
end.
```

## CASE-OF vs IF-ELSE : Quand utiliser quoi ?

### Utilisez CASE-OF quand :

✓ Vous testez **une seule variable** contre plusieurs valeurs possibles
✓ Les valeurs sont des **types ordinaux** (Integer, Char, Boolean)
✓ Vous avez **3 conditions ou plus** à tester
✓ Les valeurs sont des **constantes connues**

### Utilisez IF-ELSE quand :

✓ Vous testez des **conditions complexes** (avec `and`, `or`)
✓ Vous comparez des **types non ordinaux** (String, Real)
✓ Vous testez des **intervalles complexes** avec plusieurs variables
✓ Vous avez seulement **1 ou 2 conditions**

### Tableau comparatif

| Situation | CASE-OF | IF-ELSE |
|-----------|---------|---------|
| `if jour = 1 or jour = 2 or jour = 3` | ✓ Idéal | Possible |
| `if (age > 18) and (permis = true)` | ✗ Impossible | ✓ Idéal |
| `if nom = 'Pierre'` | ✗ String non supporté | ✓ Idéal |
| `if note = 10.5` | ✗ Real non supporté | ✓ Idéal |
| `if choix = 1 or choix = 2 or choix = 3` | ✓ Idéal | Verbeux |

## Erreurs courantes

### 1. Utiliser des types non ordinaux

```pascal
// ERREUR !
var
  nom: String;
begin
  case nom of  // String n'est pas un type ordinal
    'Pierre': WriteLn('Bonjour');
  end;
end.
```

**Solution :** Utilisez `if-else` pour les chaînes.

### 2. Oublier les deux-points

```pascal
// ERREUR !
case jour of
  1 WriteLn('Lundi');  // Manque les deux-points
end;

// CORRECT
case jour of
  1: WriteLn('Lundi');
end;
```

### 3. Valeurs qui se chevauchent

```pascal
// PROBLÈME !
case age of
  1..10: WriteLn('Enfant');
  10..20: WriteLn('Adolescent');  // 10 est dans les deux !
end;

// CORRECT
case age of
  1..9: WriteLn('Enfant');
  10..20: WriteLn('Adolescent');
end;
```

### 4. Différence avec if-then-else pour le point-virgule

Contrairement au `if-then-else`, le `case-of` accepte un point-virgule avant `else` :

```pascal
// Les deux formes sont valides dans un case-of :

// Forme 1 : avec point-virgule
case jour of
  1: WriteLn('Lundi');
  2: WriteLn('Mardi');
else
  WriteLn('Invalide');
end;

// Forme 2 : sans point-virgule
case jour of
  1: WriteLn('Lundi');
  2: WriteLn('Mardi')
else
  WriteLn('Invalide');
end;
```

### 5. Oublier le END final

```pascal
// ERREUR !
case jour of
  1: WriteLn('Lundi');
  2: WriteLn('Mardi');
// Manque le end;

// CORRECT
case jour of
  1: WriteLn('Lundi');
  2: WriteLn('Mardi');
end;
```

## Bonnes pratiques

### 1. Toujours inclure un ELSE

Même si vous pensez avoir couvert tous les cas, utilisez toujours `else` pour gérer les valeurs inattendues :

```pascal
case mois of
  1..12: // traiter le mois
else
  WriteLn('Mois invalide !');
end;
```

### 2. Indentation claire

```pascal
// BON
case choix of
  1: WriteLn('Option 1');
  2:
    begin
      WriteLn('Option 2');
      WriteLn('Plus complexe');
    end;
  3: WriteLn('Option 3');
end;

// MAUVAIS (difficile à lire)
case choix of
1: WriteLn('Option 1');
2: begin
WriteLn('Option 2');
end;
3: WriteLn('Option 3');
end;
```

### 3. Regrouper les cas similaires

```pascal
// Bon usage des virgules
case jour of
  1, 2, 3, 4, 5: WriteLn('Jour de travail');
  6, 7: WriteLn('Week-end');
end;
```

### 4. Commentaires pour les cas complexes

```pascal
case statut of
  0: WriteLn('Inactif');
  1: WriteLn('En attente');
  2: // Traitement en cours
    begin
      WriteLn('Traitement...');
      // code complexe ici
    end;
  3: WriteLn('Terminé');
end;
```

## Exemple récapitulatif complet

```pascal
program GestionRestaurant;
var
  categorie, plat: Integer;
  prix: Real;
begin
  WriteLn('=============================');
  WriteLn('  RESTAURANT LE BON PASCAL  ');
  WriteLn('=============================');
  WriteLn;
  WriteLn('Categories :');
  WriteLn('1. Entrees');
  WriteLn('2. Plats principaux');
  WriteLn('3. Desserts');
  WriteLn('4. Boissons');
  WriteLn;
  Write('Choisissez une categorie : ');
  ReadLn(categorie);
  WriteLn;
  case categorie of
    1:
      begin
        WriteLn('--- ENTREES ---');
        WriteLn('1. Salade mixte (5 euros)');
        WriteLn('2. Soupe du jour (4 euros)');
        WriteLn('3. Terrine (6 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1: prix := 5.0;
          2: prix := 4.0;
          3: prix := 6.0;
        else
          begin
            WriteLn('Plat inconnu !');
            prix := 0;
          end;
        end;
      end;
    2:
      begin
        WriteLn('--- PLATS PRINCIPAUX ---');
        WriteLn('1. Steak-frites (15 euros)');
        WriteLn('2. Poisson grille (18 euros)');
        WriteLn('3. Pates carbonara (12 euros)');
        WriteLn('4. Pizza margherita (10 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1: prix := 15.0;
          2: prix := 18.0;
          3: prix := 12.0;
          4: prix := 10.0;
        else
          begin
            WriteLn('Plat inconnu !');
            prix := 0;
          end;
        end;
      end;
    3:
      begin
        WriteLn('--- DESSERTS ---');
        WriteLn('1. Tarte aux pommes (5 euros)');
        WriteLn('2. Creme brulee (6 euros)');
        WriteLn('3. Mousse au chocolat (5 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1, 3: prix := 5.0;
          2: prix := 6.0;
        else
          begin
            WriteLn('Dessert inconnu !');
            prix := 0;
          end;
        end;
      end;
    4:
      begin
        WriteLn('--- BOISSONS ---');
        WriteLn('1. Eau (2 euros)');
        WriteLn('2. Jus (3 euros)');
        WriteLn('3. Soda (3 euros)');
        WriteLn('4. Cafe (2.5 euros)');
        Write('Votre choix : ');
        ReadLn(plat);
        case plat of
          1: prix := 2.0;
          2, 3: prix := 3.0;
          4: prix := 2.5;
        else
          begin
            WriteLn('Boisson inconnue !');
            prix := 0;
          end;
        end;
      end;
  else
    begin
      WriteLn('Categorie inconnue !');
      prix := 0;
    end;
  end;
  WriteLn;
  if prix > 0 then
  begin
    WriteLn('=============================');
    WriteLn('Prix : ', prix:0:2, ' euros');
    WriteLn('Merci pour votre commande !');
    WriteLn('=============================');
  end;
end.
```

## Résumé

L'instruction `case-of` est utilisée pour tester une variable contre plusieurs valeurs possibles :

- **Syntaxe** : `case expression of valeur1: instruction1; ... end;`
- **Types compatibles** : Integer, Char, Boolean, types énumérés
- **Types incompatibles** : String, Real
- **Intervalles** : utilisez `..` pour définir des plages (ex: `1..10`)
- **Virgules** : regroupez plusieurs valeurs (ex: `1, 3, 5`)
- **ELSE** : gérez les cas non prévus
- **BEGIN-END** : pour plusieurs instructions par cas
- Plus lisible que des `if-else` multiples pour tester une seule variable

L'instruction `case-of` rend votre code plus clair et plus facile à maintenir quand vous devez choisir parmi plusieurs options basées sur une valeur simple.

⏭️ [Boucles compteur (for-do)](/03-structures-controle/03-boucles-compteur-for-do.md)
