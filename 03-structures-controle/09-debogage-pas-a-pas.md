🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.9 Débogage pas à pas

## Introduction

Le débogage est l'art de trouver et corriger les erreurs (bugs) dans votre code. C'est une compétence essentielle pour tout programmeur. Au lieu de deviner où se trouve le problème, le débogage pas à pas vous permet d'**observer** votre programme en action, ligne par ligne, et de voir exactement ce qui se passe.

**Analogie :** C'est comme regarder un film au ralenti pour comprendre une action rapide. Ou comme suivre une recette étape par étape pour voir où vous vous êtes trompé.

**Citation célèbre :** "Le débogage, c'est comme être détective dans un film policier où vous êtes aussi le criminel." - Filipe Fortes

## Qu'est-ce qu'un bug ?

Un bug est une **erreur** dans votre programme qui provoque un comportement indésirable :

### Types de bugs

1. **Erreurs de syntaxe** : Le code ne compile pas
   ```pascal
   WriteLn('Bonjour')  // Oubli du point-virgule
   ```

2. **Erreurs d'exécution** : Le programme plante
   ```pascal
   resultat := 10 / 0;  // Division par zéro
   ```

3. **Erreurs logiques** : Le programme fonctionne mais donne des résultats incorrects
   ```pascal
   moyenne := (a + b + c) / 2;  // Devrait être / 3
   ```

Les erreurs logiques sont les plus difficiles à trouver car le programme ne plante pas !

## Le débogueur de Lazarus

Lazarus inclut un débogueur intégré puissant qui vous permet d'examiner votre programme pendant son exécution.

### Activation du débogueur

1. Ouvrez votre projet dans Lazarus
2. Menu **Exécuter** → **Options de débogage**
3. Assurez-vous que le débogage est activé

### Interface du débogueur

Quand vous déboguez, Lazarus affiche plusieurs fenêtres :

- **Fenêtre d'édition** : Votre code avec indicateur de ligne actuelle
- **Fenêtre Variables locales** : Valeurs des variables
- **Fenêtre Pile d'appels** : Historique des appels de fonctions
- **Fenêtre Points d'arrêt** : Liste de vos points d'arrêt
- **Fenêtre Console** : Sortie du programme

## Les points d'arrêt (Breakpoints)

Un point d'arrêt est un **marqueur** que vous placez sur une ligne de code. Quand le programme atteint cette ligne, il s'arrête et vous laisse examiner l'état du programme.

### Placer un point d'arrêt

**Méthode 1 : Clic dans la marge**
- Cliquez dans la marge grise à gauche du numéro de ligne
- Un cercle rouge apparaît

**Méthode 2 : Raccourci clavier**
- Placez le curseur sur la ligne
- Appuyez sur **F5** (ou Ctrl+F5 selon la configuration)

**Méthode 3 : Menu**
- Menu **Exécuter** → **Basculer point d'arrêt**

### Exemple de placement

```pascal
program ExempleBreakpoint;
var
  a, b, resultat: Integer;
begin
  a := 10;              // <- Placer un breakpoint ici
  b := 5;
  resultat := a + b;
  WriteLn('Résultat : ', resultat);
end.
```

Placez un point d'arrêt sur la ligne `a := 10;`. Quand vous exécuterez le programme en mode débogage, il s'arrêtera avant d'exécuter cette ligne.

### Supprimer un point d'arrêt

- Cliquez à nouveau sur le cercle rouge
- Ou appuyez sur **F5** sur la ligne

## Exécution pas à pas

Une fois arrêté à un point d'arrêt, vous pouvez avancer dans votre code de différentes manières :

### Commandes principales

| Commande | Raccourci | Action |
|----------|-----------|--------|
| **Exécuter** | F9 | Lance le programme en mode débogage |
| **Pause** | - | Met en pause le programme |
| **Pas à pas détaillé** | F7 | Entre dans les fonctions |
| **Pas à pas approfondi** | F8 | Passe par-dessus les fonctions |
| **Sortir** | Shift+F8 | Sort de la fonction actuelle |
| **Exécuter jusqu'au curseur** | F4 | Continue jusqu'à la ligne du curseur |
| **Arrêter** | Ctrl+F2 | Arrête le débogage |

### Pas à pas approfondi (Step Over - F8)

Exécute la ligne actuelle et passe à la suivante, **sans entrer** dans les fonctions.

```pascal
program StepOver;

procedure Afficher(x: Integer);
begin
  WriteLn('Valeur : ', x);
end;

begin
  WriteLn('Début');        // Point d'arrêt ici
  Afficher(10);           // F8 : exécute toute la procédure
  WriteLn('Fin');          // On arrive ici
end.
```

**Utilisation :** Quand vous voulez exécuter une ligne sans voir les détails des fonctions appelées.

### Pas à pas détaillé (Step Into - F7)

Entre **à l'intérieur** des fonctions et procédures.

```pascal
program StepInto;

procedure Afficher(x: Integer);
begin
  WriteLn('Valeur : ', x);  // F7 vous amène ici
end;

begin
  WriteLn('Début');        // Point d'arrêt ici
  Afficher(10);           // F7 : entre dans la procédure
  WriteLn('Fin');
end.
```

**Utilisation :** Quand vous voulez voir exactement ce qui se passe dans une fonction.

### Sortir (Step Out - Shift+F8)

Sort de la fonction actuelle et revient à l'appelant.

```pascal
procedure Calculer;
begin
  WriteLn('Calcul...');
  // Si vous êtes ici et faites Shift+F8
  WriteLn('Fin calcul');
end;

begin
  WriteLn('Début');
  Calculer();            // Vous revenez ici
  WriteLn('Fin');
  ReadLn;
end.
```

**Utilisation :** Quand vous avez fini d'examiner une fonction et voulez revenir rapidement.

## Inspection des variables

Pendant le débogage, vous pouvez voir les valeurs de toutes vos variables.

### Fenêtre Variables locales

Cette fenêtre affiche automatiquement toutes les variables de la fonction actuelle avec leurs valeurs.

```pascal
program InspectionVariables;
var
  a, b, somme: Integer;
  moyenne: Real;
begin
  a := 10;              // Point d'arrêt ici
  b := 20;
  somme := a + b;
  moyenne := somme / 2;
  WriteLn('Moyenne : ', moyenne:0:2);
end.
```

Après chaque ligne exécutée, regardez la fenêtre "Variables locales" :
- Après `a := 10` → `a = 10`, `b = ?` (non initialisé)
- Après `b := 20` → `a = 10`, `b = 20`
- Après `somme := a + b` → `somme = 30`

### Info-bulle sur survol

Passez votre souris sur une variable dans le code pour voir sa valeur actuelle.

```pascal
resultat := a + b;  // Survolez 'a', 'b' ou 'resultat'
```

### Ajouter une surveillance (Watch)

Pour surveiller une expression ou variable spécifique :

1. Menu **Débogage** → **Ajouter une surveillance**
2. Entrez le nom de la variable ou une expression
3. La valeur s'affiche dans la fenêtre "Surveillances"

**Exemples de surveillances :**
- `a` → valeur de a
- `a + b` → résultat de l'expression
- `tableau[i]` → valeur à l'indice i

### Modifier une variable pendant le débogage

Vous pouvez changer la valeur d'une variable pour tester différents scénarios :

1. Dans la fenêtre Variables locales
2. Double-cliquez sur la valeur
3. Entrez une nouvelle valeur
4. Continuez l'exécution

**Attention :** Utilisez cette fonctionnalité avec précaution !

## Exemple de débogage complet

### Programme avec bug

```pascal
program CalculMoyenneBug;
var
  note1, note2, note3: Integer;
  moyenne: Real;
begin
  WriteLn('Calcul de moyenne de 3 notes');

  Write('Note 1 : ');
  ReadLn(note1);
  Write('Note 2 : ');
  ReadLn(note2);
  Write('Note 3 : ');
  ReadLn(note3);

  // BUG : Division par 2 au lieu de 3 !
  moyenne := (note1 + note2 + note3) / 2;

  WriteLn('Moyenne : ', moyenne:0:2);
end.
```

### Processus de débogage

**Étape 1 : Identifier le symptôme**
- Le programme affiche une moyenne incorrecte
- Exemple : notes 10, 10, 10 → moyenne affichée : 15 (au lieu de 10)

**Étape 2 : Placer des points d'arrêt**
- Mettez un point d'arrêt sur la ligne du calcul de moyenne
- Mettez un point d'arrêt sur la ligne d'affichage

**Étape 3 : Exécuter en mode débogage (F9)**
- Entrez les valeurs de test : 10, 10, 10

**Étape 4 : Examiner les variables**
```
À l'arrêt sur le calcul :
- note1 = 10 ✓
- note2 = 10 ✓
- note3 = 10 ✓
- moyenne = ? (pas encore calculée)
```

**Étape 5 : Pas à pas (F8)**
- Exécutez la ligne du calcul
- Regardez la valeur de moyenne

```
Après le calcul :
- moyenne = 15.0 ✗ (devrait être 10.0)
```

**Étape 6 : Analyser le code**
```pascal
moyenne := (note1 + note2 + note3) / 2;  // Division par 2 !
```

**Étape 7 : Corriger**
```pascal
moyenne := (note1 + note2 + note3) / 3;  // Division par 3 ✓
```

**Étape 8 : Retester**
- Relancez le débogage
- Vérifiez que moyenne = 10.0 ✓

## Débogage de boucles

Les boucles peuvent être difficiles à déboguer. Voici comment procéder :

### Exemple : Recherche dans un tableau

```pascal
program DebogageBoucle;
const
  TAILLE = 5;
var
  tableau: array[1..TAILLE] of Integer;
  i, recherche: Integer;
  trouve: Boolean;
begin
  // Initialisation
  tableau[1] := 10;
  tableau[2] := 20;
  tableau[3] := 30;
  tableau[4] := 40;
  tableau[5] := 50;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  trouve := False;
  i := 1;

  // Point d'arrêt ici
  while (i <= TAILLE) and (not trouve) do
  begin
    if tableau[i] = recherche then
      trouve := True
    else
      i := i + 1;
  end;

  if trouve then
    WriteLn('Trouvé à l''indice ', i)
  else
    WriteLn('Non trouvé');
end.
```

### Débogage de la boucle

1. **Point d'arrêt** avant la boucle
2. **F8** pour entrer dans la boucle
3. **Surveillez** les variables : `i`, `trouve`, `tableau[i]`
4. À chaque itération, vérifiez :
   - `i` s'incrémente correctement
   - `tableau[i]` a la bonne valeur
   - La condition de sortie fonctionne

**Astuce :** Pour les longues boucles, placez un point d'arrêt **conditionnel** :
- Clic droit sur le point d'arrêt → Propriétés
- Ajoutez une condition : `i = 3` (s'arrête seulement quand i vaut 3)

## Débogage de fonctions et procédures

### Exemple avec appels multiples

```pascal
program DebogageFonctions;

function Carre(n: Integer): Integer;
begin
  Carre := n * n;  // Point d'arrêt ici
end;

function SommeCarres(a, b: Integer): Integer;
var
  carreA, carreB: Integer;
begin
  carreA := Carre(a);    // F7 pour entrer dans Carre
  carreB := Carre(b);    // F7 pour entrer à nouveau
  SommeCarres := carreA + carreB;
end;

begin
  WriteLn('Résultat : ', SommeCarres(3, 4));  // Point d'arrêt ici
end.
```

### Pile d'appels (Call Stack)

La fenêtre "Pile d'appels" montre la chaîne d'appels :

```
SommeCarres (ligne 12)
  ↑ appelé par
Programme principal (ligne 17)
```

**Utilité :** Comprendre comment vous êtes arrivé à une fonction.

### Navigation dans la pile

Double-cliquez sur un niveau de la pile pour voir le code à cet endroit avec ses variables locales.

## Techniques de débogage avancées

### Points d'arrêt conditionnels

Arrêter seulement si une condition est vraie.

**Exemple :** Tableau de 1000 éléments, vous voulez vous arrêter seulement quand `i = 500`

1. Placez un point d'arrêt dans la boucle
2. Clic droit → **Propriétés du point d'arrêt**
3. Condition : `i = 500`
4. Le programme ne s'arrêtera que quand i vaut 500

```pascal
for i := 1 to 1000 do
begin
  tableau[i] := i * 2;  // Breakpoint conditionnel : i = 500
end;
```

### Points d'arrêt avec actions

Au lieu de s'arrêter, le débogueur peut effectuer une action :

1. Clic droit → **Propriétés du point d'arrêt**
2. Action : **Afficher un message**
3. Message : `"i = {i}, valeur = {tableau[i]}"`
4. Cochez "Continuer après action"

Résultat : Le programme affiche les valeurs sans s'arrêter à chaque fois.

### Exécuter jusqu'au curseur (F4)

Placez le curseur sur une ligne et appuyez sur F4. Le programme s'exécutera jusqu'à cette ligne.

**Utilité :** Éviter de faire F8 vingt fois pour atteindre une ligne.

### Arrêt sur exception

Lazarus peut s'arrêter automatiquement quand une erreur se produit :

1. Menu **Outils** → **Options du débogueur**
2. Onglet **Exceptions**
3. Cochez les types d'exceptions à intercepter

## Stratégies de débogage

### Méthode 1 : Diviser pour régner

Si votre programme est long :

1. Placez des points d'arrêt à des endroits clés
2. Exécutez jusqu'au premier point d'arrêt
3. Vérifiez que tout est correct jusqu'ici
4. Continuez jusqu'au suivant
5. Localisez la section problématique
6. Déboguez en détail cette section

### Méthode 2 : Hypothèse et test

1. Formulez une hypothèse sur la cause du bug
2. Placez des points d'arrêt pour tester l'hypothèse
3. Examinez les valeurs
4. Confirmez ou infirmez l'hypothèse
5. Ajustez et recommencez

### Méthode 3 : Remonter depuis l'erreur

Si le programme plante :

1. Notez la ligne de l'erreur
2. Placez un point d'arrêt quelques lignes avant
3. Avancez ligne par ligne
4. Observez quelle ligne cause le problème

### Méthode 4 : Journalisation (Logging)

Ajoutez des WriteLn temporaires pour tracer l'exécution :

```pascal
WriteLn('DEBUG: Début de la fonction, a=', a, ', b=', b);
// Code à déboguer
WriteLn('DEBUG: Après calcul, resultat=', resultat);
```

**Avantage :** Fonctionne même sans débogueur  
**Inconvénient :** Il faut supprimer les lignes après  

## Exemples de débogage courants

### Bug 1 : Variable non initialisée

```pascal
program BugNonInitialisee;
var
  somme, i: Integer;
begin
  // BUG : somme n'est pas initialisée
  for i := 1 to 10 do
    somme := somme + i;  // somme a une valeur aléatoire au départ

  WriteLn('Somme : ', somme);
end.
```

**Débogage :**
1. Point d'arrêt sur le `for`
2. Regardez la valeur de `somme` → valeur bizarre (ex: 2845692)
3. **Solution :** `somme := 0;` avant la boucle

### Bug 2 : Indice de tableau incorrect

```pascal
program BugIndice;
var
  tableau: array[1..5] of Integer;
  i: Integer;
begin
  for i := 0 to 5 do  // BUG : commence à 0 au lieu de 1
    tableau[i] := i * 10;
end.
```

**Débogage :**
1. Point d'arrêt dans la boucle
2. Regardez `i` → vaut 0 à la première itération
3. `tableau[0]` n'existe pas ! (erreur d'exécution)
4. **Solution :** `for i := 1 to 5 do`

### Bug 3 : Condition inversée

```pascal
program BugCondition;
var
  age: Integer;
begin
  Write('Âge : ');
  ReadLn(age);

  // BUG : condition inversée
  if age < 18 then
    WriteLn('Vous êtes majeur')
  else
    WriteLn('Vous êtes mineur');
end.
```

**Débogage :**
1. Testez avec age = 20
2. Point d'arrêt sur le `if`
3. F8 pour voir quelle branche est prise
4. Branche "Vous êtes mineur" → condition incorrecte
5. **Solution :** `if age >= 18 then`

### Bug 4 : Boucle infinie

```pascal
program BugBoucleInfinie;
var
  i: Integer;
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn(i);
    // BUG : oubli d'incrémenter i
  end;
end.
```

**Débogage :**
1. Le programme ne se termine jamais
2. Pause (bouton pause ou Ctrl+Pause)
3. Regardez où le programme est arrêté → dans la boucle
4. Regardez `i` → vaut toujours 1
5. **Solution :** `i := i + 1;` dans la boucle

### Bug 5 : Erreur de calcul (division entière)

```pascal
program BugCalcul;
var
  a, b, resultat: Integer;
begin
  a := 5;
  b := 2;
  resultat := a div b;  // BUG : division entière perd la partie décimale
  WriteLn('Résultat : ', resultat);
end.
```

**Débogage :**
1. Point d'arrêt après le calcul
2. Regardez `resultat` → vaut 2 au lieu de 2.5
3. L'opérateur `div` effectue une division entière qui tronque le résultat
4. **Solution :** Déclarer `resultat: Real;` et utiliser `/` au lieu de `div`

## Outils complémentaires

### Affichage de la mémoire

Menu **Débogage** → **Voir la mémoire**

Permet de voir le contenu brut de la mémoire (avancé).

### Assembleur

Menu **Débogage** → **Afficher l'assembleur**

Montre le code machine généré (très avancé).

### Threads

Menu **Débogage** → **Threads**

Si vous utilisez plusieurs threads (multithreading).

## Conseils pratiques

### 1. Commencez simple

Ne déboguez pas tout le programme d'un coup. Isolez la partie problématique.

### 2. Utilisez des cas de test simples

Déboguez avec des valeurs simples et prévisibles :
- `a = 10, b = 5` plutôt que `a = 3847, b = 2193`

### 3. Prenez des notes

Notez vos observations :
- "À la ligne 25, i vaut 5 mais devrait valoir 6"
- "La fonction Calculer retourne 0 au lieu de 42"

### 4. Faites des pauses

Si vous êtes bloqué, faites une pause. Revenez avec un œil neuf.

### 5. Expliquez à quelqu'un (ou à un canard)

La "méthode du canard en plastique" : expliquez votre code ligne par ligne à voix haute. Souvent, vous trouverez l'erreur en expliquant.

### 6. Lisez les messages d'erreur

Les messages d'erreur donnent souvent l'emplacement exact du problème.

### 7. Vérifiez les hypothèses

Ne supposez pas que quelque chose fonctionne. Vérifiez.

### 8. Un bug à la fois

Ne corrigez pas plusieurs bugs en même temps. Réglez-les un par un.

### 9. Testez après chaque correction

Après avoir corrigé un bug, testez pour vous assurer que :
- Le bug est réglé
- Vous n'en avez pas créé de nouveaux

### 10. Utilisez le contrôle de version

Si possible, utilisez Git pour sauvegarder votre code avant de faire des modifications importantes.

## Erreurs courantes de débutants

### 1. Ne pas utiliser le débogueur

```
❌ "Je vais juste deviner où est le problème"
✓ "Je vais utiliser le débogueur pour voir exactement"
```

### 2. Trop de WriteLn de debug

```pascal
// ❌ Code rempli de WriteLn
WriteLn('DEBUG 1');
WriteLn('DEBUG a=', a);
WriteLn('DEBUG entré dans if');
// ... 50 autres WriteLn

// ✓ Utiliser le débogueur
```

### 3. Ne pas vérifier les valeurs

```
❌ "Cette variable doit valoir 10"
✓ "Je vérifie avec le débogueur : elle vaut 9"
```

### 4. Modifier le code au hasard

```
❌ Changer des choses au hasard en espérant que ça marche
✓ Comprendre le problème avant de modifier
```

### 5. Ignorer les avertissements

Les avertissements du compilateur signalent souvent des problèmes potentiels.

## Checklist de débogage

Avant d'abandonner, vérifiez :

- [ ] Les variables sont-elles initialisées ?
- [ ] Les indices de tableau sont-ils corrects ?
- [ ] Les conditions sont-elles dans le bon sens ?
- [ ] Les boucles s'arrêtent-elles correctement ?
- [ ] Les types de données sont-ils appropriés ?
- [ ] Les paramètres des fonctions sont-ils dans le bon ordre ?
- [ ] Les parenthèses sont-elles bien placées ?
- [ ] Avez-vous testé avec des valeurs limites ?
- [ ] Le code fait-il ce que vous pensez qu'il fait ?

## Résumé

Le débogage pas à pas est une compétence essentielle :

### Outils principaux
- **Points d'arrêt** (F5) : Marqueurs pour arrêter l'exécution
- **Pas à pas approfondi** (F8) : Ligne par ligne
- **Pas à pas détaillé** (F7) : Entre dans les fonctions
- **Exécution** (F9) : Lance en mode débogage
- **Variables locales** : Voir les valeurs

### Processus de débogage
1. **Identifier** le symptôme
2. **Localiser** la zone problématique
3. **Examiner** les valeurs avec le débogueur
4. **Comprendre** la cause
5. **Corriger** le code
6. **Tester** la correction

### Principes clés
- Le débogueur est votre meilleur ami
- Déboguez avec des cas simples
- Vérifiez vos hypothèses
- Un bug à la fois
- Ne devinez pas, observez

### Citation finale

"Tout le monde sait que déboguer est deux fois plus difficile que d'écrire un programme. Donc si vous êtes aussi malin que possible en l'écrivant, comment allez-vous le déboguer ?" - Brian Kernighan

**Morale :** Écrivez du code simple et clair. Plus votre code est complexe, plus il sera difficile à déboguer !

Le débogage n'est pas un signe de faiblesse, c'est une compétence professionnelle. Même les meilleurs programmeurs passent beaucoup de temps à déboguer. La différence, c'est qu'ils savent utiliser efficacement les outils de débogage !

⏭️ [Procédures et Fonctions](/04-procedures-fonctions/README.md)
