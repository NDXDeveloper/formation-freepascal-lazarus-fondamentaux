🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.8 Commentaires et documentation du code

## Qu'est-ce qu'un commentaire ?

Un **commentaire** est du texte dans votre code source qui est **ignoré par le compilateur**. C'est comme écrire des notes dans la marge d'un livre : elles aident à comprendre, mais ne font pas partie de l'histoire.

Les commentaires servent à :
- **Expliquer** ce que fait le code
- **Clarifier** les parties complexes
- **Documenter** les décisions importantes
- **Aider** les autres programmeurs (et vous-même dans 6 mois !)
- **Désactiver temporairement** du code pendant le développement

### Pourquoi commenter est essentiel

Imaginez que vous écrivez du code aujourd'hui. Dans 6 mois, vous devrez le modifier. Sans commentaires, vous passerez des heures à comprendre ce que vous aviez en tête. Avec de bons commentaires, vous comprendrez immédiatement !

**Code sans commentaire :**
```pascal
x := (a * 9 / 5) + 32;
```

**Code avec commentaire :**
```pascal
// Conversion Celsius vers Fahrenheit : F = (C × 9/5) + 32
fahrenheit := (celsius * 9 / 5) + 32;
```

Le second exemple est infiniment plus clair !

## Les différentes syntaxes de commentaires en Pascal

Pascal offre **trois façons** d'écrire des commentaires. Elles sont toutes valides et équivalentes.

### 1. Commentaire sur une ligne (//)

```pascal
// Ceci est un commentaire sur une seule ligne
WriteLn('Hello World');  // Commentaire en fin de ligne
```

**Caractéristiques :**
- Commence par `//`
- Va jusqu'à la fin de la ligne
- Le plus utilisé pour les commentaires courts
- Style moderne, venant du C++

**Exemples :**
```pascal
// Déclaration des variables
var
  age: integer;      // Âge de l'utilisateur
  nom: string;       // Nom complet

begin
  // Initialisation
  age := 25;
  nom := 'Alice';

  // Affichage des informations
  WriteLn('Nom : ', nom);
  WriteLn('Âge : ', age);
end.
```

### 2. Commentaire sur plusieurs lignes (* ... *)

```pascal
(* Ceci est un commentaire
   qui peut s'étendre
   sur plusieurs lignes *)
WriteLn('Hello World');
```

**Caractéristiques :**
- Commence par `(*` et se termine par `*)`
- Peut s'étendre sur plusieurs lignes
- Style traditionnel Pascal
- Recommandé pour les longs commentaires

**Exemple :**
```pascal
(*
  Programme : Calculatrice simple
  Auteur : Alice Dupont
  Date : 15/03/2024
  Description : Ce programme effectue des opérations
                arithmétiques de base sur deux nombres.
*)
program Calculatrice;
```

### 3. Commentaire avec accolades { ... }

```pascal
{ Ceci est un commentaire }
WriteLn('Hello World');
```

**Caractéristiques :**
- Commence par `{` et se termine par `}`
- Peut aussi s'étendre sur plusieurs lignes
- Traditionnel en Pascal
- Souvent utilisé pour les directives de compilation

**Exemple :**
```pascal
{
  Calcul de la moyenne de trois notes
  Entrées : trois notes entre 0 et 20
  Sortie : la moyenne sur 20
}
procedure CalculerMoyenne;
```

### Comparaison des trois styles

```pascal
program ExempleCommentaires;

// Style 1 : commentaire ligne simple

(* Style 2 : commentaire
   sur plusieurs lignes *)

{ Style 3 : commentaire
  avec accolades }

var
  x: integer;  // Variable de test

begin
  (* Initialisation *)
  x := 10;

  { Affichage }
  WriteLn(x);
end.
```

**Quel style choisir ?**

En pratique, la plupart des programmeurs Pascal modernes utilisent :
- `//` pour les commentaires courts (une ligne)
- `(* ... *)` ou `{ ... }` pour les commentaires longs (plusieurs lignes)

Choisissez un style et **restez cohérent** dans tout votre projet !

## Imbrication de commentaires

### Attention avec (* ... *)

Les commentaires `(* ... *)` **ne peuvent pas être imbriqués** :

```pascal
(* Commentaire externe
   (* Commentaire interne *)  // ERREUR !
   Fin du commentaire *)
```

### Solution : utiliser des styles différents

Si vous devez imbriquer des commentaires (par exemple, pour désactiver temporairement du code qui contient déjà des commentaires), mélangez les styles :

```pascal
{
  Ce bloc est désactivé temporairement
  (* Ce commentaire interne fonctionne *)
  WriteLn('Code désactivé');
}
```

## Quand commenter ?

### Ce qu'il FAUT commenter

#### 1. L'en-tête du fichier/programme

```pascal
(*
  Nom du fichier : GestionStock.pas
  Auteur : Marie Dupont
  Date de création : 15/03/2024
  Dernière modification : 20/03/2024

  Description :
    Programme de gestion de stock pour une petite entreprise.
    Permet d'ajouter, modifier et supprimer des articles.

  Dépendances : aucune
  Version : 1.2
*)
program GestionStock;
```

#### 2. Les sections importantes

```pascal
begin
  // ===== INITIALISATION =====
  compteur := 0;
  total := 0;

  // ===== TRAITEMENT DES DONNÉES =====
  for i := 1 to 10 do
  begin
    // Calcul du total
    total := total + valeurs[i];
  end;

  // ===== AFFICHAGE DES RÉSULTATS =====
  WriteLn('Total : ', total);
end.
```

#### 3. Les algorithmes complexes

```pascal
// Algorithme de tri à bulles
// Principe : on compare chaque paire d'éléments adjacents
// et on les échange s'ils sont dans le mauvais ordre
for i := 1 to n - 1 do  
begin  
  for j := 1 to n - i do
  begin
    if tableau[j] > tableau[j + 1] then
    begin
      // Échange des éléments
      temp := tableau[j];
      tableau[j] := tableau[j + 1];
      tableau[j + 1] := temp;
    end;
  end;
end;
```

#### 4. Les décisions importantes et le "pourquoi"

```pascal
// On utilise un facteur de 1.5 plutôt que 2.0 pour éviter
// de gaspiller trop de mémoire tout en gardant de bonnes performances
nouvelleTaille := ancienneTaille * 1.5;
```

#### 5. Les cas particuliers et les pièges

```pascal
// ATTENTION : cette fonction modifie le tableau en place !
procedure TrierTableau(var t: TTableau);

// BUG CONNU : ne fonctionne pas correctement avec des nombres négatifs
// TODO : corriger pour la version 2.0
resultat := CalculerRacine(nombre);

// ASTUCE : on ajoute 1 pour gérer le cas où le tableau est vide
taille := Length(tableau) + 1;
```

#### 6. Les paramètres et valeurs de retour des fonctions

```pascal
(*
  Calcule le prix TTC à partir du prix HT

  Paramètres :
    prixHT : prix hors taxes (en euros)
    tauxTVA : taux de TVA en pourcentage (ex: 20 pour 20%)

  Retour :
    Prix TTC incluant la TVA
*)
function CalculerPrixTTC(prixHT: real; tauxTVA: real): real;
```

#### 7. Les références externes

```pascal
// Source de l'algorithme :
// https://fr.wikipedia.org/wiki/Tri_à_bulles

// Formule de conversion :
// https://www.metric-conversions.org/temperature/celsius-to-fahrenheit.htm
fahrenheit := (celsius * 9 / 5) + 32;
```

### Ce qu'il NE FAUT PAS commenter

#### 1. Le code évident

**Mauvais :**
```pascal
// On ajoute 1 à i
i := i + 1;

// On affiche "Bonjour"
WriteLn('Bonjour');

// Déclaration d'une variable age de type integer
age: integer;
```

Ces commentaires ne font que répéter ce que le code dit déjà !

**Bon :**
```pascal
i := i + 1;  // Passage à l'élément suivant du tableau
```

#### 2. Le code auto-explicatif

Si votre code est clair, il n'a pas besoin de commentaire :

**Mauvais :**
```pascal
// On calcule la moyenne
m := (a + b + c) / 3;
```

**Bon :**
```pascal
moyenne := (note1 + note2 + note3) / 3;
```

Avec de bons noms de variables, pas besoin de commentaire !

#### 3. Le "comment" au lieu du "pourquoi"

**Mauvais :**
```pascal
// On multiplie par 1.2
prix := prixBase * 1.2;
```

**Bon :**
```pascal
// Application d'une marge de 20% sur le prix de base
prix := prixBase * 1.2;
```

## Les différents types de commentaires

### 1. Commentaires d'en-tête

Pour documenter un fichier, programme ou unité :

```pascal
(*
  ============================================
  Module : CalculsMathematiques
  ============================================

  Description :
    Collection de fonctions mathématiques
    utiles pour les calculs scientifiques.

  Auteur : Jean Martin
  Entreprise : Math Solutions SA
  Date : Mars 2024
  Version : 2.1

  Historique :
    v2.1 (20/03/24) : Ajout de la fonction racine cubique
    v2.0 (15/03/24) : Refonte complète du module
    v1.0 (01/03/24) : Version initiale

  Licence : MIT
*)
unit CalculsMathematiques;
```

### 2. Commentaires de fonction/procédure

```pascal
(*
  CalculerFactorielle

  Calcule la factorielle d'un nombre entier positif.
  Exemple : Factorielle(5) = 5 × 4 × 3 × 2 × 1 = 120

  @param n : Nombre dont on veut calculer la factorielle (n >= 0)
  @return : La factorielle de n
  @raises : Erreur si n est négatif

  Complexité : O(n)
  Utilise une approche récursive.
*)
function Factorielle(n: integer): integer;
```

### 3. Commentaires de section

```pascal
begin
  //===========================================
  // PHASE 1 : Initialisation des données
  //===========================================
  InitialiserTableaux;
  ChargerConfiguration;

  //===========================================
  // PHASE 2 : Traitement principal
  //===========================================
  TraiterDonnees;
  CalculerStatistiques;

  //===========================================
  // PHASE 3 : Sauvegarde et nettoyage
  //===========================================
  SauvegarderResultats;
  LibererMemoire;
end.
```

### 4. Commentaires en ligne

```pascal
var
  compteur: integer;        // Compte le nombre d'itérations
  total: real;              // Somme totale des valeurs
  estValide: boolean;       // Indique si la saisie est valide
  nomFichier: string;       // Chemin complet du fichier à traiter
```

### 5. Commentaires TODO et FIXME

```pascal
// TODO : Ajouter la validation des données d'entrée
// TODO : Implémenter la gestion des erreurs réseau
// FIXME : Bug avec les nombres négatifs - à corriger avant release
// HACK : Solution temporaire - à refactoriser proprement
// NOTE : Cette approche fonctionne mais pourrait être optimisée
// WARNING : Cette fonction est dépréciée, utiliser CalculerV2 à la place
```

Ces marqueurs spéciaux aident à retrouver rapidement ce qui reste à faire.

### 6. Commentaires de débogage

```pascal
begin
  // DEBUG : afficher les valeurs intermédiaires
  // WriteLn('x = ', x, ', y = ', y);

  resultat := x + y;

  // DEBUG : vérifier le résultat
  // if resultat < 0 then
  //   WriteLn('ATTENTION : résultat négatif !');
end.
```

## Exemples de documentation complète

### Exemple 1 : Petit programme simple

```pascal
(*
  Programme : Calculateur de moyenne
  Fichier : moyenne.pas
  Auteur : Alice Martin
  Date : 15/03/2024

  Description :
    Programme simple qui calcule la moyenne de trois notes
    saisies par l'utilisateur et affiche une appréciation.
*)
program CalculateurMoyenne;

var
  note1, note2, note3: real;      // Les trois notes à moyenner
  moyenne: real;                   // La moyenne calculée
  appreciation: string;            // L'appréciation textuelle

begin
  // ===== SAISIE DES DONNÉES =====
  WriteLn('=== CALCUL DE MOYENNE ===');
  WriteLn;

  Write('Note 1 : ');
  ReadLn(note1);

  Write('Note 2 : ');
  ReadLn(note2);

  Write('Note 3 : ');
  ReadLn(note3);

  // ===== CALCUL DE LA MOYENNE =====
  moyenne := (note1 + note2 + note3) / 3;

  // ===== DÉTERMINATION DE L'APPRÉCIATION =====
  // Barème standard français
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

  // ===== AFFICHAGE DES RÉSULTATS =====
  WriteLn;
  WriteLn('Moyenne : ', moyenne:0:2, '/20');
  WriteLn('Appréciation : ', appreciation);

  // Pause pour voir les résultats
  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Exemple 2 : Fonction bien documentée

```pascal
(*
  ConvertirTemperature

  Convertit une température entre différentes échelles.

  Paramètres :
    valeur : La température à convertir
    echelleSource : Échelle d'origine ('C', 'F' ou 'K')
    echelleCible : Échelle de destination ('C', 'F' ou 'K')

  Retour :
    La température convertie dans l'échelle cible

  Exceptions :
    Lève une erreur si l'échelle source ou cible est invalide

  Exemples :
    ConvertirTemperature(100, 'C', 'F') -> 212.0  (eau bouillante)
    ConvertirTemperature(32, 'F', 'C') -> 0.0     (eau glacée)
    ConvertirTemperature(273.15, 'K', 'C') -> 0.0 (zéro absolu)

  Note :
    Les échelles supportées sont :
    - C : Celsius
    - F : Fahrenheit
    - K : Kelvin
*)
function ConvertirTemperature(valeur: real;
                              echelleSource: char;
                              echelleCible: char): real;
var
  temperatureCelsius: real;  // Température intermédiaire en Celsius
begin
  // Étape 1 : Convertir la source vers Celsius (format pivot)
  case echelleSource of
    'C': temperatureCelsius := valeur;
    'F': temperatureCelsius := (valeur - 32) * 5 / 9;
    'K': temperatureCelsius := valeur - 273.15;
  else
    raise Exception.Create('Échelle source invalide');
  end;

  // Étape 2 : Convertir de Celsius vers la cible
  case echelleCible of
    'C': Result := temperatureCelsius;
    'F': Result := (temperatureCelsius * 9 / 5) + 32;
    'K': Result := temperatureCelsius + 273.15;
  else
    raise Exception.Create('Échelle cible invalide');
  end;
end;
```

### Exemple 3 : Module avec documentation complète

```pascal
(*
  ================================================
  Unité : GestionTableaux
  ================================================

  Description :
    Bibliothèque de fonctions pour manipuler des
    tableaux d'entiers : tri, recherche, statistiques.

  Auteur : Équipe Dev
  Version : 1.0
  Date : Mars 2024

  Dépendances : SysUtils

  Fonctions principales :
    - TrierTableau : Tri croissant d'un tableau
    - RechercherValeur : Recherche d'une valeur
    - CalculerMoyenne : Moyenne des éléments
    - TrouverMinMax : Trouve le min et le max
*)
unit GestionTableaux;

interface

type
  TTableauEntiers = array of integer;

// Tri du tableau par ordre croissant
procedure TrierTableau(var t: TTableauEntiers);

// Recherche une valeur dans le tableau (retourne l'index ou -1)
function RechercherValeur(const t: TTableauEntiers; valeur: integer): integer;

// Calcule la moyenne des éléments
function CalculerMoyenne(const t: TTableauEntiers): real;

implementation

(*
  TrierTableau

  Trie un tableau d'entiers par ordre croissant en utilisant
  l'algorithme de tri à bulles.

  @param t : Le tableau à trier (modifié en place)

  Complexité : O(n²) - Convient pour de petits tableaux

  Note : Pour de gros tableaux, préférer QuickSort
*)
procedure TrierTableau(var t: TTableauEntiers);  
var  
  i, j, temp: integer;
  n: integer;
begin
  n := Length(t);

  // Tri à bulles : on fait remonter le plus grand élément
  for i := 0 to n - 2 do
  begin
    for j := 0 to n - i - 2 do
    begin
      // Si l'élément actuel est plus grand que le suivant
      if t[j] > t[j + 1] then
      begin
        // Échange des deux éléments
        temp := t[j];
        t[j] := t[j + 1];
        t[j + 1] := temp;
      end;
    end;
  end;
end;

// Suite de l'implémentation...

end.
```

## Bonnes pratiques de commentaires

### 1. Écrivez des commentaires clairs et concis

**Mauvais :**
```pascal
// Cette fonction prend deux nombres et elle fait la somme de ces deux nombres
// et ensuite elle retourne le résultat de cette somme
function Additionner(a, b: integer): integer;
```

**Bon :**
```pascal
// Retourne la somme de deux entiers
function Additionner(a, b: integer): integer;
```

### 2. Mettez à jour les commentaires avec le code

Si vous modifiez du code, **mettez à jour les commentaires** !

**Problème :**
```pascal
// Calcule le prix avec 10% de remise
prix := prixBase * 0.80;  // Code modifié mais pas le commentaire !
```

**Solution :**
```pascal
// Calcule le prix avec 20% de remise
prix := prixBase * 0.80;
```

### 3. Utilisez un langage correct

Évitez l'argot, les fautes d'orthographe, et restez professionnel :

**Mauvais :**
```pascal
// Ici on check si c bon
// sinn on fait un truc
```

**Bon :**
```pascal
// Vérifie si les données sont valides
// Sinon, affiche un message d'erreur
```

### 4. Commentez le "pourquoi", pas le "comment"

**Mauvais :**
```pascal
// On multiplie i par 2
i := i * 2;
```

**Bon :**
```pascal
// On double la taille du buffer pour améliorer les performances
tailleBuffer := tailleBuffer * 2;
```

### 5. Utilisez des séparateurs visuels

Pour les grandes sections :

```pascal
//===============================================
//  SECTION PRINCIPALE
//===============================================

//-----------------------------------------------
// Sous-section
//-----------------------------------------------

// Commentaire normal
```

### 6. Groupez les commentaires connexes

**Mauvais :**
```pascal
var
  nom: string;  // Nom de l'utilisateur
  prenom: string;  // Prénom de l'utilisateur
  age: integer;  // Âge de l'utilisateur
  ville: string;  // Ville de résidence
```

**Bon :**
```pascal
var
  // Informations personnelles de l'utilisateur
  nom, prenom: string;
  age: integer;
  ville: string;
```

### 7. Évitez les commentaires redondants

**Mauvais :**
```pascal
// Début du programme
begin
  // Affiche un message
  WriteLn('Bonjour');
// Fin du programme
end.
```

## Commentaires pour désactiver du code

Pendant le développement, vous pouvez temporairement désactiver du code :

```pascal
begin
  WriteLn('Cette partie fonctionne');

  // Code désactivé pour les tests
  (*
  CalculerStatistiques;
  AfficherGraphique;
  SauvegarderDonnees;
  *)

  WriteLn('On saute la partie du milieu');
end.
```

**Attention :** N'oubliez pas de nettoyer le code mort avant la version finale !

## Erreurs courantes à éviter

### 1. Commentaires obsolètes

```pascal
// Calcule le prix avec 10% de remise
prix := prixBase * 0.75;  // En réalité : 25% de remise !
```

### 2. Trop de commentaires

```pascal
// Déclare une variable i
var
  i: integer;  // Variable i de type integer
// Début du programme
begin
  // On met 0 dans i
  i := 0;  // i vaut maintenant 0
  // On affiche i
  WriteLn(i);  // Affiche la valeur de i
end.  // Fin du programme
```

C'est illisible ! Le code se noie dans les commentaires.

### 3. Pas assez de commentaires

```pascal
var
  x, y, z: integer;
  k: real;
begin
  x := 10;
  y := x * 2;
  z := y div 3;
  k := z * 1.5;
  WriteLn(k);
end.
```

Impossible de comprendre ce que fait ce code !

### 4. Commentaires vagues

```pascal
// Fait des trucs
procedure Traiter;

// Calcule
resultat := Calculer(x, y, z);
```

### 5. Commentaires mensongers

```pascal
// Trie par ordre décroissant
procedure TrierCroissant(var t: array of integer);  // En fait croissant !
```

## Récapitulatif

**Les trois syntaxes :**
- `//` : commentaire sur une ligne
- `(* ... *)` : commentaire multi-lignes
- `{ ... }` : commentaire multi-lignes (alternatif)

**Ce qu'il faut commenter :**
- En-têtes de fichiers et fonctions
- Algorithmes complexes
- Décisions importantes (le "pourquoi")
- Cas particuliers et pièges
- TODO et FIXME

**Ce qu'il ne faut pas commenter :**
- Code évident
- Ce que le code dit déjà
- Répéter les noms de variables

**Bonnes pratiques :**
- Être clair et concis
- Mettre à jour avec le code
- Commenter le "pourquoi", pas le "comment"
- Utiliser un langage correct
- Rester cohérent dans le style

**Règle d'or :** Un bon code a besoin de peu de commentaires. Des commentaires sont nécessaires quand le code seul ne suffit pas à expliquer l'intention. Visez l'équilibre !

---

**Point clé :** Les commentaires sont pour les humains, le code est pour les machines. Un code bien commenté est un code qui sera encore compréhensible et maintenable dans plusieurs années. Prenez l'habitude de commenter dès le début, c'est un investissement qui vous fera gagner énormément de temps !

⏭️ [Conventions de nommage](/02-introduction-langage-pascal/09-conventions-nommage.md)
