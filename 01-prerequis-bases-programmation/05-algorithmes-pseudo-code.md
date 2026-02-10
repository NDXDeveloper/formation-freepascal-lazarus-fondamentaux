🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.5 Algorithmes et pseudo-code

## Introduction

Avant d'écrire du code dans un langage de programmation, il est essentiel de réfléchir à la logique de résolution du problème. C'est là qu'interviennent les algorithmes et le pseudo-code, des outils fondamentaux pour tout programmeur.

## Qu'est-ce qu'un algorithme ?

### Définition

Un **algorithme** est une suite finie et ordonnée d'instructions claires et précises permettant de résoudre un problème ou d'accomplir une tâche.

**Caractéristiques essentielles :**
- **Fini** : l'algorithme doit se terminer après un nombre fini d'étapes
- **Défini** : chaque étape doit être précise et sans ambiguïté
- **Efficace** : chaque étape doit être réalisable
- **Entrées** : données nécessaires au début
- **Sorties** : résultats produits à la fin

### Les algorithmes dans la vie quotidienne

Nous utilisons des algorithmes tous les jours sans nous en rendre compte :

**Algorithme pour faire du café :**
```
1. Remplir le réservoir d'eau
2. Mettre un filtre dans le porte-filtre
3. Ajouter le café moulu dans le filtre
4. Placer la cafetière sous le porte-filtre
5. Allumer la machine
6. Attendre que le café coule
7. Éteindre la machine
8. Servir le café
```

**Algorithme pour se rendre au travail :**
```
1. Sortir de chez soi
2. Marcher jusqu'à l'arrêt de bus
3. Attendre le bus n°12
4. Monter dans le bus
5. Descendre à l'arrêt "Centre-ville"
6. Marcher jusqu'au bureau
```

### Les algorithmes en informatique

En programmation, les algorithmes décrivent comment transformer des données d'entrée en résultats désirés.

**Exemple : Calculer la moyenne de trois notes**
```
Entrées : note1, note2, note3  
Traitement : moyenne = (note1 + note2 + note3) / 3  
Sortie : moyenne  
```

## Le pseudo-code

### Qu'est-ce que le pseudo-code ?

Le **pseudo-code** est une façon d'écrire un algorithme en utilisant un langage informel, à mi-chemin entre le langage naturel et un langage de programmation.

**Avantages du pseudo-code :**
- Indépendant du langage de programmation
- Plus facile à lire qu'un code réel
- Permet de se concentrer sur la logique, pas sur la syntaxe
- Facilite la communication entre développeurs
- Sert de documentation

**Inconvénients :**
- Ne peut pas être exécuté directement par un ordinateur
- Pas de syntaxe standardisée (varie selon les auteurs)

### Pourquoi utiliser le pseudo-code ?

**Avant de programmer :**
- Clarifier votre pensée
- Détecter les erreurs de logique tôt
- Planifier la structure du programme

**Pour communiquer :**
- Expliquer votre solution à d'autres
- Documenter votre approche
- Discuter d'améliorations possibles

## Conventions du pseudo-code

Il n'existe pas de standard unique, mais voici des conventions courantes :

### Instructions de base

**Affectation (donner une valeur à une variable) :**
```
variable ← valeur  
nombre ← 10  
nom ← "Alice"  
```

**Affichage (montrer quelque chose à l'utilisateur) :**
```
AFFICHER "message"  
AFFICHER variable  
AFFICHER "La somme est : ", somme  
```

**Saisie (demander une information à l'utilisateur) :**
```
LIRE variable  
SAISIR age  
DEMANDER nom  
```

**Commentaires :**
```
// Ceci est un commentaire sur une ligne
/* Ceci est un commentaire
   sur plusieurs lignes */
```

### Structure de sélection (condition)

**SI ... ALORS ... SINON**
```
SI condition ALORS
    instructions si vrai
SINON
    instructions si faux
FIN SI
```

**Exemple :**
```
SI age >= 18 ALORS
    AFFICHER "Vous êtes majeur"
SINON
    AFFICHER "Vous êtes mineur"
FIN SI
```

**Sélection multiple (choix) :**
```
SELON variable
    CAS valeur1 :
        instructions
    CAS valeur2 :
        instructions
    DÉFAUT :
        instructions
FIN SELON
```

### Structures de répétition (boucles)

**POUR (nombre d'itérations connu) :**
```
POUR variable DE début À fin FAIRE
    instructions
FIN POUR
```

**Exemple :**
```
POUR i DE 1 À 10 FAIRE
    AFFICHER i
FIN POUR
```

**TANT QUE (condition en début) :**
```
TANT QUE condition FAIRE
    instructions
FIN TANT QUE
```

**Exemple :**
```
compteur ← 1  
TANT QUE compteur <= 10 FAIRE  
    AFFICHER compteur
    compteur ← compteur + 1
FIN TANT QUE
```

**RÉPÉTER ... JUSQU'À (condition en fin) :**
```
RÉPÉTER
    instructions
JUSQU'À condition
```

**Exemple :**
```
RÉPÉTER
    AFFICHER "Entrez un nombre positif : "
    LIRE nombre
JUSQU'À nombre > 0
```

## Exemples d'algorithmes simples

### Exemple 1 : Calculer l'aire d'un rectangle

**Problème :** Calculer l'aire d'un rectangle connaissant sa longueur et sa largeur.

**Pseudo-code :**
```
ALGORITHME AireRectangle

VARIABLES
    longueur : réel
    largeur : réel
    aire : réel

DÉBUT
    AFFICHER "Entrez la longueur : "
    LIRE longueur

    AFFICHER "Entrez la largeur : "
    LIRE largeur

    aire ← longueur × largeur

    AFFICHER "L'aire du rectangle est : ", aire
FIN
```

**En Pascal :**
```pascal
program AireRectangle;  
var  
  longueur, largeur, aire: Real;
begin
  WriteLn('Entrez la longueur : ');
  ReadLn(longueur);

  WriteLn('Entrez la largeur : ');
  ReadLn(largeur);

  aire := longueur * largeur;

  WriteLn('L''aire du rectangle est : ', aire:0:2);
end.
```

### Exemple 2 : Déterminer si un nombre est pair ou impair

**Problème :** Vérifier si un nombre entier est pair ou impair.

**Pseudo-code :**
```
ALGORITHME PairOuImpair

VARIABLES
    nombre : entier

DÉBUT
    AFFICHER "Entrez un nombre entier : "
    LIRE nombre

    SI (nombre modulo 2) = 0 ALORS
        AFFICHER nombre, " est pair"
    SINON
        AFFICHER nombre, " est impair"
    FIN SI
FIN
```

**En Pascal :**
```pascal
program PairOuImpair;  
var  
  nombre: Integer;
begin
  WriteLn('Entrez un nombre entier : ');
  ReadLn(nombre);

  if (nombre mod 2) = 0 then
    WriteLn(nombre, ' est pair')
  else
    WriteLn(nombre, ' est impair');
end.
```

### Exemple 3 : Calculer la somme des N premiers entiers

**Problème :** Calculer 1 + 2 + 3 + ... + N

**Pseudo-code (version 1 - avec boucle) :**
```
ALGORITHME SommeEntiers

VARIABLES
    N : entier
    somme : entier
    i : entier

DÉBUT
    AFFICHER "Entrez N : "
    LIRE N

    somme ← 0

    POUR i DE 1 À N FAIRE
        somme ← somme + i
    FIN POUR

    AFFICHER "La somme est : ", somme
FIN
```

**Pseudo-code (version 2 - avec formule mathématique) :**
```
ALGORITHME SommeEntiersRapide

VARIABLES
    N : entier
    somme : entier

DÉBUT
    AFFICHER "Entrez N : "
    LIRE N

    somme ← N × (N + 1) / 2

    AFFICHER "La somme est : ", somme
FIN
```

**Observation :** La version 2 est plus efficace car elle n'utilise pas de boucle !

### Exemple 4 : Trouver le maximum de trois nombres

**Problème :** Déterminer le plus grand de trois nombres.

**Pseudo-code :**
```
ALGORITHME MaximumTroisNombres

VARIABLES
    a, b, c : réel
    maximum : réel

DÉBUT
    AFFICHER "Entrez le premier nombre : "
    LIRE a

    AFFICHER "Entrez le deuxième nombre : "
    LIRE b

    AFFICHER "Entrez le troisième nombre : "
    LIRE c

    // Supposer que a est le maximum
    maximum ← a

    // Vérifier si b est plus grand
    SI b > maximum ALORS
        maximum ← b
    FIN SI

    // Vérifier si c est plus grand
    SI c > maximum ALORS
        maximum ← c
    FIN SI

    AFFICHER "Le maximum est : ", maximum
FIN
```

### Exemple 5 : Table de multiplication

**Problème :** Afficher la table de multiplication d'un nombre.

**Pseudo-code :**
```
ALGORITHME TableMultiplication

VARIABLES
    nombre : entier
    i : entier
    resultat : entier

DÉBUT
    AFFICHER "Table de multiplication de quel nombre ? "
    LIRE nombre

    AFFICHER "Table de ", nombre, " :"
    AFFICHER "-------------------"

    POUR i DE 1 À 10 FAIRE
        resultat ← nombre × i
        AFFICHER nombre, " × ", i, " = ", resultat
    FIN POUR
FIN
```

## Algorithmes avec validation d'entrée

### Exemple : Demander un nombre positif

**Problème :** Continuer à demander un nombre jusqu'à ce qu'il soit positif.

**Pseudo-code :**
```
ALGORITHME DemanderNombrePositif

VARIABLES
    nombre : entier

DÉBUT
    RÉPÉTER
        AFFICHER "Entrez un nombre positif : "
        LIRE nombre

        SI nombre <= 0 ALORS
            AFFICHER "Erreur ! Le nombre doit être positif."
        FIN SI

    JUSQU'À nombre > 0

    AFFICHER "Merci, vous avez entré : ", nombre
FIN
```

### Exemple : Menu avec choix

**Pseudo-code :**
```
ALGORITHME MenuPrincipal

VARIABLES
    choix : entier

DÉBUT
    RÉPÉTER
        AFFICHER "=== MENU PRINCIPAL ==="
        AFFICHER "1. Option 1"
        AFFICHER "2. Option 2"
        AFFICHER "3. Option 3"
        AFFICHER "0. Quitter"
        AFFICHER "Votre choix : "
        LIRE choix

        SELON choix
            CAS 1 :
                AFFICHER "Vous avez choisi l'option 1"
            CAS 2 :
                AFFICHER "Vous avez choisi l'option 2"
            CAS 3 :
                AFFICHER "Vous avez choisi l'option 3"
            CAS 0 :
                AFFICHER "Au revoir !"
            DÉFAUT :
                AFFICHER "Choix invalide !"
        FIN SELON

    JUSQU'À choix = 0
FIN
```

## Algorithmes de recherche

### Recherche linéaire

**Problème :** Trouver si un élément existe dans un tableau.

**Pseudo-code :**
```
ALGORITHME RechercheLineaire

VARIABLES
    tableau : tableau de N entiers
    valeurCherchee : entier
    i : entier
    trouve : booléen
    position : entier

DÉBUT
    // Supposer que le tableau est déjà rempli

    AFFICHER "Quelle valeur cherchez-vous ? "
    LIRE valeurCherchee

    trouve ← FAUX
    position ← -1

    POUR i DE 0 À N-1 FAIRE
        SI tableau[i] = valeurCherchee ALORS
            trouve ← VRAI
            position ← i
            SORTIR DE LA BOUCLE
        FIN SI
    FIN POUR

    SI trouve ALORS
        AFFICHER "Valeur trouvée à la position ", position
    SINON
        AFFICHER "Valeur non trouvée"
    FIN SI
FIN
```

## Algorithmes de tri (introduction)

### Tri par sélection (principe simplifié)

**Problème :** Trier un tableau de nombres par ordre croissant.

**Pseudo-code simplifié :**
```
ALGORITHME TriSelection

VARIABLES
    tableau : tableau de N entiers
    i, j : entier
    minIndex : entier
    temp : entier

DÉBUT
    // Pour chaque position
    POUR i DE 0 À N-2 FAIRE
        // Trouver le minimum dans la partie non triée
        minIndex ← i

        POUR j DE i+1 À N-1 FAIRE
            SI tableau[j] < tableau[minIndex] ALORS
                minIndex ← j
            FIN SI
        FIN POUR

        // Échanger les éléments
        SI minIndex ≠ i ALORS
            temp ← tableau[i]
            tableau[i] ← tableau[minIndex]
            tableau[minIndex] ← temp
        FIN SI
    FIN POUR

    AFFICHER "Tableau trié !"
FIN
```

## Décomposition en sous-problèmes

### Utilisation de procédures et fonctions

Pour les algorithmes complexes, on les décompose en parties plus petites.

**Exemple : Calculatrice simple**

**Pseudo-code :**
```
FONCTION Addition(a, b : réel) : réel  
DÉBUT  
    RETOURNER a + b
FIN

FONCTION Soustraction(a, b : réel) : réel  
DÉBUT  
    RETOURNER a - b
FIN

FONCTION Multiplication(a, b : réel) : réel  
DÉBUT  
    RETOURNER a × b
FIN

FONCTION Division(a, b : réel) : réel  
DÉBUT  
    SI b = 0 ALORS
        AFFICHER "Erreur : division par zéro"
        RETOURNER 0
    SINON
        RETOURNER a / b
    FIN SI
FIN

ALGORITHME CalculatriceSimple

VARIABLES
    a, b : réel
    operation : caractère
    resultat : réel

DÉBUT
    AFFICHER "Entrez le premier nombre : "
    LIRE a

    AFFICHER "Entrez l'opération (+, -, *, /) : "
    LIRE operation

    AFFICHER "Entrez le second nombre : "
    LIRE b

    SELON operation
        CAS '+' :
            resultat ← Addition(a, b)
        CAS '-' :
            resultat ← Soustraction(a, b)
        CAS '*' :
            resultat ← Multiplication(a, b)
        CAS '/' :
            resultat ← Division(a, b)
        DÉFAUT :
            AFFICHER "Opération inconnue"
            RETOURNER
    FIN SELON

    AFFICHER "Résultat : ", resultat
FIN
```

## Analyse d'algorithmes (notions de base)

### Efficacité algorithmique

Tous les algorithmes ne se valent pas. Certains sont plus rapides que d'autres.

**Critères d'évaluation :**
- **Temps d'exécution** : combien de temps prend l'algorithme ?
- **Utilisation mémoire** : combien d'espace mémoire nécessite-t-il ?
- **Simplicité** : est-il facile à comprendre et à maintenir ?

### Compter les opérations

**Exemple 1 : Algorithme linéaire**
```
POUR i DE 1 À N FAIRE
    AFFICHER i
FIN POUR
```
Nombre d'opérations : N (proportionnel à la taille de l'entrée)

**Exemple 2 : Algorithme quadratique**
```
POUR i DE 1 À N FAIRE
    POUR j DE 1 À N FAIRE
        AFFICHER i, j
    FIN POUR
FIN POUR
```
Nombre d'opérations : N × N = N² (croît rapidement)

**Exemple 3 : Algorithme constant**
```
resultat ← N × (N + 1) / 2
```
Nombre d'opérations : quelques opérations seulement (indépendant de N)

### Notation Big O (introduction)

C'est une notation mathématique pour décrire l'efficacité :

- **O(1)** : Constant - toujours le même temps
- **O(log N)** : Logarithmique - très efficace
- **O(N)** : Linéaire - proportionnel à la taille
- **O(N log N)** : Quasi-linéaire - assez efficace
- **O(N²)** : Quadratique - moins efficace pour grandes données
- **O(2^N)** : Exponentiel - très inefficace

**Exemple pratique :**
- Recherche dans un tableau non trié : O(N)
- Tri par sélection : O(N²)
- Calcul mathématique direct : O(1)

## Du pseudo-code au code Pascal

### Correspondances principales

| Pseudo-code | Pascal |
|-------------|--------|
| `variable ← valeur` | `variable := valeur;` |
| `AFFICHER` | `WriteLn()` |
| `LIRE` | `ReadLn()` |
| `SI ... ALORS ... SINON` | `if ... then ... else` |
| `POUR i DE 1 À N` | `for i := 1 to N do` |
| `TANT QUE condition` | `while condition do` |
| `RÉPÉTER ... JUSQU'À` | `repeat ... until` |
| `SELON variable` | `case variable of` |
| `FONCTION/PROCÉDURE` | `function/procedure` |

### Exemple de traduction complète

**Pseudo-code :**
```
ALGORITHME Factorielle

FONCTION Fact(n : entier) : entier  
DÉBUT  
    SI n <= 1 ALORS
        RETOURNER 1
    SINON
        RETOURNER n × Fact(n - 1)
    FIN SI
FIN

DÉBUT PRINCIPAL
    VARIABLES
        nombre : entier
        resultat : entier

    AFFICHER "Entrez un nombre : "
    LIRE nombre

    resultat ← Fact(nombre)

    AFFICHER "Factorielle de ", nombre, " = ", resultat
FIN
```

**Code Pascal :**
```pascal
program Factorielle;

function Fact(n: Integer): Integer;  
begin  
  if n <= 1 then
    Fact := 1
  else
    Fact := n * Fact(n - 1);
end;

var
  nombre, resultat: Integer;

begin
  WriteLn('Entrez un nombre : ');
  ReadLn(nombre);

  resultat := Fact(nombre);

  WriteLn('Factorielle de ', nombre, ' = ', resultat);
end.
```

## Bonnes pratiques pour écrire des algorithmes

### 1. Commencez simple

Ne cherchez pas la solution parfaite immédiatement. Écrivez d'abord une version qui fonctionne, puis optimisez.

### 2. Décomposez le problème

Divisez les problèmes complexes en sous-problèmes plus simples.

```
Problème : Gérer une bibliothèque

Sous-problèmes :
- Ajouter un livre
- Rechercher un livre
- Emprunter un livre
- Retourner un livre
- Afficher la liste des livres
```

### 3. Utilisez des noms explicites

```
// Mauvais
POUR i DE 1 À n FAIRE
    x ← x + y[i]
FIN POUR

// Bon
POUR indiceLivre DE 1 À nombreLivres FAIRE
    prixTotal ← prixTotal + prixLivres[indiceLivre]
FIN POUR
```

### 4. Commentez votre algorithme

```
// Calculer la moyenne des notes positives uniquement
somme ← 0  
compteur ← 0  

POUR chaque note FAIRE
    SI note > 0 ALORS
        somme ← somme + note
        compteur ← compteur + 1
    FIN SI
FIN POUR

SI compteur > 0 ALORS
    moyenne ← somme / compteur
FIN SI
```

### 5. Testez mentalement votre algorithme

Suivez l'algorithme étape par étape avec des valeurs d'exemple pour vérifier qu'il fonctionne.

**Exemple : Vérifier si un nombre est premier**
```
Entrée : n = 7

i = 2 : 7 mod 2 = 1 (différent de 0, continuer)  
i = 3 : 7 mod 3 = 1 (différent de 0, continuer)  
i = 4 : 7 mod 4 = 3 (différent de 0, continuer)  
i = 5 : 7 mod 5 = 2 (différent de 0, continuer)  
i = 6 : 7 mod 6 = 1 (différent de 0, continuer)  

Aucun diviseur trouvé → 7 est premier ✓
```

### 6. Gérez les cas limites

N'oubliez pas les cas particuliers :
- Tableau vide
- Valeur nulle
- Nombre négatif
- Division par zéro

```
ALGORITHME RechercheMinimum

SI tableau est vide ALORS
    AFFICHER "Erreur : tableau vide"
    RETOURNER
FIN SI

minimum ← tableau[0]  
POUR i DE 1 À longueur-1 FAIRE  
    SI tableau[i] < minimum ALORS
        minimum ← tableau[i]
    FIN SI
FIN POUR
```

## Erreurs courantes à éviter

### 1. Boucles infinies

```
// ERREUR : la condition ne devient jamais fausse
i ← 1  
TANT QUE i > 0 FAIRE  
    AFFICHER i
    i ← i + 1  // i augmente, ne diminue jamais !
FIN TANT QUE
```

### 2. Indices de tableau incorrects

```
// ERREUR : dépassement de tableau
tableau de taille 10 (indices 0 à 9)  
POUR i DE 0 À 10 FAIRE  // i = 10 est hors limites !  
    AFFICHER tableau[i]
FIN POUR
```

### 3. Variables non initialisées

```
// ERREUR : somme n'est pas initialisée
POUR i DE 1 À 10 FAIRE
    somme ← somme + i  // Que vaut somme au début ?
FIN POUR

// CORRECT
somme ← 0  // Initialisation  
POUR i DE 1 À 10 FAIRE  
    somme ← somme + i
FIN POUR
```

### 4. Confusion entre affectation et comparaison

```
// ERREUR : utilisation de = au lieu de ←
SI x = 5 ALORS  // Comparaison (correct)
    x = 10      // ERREUR ! Devrait être x ← 10
FIN SI
```

## Conclusion

Le pseudo-code et les algorithmes sont des outils essentiels pour tout programmeur. Ils vous permettent de :
- Réfléchir à la logique avant de coder
- Communiquer vos idées clairement
- Détecter les erreurs tôt dans le processus
- Créer des solutions efficaces

**Points clés à retenir :**
- Un algorithme est une suite d'instructions pour résoudre un problème
- Le pseudo-code est une description informelle d'un algorithme
- Utilisez des structures de base : séquence, sélection, répétition
- Décomposez les problèmes complexes en sous-problèmes
- Testez mentalement vos algorithmes avant de les coder
- L'efficacité compte : certains algorithmes sont meilleurs que d'autres
- La clarté est importante : écrivez pour être compris

**Prochaines étapes :**
Dans la section suivante, nous apprendrons à représenter visuellement ces algorithmes avec les organigrammes, un autre outil précieux pour la conception de programmes.

**Conseil pratique :**
Prenez l'habitude d'écrire vos algorithmes en pseudo-code avant de programmer. Cela vous fera gagner beaucoup de temps et évitera de nombreuses erreurs !

⏭️ [Organigrammes et structuration de la pensée](/01-prerequis-bases-programmation/06-organigrammes-structuration-pensee.md)
