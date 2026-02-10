# Compilation des exemples - Chapitre 2 : Introduction au langage Pascal

## Prérequis

- Free Pascal Compiler (FPC) installé
- Terminal/ligne de commande

## Compilation

Pour compiler un exemple :

```bash
fpc nom-du-fichier.pas
```

Pour compiler tous les exemples :

```bash
for f in *.pas; do fpc "$f"; done
```

## Liste des exemples (60 fichiers)

### Section 2.2 : Structure d'un programme Pascal (4 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `02-minimum.pas` | ProgrammeMinimum | Programme minimal avec un seul WriteLn |
| `02-avec-variables.pas` | ProgrammeAvecVariables | Variables string et integer avec affichage |
| `02-calcul.pas` | ProgrammeCalcul | Calcul de surface de cercle avec constante Pi |
| `02-application-complete.pas` | ApplicationComplete | Application avec menu et saisie utilisateur |

**Sortie attendue `02-minimum` :**
```
Ceci est un programme minimal
```

**Sortie attendue `02-avec-variables` :**
```
Bonjour Alice  
Vous avez 25 ans  
```

**Sortie attendue `02-calcul` :**
```
Surface du cercle : 78.54
```

**Sortie attendue `02-application-complete` :**
```
=== Ma Super App v1.0 ===
1. Option 1
2. Option 2
0. Quitter
Votre choix : Vous avez choisi l'option : 0  
Fin du programme  
```

### Section 2.3 : Variables et constantes (6 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `03-exemples-affectation.pas` | ExemplesAffectation | Déclaration et affectation de variables de différents types |
| `03-exemples-constantes.pas` | ExemplesConstantes | Utilisation de constantes (Pi, TVA) |
| `03-calcul-surface.pas` | CalculSurface | Calcul de circonférence et surface d'un cercle |
| `03-calcul-prix-ttc.pas` | CalculPrixTTC | Calcul de prix TTC avec taux de TVA constant |
| `03-fiche-personnelle.pas` | FichePersonnelle | Fiche avec nom, prénom, année de naissance, ville |
| `03-conversion-temperature.pas` | ConversionTemperature | Conversion Celsius vers Fahrenheit |

**Sortie attendue `03-exemples-affectation` :**
```
Prénom : Sophie
Âge : 30 ans
Taille : 1.68 m  
Adulte : TRUE  
```

**Sortie attendue `03-calcul-surface` :**
```
=== Cercle ===
Rayon : 7.50 cm  
Circonférence : 47.12 cm  
Surface : 176.71 cm²  
```

**Sortie attendue `03-calcul-prix-ttc` :**
```
Prix HT : 100.00 €  
TVA (20%) : 20.00 €  
Prix TTC : 120.00 €  
```

**Sortie attendue `03-fiche-personnelle` :**
```
=== FICHE PERSONNELLE ===
Nom : Dupont  
Prénom : Marie  
Année de naissance : 1990  
Âge : 34 ans
Ville : Paris
```

**Sortie attendue `03-conversion-temperature` :**
```
Conversion Celsius vers Fahrenheit
========================

25.0 °C = 77.0 °F
```

### Section 2.4 : Types de données (8 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `04-exemple-integer.pas` | ExempleInteger | Variables entières : âge, effectif, température, année |
| `04-exemple-real.pas` | ExempleReal | Variables réelles : prix, taille, température, Pi |
| `04-exemple-boolean.pas` | ExempleBoolean | Variables booléennes avec TRUE/FALSE |
| `04-validation-age.pas` | ValidationAge | Validation d'âge avec conditions booléennes |
| `04-exemple-char.pas` | ExempleChar | Variables caractères : initiale, note, symbole |
| `04-calcul-moyenne.pas` | CalculMoyenne | Calcul de moyenne de 3 notes avec appréciation |
| `04-info-produit.pas` | InfoProduit | Fiche produit avec tous les types de données |
| `04-validation-age-complete.pas` | ValidationAgeComplete | Validation complète avec char, string, integer, boolean |

**Sortie attendue `04-exemple-integer` :**
```
Âge : 25
Nombre d'élèves : 30  
Température : -5 °C  
Année : 2024  
```

**Sortie attendue `04-calcul-moyenne` :**
```
Note 1 : 15.5  
Note 2 : 12.0  
Note 3 : 14.5  
Moyenne : 14.00  
A réussi : TRUE  
```

**Sortie attendue `04-info-produit` :**
```
=== FICHE PRODUIT ===
Produit : Clavier sans fil  
Catégorie : I  
Prix HT : 35.50 €  
Prix TTC : 42.60 €  
Quantité : 5  
En stock : TRUE  
Montant total : 213.00 €  
```

### Section 2.5 : Opérateurs (7 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `05-comparaisons-age.pas` | ComparaisonsAge | Comparaisons d'âge avec opérateurs relationnels |
| `05-expressions-complexes.pas` | ExpressionsComplexes | Expression combinant +, *, div, mod |
| `05-conditions-complexes.pas` | ConditionsComplexes | Opérateurs logiques and, or, not |
| `05-calculatrice-simple.pas` | CalculatriceSimple | Opérations de base sur deux nombres |
| `05-validation-inscription.pas` | ValidationInscription | Validation multicritère avec opérateurs logiques |
| `05-calcul-remise.pas` | CalculRemise | Calcul de remise selon seuils |
| `05-test-divisibilite.pas` | TestDivisibilite | Test de divisibilité avec mod |

**Sortie attendue `05-comparaisons-age` :**
```
Âge : 25
Enfant : FALSE  
Adolescent : FALSE  
Adulte : TRUE  
Senior : FALSE  
```

**Sortie attendue `05-expressions-complexes` :**
```
Résultat : 28
```

**Sortie attendue `05-test-divisibilite` :**
```
=== TEST DE DIVISIBILITÉ ===
Nombre : 30  
Divisible par 2 : TRUE  
Divisible par 3 : TRUE  
Divisible par 5 : TRUE  
Divisible par 10 : TRUE  

Vérification : TRUE
```

### Section 2.6 : Entrées/sorties console (14 fichiers)

#### Programmes non interactifs (9 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `06-exemple-write.pas` | ExempleWrite | Démonstration de Write (sans retour à la ligne) |
| `06-exemple-writeln.pas` | ExempleWriteLn | Démonstration de WriteLn (avec retour à la ligne) |
| `06-difference-write-writeln.pas` | DifferenceWriteWriteLn | Comparaison Write vs WriteLn |
| `06-afficher-variables.pas` | AfficherVariables | Affichage de variables de différents types |
| `06-plusieurs-sorties.pas` | PlusieursVariablesSortie | Plusieurs variables dans un seul WriteLn |
| `06-afficher-calculs.pas` | AfficherCalculs | Affichage de résultats de calculs |
| `06-formatage-real.pas` | FormatageReal | Formatage de nombres réels (décimales, largeur) |
| `06-formatage-integer.pas` | FormatageInteger | Formatage d'entiers (largeur de champ) |
| `06-tableau-aligne.pas` | TableauAligne | Tableau aligné avec formatage |

**Sortie attendue `06-exemple-write` :**
```
Bonjour le monde
```

**Sortie attendue `06-difference-write-writeln` :**
```
ABCD  
EF  
```

**Sortie attendue `06-formatage-real` :**
```
 1.9989999999999998E+001
19.99
     19.99
20
19.99000
```

**Sortie attendue `06-tableau-aligne` :**
```
            Nom  Age          Ville
-----------------------------------
          Alice   25          Paris
            Bob   30           Lyon
        Charlie   28      Marseille
```

#### Programmes interactifs (5 fichiers)

| Fichier | Programme | Description | Entrées attendues |
|---------|-----------|-------------|-------------------|
| `06-calculatrice-interactive.pas` | CalculatriceInteractive | Calculatrice avec choix d'opérateur | nombre, opérateur (+,-,*,/), nombre |
| `06-fiche-identite.pas` | FicheIdentite | Collecte d'informations personnelles | prénom, nom, âge, taille, ville |
| `06-calcul-moyenne.pas` | CalculMoyenneIO | Saisie de 3 notes et calcul de moyenne | 3 notes (réelles) |
| `06-conversion-temperature.pas` | ConversionTemperatureIO | Conversion avec choix de direction | choix (1 ou 2), température |
| `06-menu-interactif.pas` | MenuInteractif | Menu avec sélection d'option | numéro d'option |

**Exemple d'exécution `06-calculatrice-interactive` (entrée : 15, +, 4) :**
```
=== CALCULATRICE ===

Premier nombre : 15  
Opérateur (+, -, *, /) : +  
Deuxième nombre : 4  

Résultat : 15.00 + 4.00 = 19.00
```

**Exemple d'exécution `06-calcul-moyenne` (entrée : 14, 16, 12) :**
```
=== CALCUL DE MOYENNE ===

Note 1 : 14  
Note 2 : 16  
Note 3 : 12  

--- RÉSULTAT ---
Note 1   : 14.00  
Note 2   : 16.00  
Note 3   : 12.00  
----------------
Moyenne  : 14.00  
Appréciation : Bien  
```

### Section 2.7 : Formatage de sortie (19 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `07-formate-decimales.pas` | FormateDecimales | Formatage du nombre de décimales |
| `07-formate-largeur.pas` | FormateLargeur | Formatage avec largeur de champ |
| `07-formate-entiers.pas` | FormateEntiers | Formatage d'entiers avec largeur |
| `07-tableau-aligne.pas` | TableauAligneNum | Tableau numérique aligné à droite |
| `07-alignement-droite.pas` | AlignementDroite | Démonstration de l'alignement à droite |
| `07-alignement-gauche.pas` | AlignementGauche | Démonstration de l'alignement à gauche (largeur négative) |
| `07-tableau-noms.pas` | TableauNoms | Tableau de noms avec alignement |
| `07-tableau-bordures.pas` | TableauBordures | Tableau avec bordures en caractères |
| `07-tableau-totaux.pas` | TableauTotaux | Facture avec ligne de total |
| `07-format-prix.pas` | FormatPrix | Formatage de prix HT, TVA, TTC |
| `07-facture-complete.pas` | FactureComplete | Facture complète avec en-tête et totaux |
| `07-rapport-pourcentages.pas` | RapportPourcentages | Rapport de ventes avec pourcentages |
| `07-menu-formate.pas` | MenuFormate | Menu avec bordures en caractères ASCII |
| `07-menu-numerate.pas` | MenuNumerate | Menu numéroté avec alignement des numéros |
| `07-centrer-texte.pas` | CentrerTexte | Centrage de texte par calcul d'espaces |
| `07-barre-progression.pas` | BarreProgression | Barre de progression textuelle |
| `07-format-datetime.pas` | FormatDateTime | Formatage de dates et heures avec zéro-padding |
| `07-format-conditionnel.pas` | FormatConditionnel | Affichage conditionnel du signe +/- |
| `07-notes-formatees.pas` | NotesFormatees | Notes avec appréciation et étoiles |

**Sortie attendue `07-formate-decimales` :**
```
Sans formatage    :  3.1415926535897900E+000
0 décimale        : 3
1 décimale        : 3.1
2 décimales       : 3.14
4 décimales       : 3.1416
10 décimales      : 3.1415926536
```

**Sortie attendue `07-tableau-bordures` :**
```
+-----------------+---------+---------+
|          Article|     Prix|     Qté|
+-----------------+---------+---------+
|          Clavier|    25.99|        5|
|           Souris|    15.50|       10|
|           Écran|   199.99|        2|
+-----------------+---------+---------+
```

**Sortie attendue `07-barre-progression` :**
```
Progression : [=============       ] 65%
```

**Sortie attendue `07-format-conditionnel` :**
```
Solde : - 150.50 €
*** ATTENTION : Solde négatif ! ***
```

**Sortie attendue `07-notes-formatees` :**
```
Note : 15.5/20  
Appréciation : Bien            ***  
```

### Section 2.9 : Conventions de nommage (2 fichiers)

| Fichier | Programme | Description | Mode |
|---------|-----------|-------------|------|
| `09-calculateur-moyenne.pas` | CalculateurMoyenne | Calcul de moyenne avec fonctions bien nommées | `{$mode objfpc}` |
| `09-gestion-articles.pas` | GestionArticles | Gestion d'article avec record TArticle | `{$mode objfpc}` |

> **Note :** Ces deux fichiers utilisent `{$mode objfpc}{$H+}` car ils emploient la syntaxe `Result :=` dans les fonctions (au lieu de l'affectation au nom de la fonction).

**Sortie attendue `09-gestion-articles` :**
```
Code : ART001  
Désignation : Clavier USB  
Prix HT : 25.00 €  
Prix TTC : 30.00 €  
Stock : 15  
```

**Exemple d'exécution `09-calculateur-moyenne` (entrée : 14, 16, 18) :**
```
=== CALCUL DE MOYENNE ===
Note 1 : 14  
Note 2 : 16  
Note 3 : 18  

Moyenne : 16.00/20  
Appréciation : Très bien  
```

## Nettoyage

Pour supprimer les fichiers compilés :

```bash
rm -f *.o *.ppu  
for f in *.pas; do rm -f "${f%.pas}"; done  
```

## Résumé

- **60 fichiers** au total
- **54 programmes non interactifs** (exécution directe)
- **6 programmes interactifs** (nécessitent une saisie utilisateur)
- **2 fichiers** en mode `{$mode objfpc}` (section 09)
- **58 fichiers** en mode FreePascal par défaut
