# Compilation des exemples - Chapitre 3 : Structures de controle

## Prerequis

- Free Pascal Compiler (FPC) installe
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

## Liste des exemples (143 fichiers)

### Section 3.1 : Instructions conditionnelles if-then-else (6 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `01-exemple-if-simple.pas` | ExempleIfSimple | Exemple simple d'instruction if-then |
| `01-exemple-if-else.pas` | ExempleIfElse | Exemple d'instruction if-then-else |
| `01-exemple-else-if.pas` | ExempleElseIf | Chaine else-if pour classification de notes |
| `01-exemple-imbrique.pas` | ExempleImbrique | Instructions if imbriquees avec blocs begin..end |
| `01-exemple-operateurs.pas` | ExempleOperateurs | Operateurs logiques (and, or) dans les conditions |
| `01-gestion-acces.pas` | GestionAcces | Systeme de gestion d'acces avec conditions imbriquees |

**Exemple d'execution `01-exemple-if-simple` (entree : 25) :**
```
Entrez votre age : Vous etes majeur.
Programme termine.
```

**Exemple d'execution `01-exemple-else-if` (entree : 15) :**
```
Entrez votre note (0-20) : Tres bien
```

**Exemple d'execution `01-gestion-acces` (entree : 25, true, 100) :**
```
=== Systeme de gestion d'acces ===

Entrez votre age : Etes-vous membre ? (true/false) : Solde du compte :
--- Analyse ---
Vous etes majeur.
Vous etes membre.
Votre compte est crediteur.

--- Resultat ---
ACCES AUTORISE
```

### Section 3.2 : Instructions de choix multiple case-of (9 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `02-exemple-note-lettre.pas` | ExempleNoteLettre | Conversion d'une note numerique en mention |
| `02-exemple-intervalle.pas` | ExempleIntervalle | Intervalles dans case-of pour categoriser un age |
| `02-exemple-avec-else.pas` | ExempleAvecElse | Case-of avec clause else |
| `02-calculatrice-case.pas` | CalculatriceCaseOf | Calculatrice simple utilisant case-of |
| `02-conversion-notes.pas` | ConversionNotes | Conversion de notes lettres (A-F) en equivalents francais |
| `02-menu-application.pas` | MenuApplication | Menu principal d'application |
| `02-menu-imbriques.pas` | MenuImbriques | Case-of imbriques pour menu a deux niveaux |
| `02-gestion-restaurant.pas` | GestionRestaurant | Systeme complet de commande de restaurant |
| `02-systeme-tarification.pas` | SystemeTarification | Tarification par tranche d'age |

**Exemple d'execution `02-calculatrice-case` (entree : 10, +, 20) :**
```
=== CALCULATRICE SIMPLE ===
Premier nombre : Operation (+, -, *, /) : Deuxieme nombre : Resultat : 30.00
```

**Exemple d'execution `02-conversion-notes` (entree : B) :**
```
Entrez votre note (A, B, C, D, F) : Equivalent francais : Bien (14-15)
```

**Exemple d'execution `02-gestion-restaurant` (entree : 2, 3) :**
```
=============================
  RESTAURANT LE BON PASCAL
=============================

Categories :
1. Entrees
2. Plats principaux
3. Desserts
4. Boissons

Choisissez une categorie :
--- PLATS PRINCIPAUX ---
1. Steak-frites (15 euros)
2. Poisson grille (18 euros)
3. Pates carbonara (12 euros)
4. Pizza margherita (10 euros)
Votre choix :
=============================
Prix : 12.00 euros
Merci pour votre commande !
=============================
```

### Section 3.3 : Boucles compteur for-do (21 fichiers)

#### Programmes non interactifs (12 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `03-premier-for.pas` | PremierFor | Affiche "Bonjour !" 5 fois |
| `03-affichage-numeros.pas` | AffichageNumeros | Comptage de 1 a 10 |
| `03-alphabet-majuscules.pas` | AlphabetMajuscules | Affichage de l'alphabet en majuscules |
| `03-comparaison-to-downto.pas` | ComparaisonToDownto | Comparaison entre to et downto |
| `03-compte-a-rebours.pas` | CompteARebours | Compte a rebours avec downto |
| `03-boucle-multiple.pas` | BoucleMultiple | Instructions multiples dans un for avec begin-end |
| `03-dessin-triangle.pas` | DessinTriangle | Triangle d'etoiles avec boucles imbriquees |
| `03-somme-nombres.pas` | SommeNombres | Somme des nombres de 1 a 100 |
| `03-table-multiplication.pas` | TableMultiplication | Table de multiplication par 7 |
| `03-table-multiplication-complete.pas` | TableMultiplicationComplete | Table de multiplication 5x5 |
| `03-valeur-apres-for.pas` | ValeurApresFor | Valeur indefinie du compteur apres la boucle |
| `03-voyelles.pas` | Voyelles | Affichage des lettres a-e avec for sur Char |

**Sortie attendue `03-premier-for` :**
```
Bonjour !
Bonjour !
Bonjour !
Bonjour !
Bonjour !
```

**Sortie attendue `03-somme-nombres` :**
```
La somme des nombres de 1 a 100 est : 5050
```

**Sortie attendue `03-table-multiplication-complete` :**
```
TABLE DE MULTIPLICATION (1 a 5)

   1   2   3   4   5
   2   4   6   8  10
   3   6   9  12  15
   4   8  12  16  20
   5  10  15  20  25
```

**Sortie attendue `03-dessin-triangle` :**
```
Triangle d'etoiles :

*
**  
***  
****  
*****  
```

**Sortie attendue `03-compte-a-rebours` :**
```
Compte a rebours pour le lancement :

10...
9...
8...
7...
6...
5...
4...
3...
2...
1...
DECOLLAGE !
```

#### Programmes interactifs (9 fichiers)

| Fichier | Programme | Description | Entrees attendues |
|---------|-----------|-------------|-------------------|
| `03-boucle-variable.pas` | BoucleVariable | Boucle for avec bornes saisies | debut, fin |
| `03-boucle-expression.pas` | BoucleExpression | Boucle for avec borne superieure saisie | nombre |
| `03-factorielle.pas` | FactorielleFor | Calcul de factorielle | nombre |
| `03-recherche-maximum.pas` | RechercheMaximum | Maximum parmi 5 nombres saisis | 5 nombres |
| `03-calendrier-mois.pas` | CalendrierMois | Calendrier mensuel | premier jour, nb jours |
| `03-statistiques-nombres.pas` | StatistiquesNombres | Statistiques (somme, moyenne, min, max) | quantite + nombres |
| `03-analyseur-texte.pas` | AnalyseurTexte | Analyse de texte (voyelles, consonnes...) | texte |
| `03-generateur-motifs.pas` | GenerateurMotifs | Dessin d'un sapin | hauteur |
| `03-validation-tentatives.pas` | ValidationAvecTentatives | Validation de code avec tentatives limitees | code (entier) |

**Exemple d'execution `03-factorielle` (entree : 5) :**
```
Entrez un nombre : La factorielle de 5 est : 120
```

**Exemple d'execution `03-calendrier-mois` (entree : 1, 31) :**
```
Premier jour du mois (1=Lundi, 7=Dimanche) : Nombre de jours dans le mois :
Lun Mar Mer Jeu Ven Sam Dim
---------------------------
  1   2   3   4   5   6   7
  8   9  10  11  12  13  14
 15  16  17  18  19  20  21
 22  23  24  25  26  27  28
 29  30  31
```

**Exemple d'execution `03-analyseur-texte` (entree : Bonjour) :**
```
=== ANALYSEUR DE TEXTE ===

Entrez un texte :

Analyse en cours...

=== RESULTATS ===
Longueur totale : 7 caracteres
Voyelles : 3
Consonnes : 4
Chiffres : 0
Espaces : 0
Autres caracteres : 0

Apercu caractere par caractere :
Bonjour
```

### Section 3.4 : Boucles conditionnelles while et repeat (15 fichiers)

#### Programmes non interactifs (6 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `04-premier-while.pas` | PremierWhile | Compteur de 1 a 5 avec while |
| `04-premier-repeat.pas` | PremierRepeat | Compteur de 1 a 5 avec repeat-until |
| `04-comparaison-while-repeat.pas` | ComparaisonWhileRepeat | Comparaison while vs repeat |
| `04-repeat-toujours-une-fois.pas` | RepeatToujoursUneFois | Repeat s'execute au moins une fois |
| `04-while-imbriques.pas` | WhileImbriques | Boucles while imbriquees |
| `04-while-jamais-execute.pas` | WhileJamaisExecute | While qui ne s'execute jamais |

**Sortie attendue `04-premier-while` :**
```
Compteur = 1
Compteur = 2
Compteur = 3
Compteur = 4
Compteur = 5
Fin de la boucle
```

**Sortie attendue `04-comparaison-while-repeat` :**
```
=== Avec WHILE ===
WHILE : Aucune execution car condition fausse au depart

=== Avec REPEAT ===
REPEAT : i = 10
REPEAT : Execute une fois malgre la condition vraie
```

#### Programmes interactifs (9 fichiers)

| Fichier | Programme | Description | Entrees attendues |
|---------|-----------|-------------|-------------------|
| `04-validation-age-while.pas` | ValidationAgeWhile | Validation d'age (0-120) avec while | age |
| `04-validation-repeat-age.pas` | ValidationRepeatAge | Validation d'age (0-120) avec repeat | age |
| `04-calcul-moyenne-sentinelle.pas` | CalculMoyenneSentinelle | Moyenne avec valeur sentinelle (0) | nombres, 0 pour finir |
| `04-menu-simple.pas` | MenuSimple | Menu interactif A/B/C/Quitter | choix (1-4) |
| `04-jeu-devinette.pas` | JeuDevinette | Devinette d'un nombre (1-100) | propositions |
| `04-recherche-nombre.pas` | RechercheNombre | Recherche dans un tableau | 10 nombres + cible |
| `04-systeme-connexion.pas` | SystemeConnexion | Connexion avec 3 tentatives | mot de passe |
| `04-gestion-compte-client.pas` | GestionCompteClient | Distributeur automatique | PIN + operations |
| `04-melange-boucles.pas` | MelangeBoucles | Melange repeat et for avec O/N | nombre + continuer |

**Exemple d'execution `04-validation-age-while` (entree : 25) :**
```
Entrez votre age (0-120) : Age accepte : 25 ans
```

### Section 3.5 : Instructions break et continue (19 fichiers)

#### Programmes non interactifs (10 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `05-premier-break.pas` | PremierBreak | Premier exemple de break avec for |
| `05-premier-continue.pas` | PremierContinue | Premier exemple de continue avec for |
| `05-continue-avec-while.pas` | ContinueAvecWhile | Continue avec while (nombres pairs) |
| `05-comparaison-break-continue.pas` | ComparaisonBreakContinue | Comparaison break vs continue |
| `05-break-imbrique.pas` | BreakImbrique | Break dans boucles imbriquees |
| `05-continue-imbrique.pas` | ContinueImbrique | Continue dans boucles imbriquees |
| `05-sortie-double-break.pas` | SortieDoubleBreak | Sortie de boucles imbriquees avec drapeau |
| `05-traitement-commandes.pas` | TraitementCommandes | Filtrage de commandes annulees |
| `05-traitement-par-lots.pas` | TraitementParLots | Traitement par lots avec continue |
| `05-recherche-multicriteres.pas` | RechercheMulticriteres | Recherche du premier pair et premier multiple de 5 |

**Sortie attendue `05-premier-break` :**
```
Comptage de 1 a 10, mais arret a 5 :
1
2
3
4
Arret a 5
Apres la boucle
```

**Sortie attendue `05-comparaison-break-continue` :**
```
=== Avec BREAK ===
1
2
3
4
Boucle terminee

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
Boucle terminee
```

**Sortie attendue `05-traitement-commandes` :**
```
=== TRAITEMENT DES COMMANDES ===

Commande #101 - Montant : 150.50 EUR - TRAITEE
Commande #102 - ANNULEE (ignoree)
Commande #103 - Montant : 200.00 EUR - TRAITEE
Commande #104 - ANNULEE (ignoree)
Commande #105 - Montant : 300.00 EUR - TRAITEE

Total des commandes valides : 650.50 EUR
```

#### Programmes interactifs (9 fichiers)

| Fichier | Programme | Description | Entrees attendues |
|---------|-----------|-------------|-------------------|
| `05-break-avec-while.pas` | BreakAvecWhile | Break avec while infini | nombres, -1 pour finir |
| `05-break-avec-repeat.pas` | BreakAvecRepeat | Break avec repeat-until | nombres, -1 pour finir |
| `05-continue-avec-repeat.pas` | ContinueAvecRepeat | Continue avec repeat-until | nombres |
| `05-recherche-avec-break.pas` | RechercheAvecBreak | Recherche dans un tableau avec break | 5 nombres + cible |
| `05-filtrage-nombres.pas` | FiltrageNombres | Filtrage des negatifs avec continue | 5 nombres |
| `05-validation-mot-de-passe.pas` | ValidationMotDePasse | Validation de mot de passe | mot de passe |
| `05-menu-interactif-avance.pas` | MenuInteractifAvance | Menu interactif avec sous-menus | choix |
| `05-analyse-texte-filtre.pas` | AnalyseTexteFiltre | Analyse de texte avec filtrage | texte |
| `05-recherche-optimisee.pas` | RechercheOptimisee | Recherche dans un tableau trie | nombre |

### Section 3.6 : Imbrication de structures (18 fichiers)

#### Programmes non interactifs (8 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `06-table-multiplication-2d.pas` | TableMultiplication2D | Table de multiplication 2D (1 a 10) |
| `06-cube-3d.pas` | Cube3D | Coordonnees 3D avec 3 niveaux de boucles |
| `06-while-imbrique.pas` | WhileImbrique | While dans while, groupes de nombres |
| `06-nombres-pairs.pas` | NombresPairs | IF dans FOR, pairs et impairs |
| `06-jours-semaine.pas` | JoursSemaine | CASE dans FOR, jours de la semaine |
| `06-filtrage-donnees.pas` | FiltrageDonnees | CONTINUE avec imbrication, filtrage multiples de 5 |
| `06-emploi-du-temps.pas` | EmploiDuTemps | Generateur d'emploi du temps |
| `06-tri-bulles.pas` | TriBulles | Tri a bulles avec affichage des passes |

**Sortie attendue `06-table-multiplication-2d` :**
```
TABLE DE MULTIPLICATION (1 a 10)

      1   2   3   4   5   6   7   8   9  10
   ----------------------------------------
 1 |   1   2   3   4   5   6   7   8   9  10
 2 |   2   4   6   8  10  12  14  16  18  20
 3 |   3   6   9  12  15  18  21  24  27  30
 4 |   4   8  12  16  20  24  28  32  36  40
 5 |   5  10  15  20  25  30  35  40  45  50
 6 |   6  12  18  24  30  36  42  48  54  60
 7 |   7  14  21  28  35  42  49  56  63  70
 8 |   8  16  24  32  40  48  56  64  72  80
 9 |   9  18  27  36  45  54  63  72  81  90
10 |  10  20  30  40  50  60  70  80  90 100
```

**Sortie attendue `06-jours-semaine` :**
```
Les jours de la semaine :

Jour 1 : Lundi (travail)
Jour 2 : Mardi (travail)
Jour 3 : Mercredi (travail)
Jour 4 : Jeudi (travail)
Jour 5 : Vendredi (travail)
Jour 6 : Samedi (week-end)
Jour 7 : Dimanche (week-end)
```

#### Programmes interactifs (10 fichiers)

| Fichier | Programme | Description | Entrees attendues |
|---------|-----------|-------------|-------------------|
| `06-if-imbriques.pas` | IfImbriques | IF imbriques, age et permis | age, true/false |
| `06-notes-imbriquees.pas` | NotesImbriquees | IF-ELSE IF, mention selon la note | note (0-20) |
| `06-acces-securise.pas` | AccesSecurise | Controle d'acces securise en cascade | age, true/false x2 |
| `06-motif-etoiles.pas` | MotifEtoiles | Triangle d'etoiles | hauteur |
| `06-menu-avec-validation.pas` | MenuAvecValidation | Menu interactif avec validation | choix |
| `06-affichage-conditionnel.pas` | AffichageConditionnel | Affichage detaille ou simple | nombre, true/false |
| `06-gestion-notes.pas` | GestionNotes | Gestion de notes avec mentions | nombre d'eleves + notes |
| `06-recherche-matrice.pas` | RechercheMatrice | Recherche dans une matrice | valeur |
| `06-statistiques-matrice.pas` | StatistiquesMatrice | Statistiques sur matrice | lignes, colonnes, valeurs |
| `06-morpion-structure.pas` | MorpionStructure | Jeu du morpion | coups (ligne, colonne) |

**Exemple d'execution `06-notes-imbriquees` (entree : 15) :**
```
Entrez la note (0-20) : Note : 15/20
Mention : Bien
```

**Exemple d'execution `06-if-imbriques` (entree : 25, true) :**
```
Age : Avez-vous le permis ? (true/false) : Vous etes majeur.
Vous pouvez conduire.
```

### Section 3.7 : Gestion d'erreurs simples (18 fichiers)

| Fichier | Programme | Description | Entrees attendues |
|---------|-----------|-------------|-------------------|
| `07-sans-gestion-erreurs.pas` | SansGestionErreurs | Sans gestion d'erreurs (crash possible) | dividende, diviseur |
| `07-avec-gestion-erreurs.pas` | AvecGestionErreurs | Avec gestion d'erreurs | dividende, diviseur |
| `07-validation-age-simple.pas` | ValidationAgeSimple | Validation simple de l'age | age |
| `07-validation-avec-boucle.pas` | ValidationAvecBoucle | Validation avec boucle while | age |
| `07-validation-repeat-nombre.pas` | ValidationRepeatNombre | Validation avec repeat-until | nombre positif |
| `07-validation-multiple.pas` | ValidationMultiple | Validation multiple d'une note | note |
| `07-division-securisee.pas` | DivisionSecurisee | Division securisee | dividende, diviseur |
| `07-racine-carree-securisee.pas` | RacineCarreeSecurisee | Racine carree securisee | nombre |
| `07-acces-tableau-securise.pas` | AccesTableauSecurise | Acces securise a un tableau | indice |
| `07-bon-message-erreur.pas` | BonMessageErreur | Bon message d'erreur avec contexte | note |
| `07-validation-avec-booleen.pas` | ValidationAvecBooleen | Validation d'email avec retour booleen | email |
| `07-validation-avec-code.pas` | ValidationAvecCode | Validation de mot de passe avec codes | mot de passe |
| `07-recherche-avec-sentinelle.pas` | RechercheAvecSentinelle | Recherche avec valeur sentinelle | valeur |
| `07-calculatrice-robuste.pas` | CalculatriceRobuste | Calculatrice avec gestion d'erreurs | nombre, operateur, nombre |
| `07-systeme-notation.pas` | SystemeNotation | Notation avec validation et mention | notes |
| `07-gestion-stock.pas` | GestionStock | Gestion de stock avec alertes | operations |
| `07-conversion-temperature-robuste.pas` | ConversionTemperatureRobuste | Conversion temperature avec zero absolu | temperature |
| `07-compte-bancaire.pas` | CompteBancaire | Compte bancaire avec depot/retrait | operations |

> **Note :** `07-sans-gestion-erreurs` provoque intentionnellement une erreur d'execution (division par zero) pour illustrer l'importance de la gestion d'erreurs.

**Exemple d'execution `07-avec-gestion-erreurs` (entree : 25, 0) :**
```
Entrez le premier nombre : Entrez le deuxieme nombre : ERREUR : Division par zero impossible !
```

**Exemple d'execution `07-division-securisee` (entree : 10, 0) :**
```
Dividende : Diviseur :
═══════════════════════════
   ERREUR : Division par 0
═══════════════════════════
Le diviseur ne peut pas etre zero.
```

### Section 3.8 : Validation des entrees utilisateur (25 fichiers)

| Fichier | Programme | Description | Entrees attendues |
|---------|-----------|-------------|-------------------|
| `08-sans-validation.pas` | SansValidation | Programme fragile sans validation | age |
| `08-avec-validation.pas` | AvecValidation | Programme robuste avec validation | age (0-150) |
| `08-validation-plage.pas` | ValidationPlage | Validation d'une note (0-20) | note |
| `08-validation-type.pas` | ValidationType | Validation de type (A/B/C) | choix |
| `08-validation-format.pas` | ValidationFormat | Validation de code postal (5 chiffres) | code postal |
| `08-validation-coherence.pas` | ValidationCoherence | Validation de coherence entre donnees | dates |
| `08-validation-repeat-pourcentage.pas` | ValidationRepeatPourcentage | Validation d'un pourcentage (0-100) | pourcentage |
| `08-validation-while.pas` | ValidationWhile | Validation avec while et compteur | entree |
| `08-validation-fonction.pas` | ValidationFonction | Fonction de validation reutilisable | entier |
| `08-validation-procedure.pas` | ValidationProcedure | Procedure de validation encapsulee | entree |
| `08-validation-entier-intervalle.pas` | ValidationEntierIntervalle | Validation d'entier dans intervalle | entier |
| `08-validation-decimal-positif.pas` | ValidationDecimalPositif | Validation de decimal positif | nombre reel |
| `08-validation-precision.pas` | ValidationPrecision | Note avec max 1 decimale | note |
| `08-validation-caractere.pas` | ValidationCaractere | Reponse O/N | caractere |
| `08-validation-non-vide.pas` | ValidationNonVide | Chaine non vide (uses SysUtils) | texte |
| `08-validation-longueur.pas` | ValidationLongueur | Mot de passe (8-20 caracteres) | mot de passe |
| `08-validation-email.pas` | ValidationEmail | Validation format email | email |
| `08-menu-validation.pas` | MenuValidation | Menu avec validation (1-4) | choix |
| `08-choix-textuels.pas` | ChoixTextuels | Choix textuels avec minuscules (uses SysUtils) | texte |
| `08-validation-combinee.pas` | ValidationCombinee | Code 6 caracteres (chiffres+lettres) | code |
| `08-validation-date.pas` | ValidationDate | Validation de date avec annee bissextile | jour, mois, annee |
| `08-formulaire-inscription.pas` | FormulaireInscription | Formulaire complet (uses SysUtils) | nom, email, age, mdp |
| `08-systeme-paiement.pas` | SystemePaiement | Systeme de paiement avec rendu | montant, paiement |
| `08-configuration-profil.pas` | ConfigurationProfil | Configuration de profil utilisateur | pseudo, avatar, notifs |
| `08-compteur-tentatives.pas` | CompteurTentatives | Code secret avec tentatives limitees | code |

**Exemple d'execution `08-sans-validation` (entree : 25) :**
```
Age : Dans 10 ans, vous aurez 35 ans
```

**Exemple d'execution `08-avec-validation` (entree : 25) :**
```
Age (0-150) : Dans 10 ans, vous aurez 35 ans
```

### Section 3.9 : Debogage pas a pas (12 fichiers)

#### Programmes non interactifs (9 fichiers)

| Fichier | Programme | Description |
|---------|-----------|-------------|
| `09-exemple-breakpoint.pas` | ExempleBreakpoint | Placement d'un point d'arret |
| `09-step-over.pas` | StepOver | Pas a pas approfondi (Step Over - F8) |
| `09-step-into.pas` | StepInto | Pas a pas detaille (Step Into - F7) |
| `09-inspection-variables.pas` | InspectionVariables | Inspection des variables locales |
| `09-debogage-fonctions.pas` | DebogageFonctions | Debogage de fonctions et pile d'appels |
| `09-bug-non-initialisee.pas` | BugNonInitialisee | Bug : variable non initialisee |
| `09-bug-indice.pas` | BugIndice | Bug : indice de tableau hors limites |
| `09-bug-boucle-infinie.pas` | BugBoucleInfinie | Bug : boucle infinie |
| `09-bug-calcul.pas` | BugCalcul | Bug : division entiere (div au lieu de /) |

> **Attention :** `09-bug-boucle-infinie` contient une boucle infinie intentionnelle. `09-bug-indice` provoque une erreur d'execution (depassement d'indice). `09-bug-non-initialisee` a un comportement indefini. Ces programmes sont destines a l'etude du debogage.

**Sortie attendue `09-exemple-breakpoint` :**
```
Resultat : 15
```

**Sortie attendue `09-debogage-fonctions` :**
```
Resultat : 25
```

**Sortie attendue `09-step-into` :**
```
Debut
Valeur : 10
Fin
```

**Sortie attendue `09-bug-calcul` :**
```
Resultat : 2
```

#### Programmes interactifs (3 fichiers)

| Fichier | Programme | Description | Entrees attendues |
|---------|-----------|-------------|-------------------|
| `09-calcul-moyenne-bug.pas` | CalculMoyenneBug | Bug : division par 2 au lieu de 3 | 3 notes |
| `09-debogage-boucle.pas` | DebogageBoucle | Debogage d'une boucle de recherche | nombre |
| `09-bug-condition.pas` | BugCondition | Bug : condition inversee (majeur/mineur) | age |

**Exemple d'execution `09-calcul-moyenne-bug` (entree : 15, 12, 18) :**
```
Calcul de moyenne de 3 notes
Note 1 : Note 2 : Note 3 : Moyenne : 15.00
```

**Exemple d'execution `09-bug-condition` (entree : 10) :**
```
Age : Vous etes majeur
```
> **Note :** La sortie est incorrecte (10 ans = mineur). C'est le bug intentionnel a trouver en deboguant.

## Nettoyage

Pour supprimer les fichiers compiles :

```bash
rm -f *.o *.ppu
for f in *.pas; do rm -f "${f%.pas}"; done
```

## Resume

- **143 fichiers** au total
- **45 programmes non interactifs** (execution directe)
- **98 programmes interactifs** (necessitent une saisie utilisateur)
- **3 fichiers** avec `uses SysUtils` (sections 08)
- **3 fichiers** avec bugs intentionnels a ne pas executer directement (boucle infinie, indice hors limites, variable non initialisee)
- **1 fichier** illustrant un crash volontaire (division par zero sans gestion d'erreurs)
- Tous les fichiers utilisent le mode FreePascal par defaut
