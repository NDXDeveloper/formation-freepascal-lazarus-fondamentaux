# Compilation des exemples - Chapitre 05

## Prérequis

- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure

## Compilation

Pour compiler un exemple :

```bash
fpc nom-du-fichier.pas
```

Pour compiler tous les exemples :

```bash
for f in *.pas; do fpc "$f"; done
```

## Nettoyage

Supprimer les fichiers objets et les exécutables :

```bash
rm -f *.o
# Supprimer les exécutables (fichiers sans extension)
find . -maxdepth 1 -type f ! -name "*.*" -delete
```

## Liste des exemples (103 fichiers)

### Section 5.1 : Tableaux statiques (6 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `01-exemple-acces.pas` | Déclaration et accès aux éléments d'un tableau | Non |
| `01-parcours-tableau.pas` | Parcours d'un tableau avec boucle for | Oui |
| `01-calcul-moyenne.pas` | Calcul de la moyenne des éléments d'un tableau | Oui |
| `01-recherche-max.pas` | Recherche du maximum dans un tableau | Oui |
| `01-taille-tableau.pas` | Utilisation de Low, High et Length sur un tableau | Non |
| `01-statistiques-notes.pas` | Statistiques complètes sur un tableau de notes | Oui |

### Section 5.2 : Tableaux multidimensionnels (10 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `02-exemple-acces-2d.pas` | Déclaration et accès à un tableau 2D | Non |
| `02-parcours-tableau-2d.pas` | Parcours d'un tableau 2D avec boucles imbriquées | Non |
| `02-tableau-notes.pas` | Tableau de notes par élève et matière | Oui |
| `02-somme-lignes.pas` | Calcul de la somme par ligne d'un tableau 2D | Non |
| `02-moyenne-colonnes.pas` | Calcul de la moyenne par colonne d'un tableau 2D | Non |
| `02-recherche-valeur.pas` | Recherche d'une valeur dans un tableau 2D | Oui |
| `02-matrice-identite.pas` | Création et affichage d'une matrice identité | Non |
| `02-exemple-3d.pas` | Tableau à trois dimensions | Non |
| `02-low-high-multidim.pas` | Low et High sur tableaux multidimensionnels | Non |
| `02-morpion.pas` | Jeu de morpion avec tableau 2D | Non |

### Section 5.3 : Chaînes de caractères (23 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `03-init-chaines.pas` | Initialisation et affectation de chaînes | Non |
| `03-concatenation.pas` | Concaténation de chaînes avec + et Concat | Non |
| `03-longueur-chaine.pas` | Longueur d'une chaîne avec Length | Non |
| `03-acces-caractere.pas` | Accès aux caractères individuels d'une chaîne | Non |
| `03-modification-caractere.pas` | Modification de caractères dans une chaîne | Non |
| `03-exemple-copy.pas` | Extraction de sous-chaîne avec Copy | Non |
| `03-exemple-pos.pas` | Recherche de position avec Pos | Non |
| `03-exemple-delete.pas` | Suppression dans une chaîne avec Delete | Non |
| `03-exemple-insert.pas` | Insertion dans une chaîne avec Insert | Non |
| `03-exemple-casse.pas` | Conversion majuscules/minuscules | Non |
| `03-exemple-trim.pas` | Suppression des espaces avec Trim | Non |
| `03-comparaison-chaines.pas` | Comparaison de chaînes | Non |
| `03-comparaison-insensible.pas` | Comparaison insensible à la casse | Non |
| `03-nombre-vers-string.pas` | Conversion nombre vers chaîne | Non |
| `03-string-vers-nombre.pas` | Conversion chaîne vers nombre | Non |
| `03-inverser-chaine.pas` | Inversion d'une chaîne caractère par caractère | Non |
| `03-compter-voyelles.pas` | Comptage des voyelles dans une chaîne | Non |
| `03-palindrome.pas` | Vérification si une chaîne est un palindrome | Non |
| `03-extraire-noms.pas` | Extraction prénom/nom depuis une chaîne | Non |
| `03-remplacer-mot.pas` | Remplacement d'un mot dans une chaîne | Non |
| `03-chaines-multiligne.pas` | Chaînes sur plusieurs lignes | Non |
| `03-caracteres-speciaux.pas` | Caractères spéciaux et codes ASCII | Non |
| `03-difference-types.pas` | Différence entre String, ShortString et String[n] | Non |

### Section 5.4 : Enregistrements Records (11 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `04-exemple-record.pas` | Déclaration et utilisation d'un record | Non |
| `04-saisie-personne.pas` | Saisie des champs d'un record | Oui |
| `04-exemple-with.pas` | Instruction with pour simplifier l'accès aux champs | Non |
| `04-copie-record.pas` | Copie complète d'un record | Non |
| `04-record-procedure.pas` | Passage d'un record en paramètre de procédure | Non |
| `04-modifier-record.pas` | Modification d'un record par référence (var) | Non |
| `04-fonction-record.pas` | Fonction retournant un record | Non |
| `04-gestion-date.pas` | Gestion de dates avec records | Non |
| `04-carnet-adresses.pas` | Carnet d'adresses avec tableau de records | Oui |
| `04-rectangle.pas` | Calculs géométriques avec un record rectangle | Non |
| `04-gestion-stock.pas` | Gestion de stock de produits | Non |

### Section 5.5 : Enregistrements imbriqués (6 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `05-acces-imbriques.pas` | Accès aux champs de records imbriqués | Non |
| `05-affichage-imbrique.pas` | Affichage formaté d'un record imbriqué | Non |
| `05-geometrie.pas` | Géométrie avec points et rectangles imbriqués | Non |
| `05-carnet-complet.pas` | Carnet d'adresses avec records imbriqués | Non |
| `05-gestion-commande.pas` | Gestion de commande avec records imbriqués | Non |
| `05-entreprise.pas` | Structure d'entreprise avec records imbriqués | Non |

### Section 5.6 : Tableaux d'enregistrements (6 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `06-acces-tableau-record.pas` | Accès aux éléments d'un tableau de records | Non |
| `06-parcours-tableau.pas` | Parcours et affichage d'un tableau de records | Non |
| `06-saisie-tableau.pas` | Saisie interactive d'un tableau de records | Oui |
| `06-gestion-classe.pas` | Gestion d'une classe d'élèves avec records | Non |
| `06-catalogue-produits.pas` | Catalogue de produits avec recherche et tri | Non |
| `06-carnet-adresses.pas` | Carnet d'adresses complet avec menu | Oui |

### Section 5.7 : Types énumérés (15 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `07-exemple-enum.pas` | Déclaration et utilisation d'un type énuméré | Non |
| `07-comparaison-enum.pas` | Comparaison de valeurs énumérées | Non |
| `07-fonction-ord.pas` | Fonction Ord sur type énuméré | Non |
| `07-fonction-succ.pas` | Fonction Succ sur type énuméré | Non |
| `07-fonction-pred.pas` | Fonction Pred sur type énuméré | Non |
| `07-parcours-enum.pas` | Parcours d'un type énuméré avec boucle for | Non |
| `07-case-enum.pas` | Instruction case avec type énuméré | Non |
| `07-gestion-etat.pas` | Machine à états avec type énuméré | Non |
| `07-comptage-categorie.pas` | Comptage par catégorie avec type énuméré | Non |
| `07-feux-tricolores.pas` | Simulation de feux tricolores | Non |
| `07-systeme-notation.pas` | Système de notation avec mentions | Oui |
| `07-gestion-saisons.pas` | Gestion des saisons avec deux types énumérés | Non |
| `07-menu-navigation.pas` | Menu de navigation interactif | Oui |
| `07-conversion-enum.pas` | Conversion entre type énuméré et String | Non |
| `07-enum-dans-record.pas` | Type énuméré dans un enregistrement | Non |
| `07-tableau-enum.pas` | Tableau indexé par un type énuméré | Oui |

### Section 5.8 : Types ensemble - Set (14 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `08-test-appartenance.pas` | Test d'appartenance avec l'opérateur IN | Non |
| `08-union.pas` | Opération d'union (+) sur les ensembles | Non |
| `08-intersection.pas` | Opération d'intersection (*) sur les ensembles | Non |
| `08-difference.pas` | Opération de différence (-) sur les ensembles | Non |
| `08-comparaison-ensembles.pas` | Comparaison d'ensembles (égalité, inclusion) | Non |
| `08-ajouter-element.pas` | Ajout d'élément avec Include et opérateur + | Non |
| `08-retirer-element.pas` | Suppression d'élément avec Exclude et opérateur - | Non |
| `08-jours-presence.pas` | Jours de présence avec opérations ensemblistes | Non |
| `08-validation-mot-de-passe.pas` | Validation de mot de passe avec analyse de caractères | Oui |
| `08-gestion-permissions.pas` | Gestion des permissions avec ensembles | Non |
| `08-voyelles-consonnes.pas` | Comptage de voyelles et consonnes | Oui |
| `08-planning-hebdo.pas` | Planification hebdomadaire avec ensembles | Non |
| `08-ensembles-caracteres.pas` | Ensembles de caractères pour validation | Oui |
| `08-ensemble-vide-complet.pas` | Ensemble vide et ensemble complet | Non |

### Section 5.9 : Types intervalle (8 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `09-exemple-intervalle.pas` | Déclaration et utilisation de types intervalle | Non |
| `09-tableau-intervalle.pas` | Tableau indexé par un type intervalle | Oui |
| `09-intervalle-caracteres.pas` | Intervalles de caractères pour validation | Oui |
| `09-gestion-dates.pas` | Gestion de dates avec types intervalle | Oui |
| `09-systeme-notation.pas` | Système de notation avec conversion d'échelles | Oui |
| `09-gestion-heures.pas` | Gestion d'horaires avec types intervalle | Oui |
| `09-gestion-ascenseur.pas` | Gestion d'un ascenseur avec type intervalle | Oui |
| `09-code-pin.pas` | Code PIN à 4 chiffres avec type intervalle | Oui |

### Section 5.10 : Définition de types personnalisés (3 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `10-bibliotheque.pas` | Système de gestion de bibliothèque | Non |
| `10-gestion-planning.pas` | Gestion de planning hebdomadaire | Non |
| `10-systeme-commandes.pas` | Système de commandes complet | Non |

## Notes

- Les fichiers utilisant `Result` nécessitent la directive `{$mode objfpc}{$H+}` (déjà incluse dans les fichiers concernés)
- Les fichiers utilisant `try/except` nécessitent `{$mode objfpc}{$H+}` et `{$R+}` (déjà inclus)
- Les programmes interactifs (Interactif = Oui) attendent une saisie utilisateur via `ReadLn`
- Les notes du compilateur concernant des variables non utilisées sont normales pour des exemples de démonstration
- Certains fichiers utilisent `uses SysUtils` pour les fonctions de manipulation de chaînes

## Corrections apportées aux fichiers .md

1. **`02-tableaux-multidimensionnels.md`** : `Low(tableau, 1)` corrigé en `Low(tableau)` et `Low(tableau, 2)` corrigé en `Low(tableau[1])` (syntaxe non supportée par FreePascal 3.2.2)
2. **`05-enregistrements-imbriques.md`** : `program Entreprise;` renommé en `program ExempleEntreprise;` (conflit d'identifiant avec la variable `entreprise`)
3. **`08-types-ensemble-set.md`** : `ensemble1 < ensemble2` remplacé par `(ensemble1 <= ensemble2) and (ensemble1 <> ensemble2)` (opérateur `<` non implémenté pour les sets en FreePascal)
