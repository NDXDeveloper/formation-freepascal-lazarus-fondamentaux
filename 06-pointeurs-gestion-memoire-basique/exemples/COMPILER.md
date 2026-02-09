# Compilation des exemples — Chapitre 06

## Prérequis

- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure

## Compilation

Pour compiler un exemple :

```bash
fpc nom-du-fichier.pas
```

Pour compiler avec détection de fuites mémoire (HeapTrc) :

```bash
fpc -gh nom-du-fichier.pas
```

## Nettoyage

```bash
rm -f *.o
# Supprimer les exécutables (même nom que le .pas sans extension)
```

## Liste des exemples (47 fichiers)

### Section 6.1 — Concept de pointeur (1 fichier)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `01-exemple-pointeur.pas` | Démonstration basique des pointeurs | Oui (ReadLn) |

### Section 6.2 — Déclaration et utilisation des pointeurs (4 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `02-passage-pointeur-parametre.pas` | Passage de pointeur en paramètre | Non |
| `02-retourner-pointeur.pas` | Fonction retournant un pointeur | Non |
| `02-partage-donnees.pas` | Partage de données via pointeurs | Non |
| `02-echange-valeurs.pas` | Échange de valeurs via pointeurs | Non |

### Section 6.3 — Allocation dynamique de mémoire (5 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `03-cycle-vie-pointeur.pas` | Les 4 étapes du cycle de vie d'un pointeur | Non |
| `03-chaine-dynamique.pas` | Allocation dynamique de chaîne | Non |
| `03-record-dynamique.pas` | Allocation dynamique d'enregistrement | Non |
| `03-plusieurs-dynamiques.pas` | Plusieurs variables dynamiques | Non |
| `03-creer-liberer-personne.pas` | Fonctions de création/libération | Non |

### Section 6.4 — Pointeurs et tableaux (8 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `04-pointeur-vers-tableau.pas` | Pointeur vers un tableau statique | Non |
| `04-parcours-inc.pas` | Parcours de tableau avec Inc (arithmétique pointeur) | Non |
| `04-tableau-dynamique-new.pas` | Allocation manuelle de tableau avec New | Non |
| `04-array-of-redim.pas` | Tableau dynamique avec SetLength | Non |
| `04-matrice-dynamique.pas` | Matrice dynamique 2D | Non |
| `04-passage-tableau.pas` | Passage de tableaux dynamiques en paramètre | Non |
| `04-redimensionnement.pas` | Redimensionnement préservant les valeurs | Non |
| `04-tableau-de-pointeurs.pas` | Tableau de pointeurs vers enregistrements | Non |

### Section 6.5 — Pointeurs et enregistrements (6 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `05-liste-simple.pas` | Liste chaînée manuelle à 3 noeuds | Non |
| `05-liste-chainee.pas` | Liste chaînée avec AjouterDebut/Afficher/Liberer | Non |
| `05-relations-pointeurs.pas` | Enregistrements avec relations (conjoint) | Non |
| `05-etudiant-notes.pas` | Enregistrement dynamique avec tableau de notes | Non |
| `05-carnet-adresses.pas` | Carnet d'adresses avec liste chaînée | Non |
| `05-file-attente.pas` | File d'attente (Queue) avec Enfiler/Defiler | Non |

### Section 6.6 — Listes chaînées simples (12 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `06-afficher-liste.pas` | Création et affichage d'une liste chaînée | Non |
| `06-doubler-valeurs.pas` | Parcours avec traitement (doubler valeurs) | Non |
| `06-rechercher.pas` | Recherche d'une valeur dans une liste | Non |
| `06-trouver-position.pas` | Trouver la position d'une valeur | Non |
| `06-supprimer-debut.pas` | Suppression du premier élément | Non |
| `06-supprimer-valeur.pas` | Suppression d'un élément par valeur | Non |
| `06-compter-elements.pas` | Compter les éléments d'une liste | Non |
| `06-inverser-liste.pas` | Inverser une liste chaînée | Non |
| `06-copier-liste.pas` | Copier une liste (copie indépendante) | Non |
| `06-trier-liste.pas` | Tri à bulles d'une liste chaînée | Non |
| `06-liberer-liste.pas` | Libération correcte de tous les noeuds | Non |
| `06-gestionnaire-liste.pas` | Programme complet de gestion de liste | Oui (ReadLn) |

### Section 6.7 — Arbres binaires basics (5 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `07-creer-arbre.pas` | Création manuelle d'un arbre binaire | Non |
| `07-parcours-arbre.pas` | Les 4 types de parcours (pré/in/post/largeur) | Non |
| `07-rechercher-abr.pas` | Recherche dans un arbre binaire de recherche | Non |
| `07-afficher-arbre.pas` | Affichage visuel horizontal d'un arbre | Non |
| `07-gestionnaire-abr.pas` | Programme complet de gestion d'ABR | Oui (ReadLn) |

### Section 6.8 — Fuites mémoire et bonnes pratiques (4 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `08-fuite-detection.pas` | Détection de fuite avec HeapTrc (compiler avec -gh) | Non |
| `08-references-circulaires.pas` | Gestion des références circulaires | Non |
| `08-compteur-references.pas` | Compteur de références pour partage de données | Non |
| `08-encapsulation.pas` | Encapsulation création/destruction avec try-finally | Non |

### Section 6.9 — Débogage des problèmes mémoire (2 fichiers)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `09-sentinelles-debug.pas` | Pattern sentinelles pour détecter la corruption | Non |
| `09-mode-verbeux.pas` | Mode verbeux pour tracer les opérations | Non |

## Notes

### Directives de compilation

- Les fichiers utilisant `Result` nécessitent `{$mode objfpc}{$H+}` (déjà inclus dans les fichiers concernés)
- Le fichier `08-fuite-detection.pas` est conçu pour être compilé avec `fpc -gh` afin de montrer la détection de fuites mémoire par HeapTrc
- Le fichier `09-mode-verbeux.pas` utilise `uses SysUtils` pour `IntToStr`
- Le fichier `04-tableau-de-pointeurs.pas` utilise `uses SysUtils` pour `IntToStr`

### Programmes interactifs

Les programmes marqués "Interactif" contiennent un `ReadLn` et attendent une entrée utilisateur avant de se terminer :
- `01-exemple-pointeur.pas`
- `06-gestionnaire-liste.pas`
- `07-gestionnaire-abr.pas`

### Corrections apportées aux fichiers .md

1. **`02-declaration-utilisation-pointeurs.md`** : Ajout du type nommé `PInteger = ^Integer` car FreePascal n'autorise pas les types pointeurs anonymes (`^Integer`) dans les paramètres de procédure
2. **`04-pointeurs-tableaux.md`** : Remplacement du type anonyme `^array[1..5] of Integer` par les types nommés `TTableau5` et `PTableau5` car FreePascal n'autorise pas les types pointeur-vers-tableau anonymes dans les déclarations de variables

### Sortie variable

- Les programmes affichant des adresses mémoire (`01-exemple-pointeur.pas`) produisent des valeurs différentes à chaque exécution — c'est normal
