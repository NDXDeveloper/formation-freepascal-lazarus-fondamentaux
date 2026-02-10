# Compilation des exemples du chapitre 04 - Procédures et Fonctions

## Prérequis

- **Free Pascal Compiler (fpc)** version 3.2.2 ou supérieure
- Installation sous Linux (Debian/Ubuntu) : `sudo apt install fp-compiler`

## Compiler un exemple

```bash
fpc nom-du-fichier.pas
```

Exemple :
```bash
fpc 01-exemple-procedure.pas
```

Cela génère un exécutable du même nom (sans extension) et un fichier `.o` (objet).

## Exécuter un exemple compilé

```bash
./nom-du-fichier
```

Exemple :
```bash
./01-exemple-procedure
```

## Compiler et exécuter en une commande

```bash
fpc 01-exemple-procedure.pas && ./01-exemple-procedure
```

## Compiler tous les exemples d'un coup

```bash
for f in *.pas; do echo "=== Compilation de $f ===" && fpc "$f" 2>&1; done
```

## Nettoyer les fichiers générés (binaires et .o)

```bash
rm -f *.o  
find . -maxdepth 1 -type f ! -name "*.pas" ! -name "*.md" -executable -delete  
```

## Liste des exemples

### Section 4.1 - Différence entre procédure et fonction

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `01-exemple-procedure.pas` | Procédure simple qui affiche un message | Non |
| `01-exemple-fonction.pas` | Fonction qui calcule le carré d'un nombre | Non |
| `01-procedure-vs-fonction.pas` | Exemple combinant procédure et fonction pour tester la parité | Oui |

### Section 4.2 - Déclaration et appel

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `02-exemple-declaration.pas` | Procédure simple avec déclaration et appel | Non |
| `02-exemple-fonction-result.pas` | Fonction simple avec Result et affectation | Non |
| `02-appel-procedure.pas` | Appel multiple de procédures | Non |
| `02-appel-fonction.pas` | Appel de fonction dans une affectation | Non |
| `02-ordre-declaration.pas` | Ordre correct de déclaration des procédures | Non |
| `02-avec-forward.pas` | Déclaration anticipée (forward) de procédure | Non |
| `02-exemple-complet.pas` | Exemple complet combinant procédures et fonctions | Non |

### Section 4.3 - Paramètres par valeur

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `03-exemple-parametre.pas` | Procédure simple avec un paramètre String | Non |
| `03-demonstration-valeur.pas` | Démonstration que le paramètre par valeur est une copie | Non |
| `03-gestion-notes.pas` | Exemple complet de gestion de notes avec fonctions et procédures | Non |

### Section 4.4 - Paramètres par référence (var)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `04-gestion-coordonnees.pas` | Système de coordonnées avec paramètres var | Non |

### Section 4.5 - Paramètres constants (const)

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `05-comparaison-parametres.pas` | Comparaison des trois types de paramètres (valeur, var, const) | Non |
| `05-gestion-messages.pas` | Système de gestion de messages avec const, var et Result | Non |

### Section 4.6 - Paramètres par défaut

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `06-systeme-notification.pas` | Système de notification avec paramètres optionnels | Non |

### Section 4.7 - Surcharge de procédures/fonctions

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `07-calculatrice.pas` | Calculatrice avec fonctions surchargées (overload) | Non |

### Section 4.8 - Variables locales vs globales

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `08-exemple-variables.pas` | Exemple simple de variable globale avec procédure et fonction | Non |
| `08-compteur-global.pas` | Variable globale compteur partagée entre procédures | Non |
| `08-melange-variables.pas` | Mélange de variables locales et globales | Non |
| `08-masquage.pas` | Masquage (shadowing) d'une variable globale par une locale | Non |
| `08-demonstration-portee.pas` | Démonstration de la portée des variables locales et globales | Non |
| `08-gestion-compte.pas` | Gestion d'un compte bancaire avec variables globales et locales | Non |

### Section 4.9 - Récursivité

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `09-tours-hanoi.pas` | Tours de Hanoï - exemple classique de récursivité | Non |

### Section 4.10 - Fonctions prédéfinies utiles

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `10-mini-calculatrice.pas` | Mini-calculatrice avec fonctions prédéfinies (Math, SysUtils) | Oui |

### Section 4.11 - Organisation modulaire du code

| Fichier | Description | Interactif |
|---------|-------------|:----------:|
| `11-code-monolithique.pas` | Exemple de mauvais code monolithique (anti-pattern) | Oui |
| `11-code-modulaire.pas` | Version bien organisée en modules (bonnes pratiques) | Oui |
| `11-programme-structure.pas` | Programme structuré avec sections logiques et gestion utilisateurs | Oui |
| `11-bibliotheque.pas` | Système complet de gestion de bibliothèque modulaire | Oui |

## Notes

- Les exemples marqués **Interactif = Oui** attendent une saisie utilisateur (`ReadLn`).
- Certains fichiers utilisent la directive `{$mode objfpc}{$H+}` pour activer le mot-clé `Result` dans les fonctions.
- Les exemples de la section 4.11 marqués "Interactif" contiennent des boucles avec menu ; tapez `0` ou `3` pour quitter.
- Total : **30 exemples** compilables.
