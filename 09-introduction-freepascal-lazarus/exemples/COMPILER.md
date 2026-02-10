# Compilation des exemples - Chapitre 09 : Introduction à FreePascal et Lazarus

## Compilateurs

### Programmes console (fpc)

```bash
fpc fichier.pas
```

### Projets Lazarus GUI (lazbuild)

```bash
lazbuild projet.lpi
```

Versions testées :
- Free Pascal Compiler 3.2.2+dfsg-32 (Linux x86-64)
- Lazarus 3.0 (lazbuild)

## Nettoyage après compilation

### Programmes console

```bash
rm -f *.o *.ppu
# Supprimer les exécutables (même nom que les .pas sans extension)
```

### Projets Lazarus

```bash
rm -rf lib/ *.res NomProjet
```

## Liste des exemples (9 fichiers + 2 projets Lazarus)

### Section 9.1 : Histoire et philosophie du projet FreePascal

Aucun exemple compilable (section théorique).

### Section 9.2 : Différences avec Turbo Pascal

Aucun exemple compilable (fragments illustratifs uniquement).

### Section 9.3 : L'écosystème Lazarus

Aucun exemple compilable (section descriptive).

### Section 9.4 : Installation sur Windows

Aucun exemple compilable (guide d'installation).

### Section 9.5 : Installation sur Ubuntu/Linux

Aucun exemple compilable (guide d'installation).

### Section 9.6 : Premier projet avec Lazarus IDE

| Fichier | Description | Type | Interactif |
|---------|-------------|------|:----------:|
| `06-hello-world-console.pas` | Programme console "Hello World" - premier projet | Console (fpc) | Oui (ReadLn) |
| `06-hello-world-ameliore.pas` | Programme console amélioré avec mise en forme | Console (fpc) | Oui (ReadLn) |
| `06-appli-graphique/` | Première application graphique avec bouton | Lazarus (lazbuild) | Oui (GUI) |
| `06-appli-compteur/` | Application graphique avec compteur de clics | Lazarus (lazbuild) | Oui (GUI) |

### Section 9.7 : Structure d'un projet Lazarus

| Fichier | Description | Type | Interactif |
|---------|-------------|------|:----------:|
| `07-hello-world-sysutils.pas` | Programme console avec SysUtils | Console (fpc) | Oui (ReadLn) |
| `UnitCalculs.pas` | Unité de calculs réutilisable | Unité (fpc) | - |
| `07-test-unit-calculs.pas` | Programme de test pour UnitCalculs | Console (fpc) | Non |

### Section 9.8 : Compilation et exécution

| Fichier | Description | Type | Interactif |
|---------|-------------|------|:----------:|
| `08-parametres-programme.pas` | Affichage des paramètres de ligne de commande | Console (fpc) | Non |
| `08-mesurer-performance.pas` | Mesure du temps d'exécution | Console (fpc) | Non |

### Section 9.9 : Configuration de base de l'IDE

Aucun exemple compilable (guide de configuration).

### Section 9.10 : Utilisation de l'aide et documentation

Aucun exemple compilable (section descriptive).

## Compilation des projets Lazarus

### 06-appli-graphique (MonAppli)

```bash
cd 06-appli-graphique  
lazbuild MonAppli.lpi  
```

### 06-appli-compteur (AppliCompteur)

```bash
cd 06-appli-compteur  
lazbuild AppliCompteur.lpi  
```

## Notes

- Les programmes console utilisent `fpc fichier.pas` pour la compilation.
- Les projets Lazarus (GUI) utilisent `lazbuild projet.lpi` pour la compilation.
- `{$mode objfpc}{$H+}` est utilisé quand SysUtils, Classes ou des exceptions sont nécessaires.
- Les programmes interactifs console (ReadLn) ne peuvent pas être testés automatiquement.
- Les projets Lazarus sont des applications graphiques interactives.
- `UnitCalculs.pas` est une unité compilée automatiquement lors de la compilation de `07-test-unit-calculs.pas`.

## Corrections apportées aux fichiers .md

### Programmes rendus complets (ajout de `program` et/ou `{$mode objfpc}{$H+}`)

1. **`08-compilation-execution.md`** : Ajout de `program ParametresProgramme;` au code des paramètres de ligne de commande.
2. **`08-compilation-execution.md`** : Ajout de `program MesurerPerformance;` et `{$mode objfpc}{$H+}` au code de mesure de performance.
