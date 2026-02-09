# Compilation des exemples - Chapitre 20

## Prérequis
- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure

## Liste des fichiers (17 fichiers : 13 programs + 2 units de support + 2 units)

### Section 20.1 — Premiers pas avec le débogueur
| Fichier | Type | Description |
|---------|------|-------------|
| `01-demo-debogage.pas` | program | Programme de démonstration pour pratiquer le débogueur (pile d'appels, variables, exceptions) |

### Section 20.2 — Points d'arrêt conditionnels
| Fichier | Type | Description |
|---------|------|-------------|
| `02-demo-points-arret.pas` | program | Traitement de 1000 commandes avec anomalies injectées pour pratiquer les breakpoints conditionnels |

### Section 20.3 — Inspection de variables et expressions
| Fichier | Type | Description |
|---------|------|-------------|
| `03-demo-inspection.pas` | program | Structures complexes (records, tableaux, objets, expressions) pour pratiquer l'inspection |

### Section 20.4 — Profiling basique
| Fichier | Type | Description |
|---------|------|-------------|
| `04-mesure-performance.pas` | program | Mesure du temps d'exécution avec GetTickCount64 |
| `04-comparaison-algorithmes.pas` | program | Comparaison boucle O(n) vs formule O(1) |
| `04-tableau-profiling.pas` | program | Rapport de profiling avec tableau formaté et pourcentages |

### Section 20.5 — Optimisation des algorithmes courants
| Fichier | Type | Description |
|---------|------|-------------|
| `05-comparaison-tris.pas` | program | Comparaison tri à bulles O(n²) vs tri rapide O(n log n) |
| `05-recherche-lin-vs-bin.pas` | program | Comparaison recherche linéaire O(n) vs binaire O(log n) sur 1M éléments |

### Section 20.6 — Gestion efficace de la mémoire
| Fichier | Type | Description |
|---------|------|-------------|
| `06-test-fuite.pas` | program | Détection de fuite mémoire avec HeapTrc |
| `06-gestion-memoire-patterns.pas` | program | 6 patterns de gestion mémoire (New/Dispose, Create/Free, FreeAndNil, etc.) |

### Section 20.7 — Outils de détection des fuites mémoire
| Fichier | Type | Description |
|---------|------|-------------|
| `07-test-fuite-simple.pas` | program | Test HeapTrc avec fuite et sans fuite |
| `07-test-fuites-multiples.pas` | program | Détection de fuites multiples avec HeapTrc |

### Section 20.8 — Logging structuré et niveaux de log
| Fichier | Type | Description |
|---------|------|-------------|
| `SimpleLogger.pas` | unit | Unité de logging avec niveaux (Debug, Info, Warning, Error, Fatal) |
| `08-test-logger.pas` | program | Démonstration du logger avec niveaux de log |

### Section 20.9 — Tests unitaires avec FPCUnit
| Fichier | Type | Description |
|---------|------|-------------|
| `Calculatrice.pas` | unit | Unité Calculatrice à tester avec FPCUnit |
| `TestCalculatrice.pas` | unit | Tests unitaires pour l'unité Calculatrice |
| `09-all-tests.pas` | program | Programme principal exécutant tous les tests unitaires |

## Compilation

### Programmes indépendants (sections 20.1 à 20.7)
```bash
fpc 01-demo-debogage.pas
fpc 02-demo-points-arret.pas
fpc 03-demo-inspection.pas
fpc 04-mesure-performance.pas
fpc 04-comparaison-algorithmes.pas
fpc 04-tableau-profiling.pas
fpc 05-comparaison-tris.pas
fpc 05-recherche-lin-vs-bin.pas
fpc 06-test-fuite.pas
fpc 06-gestion-memoire-patterns.pas
fpc 07-test-fuite-simple.pas
fpc 07-test-fuites-multiples.pas
```

### Logger (section 20.8) — compiler l'unité d'abord
```bash
fpc SimpleLogger.pas
fpc 08-test-logger.pas
```

### Tests unitaires (section 20.9) — compiler le programme principal
```bash
fpc 09-all-tests.pas
```
(Compile automatiquement Calculatrice.pas et TestCalculatrice.pas)

## Compilation en lot
```bash
for f in 01-demo-debogage 02-demo-points-arret 03-demo-inspection \
         04-mesure-performance 04-comparaison-algorithmes 04-tableau-profiling \
         05-comparaison-tris 05-recherche-lin-vs-bin \
         06-test-fuite 06-gestion-memoire-patterns \
         07-test-fuite-simple 07-test-fuites-multiples \
         08-test-logger 09-all-tests; do
  echo "Compilation de $f.pas..." && fpc "$f.pas"
done
```

## Exécution

```bash
./01-demo-debogage
./02-demo-points-arret
./03-demo-inspection
./04-mesure-performance
./04-comparaison-algorithmes
./04-tableau-profiling
./05-comparaison-tris
./05-recherche-lin-vs-bin
./06-test-fuite
./06-gestion-memoire-patterns
./07-test-fuite-simple
./07-test-fuites-multiples
./08-test-logger
./09-all-tests --all --format=plain
```

## Notes

- Les programmes 01 à 03 sont conçus pour être ouverts dans Lazarus IDE afin de pratiquer le débogueur (points d'arrêt, inspection, pile d'appels), mais ils fonctionnent aussi en ligne de commande
- Les programmes HeapTrc (06, 07) génèrent des fichiers `.log` dans le répertoire courant
- Le programme 07-test-fuites-multiples crée un fichier `test_fuite2.txt`
- Le programme 08-test-logger crée un fichier `application.log`
- Le programme 08-test-logger a un comportement partiellement aléatoire (connexion BDD)

## Nettoyage
```bash
rm -f *.o *.ppu
rm -f 01-demo-debogage 02-demo-points-arret 03-demo-inspection
rm -f 04-mesure-performance 04-comparaison-algorithmes 04-tableau-profiling
rm -f 05-comparaison-tris 05-recherche-lin-vs-bin
rm -f 06-test-fuite 06-gestion-memoire-patterns
rm -f 07-test-fuite-simple 07-test-fuites-multiples
rm -f 08-test-logger 09-all-tests
rm -f heaptrc.log rapport.log fuites_multiples.log application.log test_fuite2.txt
```
