# Chapitre 11 : POO avancée - Héritage — Compilation des exemples

## Prérequis
- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure

## Compilation individuelle

```bash
fpc 01-exemple-heritage.pas
fpc 02-gestion-personnes.pas
fpc 03-redefinition-methodes.pas
fpc 03-formes-geometriques.pas
fpc 04-liaison-dynamique.pas
fpc 04-systeme-paiement.pas
fpc 05-formes-abstraites.pas
fpc 05-systeme-persistence.pas
fpc 06-zoo-polymorphe.pas
fpc 06-systeme-fichiers.pas
fpc 07-transtypage-zoo.pas
fpc 08-heritage-constructeurs.pas
fpc 08-systeme-logging.pas
fpc 09-hierarchie-personnel.pas
fpc 10-demo-classname.pas
fpc 10-demo-inheritsfrom.pas
fpc 10-demo-tobject.pas
```

## Compilation de tous les exemples

```bash
for f in *.pas; do echo "=== $f ===" && fpc "$f"; done
```

## Nettoyage

```bash
rm -f *.o *.ppu
rm -f 01-exemple-heritage 02-gestion-personnes 03-redefinition-methodes
rm -f 03-formes-geometriques 04-liaison-dynamique 04-systeme-paiement
rm -f 05-formes-abstraites 05-systeme-persistence 06-zoo-polymorphe
rm -f 06-systeme-fichiers 07-transtypage-zoo 08-heritage-constructeurs
rm -f 08-systeme-logging 09-hierarchie-personnel
rm -f 10-demo-classname 10-demo-inheritsfrom 10-demo-tobject
rm -f donnees.txt
```

## Liste des exemples (17 fichiers)

| Fichier | Section | Description |
|---------|---------|-------------|
| `01-exemple-heritage.pas` | 11.1 | Concept d'héritage (TAnimal, TChien, TChat) |
| `02-gestion-personnes.pas` | 11.2 | Classes dérivées (TPersonne, TEmploye, TClient) |
| `03-redefinition-methodes.pas` | 11.3 | Masquage vs redéfinition de méthodes |
| `03-formes-geometriques.pas` | 11.3 | Formes géométriques avec redéfinition |
| `04-liaison-dynamique.pas` | 11.4 | Méthodes virtuelles et liaison dynamique (véhicules) |
| `04-systeme-paiement.pas` | 11.4 | Système de paiement polymorphe |
| `05-formes-abstraites.pas` | 11.5 | Classes abstraites avec formes géométriques |
| `05-systeme-persistence.pas` | 11.5 | Système de persistence abstrait (fichier et mémoire) |
| `06-zoo-polymorphe.pas` | 11.6 | Zoo polymorphe avec hiérarchie d'animaux |
| `06-systeme-fichiers.pas` | 11.6 | Système de fichiers polymorphe (TFichier, TDossier) |
| `07-transtypage-zoo.pas` | 11.7 | Transtypage (is/as) avec zoo |
| `08-heritage-constructeurs.pas` | 11.8 | Inherited avec TPersonne/TEmploye/TManager |
| `08-systeme-logging.pas` | 11.8 | Système de logging hiérarchique avec inherited |
| `09-hierarchie-personnel.pas` | 11.9 | Hiérarchie complète de personnel |
| `10-demo-classname.pas` | 11.10 | Démonstration de ClassName |
| `10-demo-inheritsfrom.pas` | 11.10 | Démonstration de InheritsFrom |
| `10-demo-tobject.pas` | 11.10 | Exemple complet des méthodes de TObject |

## Notes

- La plupart des programmes sont interactifs (attendent Entrée pour quitter)
- `05-systeme-persistence.pas` crée un fichier `donnees.txt` à l'exécution
- `02-gestion-personnes.pas`, `08-systeme-logging.pas`, `09-hierarchie-personnel.pas` et `10-demo-tobject.pas` ont des sorties dépendantes de la date/heure
- `07-transtypage-zoo.pas` génère un avertissement de compilation (variable `Oiseau` assignée mais non utilisée) — c'est volontaire, le code provient du .md
- `10-demo-classname.pas` et `10-demo-inheritsfrom.pas` sont non-interactifs (pas de ReadLn)
