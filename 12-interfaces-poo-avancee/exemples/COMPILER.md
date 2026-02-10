# Chapitre 12 : Interfaces et POO avancée — Compilation des exemples

## Prérequis
- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure

## Compilation individuelle

L'unité `UNotifications.pas` doit être compilée **avant** le programme `02-test-notifications.pas`.

```bash
fpc UNotifications.pas  
fpc 01-exemple-interface.pas  
fpc 02-test-notifications.pas  
fpc 04-comptage-references.pas  
fpc 05-cycle-de-vie.pas  
```

## Compilation de tous les exemples

```bash
fpc UNotifications.pas && for f in [0-9]*.pas; do echo "=== $f ===" && fpc "$f"; done
```

## Nettoyage

```bash
rm -f *.o *.ppu  
rm -f 01-exemple-interface 02-test-notifications 04-comptage-references 05-cycle-de-vie  
```

## Liste des exemples (5 fichiers)

| Fichier | Section | Description |
|---------|---------|-------------|
| `01-exemple-interface.pas` | 12.1 | Exemple simple d'interface IVolant avec TOiseau et TAvion |
| `UNotifications.pas` | 12.2 | Unité de notifications avec interface INotificateur |
| `02-test-notifications.pas` | 12.2 | Programme de test utilisant l'unité UNotifications |
| `04-comptage-references.pas` | 12.4 | Démonstration du comptage de références avec RefCount |
| `05-cycle-de-vie.pas` | 12.5 | Cycle de vie complet d'un objet interface avec compteur |

## Notes

- Tous les programmes sont non-interactifs (pas de ReadLn)
- `02-test-notifications.pas` dépend de l'unité `UNotifications.pas` (doit être compilée en premier)
- Les sections 12.3, 12.6, 12.7, 12.8, 12.9 et 12.10 ne contiennent que des fragments de code (pas de programmes complets)
- Le .md de la section 12.5 a été corrigé : le cast `TMessagerie(Ref1)` (interface vers classe) provoquait un crash en FPC — remplacé par une variable objet séparée
