# Compilation des exemples - Chapitre 14 : Introduction aux applications graphiques

## Prérequis
- Free Pascal Compiler (fpc) version 3.2.2 ou supérieure
- Lazarus IDE 3.0 (lazbuild en ligne de commande)

## Liste des exemples (7 projets Lazarus = 28 fichiers)

### Section 14.1 — Programmation événementielle : concepts

Aucun exemple compilable (section théorique).

### Section 14.2 — Première application fenêtrée

| Projet | Description |
|--------|-------------|
| `02-premiere-fenetre/` | Squelette minimal d'une application Lazarus (fenêtre vide) |

Fichiers : `PremiereFenetre.lpr`, `PremiereFenetre.lpi`, `unit1.pas`, `unit1.lfm`

### Section 14.3 — Formulaires (TForm)

| Projet | Description |
|--------|-------------|
| `03-demo-form-events/` | Événements TForm : OnCreate, OnResize, OnCloseQuery, OnKeyDown |

Fichiers : `DemoFormEvents.lpr`, `DemoFormEvents.lpi`, `unit1.pas`, `unit1.lfm`
Composants : TLabel, TStatusBar (3 panneaux affichant taille et position)

### Section 14.4 — Composants de base (TButton, TEdit, TLabel)

| Projet | Description |
|--------|-------------|
| `04-formulaire-validation/` | Formulaire de validation avec nom, email et âge |

Fichiers : `FormulaireValidation.lpr`, `FormulaireValidation.lpi`, `unit1.pas`, `unit1.lfm`
Composants : 3× TLabel, 3× TEdit, 2× TButton (Valider/Annuler)

### Section 14.5 — Événements et handlers

| Projet | Description |
|--------|-------------|
| `05-demo-evenements/` | Handlers partagés, Tag pour identification, assignation dynamique |

Fichiers : `DemoEvenements.lpr`, `DemoEvenements.lpi`, `unit1.pas`, `unit1.lfm`
Composants : 3× TGroupBox, 6× TButton, 3× TLabel, 1× TCheckBox

### Section 14.6 — Propriétés des composants

Aucun exemple compilable (section de référence encyclopédique ; les propriétés sont démontrées dans tous les autres projets).

### Section 14.7 — Layouts et anchors

| Projet | Description |
|--------|-------------|
| `07-demo-anchors/` | Interface de recherche avec Anchors (redimensionnement adaptatif) |

Fichiers : `DemoAnchors.lpr`, `DemoAnchors.lpi`, `unit1.pas`, `unit1.lfm`
Composants : TLabel, TEdit, TMemo, 3× TButton avec Anchors variés

### Section 14.8 — Menus et barres d'outils

| Projet | Description |
|--------|-------------|
| `08-popup-menu/` | TMemo avec TMainMenu et TPopupMenu (Couper/Copier/Coller/Supprimer) |

Fichiers : `PopupMenuDemo.lpr`, `PopupMenuDemo.lpi`, `unit1.pas`, `unit1.lfm`
Composants : TMemo, TMainMenu (Fichier + Édition), TPopupMenu (6 items)

### Section 14.9 — Boîtes de dialogue standard

| Projet | Description |
|--------|-------------|
| `09-editeur-dialogues/` | Éditeur de texte complet avec toutes les boîtes de dialogue standard |

Fichiers : `EditeurDialogues.lpr`, `EditeurDialogues.lpi`, `unit1.pas`, `unit1.lfm`
Composants : TMemo, TMainMenu (Fichier + Format + Recherche), TOpenDialog, TSaveDialog, TColorDialog, TFontDialog, TFindDialog, TReplaceDialog

## Compilation

### Projets Lazarus (lazbuild)

```bash
cd 02-premiere-fenetre && lazbuild PremiereFenetre.lpi && cd ..
cd 03-demo-form-events && lazbuild DemoFormEvents.lpi && cd ..
cd 04-formulaire-validation && lazbuild FormulaireValidation.lpi && cd ..
cd 05-demo-evenements && lazbuild DemoEvenements.lpi && cd ..
cd 07-demo-anchors && lazbuild DemoAnchors.lpi && cd ..
cd 08-popup-menu && lazbuild PopupMenuDemo.lpi && cd ..
cd 09-editeur-dialogues && lazbuild EditeurDialogues.lpi && cd ..
```

### Compilation en lot

```bash
for dir in 02-premiere-fenetre 03-demo-form-events 04-formulaire-validation \
           05-demo-evenements 07-demo-anchors 08-popup-menu 09-editeur-dialogues; do
  echo "=== $dir ===" && cd "$dir" && lazbuild *.lpi && cd ..
done
```

## Notes

- Tous les projets sont des applications graphiques Lazarus (GUI) nécessitant `lazbuild`
- Aucun programme console dans ce chapitre (100% GUI)
- Les sections 14.1 et 14.6 sont purement théoriques/encyclopédiques
- Le projet 03 utilise `KeyPreview = True` pour capturer les touches au niveau du formulaire
- Le projet 05 démontre `@Handler` (assignation de méthode) en `{$mode objfpc}`
- Le projet 07 est conçu pour être redimensionné — testez en tirant les bords de la fenêtre
- Le projet 08 utilise `Clipbrd` pour vérifier le contenu du presse-papiers
- Le projet 09 est le plus complet : éditeur avec ouverture/sauvegarde, police, couleur, recherche/remplacement et confirmation avant fermeture

## Nettoyage

```bash
for dir in 02-premiere-fenetre 03-demo-form-events 04-formulaire-validation \
           05-demo-evenements 07-demo-anchors 08-popup-menu 09-editeur-dialogues; do
  rm -rf "$dir/lib" "$dir"/*.res
  # Supprimer l'exécutable (même nom que le .lpr sans extension)
  lpr=$(ls "$dir"/*.lpr 2>/dev/null | head -1)
  [ -n "$lpr" ] && rm -f "$dir/$(basename "$lpr" .lpr)"
done
```
