# Chapitre 15 : Composants LCL fondamentaux — Exemples

## Prérequis

- **Lazarus IDE** avec le paquet LCL installé
- Commande `lazbuild` disponible dans le PATH

## Sections sans exemples

- **15.1** Architecture de la LCL (théorie pure, diagrammes)
- **15.2** Hiérarchie des composants (référence des classes et héritage)

## Projets Lazarus (sections 15.3 à 15.9)

Chaque projet est un dossier contenant 4 fichiers : `.lpr`, `.lpi`, `unit1.pas`, `unit1.lfm`.

### Section 15.3 — Conteneurs (TPanel, TGroupBox, TPageControl)

| Dossier | Projet | Description |
|---------|--------|-------------|
| `03-conteneurs/` | DemoConteneurs | PanelTop/Bottom/Client, PageControl 3 onglets, GroupBox avec RadioButtons |

### Section 15.4 — Listes (TListBox, TComboBox, TTreeView)

| Dossier | Projet | Description |
|---------|--------|-------------|
| `04-listes/` | DemoListes | ListBox ajout/suppression, ComboBox cascade pays→ville, TreeView explorateur |

### Section 15.5 — Grilles (TStringGrid, TDrawGrid)

| Dossier | Projet | Description |
|---------|--------|-------------|
| `05-grilles/` | DemoGrilles | Carnet d'adresses TStringGrid avec ajout/suppression de contacts |

### Section 15.6 — Composants de saisie avancés

| Dossier | Projet | Description |
|---------|--------|-------------|
| `06-calculateur-prix/` | CalculateurPrix | TSpinEdit, TFloatSpinEdit, TTrackBar, TDateEdit, TTimeEdit |

### Section 15.7 — Composants d'affichage (TImage, TShape)

| Dossier | Projet | Description |
|---------|--------|-------------|
| `07-shapes-demo/` | ShapesDemo | Feu tricolore TShape, animation pulsante avec TTimer |

### Section 15.8 — Timers et traitement asynchrone

| Dossier | Projet | Description |
|---------|--------|-------------|
| `08-compte-a-rebours/` | CompteARebours | Compte à rebours TTimer + TSpinEdit, démarrer/annuler |

### Section 15.9 — Actions et TActionList

| Dossier | Projet | Description |
|---------|--------|-------------|
| `09-editeur-actions/` | EditeurActions | Éditeur texte avec TActionList, TMainMenu, OnUpdate |

## Compilation

### Compiler un projet individuel

```bash
cd exemples/03-conteneurs  
lazbuild DemoConteneurs.lpi  
```

### Compilation en lot

```bash
cd exemples  
for dir in 03-conteneurs 04-listes 05-grilles 06-calculateur-prix 07-shapes-demo 08-compte-a-rebours 09-editeur-actions; do  
  echo "=== $dir ==="
  (cd "$dir" && lazbuild *.lpi)
done
```

## Nettoyage

```bash
cd exemples  
for dir in 03-conteneurs 04-listes 05-grilles 06-calculateur-prix 07-shapes-demo 08-compte-a-rebours 09-editeur-actions; do  
  rm -rf "$dir/lib" "$dir"/*.res
  # Supprimer l'exécutable (nom sans extension sous Linux)
  find "$dir" -maxdepth 1 -type f -executable -delete
done
```

## Résumé

| Section | Dossier | Fichiers |
|---------|---------|----------|
| 15.3 | `03-conteneurs/` | 4 (lpr + lpi + pas + lfm) |
| 15.4 | `04-listes/` | 4 |
| 15.5 | `05-grilles/` | 4 |
| 15.6 | `06-calculateur-prix/` | 4 |
| 15.7 | `07-shapes-demo/` | 4 |
| 15.8 | `08-compte-a-rebours/` | 4 |
| 15.9 | `09-editeur-actions/` | 4 |
| **Total** | **7 projets** | **28 fichiers** |
