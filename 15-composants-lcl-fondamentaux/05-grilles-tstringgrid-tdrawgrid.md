🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.5 Grilles (TStringGrid, TDrawGrid)

## Introduction

Les **grilles** (grids en anglais) sont des composants qui affichent des données sous forme de **tableau** avec des lignes et des colonnes. Elles ressemblent aux feuilles de calcul Excel ou aux tableaux HTML.

Dans ce chapitre, nous allons explorer deux composants de grille :

- **TStringGrid** : grille pour afficher du texte (la plus utilisée)
- **TDrawGrid** : grille pour dessiner du contenu personnalisé

---

## Qu'est-ce qu'une Grille ?

### Concept

Une grille est un tableau à deux dimensions composé de **cellules**. Chaque cellule est identifiée par :
- Un numéro de **colonne** (Col)
- Un numéro de **ligne** (Row)

### Représentation Visuelle

```
       Col 0    Col 1    Col 2    Col 3
     ┌────────┬────────┬────────┬────────┐
Row 0│ Nom    │ Prénom │ Âge    │ Ville  │ ← En-tête (ligne fixe)
     ├────────┼────────┼────────┼────────┤
Row 1│ Dupont │ Jean   │ 35     │ Paris  │
     ├────────┼────────┼────────┼────────┤
Row 2│ Martin │ Sophie │ 28     │ Lyon   │
     ├────────┼────────┼────────┼────────┤
Row 3│ Durand │ Pierre │ 42     │ Lille  │
     └────────┴────────┴────────┴────────┘
```

### Analogie

Une grille, c'est comme :
- **Une feuille de calcul** (Excel, Calc)
- **Un tableau de données** (base de données)
- **Un planning** (emploi du temps)
- **Un échiquier** (8×8 cases)

### Pourquoi Utiliser des Grilles ?

✅ **Afficher des données tabulaires** : résultats de base de données, tableaux de chiffres  
✅ **Éditer des données** : saisie structurée dans des cellules  
✅ **Comparer des données** : aligner visuellement des informations  
✅ **Créer des interfaces** : tableaux de bord, calendriers, jeux

---

## TStringGrid : La Grille de Texte

### Présentation

`TStringGrid` est une grille qui affiche et édite du **texte** dans ses cellules. C'est le composant de grille le plus utilisé.

### Hiérarchie

```
TWinControl
  └─ TCustomControl
       └─ TCustomGrid
            └─ TCustomDrawGrid
                 └─ TCustomStringGrid
                      └─ TStringGrid
```

### Propriétés Fondamentales

#### Dimensions de la Grille

```pascal
property ColCount: Integer;    // Nombre de colonnes  
property RowCount: Integer;    // Nombre de lignes  
property FixedCols: Integer;   // Colonnes fixes (en-têtes gauche)  
property FixedRows: Integer;   // Lignes fixes (en-têtes haut)  
```

**Exemple :**

```pascal
StringGrid1.ColCount := 4;      // 4 colonnes  
StringGrid1.RowCount := 10;     // 10 lignes  
StringGrid1.FixedCols := 1;     // 1ère colonne fixe (numéros de ligne)  
StringGrid1.FixedRows := 1;     // 1ère ligne fixe (en-têtes)  
```

#### Taille des Cellules

```pascal
property DefaultColWidth: Integer;   // Largeur par défaut des colonnes  
property DefaultRowHeight: Integer;  // Hauteur par défaut des lignes  
property ColWidths[Col: Integer]: Integer;   // Largeur d'une colonne spécifique  
property RowHeights[Row: Integer]: Integer;  // Hauteur d'une ligne spécifique  
```

**Exemple :**

```pascal
// Largeur par défaut
StringGrid1.DefaultColWidth := 100;  
StringGrid1.DefaultRowHeight := 25;  

// Personnaliser une colonne
StringGrid1.ColWidths[0] := 50;   // 1ère colonne plus étroite  
StringGrid1.ColWidths[1] := 150;  // 2ème colonne plus large  
```

#### Accès aux Cellules

```pascal
property Cells[Col, Row: Integer]: string;  // Contenu d'une cellule
```

**C'est la propriété la plus importante !** Elle permet de lire et écrire dans les cellules.

```pascal
// Écrire dans une cellule
StringGrid1.Cells[0, 0] := 'Nom';  
StringGrid1.Cells[1, 0] := 'Prénom';  

// Lire une cellule
var
  Texte: string;
begin
  Texte := StringGrid1.Cells[1, 2];  // Colonne 1, Ligne 2
end;
```

### Exemple de Base

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration de la grille
  StringGrid1.ColCount := 4;
  StringGrid1.RowCount := 5;
  StringGrid1.FixedCols := 0;
  StringGrid1.FixedRows := 1;

  // En-têtes (ligne 0 = fixe)
  StringGrid1.Cells[0, 0] := 'Nom';
  StringGrid1.Cells[1, 0] := 'Prénom';
  StringGrid1.Cells[2, 0] := 'Âge';
  StringGrid1.Cells[3, 0] := 'Ville';

  // Données (lignes 1-4)
  StringGrid1.Cells[0, 1] := 'Dupont';
  StringGrid1.Cells[1, 1] := 'Jean';
  StringGrid1.Cells[2, 1] := '35';
  StringGrid1.Cells[3, 1] := 'Paris';

  StringGrid1.Cells[0, 2] := 'Martin';
  StringGrid1.Cells[1, 2] := 'Sophie';
  StringGrid1.Cells[2, 2] := '28';
  StringGrid1.Cells[3, 2] := 'Lyon';
end;
```

### Lignes et Colonnes Fixes

Les lignes/colonnes **fixes** sont des zones qui :
- Ne défilent pas (restent visibles)
- Ont une couleur de fond différente
- Servent généralement d'en-têtes

```
┌─────────┬──────────────────────────────┐
│ Fixe    │ Colonnes normales (défilent) │
├─────────┼──────────────────────────────┤
│ Lignes  │                              │
│ Fixes   │     Zone de données          │
│         │     (défile)                 │
└─────────┴──────────────────────────────┘
```

**Configuration typique :**

```pascal
// En-tête de colonnes uniquement
StringGrid1.FixedCols := 0;  
StringGrid1.FixedRows := 1;  

// En-tête de colonnes + numéros de lignes
StringGrid1.FixedCols := 1;  
StringGrid1.FixedRows := 1;  
```

### Sélection

#### Propriétés de Sélection

```pascal
property Col: Integer;         // Colonne sélectionnée  
property Row: Integer;         // Ligne sélectionnée  
property Selection: TGridRect; // Rectangle de sélection (pour sélection multiple)  
```

#### Options de Sélection

```pascal
property Options: TGridOptions;
```

Options importantes dans `Options` :

```pascal
goRowSelect       // Sélectionner la ligne entière  
goRangeSelect     // Permettre la sélection de plages  
goEditing         // Permettre l'édition des cellules  
goTabs            // Touche Tab change de cellule  
goAlwaysShowEditor // Toujours afficher l'éditeur  
```

**Exemple :**

```pascal
// Activer l'édition
StringGrid1.Options := StringGrid1.Options + [goEditing];

// Sélection de lignes complètes
StringGrid1.Options := StringGrid1.Options + [goRowSelect];

// Désactiver l'édition
StringGrid1.Options := StringGrid1.Options - [goEditing];
```

#### Obtenir la Cellule Sélectionnée

```pascal
procedure TForm1.StringGrid1Click(Sender: TObject);  
begin  
  ShowMessage(Format('Cellule [%d, %d] = %s',
    [StringGrid1.Col, StringGrid1.Row,
     StringGrid1.Cells[StringGrid1.Col, StringGrid1.Row]]));
end;
```

#### Sélectionner par Code

```pascal
// Sélectionner une cellule
StringGrid1.Col := 2;  
StringGrid1.Row := 3;  

// Sélectionner une plage
var
  Rect: TGridRect;
begin
  Rect.Left := 1;
  Rect.Top := 1;
  Rect.Right := 3;
  Rect.Bottom := 5;
  StringGrid1.Selection := Rect;
end;
```

### Ajouter/Supprimer des Lignes et Colonnes

#### Ajouter

```pascal
// Augmenter le nombre de lignes
StringGrid1.RowCount := StringGrid1.RowCount + 1;

// Augmenter le nombre de colonnes
StringGrid1.ColCount := StringGrid1.ColCount + 1;
```

#### Insérer

```pascal
// Insérer une ligne à la position 3
StringGrid1.InsertRowWithValues(3, ['Valeur1', 'Valeur2', 'Valeur3']);

// Ou manuellement
StringGrid1.RowCount := StringGrid1.RowCount + 1;
// Décaler les données manuellement si nécessaire
```

#### Supprimer

```pascal
// Supprimer la ligne actuelle
procedure TForm1.SupprimerLigneSelectionnee;  
var  
  i, j: Integer;
begin
  if StringGrid1.Row < StringGrid1.FixedRows then Exit;

  // Décaler les lignes suivantes
  for i := StringGrid1.Row to StringGrid1.RowCount - 2 do
  begin
    for j := 0 to StringGrid1.ColCount - 1 do
      StringGrid1.Cells[j, i] := StringGrid1.Cells[j, i + 1];
  end;

  // Supprimer la dernière ligne
  StringGrid1.RowCount := StringGrid1.RowCount - 1;
end;
```

### Vider la Grille

```pascal
// Vider toutes les cellules
procedure TForm1.ViderGrille;  
var  
  i, j: Integer;
begin
  for i := 0 to StringGrid1.ColCount - 1 do
    for j := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do
      StringGrid1.Cells[i, j] := '';
end;

// Ou réinitialiser complètement
StringGrid1.RowCount := StringGrid1.FixedRows + 1;  // Garde les en-têtes
```

### Parcourir la Grille

```pascal
// Parcourir toutes les cellules
procedure TForm1.ParcoursComplet;  
var  
  Col, Row: Integer;
begin
  for Row := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do
  begin
    for Col := 0 to StringGrid1.ColCount - 1 do
    begin
      // Traiter StringGrid1.Cells[Col, Row]
      Memo1.Lines.Add(Format('[%d,%d] = %s',
        [Col, Row, StringGrid1.Cells[Col, Row]]));
    end;
  end;
end;

// Parcourir une colonne
procedure TForm1.ParcoursColonne(NumCol: Integer);  
var  
  Row: Integer;
begin
  for Row := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do
  begin
    Memo1.Lines.Add(StringGrid1.Cells[NumCol, Row]);
  end;
end;

// Parcourir une ligne
procedure TForm1.ParcoursLigne(NumRow: Integer);  
var  
  Col: Integer;
begin
  for Col := 0 to StringGrid1.ColCount - 1 do
  begin
    Memo1.Lines.Add(StringGrid1.Cells[Col, NumRow]);
  end;
end;
```

### Rechercher dans la Grille

```pascal
function TForm1.RechercherTexte(const Texte: string): Boolean;  
var  
  Col, Row: Integer;
begin
  Result := False;
  for Row := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do
  begin
    for Col := 0 to StringGrid1.ColCount - 1 do
    begin
      if StringGrid1.Cells[Col, Row] = Texte then
      begin
        StringGrid1.Col := Col;
        StringGrid1.Row := Row;
        Result := True;
        Exit;
      end;
    end;
  end;
end;
```

### Événements Importants

```pascal
property OnSelectCell: TSelectCellEvent;    // Avant de sélectionner une cellule  
property OnClick: TNotifyEvent;             // Clic sur la grille  
property OnDblClick: TNotifyEvent;          // Double-clic  
property OnDrawCell: TOnDrawCell;           // Dessiner une cellule personnalisée  
property OnGetEditText: TGetEditEvent;      // Obtenir le texte à éditer  
property OnSetEditText: TSetEditEvent;      // Appliquer le texte édité  
property OnValidateEntry: TValidateEntryEvent;  // Valider la saisie  
```

#### Exemple : Valider la Saisie

```pascal
procedure TForm1.StringGrid1ValidateEntry(Sender: TObject; ACol, ARow: Integer;
  const OldValue: string; var NewValue: string);
begin
  // Colonne 2 = Âge, doit être un nombre
  if ACol = 2 then
  begin
    if not TryStrToInt(NewValue, DummyInt) then
    begin
      ShowMessage('L''âge doit être un nombre !');
      NewValue := OldValue;  // Annule la modification
    end;
  end;
end;
```

#### Exemple : Empêcher la Sélection

```pascal
procedure TForm1.StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  // Empêcher la sélection des cellules en-têtes
  if (ACol = 0) or (ARow = 0) then
    CanSelect := False;
end;
```

### Apparence Personnalisée

#### Couleurs

```pascal
property FixedColor: TColor;    // Couleur des cellules fixes  
property Color: TColor;          // Couleur de fond normale  
property Font: TFont;            // Police  
```

#### Lignes de Grille

```pascal
property GridLineWidth: Integer;  // Épaisseur des lignes
```

### Copier/Coller

Les grilles supportent le copier/coller via le presse-papier :

```pascal
uses
  Clipbrd;

// Copier la sélection dans le presse-papier
procedure TForm1.CopierSelection;  
var  
  i, j: Integer;
  Texte: string;
  Rect: TGridRect;
begin
  Rect := StringGrid1.Selection;
  Texte := '';

  for i := Rect.Top to Rect.Bottom do
  begin
    for j := Rect.Left to Rect.Right do
    begin
      Texte := Texte + StringGrid1.Cells[j, i];
      if j < Rect.Right then
        Texte := Texte + #9;  // Tab entre colonnes
    end;
    Texte := Texte + #13#10;  // Nouvelle ligne
  end;

  Clipboard.AsText := Texte;
end;
```

### Exporter vers CSV

```pascal
procedure TForm1.ExporterCSV(const NomFichier: string);  
var  
  Fichier: TextFile;
  Row, Col: Integer;
  Ligne: string;
begin
  AssignFile(Fichier, NomFichier);
  try
    Rewrite(Fichier);

    // Pour chaque ligne
    for Row := 0 to StringGrid1.RowCount - 1 do
    begin
      Ligne := '';
      // Pour chaque colonne
      for Col := 0 to StringGrid1.ColCount - 1 do
      begin
        Ligne := Ligne + StringGrid1.Cells[Col, Row];
        if Col < StringGrid1.ColCount - 1 then
          Ligne := Ligne + ';';  // Séparateur
      end;
      WriteLn(Fichier, Ligne);
    end;

  finally
    CloseFile(Fichier);
  end;
end;
```

### Importer depuis CSV

```pascal
procedure TForm1.ImporterCSV(const NomFichier: string);  
var  
  Fichier: TextFile;
  Ligne: string;
  Liste: TStringList;
  Row, Col: Integer;
begin
  if not FileExists(NomFichier) then Exit;

  Liste := TStringList.Create;
  try
    AssignFile(Fichier, NomFichier);
    Reset(Fichier);

    Row := 0;
    while not Eof(Fichier) do
    begin
      ReadLn(Fichier, Ligne);

      // Découper la ligne
      Liste.Clear;
      Liste.Delimiter := ';';
      Liste.DelimitedText := Ligne;

      // Ajuster la taille de la grille si nécessaire
      if Row >= StringGrid1.RowCount then
        StringGrid1.RowCount := Row + 1;
      if Liste.Count > StringGrid1.ColCount then
        StringGrid1.ColCount := Liste.Count;

      // Remplir la ligne
      for Col := 0 to Liste.Count - 1 do
        StringGrid1.Cells[Col, Row] := Liste[Col];

      Inc(Row);
    end;

    CloseFile(Fichier);
  finally
    Liste.Free;
  end;
end;
```

---

## TDrawGrid : La Grille Personnalisée

### Présentation

`TDrawGrid` est similaire à `TStringGrid`, mais au lieu de contenir du texte, elle vous laisse **dessiner** le contenu de chaque cellule comme vous le souhaitez.

### Hiérarchie

```
TWinControl
  └─ TCustomControl
       └─ TCustomGrid
            └─ TCustomDrawGrid
                 └─ TDrawGrid
```

### Différence avec TStringGrid

| Caractéristique | TStringGrid | TDrawGrid |
|-----------------|-------------|-----------|
| **Contenu** | Texte automatique | Dessin manuel |
| **Propriété Cells** | ✅ Oui | ❌ Non |
| **OnDrawCell** | Optionnel (surcharge) | **Obligatoire** |
| **Usage** | Données textuelles | Graphiques, couleurs, icônes |

### Pourquoi Utiliser TDrawGrid ?

- Afficher des **couleurs** dans les cellules (palettes)
- Dessiner des **graphiques** (barres, formes)
- Afficher des **images** ou **icônes**
- Créer des **jeux** (échiquier, démineur)
- Visualiser des **données** de manière personnalisée

### Propriétés

TDrawGrid a les mêmes propriétés de base que TStringGrid :

```pascal
property ColCount: Integer;  
property RowCount: Integer;  
property DefaultColWidth: Integer;  
property DefaultRowHeight: Integer;  
property FixedCols: Integer;  
property FixedRows: Integer;  
property Col: Integer;  
property Row: Integer;  
```

Mais **pas** de propriété `Cells` !

### L'Événement OnDrawCell

C'est **l'événement central** de TDrawGrid. Il est appelé pour chaque cellule visible.

```pascal
procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  // Dessiner la cellule [ACol, ARow] dans le rectangle Rect
end;
```

**Paramètres :**

- `ACol, ARow` : position de la cellule à dessiner
- `Rect` : rectangle de la cellule (coordonnées pour dessiner)
- `State` : état de la cellule (sélectionnée, fixe, focusée...)

**État de la cellule (State) :**

```pascal
TGridDrawState = set of (
  gdSelected,   // Cellule sélectionnée
  gdFocused,    // Cellule a le focus
  gdFixed       // Cellule fixe (en-tête)
);
```

### Exemple 1 : Palette de Couleurs

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Grille 8x8 pour palette de couleurs
  DrawGrid1.ColCount := 8;
  DrawGrid1.RowCount := 8;
  DrawGrid1.DefaultColWidth := 40;
  DrawGrid1.DefaultRowHeight := 40;
  DrawGrid1.FixedCols := 0;
  DrawGrid1.FixedRows := 0;
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  Couleur: TColor;
begin
  // Calculer une couleur basée sur la position
  Couleur := RGB(ACol * 32, ARow * 32, (ACol + ARow) * 16);

  // Remplir la cellule avec cette couleur
  DrawGrid1.Canvas.Brush.Color := Couleur;
  DrawGrid1.Canvas.FillRect(Rect);

  // Bordure si sélectionnée
  if gdSelected in State then
  begin
    DrawGrid1.Canvas.Pen.Color := clRed;
    DrawGrid1.Canvas.Pen.Width := 3;
    DrawGrid1.Canvas.Rectangle(Rect);
  end;
end;

procedure TForm1.DrawGrid1Click(Sender: TObject);  
var  
  Couleur: TColor;
begin
  // Afficher la couleur sélectionnée
  Couleur := RGB(DrawGrid1.Col * 32, DrawGrid1.Row * 32,
                 (DrawGrid1.Col + DrawGrid1.Row) * 16);
  Panel1.Color := Couleur;
end;
```

### Exemple 2 : Échiquier

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Grille 8x8 pour échiquier
  DrawGrid1.ColCount := 8;
  DrawGrid1.RowCount := 8;
  DrawGrid1.DefaultColWidth := 50;
  DrawGrid1.DefaultRowHeight := 50;
  DrawGrid1.FixedCols := 0;
  DrawGrid1.FixedRows := 0;
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  EstBlanc: Boolean;
begin
  // Cases blanches et noires alternées
  EstBlanc := (ACol + ARow) mod 2 = 0;

  if EstBlanc then
    DrawGrid1.Canvas.Brush.Color := clWhite
  else
    DrawGrid1.Canvas.Brush.Color := clGray;

  DrawGrid1.Canvas.FillRect(Rect);

  // Bordure de sélection
  if gdSelected in State then
  begin
    DrawGrid1.Canvas.Pen.Color := clYellow;
    DrawGrid1.Canvas.Pen.Width := 3;
    DrawGrid1.Canvas.Rectangle(Rect);
  end;
end;
```

### Exemple 3 : Graphique à Barres

```pascal
type
  TForm1 = class(TForm)
    // ...
  private
    FValeurs: array[0..9] of Integer;
  end;

procedure TForm1.FormCreate(Sender: TObject);  
var  
  i: Integer;
begin
  // Générer des valeurs aléatoires
  Randomize;
  for i := 0 to 9 do
    FValeurs[i] := Random(100);

  // Configuration grille
  DrawGrid1.ColCount := 10;
  DrawGrid1.RowCount := 1;
  DrawGrid1.DefaultColWidth := 50;
  DrawGrid1.DefaultRowHeight := 150;
  DrawGrid1.FixedCols := 0;
  DrawGrid1.FixedRows := 0;
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  Hauteur: Integer;
  BarRect: TRect;
begin
  // Fond blanc
  DrawGrid1.Canvas.Brush.Color := clWhite;
  DrawGrid1.Canvas.FillRect(Rect);

  // Calculer la hauteur de la barre (proportionnelle à la valeur)
  Hauteur := Round(FValeurs[ACol] * (Rect.Bottom - Rect.Top) / 100);

  // Rectangle de la barre (du bas vers le haut)
  BarRect := Rect;
  BarRect.Top := Rect.Bottom - Hauteur;

  // Dessiner la barre
  DrawGrid1.Canvas.Brush.Color := clBlue;
  DrawGrid1.Canvas.FillRect(BarRect);

  // Afficher la valeur
  DrawGrid1.Canvas.Font.Color := clBlack;
  DrawGrid1.Canvas.TextOut(Rect.Left + 5, Rect.Top + 5,
                          IntToStr(FValeurs[ACol]));
end;
```

### Dessiner du Texte dans TDrawGrid

Même si TDrawGrid n'a pas de propriété Cells, vous pouvez dessiner du texte :

```pascal
procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  Texte: string;
  TexteRect: TRect;
begin
  // Fond
  if gdFixed in State then
    DrawGrid1.Canvas.Brush.Color := clBtnFace
  else if gdSelected in State then
    DrawGrid1.Canvas.Brush.Color := clHighlight
  else
    DrawGrid1.Canvas.Brush.Color := clWhite;

  DrawGrid1.Canvas.FillRect(Rect);

  // Texte
  Texte := Format('[%d,%d]', [ACol, ARow]);

  // Centrer le texte
  TexteRect := Rect;
  DrawGrid1.Canvas.TextRect(TexteRect, Texte,
    [tfCenter, tfVerticalCenter, tfSingleLine]);
end;
```

### Associer des Données

Comme TDrawGrid n'a pas de Cells, vous devez stocker vos données séparément :

```pascal
type
  TForm1 = class(TForm)
    // ...
  private
    FData: array of array of string;  // Tableau 2D
  end;

procedure TForm1.InitialiserDonnees;  
var  
  i, j: Integer;
begin
  // Créer le tableau
  SetLength(FData, DrawGrid1.ColCount, DrawGrid1.RowCount);

  // Remplir avec des données
  for i := 0 to DrawGrid1.ColCount - 1 do
    for j := 0 to DrawGrid1.RowCount - 1 do
      FData[i, j] := Format('Cell[%d,%d]', [i, j]);
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  // Dessiner le contenu de FData[ACol, ARow]
  DrawGrid1.Canvas.TextOut(Rect.Left + 5, Rect.Top + 5,
                          FData[ACol, ARow]);
end;
```

---

## Comparaison TStringGrid vs TDrawGrid

| Aspect | TStringGrid | TDrawGrid |
|--------|-------------|-----------|
| **Contenu** | Texte automatique | Dessin manuel |
| **Propriété Cells** | ✅ Oui | ❌ Non |
| **Facilité** | Simple pour texte | Plus complexe |
| **Flexibilité** | Limitée au texte | Totalement libre |
| **Performance** | Bonne | Dépend du code OnDrawCell |
| **Édition** | Native | Doit être codée |
| **Usage typique** | Tableaux de données | Visualisations, jeux |

### Quand Utiliser Quoi ?

#### Utilisez TStringGrid quand :
- Vous affichez principalement du **texte**
- Vous voulez l'**édition** native
- Vous n'avez pas besoin de dessin personnalisé
- Vous voulez un code simple et rapide

**Exemples :** Tableau de données, liste de résultats, feuille de calcul simple

#### Utilisez TDrawGrid quand :
- Vous voulez un **contrôle total** sur l'apparence
- Vous devez afficher des **couleurs**, **graphiques**, **images**
- Vous créez un **jeu** ou une **visualisation**
- L'aspect visuel est plus important que le texte

**Exemples :** Palette de couleurs, échiquier, graphiques, calendrier visuel

---

## Propriétés et Options Communes

### Options de la Grille

```pascal
type
  TGridOption = (
    goFixedVertLine,    // Lignes verticales dans zone fixe
    goFixedHorzLine,    // Lignes horizontales dans zone fixe
    goVertLine,         // Lignes verticales normales
    goHorzLine,         // Lignes horizontales normales
    goRangeSelect,      // Sélection de plages
    goDrawFocusSelected,// Focus visible sur sélection
    goRowSizing,        // Redimensionner les lignes à la souris
    goColSizing,        // Redimensionner les colonnes à la souris
    goRowMoving,        // Déplacer les lignes
    goColMoving,        // Déplacer les colonnes
    goEditing,          // Édition (StringGrid)
    goTabs,             // Tab change de cellule
    goRowSelect,        // Sélectionner ligne entière
    goAlwaysShowEditor, // Éditeur toujours visible
    goThumbTracking,    // Mise à jour pendant redimensionnement
    goColSpanning,      // Fusion de cellules
    goRelaxedRowSelect, // Sélection ligne moins stricte
    goDblClickAutoSize, // Double-clic ajuste taille
    goSmoothScroll      // Défilement fluide
  );
```

**Configuration typique pour tableau de données :**

```pascal
StringGrid1.Options := [
  goFixedVertLine, goFixedHorzLine,  // Lignes fixes
  goVertLine, goHorzLine,             // Toutes les lignes
  goRangeSelect,                      // Sélection multiple
  goColSizing,                        // Redimensionner colonnes
  goEditing,                          // Édition
  goTabs                              // Navigation Tab
];
```

### Défilement

```pascal
property ScrollBars: TScrollStyle;  // ssNone, ssHorizontal, ssVertical, ssBoth, ssAutoHorizontal, ssAutoVertical, ssAutoBoth
```

### Alignement

```pascal
// Pas de propriété native d'alignement dans les cellules
// Doit être fait via OnDrawCell ou en sous-classant
```

---

## Astuces et Techniques Avancées

### 1. Cellules de Hauteurs Variables

```pascal
procedure TForm1.AjusterHauteurLigne(NumLigne: Integer);  
var  
  MaxHauteur: Integer;
  Col: Integer;
begin
  MaxHauteur := 25;  // Minimum

  // Calculer la hauteur nécessaire basée sur le contenu
  for Col := 0 to StringGrid1.ColCount - 1 do
  begin
    // Logique pour calculer hauteur...
  end;

  StringGrid1.RowHeights[NumLigne] := MaxHauteur;
end;
```

### 2. Fusion de Cellules (Col Spanning)

```pascal
// Activer le spanning
StringGrid1.Options := StringGrid1.Options + [goColSpanning];

// Implémenter OnPrepareCanvas
procedure TForm1.StringGrid1PrepareCanvas(Sender: TObject; ACol, ARow: Integer;
  AState: TGridDrawState);
begin
  // Logique pour fusionner des cellules
  // (nécessite un code plus avancé)
end;
```

### 3. Formatage Conditionnel

```pascal
procedure TForm1.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  Valeur: Integer;
begin
  // Appel du dessin par défaut
  StringGrid1.DefaultDrawCell(ACol, ARow, Rect, State);

  // Coloration conditionnelle
  if ACol = 2 then  // Colonne "Âge"
  begin
    if TryStrToInt(StringGrid1.Cells[ACol, ARow], Valeur) then
    begin
      if Valeur < 18 then
        StringGrid1.Canvas.Font.Color := clBlue
      else if Valeur > 65 then
        StringGrid1.Canvas.Font.Color := clRed;
    end;
  end;
end;
```

### 4. Auto-ajustement de Colonnes

```pascal
procedure TForm1.AutoAjusterColonne(NumCol: Integer);  
var  
  Row: Integer;
  MaxLargeur: Integer;
  Largeur: Integer;
begin
  MaxLargeur := 50;  // Minimum

  for Row := 0 to StringGrid1.RowCount - 1 do
  begin
    Largeur := StringGrid1.Canvas.TextWidth(StringGrid1.Cells[NumCol, Row]) + 10;
    if Largeur > MaxLargeur then
      MaxLargeur := Largeur;
  end;

  StringGrid1.ColWidths[NumCol] := MaxLargeur;
end;

// Auto-ajuster toutes les colonnes
procedure TForm1.AutoAjusterTout;  
var  
  i: Integer;
begin
  for i := 0 to StringGrid1.ColCount - 1 do
    AutoAjusterColonne(i);
end;
```

### 5. Tri de Grille

```pascal
procedure TForm1.TrierParColonne(NumCol: Integer; Croissant: Boolean);  
var  
  Liste: TStringList;
  i, j: Integer;
  Ligne: string;
begin
  Liste := TStringList.Create;
  try
    // Copier les lignes dans la liste
    for i := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do
    begin
      Ligne := '';
      for j := 0 to StringGrid1.ColCount - 1 do
      begin
        if j > 0 then Ligne := Ligne + #9;
        Ligne := Ligne + StringGrid1.Cells[j, i];
      end;
      Liste.AddObject(StringGrid1.Cells[NumCol, i], TObject(PtrInt(i)));
    end;

    // Trier
    Liste.Sort;
    if not Croissant then
    begin
      for i := 0 to Liste.Count div 2 - 1 do
        Liste.Exchange(i, Liste.Count - 1 - i);
    end;

    // Réappliquer dans la grille
    // (code de réorganisation...)

  finally
    Liste.Free;
  end;
end;
```

---

## Bonnes Pratiques

### 1. Initialiser Correctement

```pascal
// ✅ BON : Définir les dimensions d'abord
StringGrid1.ColCount := 5;  
StringGrid1.RowCount := 10;  
StringGrid1.FixedRows := 1;  
// Puis remplir les cellules

// ❌ MAUVAIS : Remplir sans définir les dimensions
StringGrid1.Cells[10, 20] := 'Test';  // Exception si pas assez de lignes/colonnes !
```

### 2. Vérifier les Indices

```pascal
// ✅ BON
if (ACol >= 0) and (ACol < StringGrid1.ColCount) and
   (ARow >= 0) and (ARow < StringGrid1.RowCount) then
begin
  StringGrid1.Cells[ACol, ARow] := 'Valeur';
end;

// ❌ MAUVAIS : Accès sans vérification
StringGrid1.Cells[ACol, ARow] := 'Valeur';  // Peut crasher !
```

### 3. Performances avec BeginUpdate/EndUpdate

```pascal
// ✅ BON : Pour beaucoup de modifications
StringGrid1.BeginUpdate;  
try  
  for i := 0 to 1000 do
    for j := 0 to 10 do
      StringGrid1.Cells[j, i] := Format('Cell[%d,%d]', [j, i]);
finally
  StringGrid1.EndUpdate;
end;
```

### 4. Utiliser DefaultDrawCell

Dans OnDrawCell, appelez `DefaultDrawCell` pour le dessin de base :

```pascal
procedure TForm1.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  // Dessin par défaut
  StringGrid1.DefaultDrawCell(ACol, ARow, Rect, State);

  // Ajouts personnalisés
  if ACol = 2 then
    StringGrid1.Canvas.Font.Style := [fsBold];
end;
```

### 5. Libérer les Objets Associés

Si vous utilisez Objects pour associer des données :

```pascal
// ✅ BON : Libérer avant de détruire
for i := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do  
begin  
  if Assigned(StringGrid1.Objects[0, i]) then
    StringGrid1.Objects[0, i].Free;
end;
```

---

## Exemple Complet : Carnet d'Adresses

```pascal
type
  TContact = class
    Nom: string;
    Prenom: string;
    Telephone: string;
    Email: string;
  end;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration grille
  StringGrid1.ColCount := 4;
  StringGrid1.RowCount := 1;
  StringGrid1.FixedRows := 1;
  StringGrid1.FixedCols := 0;

  // En-têtes
  StringGrid1.Cells[0, 0] := 'Nom';
  StringGrid1.Cells[1, 0] := 'Prénom';
  StringGrid1.Cells[2, 0] := 'Téléphone';
  StringGrid1.Cells[3, 0] := 'Email';

  // Largeurs colonnes
  StringGrid1.ColWidths[0] := 100;
  StringGrid1.ColWidths[1] := 100;
  StringGrid1.ColWidths[2] := 120;
  StringGrid1.ColWidths[3] := 200;

  // Options
  StringGrid1.Options := [goFixedVertLine, goFixedHorzLine,
                         goVertLine, goHorzLine,
                         goRowSelect, goColSizing];
end;

procedure TForm1.BtnAjouterClick(Sender: TObject);  
var  
  Contact: TContact;
  Ligne: Integer;
begin
  // Validation
  if (EditNom.Text = '') or (EditPrenom.Text = '') then
  begin
    ShowMessage('Nom et prénom obligatoires');
    Exit;
  end;

  // Créer contact
  Contact := TContact.Create;
  Contact.Nom := EditNom.Text;
  Contact.Prenom := EditPrenom.Text;
  Contact.Telephone := EditTelephone.Text;
  Contact.Email := EditEmail.Text;

  // Ajouter ligne
  Ligne := StringGrid1.RowCount;
  StringGrid1.RowCount := Ligne + 1;

  // Remplir
  StringGrid1.Cells[0, Ligne] := Contact.Nom;
  StringGrid1.Cells[1, Ligne] := Contact.Prenom;
  StringGrid1.Cells[2, Ligne] := Contact.Telephone;
  StringGrid1.Cells[3, Ligne] := Contact.Email;
  StringGrid1.Objects[0, Ligne] := Contact;

  // Effacer les champs
  EditNom.Clear;
  EditPrenom.Clear;
  EditTelephone.Clear;
  EditEmail.Clear;
  EditNom.SetFocus;
end;

procedure TForm1.BtnSupprimerClick(Sender: TObject);  
var  
  Ligne: Integer;
begin
  Ligne := StringGrid1.Row;
  if Ligne < StringGrid1.FixedRows then Exit;

  // Libérer l'objet associé
  if Assigned(StringGrid1.Objects[0, Ligne]) then
    StringGrid1.Objects[0, Ligne].Free;

  // Supprimer la ligne (décaler les suivantes)
  StringGrid1.DeleteRow(Ligne);
end;

procedure TForm1.FormDestroy(Sender: TObject);  
var  
  i: Integer;
begin
  // Libérer tous les contacts
  for i := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do
  begin
    if Assigned(StringGrid1.Objects[0, i]) then
      StringGrid1.Objects[0, i].Free;
  end;
end;
```

---

## Points Clés à Retenir

1. **TStringGrid** : grille de texte, la plus utilisée
   - Propriété `Cells[Col, Row]` pour accéder aux cellules
   - `ColCount` et `RowCount` définissent les dimensions
   - `FixedRows` et `FixedCols` pour les en-têtes

2. **TDrawGrid** : grille personnalisée avec dessin manuel
   - Événement `OnDrawCell` obligatoire
   - Contrôle total sur l'apparence
   - Doit gérer ses propres données (pas de Cells)

3. **Indices** : toujours de 0 à Count-1
   - Vérifier les limites avant d'accéder

4. **Options** : ensemble configurant le comportement
   - goEditing, goRowSelect, goColSizing, etc.

5. **Performances** : utiliser BeginUpdate/EndUpdate

6. **Objets associés** : penser à libérer la mémoire

7. **Exportation** : facile vers CSV ou autres formats

8. **Choisir le bon composant** selon vos besoins
   - TStringGrid pour données textuelles
   - TDrawGrid pour visualisations personnalisées

---

## Conclusion

Les grilles sont des composants puissants pour afficher des données tabulaires. TStringGrid couvre la majorité des besoins avec sa simplicité, tandis que TDrawGrid offre une flexibilité totale pour des cas spéciaux.

Maîtriser ces composants vous permet de créer :
- Des tableaux de données professionnels
- Des interfaces de saisie structurées
- Des visualisations personnalisées
- Des jeux et applications graphiques

Dans la section suivante, nous explorerons les **composants de saisie avancés** pour enrichir encore vos interfaces.

---

**Prochaine étape :** 15.6 Composants de saisie avancés

⏭️ [Composants de saisie avancés](/15-composants-lcl-fondamentaux/06-composants-saisie-avances.md)
