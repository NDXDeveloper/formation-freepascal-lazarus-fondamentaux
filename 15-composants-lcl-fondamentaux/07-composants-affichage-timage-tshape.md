🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.7 Composants d'affichage (TImage, TShape)

## Introduction

Les **composants d'affichage** permettent de présenter visuellement des informations non-textuelles. Ils enrichissent l'interface utilisateur en ajoutant des éléments graphiques : images, icônes, formes géométriques, séparateurs visuels, etc.

Dans ce chapitre, nous explorerons deux composants fondamentaux :

- **TImage** : affichage et manipulation d'images (JPEG, PNG, BMP, etc.)
- **TShape** : formes géométriques simples (rectangles, cercles, lignes)

Ces composants sont essentiels pour créer des interfaces visuellement attractives et professionnelles.

---

## Pourquoi Utiliser des Composants d'Affichage ?

### Sans Composants d'Affichage

Une interface uniquement textuelle peut sembler :
- Austère et peu engageante
- Difficile à scanner visuellement
- Moins intuitive

### Avec Composants d'Affichage

✅ **Identité visuelle** : logos, bannières  
✅ **Communication rapide** : icônes universelles  
✅ **Séparation visuelle** : lignes, cadres  
✅ **Feedback visuel** : indicateurs colorés  
✅ **Attractivité** : interfaces modernes

---

## TImage : Le Composant d'Affichage d'Images

### Présentation

`TImage` est un composant qui affiche des **images** dans divers formats. Il peut charger, afficher et manipuler des images bitmap, JPEG, PNG, GIF, et autres.

### Hiérarchie

```
TControl
  └─ TGraphicControl
       └─ TImage
```

**Important :** TImage hérite de `TGraphicControl` (branche parallèle à `TWinControl`, les deux héritant de `TControl`), donc :
- Pas de handle système (léger en ressources)
- Ne peut pas recevoir le focus
- Ne peut pas contenir d'autres composants

### Apparence Visuelle

```
┌────────────────────────┐
│                        │
│     [Image affichée]   │
│                        │
│                        │
└────────────────────────┘
```

### Propriétés Principales

#### La Propriété Picture

C'est **la propriété centrale** de TImage :

```pascal
property Picture: TPicture;  // Contient l'image
```

`TPicture` est un conteneur qui peut stocker différents types d'images :
- **Bitmap** (BMP)
- **JPEG** (JPG)
- **PNG**
- **GIF**
- **Icône** (ICO)
- Et autres formats supportés

#### Propriétés de Mise en Page

```pascal
property Stretch: Boolean;          // Étirer l'image pour remplir le composant
property Proportional: Boolean;     // Conserver les proportions lors de l'étirement
property Center: Boolean;           // Centrer l'image dans le composant
property Transparent: Boolean;      // Transparence (si supportée par l'image)
```

#### Propriétés Visuelles

```pascal
property Width: Integer;    // Largeur du composant
property Height: Integer;   // Hauteur du composant
```

### Charger une Image

Il existe plusieurs façons de charger une image dans un TImage.

#### Méthode 1 : Dans l'IDE (au Design)

1. Cliquez sur le TImage dans votre formulaire
2. Dans l'Inspecteur d'Objets, trouvez la propriété `Picture`
3. Cliquez sur les points de suspension **[...]**
4. Cliquez sur "Charger" (Load)
5. Sélectionnez votre fichier image

#### Méthode 2 : Par Code - LoadFromFile

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Charger une image depuis un fichier
  Image1.Picture.LoadFromFile('C:\Images\photo.jpg');
end;
```

#### Méthode 3 : Depuis les Ressources

```pascal
// Si l'image est dans les ressources
Image1.Picture.LoadFromResourceName(HInstance, 'MON_IMAGE');
```

#### Méthode 4 : Créer Dynamiquement

```pascal
procedure TForm1.CreerImageDynamique;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := 200;
    Bmp.Height := 200;

    // Dessiner sur le bitmap
    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(0, 0, 200, 200);

    // Assigner au TImage
    Image1.Picture.Bitmap := Bmp;
  finally
    Bmp.Free;
  end;
end;
```

### Vérifier si une Image est Chargée

```pascal
if Assigned(Image1.Picture.Graphic) and
   not Image1.Picture.Graphic.Empty then
  ShowMessage('Une image est chargée')
else
  ShowMessage('Aucune image');
```

### Sauvegarder une Image

```pascal
procedure TForm1.SauvegarderImage;
begin
  if Assigned(Image1.Picture.Graphic) then
    Image1.Picture.SaveToFile('C:\Images\sauvegarde.bmp');
end;
```

### Effacer une Image

```pascal
Image1.Picture.Clear;  // Efface l'image
```

### Modes d'Affichage

#### Mode Normal (par défaut)

```pascal
Image1.Stretch := False;
Image1.Center := False;
```

L'image est affichée en taille réelle, ancrée en haut à gauche.

```
┌────────────────────┐
│[Image]             │  ← Image taille réelle
│                    │
│                    │
└────────────────────┘
```

#### Mode Stretch (Étiré)

```pascal
Image1.Stretch := True;
Image1.Proportional := False;
```

L'image remplit **tout le composant**, même si cela déforme l'image.

```
┌────────────────────┐
│████████████████████│  ← Image étirée
│████████████████████│     (peut être déformée)
│████████████████████│
└────────────────────┘
```

⚠️ **Attention** : Sans `Proportional := True`, l'image peut être déformée.

#### Mode Stretch Proportionnel

```pascal
Image1.Stretch := True;
Image1.Proportional := True;
```

L'image est **redimensionnée pour tenir** dans le composant tout en **conservant ses proportions**.

```
┌────────────────────┐
│                    │
│  ████████████████  │  ← Image redimensionnée
│  ████████████████  │     proportions conservées
│                    │
└────────────────────┘
```

C'est généralement le **mode recommandé** pour afficher des photos.

#### Mode Centré

```pascal
Image1.Stretch := False;
Image1.Center := True;
```

L'image est affichée en taille réelle, **centrée** dans le composant.

```
┌────────────────────┐
│                    │
│     [Image]        │  ← Image centrée
│                    │
└────────────────────┘
```

### Exemple : Visionneuse d'Images

```pascal
type
  TForm1 = class(TForm)
    Image1: TImage;
    BtnCharger: TButton;
    BtnPrecedent: TButton;
    BtnSuivant: TButton;
    OpenDialog1: TOpenDialog;
    CheckBoxStretch: TCheckBox;
    CheckBoxProportional: TCheckBox;
    LabelInfo: TLabel;
    procedure BtnChargerClick(Sender: TObject);
    procedure CheckBoxStretchChange(Sender: TObject);
    procedure CheckBoxProportionalChange(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration de l'image
  Image1.Align := alClient;
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Center := True;

  // Configuration du dialogue
  OpenDialog1.Filter := 'Images|*.jpg;*.jpeg;*.png;*.bmp;*.gif|Tous fichiers|*.*';

  // État des checkboxes
  CheckBoxStretch.Checked := Image1.Stretch;
  CheckBoxProportional.Checked := Image1.Proportional;
end;

procedure TForm1.BtnChargerClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Image1.Picture.LoadFromFile(OpenDialog1.FileName);

      // Afficher les informations
      LabelInfo.Caption := Format('Image : %s (%dx%d pixels)', [
        ExtractFileName(OpenDialog1.FileName),
        Image1.Picture.Width,
        Image1.Picture.Height
      ]);
    except
      on E: Exception do
        ShowMessage('Erreur lors du chargement : ' + E.Message);
    end;
  end;
end;

procedure TForm1.CheckBoxStretchChange(Sender: TObject);
begin
  Image1.Stretch := CheckBoxStretch.Checked;
end;

procedure TForm1.CheckBoxProportionalChange(Sender: TObject);
begin
  Image1.Proportional := CheckBoxProportional.Checked;
end;
```

### Manipulation d'Images

#### Obtenir les Dimensions

```pascal
var
  Largeur, Hauteur: Integer;
begin
  if Assigned(Image1.Picture.Graphic) then
  begin
    Largeur := Image1.Picture.Width;
    Hauteur := Image1.Picture.Height;
    ShowMessage(Format('Dimensions : %d x %d', [Largeur, Hauteur]));
  end;
end;
```

#### Redimensionner une Image

```pascal
procedure TForm1.RedimensionnerImage(NouvelleLargeur, NouvelleHauteur: Integer);
var
  Bmp, BmpRedim: TBitmap;
begin
  if not Assigned(Image1.Picture.Graphic) then Exit;

  Bmp := TBitmap.Create;
  BmpRedim := TBitmap.Create;
  try
    // Copier l'image actuelle
    Bmp.Assign(Image1.Picture.Bitmap);

    // Créer bitmap redimensionné
    BmpRedim.Width := NouvelleLargeur;
    BmpRedim.Height := NouvelleHauteur;

    // Redimensionner
    BmpRedim.Canvas.StretchDraw(Rect(0, 0, NouvelleLargeur, NouvelleHauteur), Bmp);

    // Assigner au TImage
    Image1.Picture.Bitmap := BmpRedim;
  finally
    Bmp.Free;
    BmpRedim.Free;
  end;
end;
```

#### Faire Pivoter une Image

```pascal
uses
  GraphUtil;  // Pour RotateBitmap

procedure TForm1.PivoterImage90;
var
  Bmp: TBitmap;
begin
  if not Assigned(Image1.Picture.Bitmap) then Exit;

  Bmp := TBitmap.Create;
  try
    Bmp.Assign(Image1.Picture.Bitmap);

    // Pivoter de 90 degrés (fonction de GraphUtil)
    // Note: GraphUtil n'a pas toujours RotateBitmap sur toutes les versions
    // Vous devrez peut-être implémenter manuellement

    Image1.Picture.Bitmap := Bmp;
  finally
    Bmp.Free;
  end;
end;
```

#### Convertir en Niveaux de Gris

```pascal
procedure TForm1.ConvertirNoirEtBlanc;
var
  x, y: Integer;
  Pixel: TColor;
  Gris: Byte;
  Bmp: TBitmap;
begin
  if not Assigned(Image1.Picture.Bitmap) then Exit;

  Bmp := Image1.Picture.Bitmap;
  Bmp.BeginUpdate;
  try
    for y := 0 to Bmp.Height - 1 do
    begin
      for x := 0 to Bmp.Width - 1 do
      begin
        Pixel := Bmp.Canvas.Pixels[x, y];

        // Calculer niveau de gris
        Gris := Round(
          Red(Pixel) * 0.299 +
          Green(Pixel) * 0.587 +
          Blue(Pixel) * 0.114
        );

        // Appliquer le gris
        Bmp.Canvas.Pixels[x, y] := RGB(Gris, Gris, Gris);
      end;
    end;
  finally
    Bmp.EndUpdate;
  end;
end;
```

### Événements

```pascal
property OnClick: TNotifyEvent;      // Clic sur l'image
property OnDblClick: TNotifyEvent;   // Double-clic
property OnMouseMove: TMouseMoveEvent; // Mouvement de souris
```

#### Exemple : Afficher Coordonnées de Souris

```pascal
procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  StatusBar1.SimpleText := Format('Position : X=%d, Y=%d', [X, Y]);
end;
```

### Formats d'Images Supportés

| Format | Extension | Caractéristiques |
|--------|-----------|------------------|
| **Bitmap** | .bmp | Non compressé, qualité maximale, fichiers volumineux |
| **JPEG** | .jpg, .jpeg | Compressé avec perte, photos, fichiers plus petits |
| **PNG** | .png | Compressé sans perte, transparence alpha |
| **GIF** | .gif | Palette 256 couleurs, animations, transparence |
| **Icône** | .ico | Petites images, multi-résolutions |

### Cas d'Usage Typiques

#### 1. Logo de l'Application

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  ImageLogo.Picture.LoadFromFile('logo.png');
  ImageLogo.Stretch := True;
  ImageLogo.Proportional := True;
  ImageLogo.Center := True;
end;
```

#### 2. Galerie de Photos

```pascal
type
  TForm1 = class(TForm)
  private
    FPhotos: TStringList;
    FIndex: Integer;
    procedure ChargerPhoto(Index: Integer);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPhotos := TStringList.Create;
  FPhotos.Add('photo1.jpg');
  FPhotos.Add('photo2.jpg');
  FPhotos.Add('photo3.jpg');

  FIndex := 0;
  ChargerPhoto(FIndex);
end;

procedure TForm1.ChargerPhoto(Index: Integer);
begin
  if (Index >= 0) and (Index < FPhotos.Count) then
  begin
    Image1.Picture.LoadFromFile(FPhotos[Index]);
    LabelPhoto.Caption := Format('Photo %d/%d', [Index + 1, FPhotos.Count]);
  end;
end;

procedure TForm1.BtnSuivantClick(Sender: TObject);
begin
  Inc(FIndex);
  if FIndex >= FPhotos.Count then
    FIndex := 0;
  ChargerPhoto(FIndex);
end;

procedure TForm1.BtnPrecedentClick(Sender: TObject);
begin
  Dec(FIndex);
  if FIndex < 0 then
    FIndex := FPhotos.Count - 1;
  ChargerPhoto(FIndex);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPhotos.Free;
end;
```

#### 3. Aperçu d'Image (Thumbnail)

```pascal
procedure TForm1.CreerMiniature(const NomFichier: string; Taille: Integer);
var
  ImageSource: TImage;
  Bmp: TBitmap;
  Ratio: Double;
  NouvelleLargeur, NouvelleHauteur: Integer;
begin
  ImageSource := TImage.Create(nil);
  Bmp := TBitmap.Create;
  try
    ImageSource.Picture.LoadFromFile(NomFichier);

    // Calculer nouvelles dimensions (garder proportions)
    if ImageSource.Picture.Width > ImageSource.Picture.Height then
    begin
      NouvelleLargeur := Taille;
      Ratio := Taille / ImageSource.Picture.Width;
      NouvelleHauteur := Round(ImageSource.Picture.Height * Ratio);
    end
    else
    begin
      NouvelleHauteur := Taille;
      Ratio := Taille / ImageSource.Picture.Height;
      NouvelleLargeur := Round(ImageSource.Picture.Width * Ratio);
    end;

    // Créer miniature
    Bmp.Width := NouvelleLargeur;
    Bmp.Height := NouvelleHauteur;
    Bmp.Canvas.StretchDraw(
      Rect(0, 0, NouvelleLargeur, NouvelleHauteur),
      ImageSource.Picture.Graphic
    );

    // Afficher
    ImageMiniature.Picture.Bitmap := Bmp;
  finally
    ImageSource.Free;
    Bmp.Free;
  end;
end;
```

#### 4. Filigrane (Watermark)

```pascal
procedure TForm1.AjouterFiligrane(const Texte: string);
begin
  if not Assigned(Image1.Picture.Bitmap) then Exit;

  with Image1.Picture.Bitmap.Canvas do
  begin
    Font.Name := 'Arial';
    Font.Size := 20;
    Font.Color := clWhite;
    Font.Style := [fsBold];

    // Semi-transparence (via Brush.Style)
    Brush.Style := bsClear;

    // Dessiner le texte
    TextOut(10, Image1.Picture.Height - 30, Texte);
  end;
end;
```

---

## TShape : Formes Géométriques

### Présentation

`TShape` permet d'afficher des **formes géométriques simples** : rectangles, cercles, ellipses, lignes, etc. C'est utile pour créer des séparateurs visuels, des indicateurs colorés, ou des éléments décoratifs.

### Hiérarchie

```
TControl
  └─ TGraphicControl
       └─ TShape
```

Comme TImage, TShape hérite de `TGraphicControl` (léger, sans handle).

### Apparence Visuelle

Exemples de formes :

```
Rectangle:        Cercle:         Ligne:
┌────────┐         ╭───╮           ╱
│        │         │   │          ╱
│        │         │   │         ╱
└────────┘         ╰───╯        ╱
```

### Propriété Principale : Shape

```pascal
type
  TShapeType = (
    stRectangle,      // Rectangle
    stSquare,         // Carré
    stRoundRect,      // Rectangle arrondi
    stRoundSquare,    // Carré arrondi
    stEllipse,        // Ellipse
    stCircle,         // Cercle
    stSquaredDiamond, // Diamant
    stDiamond,        // Diamant proportionnel
    stTriangle,       // Triangle haut
    stTriangleLeft,   // Triangle gauche
    stTriangleRight,  // Triangle droit
    stTriangleDown,   // Triangle bas
    stHexagon,        // Hexagone
    stStar            // Étoile (pas toujours disponible)
  );

property Shape: TShapeType;  // Type de forme
```

### Propriétés de Style

#### Bordure (Pen)

```pascal
property Pen: TPen;  // Style de la bordure
```

Propriétés du Pen :
```pascal
Pen.Color: TColor;      // Couleur de la bordure
Pen.Width: Integer;     // Épaisseur de la bordure
Pen.Style: TPenStyle;   // Style (psSolid, psDash, psDot, etc.)
```

#### Remplissage (Brush)

```pascal
property Brush: TBrush;  // Style de remplissage
```

Propriétés du Brush :
```pascal
Brush.Color: TColor;      // Couleur de remplissage
Brush.Style: TBrushStyle; // Style (bsSolid, bsClear, bsHorizontal, etc.)
```

### Exemples de Formes

#### Rectangle Simple

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Shape1.Shape := stRectangle;
  Shape1.Width := 100;
  Shape1.Height := 80;
  Shape1.Brush.Color := clBlue;
  Shape1.Pen.Color := clBlack;
  Shape1.Pen.Width := 2;
end;
```

#### Cercle Rouge

```pascal
Shape2.Shape := stCircle;
Shape2.Width := 50;
Shape2.Height := 50;  // Même dimension pour un cercle parfait
Shape2.Brush.Color := clRed;
Shape2.Pen.Color := clMaroon;
Shape2.Pen.Width := 3;
```

#### Ligne Horizontale (Séparateur)

```pascal
ShapeLigne.Shape := stRectangle;
ShapeLigne.Width := 300;
ShapeLigne.Height := 2;
ShapeLigne.Brush.Color := clGray;
ShapeLigne.Pen.Style := psClear;  // Pas de bordure
```

#### Ellipse Verte Semi-Transparente

```pascal
Shape3.Shape := stEllipse;
Shape3.Width := 120;
Shape3.Height := 80;
Shape3.Brush.Color := clLime;
Shape3.Pen.Color := clGreen;
Shape3.Pen.Width := 1;
```

#### Rectangle Arrondi

```pascal
Shape4.Shape := stRoundRect;
Shape4.Width := 100;
Shape4.Height := 60;
Shape4.Brush.Color := clYellow;
Shape4.Pen.Color := clOlive;
Shape4.Pen.Width := 2;
```

### Styles de Bordure (Pen.Style)

```pascal
type
  TPenStyle = (
    psSolid,       // ────────── Ligne pleine
    psDash,        // ─ ─ ─ ─ ─  Tirets
    psDot,         // · · · · ·  Points
    psDashDot,     // ─·─·─·─·─  Tiret-point
    psDashDotDot,  // ─··─··─··  Tiret-point-point
    psClear        // (invisible)
  );
```

**Exemple :**

```pascal
ShapeCadre.Pen.Style := psDash;     // Bordure en tirets
ShapeCadre.Pen.Color := clBlack;
ShapeCadre.Pen.Width := 1;
```

### Styles de Remplissage (Brush.Style)

```pascal
type
  TBrushStyle = (
    bsSolid,        // Remplissage plein
    bsClear,        // Transparent (pas de remplissage)
    bsHorizontal,   // Lignes horizontales
    bsVertical,     // Lignes verticales
    bsFDiagonal,    // Diagonales /
    bsBDiagonal,    // Diagonales \
    bsCross,        // Grille +
    bsDiagCross     // Grille X
  );
```

**Exemple :**

```pascal
// Forme avec motif hachuré
ShapeMotif.Brush.Style := bsCross;
ShapeMotif.Brush.Color := clRed;
```

### Cas d'Usage Typiques

#### 1. Séparateur Visuel

```pascal
procedure TForm1.CreerSeparateur;
begin
  ShapeSeparateur.Shape := stRectangle;
  ShapeSeparateur.Align := alTop;
  ShapeSeparateur.Height := 2;
  ShapeSeparateur.Brush.Color := clGray;
  ShapeSeparateur.Pen.Style := psClear;
end;
```

#### 2. Indicateur de Statut (Feu Tricolore)

```pascal
type
  TStatut = (stRouge, stOrange, stVert);

procedure TForm1.AfficherStatut(Statut: TStatut);
begin
  // Réinitialiser
  ShapeRouge.Brush.Color := clGray;
  ShapeOrange.Brush.Color := clGray;
  ShapeVert.Brush.Color := clGray;

  // Allumer le bon
  case Statut of
    stRouge: ShapeRouge.Brush.Color := clRed;
    stOrange: ShapeOrange.Brush.Color := clYellow;
    stVert: ShapeVert.Brush.Color := clLime;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration des 3 cercles
  ShapeRouge.Shape := stCircle;
  ShapeRouge.Width := 30;
  ShapeRouge.Height := 30;

  ShapeOrange.Shape := stCircle;
  ShapeOrange.Width := 30;
  ShapeOrange.Height := 30;

  ShapeVert.Shape := stCircle;
  ShapeVert.Width := 30;
  ShapeVert.Height := 30;

  AfficherStatut(stVert);  // Vert par défaut
end;
```

#### 3. Barre de Progression Visuelle

```pascal
procedure TForm1.AfficherProgression(Pourcentage: Integer);
var
  Largeur: Integer;
begin
  // Calculer la largeur de la barre
  Largeur := Round((ShapeFond.Width * Pourcentage) / 100);

  // Ajuster la barre de progression
  ShapeProgression.Width := Largeur;

  LabelPourcent.Caption := IntToStr(Pourcentage) + '%';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Fond de la barre
  ShapeFond.Shape := stRectangle;
  ShapeFond.Brush.Color := clSilver;
  ShapeFond.Width := 200;
  ShapeFond.Height := 20;

  // Barre de progression
  // Note : TShape (TGraphicControl) ne peut pas être Parent d'un autre composant.
  // Les deux TShape doivent avoir le même Parent (ex: le formulaire ou un TPanel)
  // et être superposés par positionnement.
  ShapeProgression.Shape := stRectangle;
  ShapeProgression.Brush.Color := clGreen;
  ShapeProgression.Height := 20;
  ShapeProgression.Parent := ShapeFond.Parent;  // Même parent que ShapeFond
  ShapeProgression.Left := ShapeFond.Left;
  ShapeProgression.Top := ShapeFond.Top;

  AfficherProgression(0);
end;
```

#### 4. Cadre Décoratif

```pascal
procedure TForm1.CreerCadreDecoratif;
begin
  ShapeCadre.Shape := stRoundRect;
  ShapeCadre.Brush.Style := bsClear;  // Transparent
  ShapeCadre.Pen.Color := clBlue;
  ShapeCadre.Pen.Width := 3;
  ShapeCadre.Width := 250;
  ShapeCadre.Height := 150;
end;
```

#### 5. Indicateur Coloré (Tag, Badge)

```pascal
procedure TForm1.CreerBadge(const Couleur: TColor);
begin
  ShapeBadge.Shape := stCircle;
  ShapeBadge.Width := 20;
  ShapeBadge.Height := 20;
  ShapeBadge.Brush.Color := Couleur;
  ShapeBadge.Pen.Style := psClear;
end;

// Utilisation
CreerBadge(clRed);    // Badge rouge (nouveau message)
CreerBadge(clGreen);  // Badge vert (en ligne)
CreerBadge(clOrange); // Badge orange (occupé)
```

### Animer un TShape

```pascal
type
  TForm1 = class(TForm)
    Shape1: TShape;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    FCroissant: Boolean;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Shape1.Shape := stCircle;
  Shape1.Width := 50;
  Shape1.Height := 50;
  Shape1.Brush.Color := clRed;

  Timer1.Interval := 50;  // 50 ms
  Timer1.Enabled := True;
  FCroissant := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Animation : faire pulser le cercle
  if FCroissant then
  begin
    Shape1.Width := Shape1.Width + 2;
    Shape1.Height := Shape1.Height + 2;
    if Shape1.Width >= 100 then
      FCroissant := False;
  end
  else
  begin
    Shape1.Width := Shape1.Width - 2;
    Shape1.Height := Shape1.Height - 2;
    if Shape1.Width <= 50 then
      FCroissant := True;
  end;
end;
```

---

## Combiner TImage et TShape

### Exemple : Cadre autour d'une Image

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Image
  Image1.Picture.LoadFromFile('photo.jpg');
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.SetBounds(50, 50, 200, 200);

  // Cadre (Shape)
  ShapeCadre.Shape := stRectangle;
  ShapeCadre.Brush.Style := bsClear;  // Transparent
  ShapeCadre.Pen.Color := clGold;
  ShapeCadre.Pen.Width := 5;
  ShapeCadre.SetBounds(
    Image1.Left - 10,
    Image1.Top - 10,
    Image1.Width + 20,
    Image1.Height + 20
  );
  ShapeCadre.SendToBack;  // Derrière l'image
end;
```

### Exemple : Indicateur sur une Image

```pascal
procedure TForm1.AfficherIndicateurSurImage(X, Y: Integer);
begin
  ShapeIndicateur.Shape := stCircle;
  ShapeIndicateur.Width := 20;
  ShapeIndicateur.Height := 20;
  ShapeIndicateur.Brush.Color := clRed;
  ShapeIndicateur.Pen.Color := clWhite;
  ShapeIndicateur.Pen.Width := 2;

  // Positionner par-dessus l'image
  // Note : TImage (TGraphicControl) ne peut pas être Parent d'un autre composant.
  // On utilise le même parent que l'image et on ajuste les coordonnées.
  ShapeIndicateur.Parent := Image1.Parent;
  ShapeIndicateur.Left := Image1.Left + X - 10;
  ShapeIndicateur.Top := Image1.Top + Y - 10;
  ShapeIndicateur.BringToFront;
end;

procedure TForm1.Image1Click(Sender: TObject);
var
  P: TPoint;
begin
  P := Image1.ScreenToClient(Mouse.CursorPos);
  AfficherIndicateurSurImage(P.X, P.Y);
end;
```

---

## Tableau Comparatif

| Aspect | TImage | TShape |
|--------|--------|--------|
| **Contenu** | Images (BMP, JPG, PNG...) | Formes géométriques |
| **Complexité** | Peut être complexe | Toujours simple |
| **Fichiers** | Charge depuis fichiers | Créé par code |
| **Personnalisation** | Limitée | Haute (couleurs, styles) |
| **Performance** | Dépend de la taille | Très légère |
| **Usage typique** | Photos, logos, icônes | Séparateurs, indicateurs |
| **Transparence** | Supportée (PNG) | Via Brush.Style |

---

## Bonnes Pratiques

### 1. Optimiser les Images

```pascal
// ✅ BON : Charger des images de taille appropriée
// Si vous affichez une miniature 100x100, ne chargez pas une image 4000x3000

// ❌ MAUVAIS : Charger des images énormes
Image1.Picture.LoadFromFile('photo_20MB.jpg');  // Très lent !
Image1.Stretch := True;  // Redimensionne à chaque affichage
```

### 2. Gérer les Erreurs de Chargement

```pascal
// ✅ BON : Toujours utiliser try-except
try
  Image1.Picture.LoadFromFile(NomFichier);
except
  on E: Exception do
  begin
    ShowMessage('Impossible de charger l''image : ' + E.Message);
    // Charger une image par défaut
    Image1.Picture.LoadFromFile('image_defaut.png');
  end;
end;

// ❌ MAUVAIS : Pas de gestion d'erreur
Image1.Picture.LoadFromFile(NomFichier);  // Peut crasher !
```

### 3. Libérer la Mémoire

```pascal
// ✅ BON : Vider les images quand non utilisées
Image1.Picture.Clear;  // Libère la mémoire

// Pour plusieurs images
for i := 0 to ComponentCount - 1 do
begin
  if Components[i] is TImage then
    TImage(Components[i]).Picture.Clear;
end;
```

### 4. Vérifier l'Existence des Fichiers

```pascal
// ✅ BON
if FileExists(NomFichier) then
  Image1.Picture.LoadFromFile(NomFichier)
else
  ShowMessage('Fichier introuvable : ' + NomFichier);

// ❌ MAUVAIS : Pas de vérification
Image1.Picture.LoadFromFile(NomFichier);  // Exception si fichier absent
```

### 5. Utiliser les Bons Formats

```pascal
// ✅ BON : Choisir le format adapté
// - PNG pour logos, icônes (transparence)
// - JPEG pour photos (compression)
// - BMP pour qualité maximale (mais lourd)

// ❌ MAUVAIS : Tout en BMP
// Les fichiers BMP sont très volumineux
```

### 6. Positionner Correctement les Shapes

```pascal
// ✅ BON : Utiliser Align pour les séparateurs
ShapeSeparateur.Align := alTop;
ShapeSeparateur.Height := 2;

// ✅ BON : Ancrer pour suivre le redimensionnement
Shape1.Anchors := [akLeft, akTop, akRight];  // S'étire horizontalement

// ❌ MAUVAIS : Position fixe sans ancrage
Shape1.Left := 100;
Shape1.Top := 50;
// Ne s'adapte pas au redimensionnement du formulaire
```

### 7. Nommer Correctement

```pascal
// ✅ BON : Noms descriptifs
ImageLogo, ImageProfil, ImageProduit
ShapeSeparateur, ShapeIndicateur, ShapeCadre

// ❌ MAUVAIS : Noms génériques
Image1, Image2, Image3
Shape1, Shape2, Shape3
```

---

## Exemple Complet : Visionneuse d'Images avec Indicateurs

```pascal
type
  TFormVisionneuse = class(TForm)
    ImagePrincipale: TImage;
    ShapeCadre: TShape;
    ShapeIndicateurGauche: TShape;
    ShapeIndicateurDroit: TShape;
    BtnPrecedent: TButton;
    BtnSuivant: TButton;
    LabelInfo: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure BtnPrecedentClick(Sender: TObject);
    procedure BtnSuivantClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FImages: TStringList;
    FIndexCourant: Integer;
    FClignotement: Boolean;
    procedure ChargerImage(Index: Integer);
    procedure MettreAJourIndicateurs;
  end;

procedure TFormVisionneuse.FormCreate(Sender: TObject);
begin
  // Liste des images
  FImages := TStringList.Create;
  FImages.Add('photo1.jpg');
  FImages.Add('photo2.jpg');
  FImages.Add('photo3.jpg');
  FImages.Add('photo4.jpg');

  // Configuration de l'image principale
  ImagePrincipale.Align := alClient;
  ImagePrincipale.Stretch := True;
  ImagePrincipale.Proportional := True;
  ImagePrincipale.Center := True;

  // Cadre autour de l'image
  ShapeCadre.Align := alClient;
  ShapeCadre.Shape := stRectangle;
  ShapeCadre.Brush.Style := bsClear;
  ShapeCadre.Pen.Color := clSilver;
  ShapeCadre.Pen.Width := 3;
  ShapeCadre.SendToBack;

  // Indicateurs (cercles)
  ShapeIndicateurGauche.Shape := stCircle;
  ShapeIndicateurGauche.Width := 15;
  ShapeIndicateurGauche.Height := 15;
  ShapeIndicateurGauche.Brush.Color := clGray;
  ShapeIndicateurGauche.Pen.Style := psClear;

  ShapeIndicateurDroit.Shape := stCircle;
  ShapeIndicateurDroit.Width := 15;
  ShapeIndicateurDroit.Height := 15;
  ShapeIndicateurDroit.Brush.Color := clGray;
  ShapeIndicateurDroit.Pen.Style := psClear;

  // Timer pour clignotement
  Timer1.Interval := 500;
  Timer1.Enabled := True;
  FClignotement := False;

  // Charger première image
  FIndexCourant := 0;
  ChargerImage(FIndexCourant);
end;

procedure TFormVisionneuse.ChargerImage(Index: Integer);
begin
  if (Index >= 0) and (Index < FImages.Count) then
  begin
    try
      ImagePrincipale.Picture.LoadFromFile(FImages[Index]);
      LabelInfo.Caption := Format('Image %d/%d - %s', [
        Index + 1,
        FImages.Count,
        ExtractFileName(FImages[Index])
      ]);

      MettreAJourIndicateurs;
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;

procedure TFormVisionneuse.MettreAJourIndicateurs;
begin
  // Indicateur gauche : rouge si on peut aller à gauche
  if FIndexCourant > 0 then
    ShapeIndicateurGauche.Brush.Color := clLime
  else
    ShapeIndicateurGauche.Brush.Color := clGray;

  // Indicateur droit : rouge si on peut aller à droite
  if FIndexCourant < FImages.Count - 1 then
    ShapeIndicateurDroit.Brush.Color := clLime
  else
    ShapeIndicateurDroit.Brush.Color := clGray;
end;

procedure TFormVisionneuse.BtnPrecedentClick(Sender: TObject);
begin
  if FIndexCourant > 0 then
  begin
    Dec(FIndexCourant);
    ChargerImage(FIndexCourant);
  end;
end;

procedure TFormVisionneuse.BtnSuivantClick(Sender: TObject);
begin
  if FIndexCourant < FImages.Count - 1 then
  begin
    Inc(FIndexCourant);
    ChargerImage(FIndexCourant);
  end;
end;

procedure TFormVisionneuse.Timer1Timer(Sender: TObject);
begin
  // Faire clignoter le cadre
  FClignotement := not FClignotement;
  if FClignotement then
    ShapeCadre.Pen.Color := clBlue
  else
    ShapeCadre.Pen.Color := clSilver;
end;

procedure TFormVisionneuse.FormDestroy(Sender: TObject);
begin
  FImages.Free;
end;
```

---

## Points Clés à Retenir

1. **TImage** : affichage d'images variées
   - `Picture.LoadFromFile()` pour charger
   - `Stretch` et `Proportional` pour l'affichage
   - Supporte BMP, JPEG, PNG, GIF, ICO

2. **TShape** : formes géométriques simples
   - `Shape` définit le type (cercle, rectangle, etc.)
   - `Brush.Color` pour le remplissage
   - `Pen.Color` et `Pen.Width` pour la bordure

3. **Héritage de TGraphicControl** :
   - Légers en ressources
   - Pas de handle système
   - Ne peuvent pas contenir d'autres composants

4. **Optimisation** :
   - Charger des images de taille appropriée
   - Libérer la mémoire avec `Picture.Clear`
   - Gérer les erreurs de chargement

5. **Cas d'usage** :
   - TImage : logos, photos, icônes, galeries
   - TShape : séparateurs, indicateurs, badges, barres

6. **Combinaison** : TImage et TShape fonctionnent bien ensemble
   - Cadres autour d'images
   - Indicateurs sur images

7. **Toujours vérifier** :
   - Existence des fichiers (`FileExists`)
   - Images chargées (`Assigned`, `Empty`)
   - Gérer les exceptions

---

## Conclusion

Les composants d'affichage `TImage` et `TShape` enrichissent considérablement vos interfaces en ajoutant des éléments visuels. Ils permettent de :

- **Communiquer visuellement** : logos, icônes, photos
- **Guider l'utilisateur** : indicateurs, séparateurs
- **Améliorer l'esthétique** : design moderne et professionnel
- **Fournir du feedback** : états visuels, progression

Bien utilisés, ces composants transforment une interface fonctionnelle en une expérience utilisateur agréable et intuitive.

Dans la section suivante, nous explorerons les **Timers et traitement asynchrone** pour créer des animations et gérer des tâches périodiques.

---

**Prochaine étape :** 15.8 Timers et traitement asynchrone

⏭️ [Timers et traitement asynchrone](/15-composants-lcl-fondamentaux/08-timers-traitement-asynchrone.md)
