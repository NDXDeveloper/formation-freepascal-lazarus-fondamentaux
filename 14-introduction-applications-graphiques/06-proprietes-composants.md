🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.6 Propriétés des composants

## Introduction

Les **propriétés** sont les caractéristiques d'un composant : sa position, sa taille, sa couleur, son texte, etc. Elles définissent **ce qu'est** et **comment apparaît** un composant.

Dans cette section, nous allons explorer :
- Ce qu'est une propriété
- Comment les modifier (design-time et run-time)
- Les propriétés communes à tous les composants
- Les différents types de propriétés
- L'Inspecteur d'Objets en détail

Comprendre les propriétés est essentiel pour créer des interfaces professionnelles et bien conçues !

---

## Qu'est-ce qu'une propriété ?

### Définition

Une **propriété** est une caractéristique d'un composant que vous pouvez lire ou modifier.

Techniquement, c'est un **membre spécial** d'une classe qui utilise des méthodes pour accéder à une donnée interne.

### Exemple simple

```pascal
// Lire une propriété
var
  Texte: string;
begin
  Texte := Button1.Caption;  // Lecture
end;

// Modifier une propriété
Button1.Caption := 'Nouveau texte';  // Écriture
```

### Propriété vs Variable

**Variable normale :**
```pascal
var
  MaVariable: Integer;
begin
  MaVariable := 10;  // Accès direct
end;
```

**Propriété :**
```pascal
property Width: Integer read FWidth write SetWidth;
//                       ^^^^        ^^^^^
//                      Getter      Setter
```

Quand vous écrivez `Button1.Width := 100`, le système appelle en réalité la méthode `SetWidth(100)`, qui peut :
- Valider la valeur
- Déclencher un redessin
- Modifier d'autres propriétés
- Etc.

**Avantages :**
- Contrôle sur l'accès aux données
- Validation automatique
- Effets de bord gérés
- Encapsulation

---

## Design-time vs Run-time

### Design-time (Temps de conception)

C'est quand vous créez votre interface dans **Lazarus IDE**, avant de compiler.

**Modification :** Via l'**Inspecteur d'Objets**

```
┌────────────────────────────┐
│ Inspecteur d'Objets        │
├────────────────────────────┤
│ Button1: TButton           │
├────────────────────────────┤
│ Caption    = 'Cliquez-moi' │ ← Modifier ici
│ Width      = 100           │
│ Height     = 25            │
│ Enabled    = True          │
└────────────────────────────┘
```

**Avantages :**
- Visuel et immédiat
- Facile pour les débutants
- Sauvegardé dans le fichier .lfm

### Run-time (Temps d'exécution)

C'est quand votre programme **s'exécute**.

**Modification :** Via le **code Pascal**

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Caption := 'Cliqué !';      // Changer au run-time
  Button1.Width := Button1.Width + 10;
  Button1.Enabled := False;
end;
```

**Avantages :**
- Dynamique et flexible
- Réagit aux actions utilisateur
- Permet la logique conditionnelle

### Quand utiliser quoi ?

| Situation | Méthode |
|-----------|---------|
| Apparence initiale fixe | Design-time |
| Texte statique | Design-time |
| Position/taille fixes | Design-time |
| Réaction à des événements | Run-time |
| Valeurs calculées | Run-time |
| Création dynamique | Run-time |
| Interface adaptative | Run-time |

**Exemple pratique :**
```pascal
// Design-time (Inspecteur d'Objets)
Button1.Caption := 'Calculer'
Button1.Width := 100
Button1.Top := 50

// Run-time (Code)
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Adapter dynamiquement
  if Edit1.Text = '' then
  begin
    Button1.Enabled := False;
    LabelInfo.Caption := 'Remplissez le champ';
  end;
end;
```

---

## L'Inspecteur d'Objets en détail

### Structure de l'Inspecteur

```
┌────────────────────────────────────┐
│ Button1: TButton              ▼    │ ← Sélecteur de composant
├────────────────────────────────────┤
│ [Propriétés] | [Événements]        │ ← Onglets
├────────────────────────────────────┤
│ □ Action                           │ ← Catégorie (pliable)
│   Action        = <aucun>          │
│ □ Apparence                        │
│   Caption       = 'Button1'        │
│   Color         = clBtnFace        │
│   Font          = ...              │
│ ■ Position                         │ ← Catégorie dépliée
│   Left          = 10               │
│   Top           = 10               │
│   Width         = 75               │
│   Height        = 25               │
│ □ Comportement                     │
│   Enabled       = True             │
│   Visible       = True             │
└────────────────────────────────────┘
```

### Types d'éditeurs de propriétés

#### 1. Édition de texte simple
```
Caption    = 'Mon texte'     [____________]
```
Vous tapez directement la valeur.

#### 2. Nombres
```
Width      = 100             [____100____]
```
Vous pouvez taper ou utiliser les flèches haut/bas.

#### 3. Booléens (True/False)
```
Enabled    = True            [▼ True  ]
                                False
```
Liste déroulante avec True/False.

#### 4. Énumérations
```
Alignment  = taCenter        [▼ taCenter       ]
                                taLeftJustify
                                taRightJustify
```
Liste de valeurs prédéfinies.

#### 5. Couleurs
```
Color      = clWhite         [▼ clWhite  ] [■]
```
Liste de couleurs + bouton pour le sélecteur de couleurs.

#### 6. Polices
```
Font       = ...             [...]
```
Bouton [...] ouvre un éditeur de police.

#### 7. Propriétés complexes (objets)
```
Font       = (TFont)         [+]
  ├─ Name     = 'Arial'
  ├─ Size     = 10
  ├─ Style    = []
  └─ Color    = clBlack
```
Cliquez sur [+] pour déplier et voir les sous-propriétés.

### Recherche dans l'Inspecteur

En haut de l'Inspecteur, il y a une zone de recherche :
```
[🔍 Recherche...            ]
```

Tapez "width" pour ne voir que les propriétés contenant "width".

### Filtrage par catégorie

Vous pouvez plier/déplier les catégories pour mieux organiser :
- Cliquez sur □ pour déplier
- Cliquez sur ■ pour replier

---

## Propriétés communes : Position et Taille

Tous les composants visuels (héritant de TControl) ont ces propriétés.

### Left et Top

```pascal
// Position horizontale (pixels depuis le bord gauche du parent)
Button1.Left := 10;

// Position verticale (pixels depuis le bord haut du parent)
Button1.Top := 50;
```

**Type :** Integer  
**Unité :** Pixels  
**Origine :** Coin supérieur gauche du conteneur parent  

**Exemple : Centrer un bouton**
```pascal
Button1.Left := (Form1.ClientWidth - Button1.Width) div 2;
Button1.Top := (Form1.ClientHeight - Button1.Height) div 2;
```

### Width et Height

```pascal
// Largeur du composant
Button1.Width := 100;

// Hauteur du composant
Button1.Height := 30;
```

**Type :** Integer  
**Unité :** Pixels  

**Exemple : Agrandir progressivement**
```pascal
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Button1.Width := Button1.Width + 1;
  if Button1.Width > 200 then
    Timer1.Enabled := False;
end;
```

### BoundsRect

```pascal
// Rectangle complet (Left, Top, Right, Bottom)
var
  R: TRect;
begin
  R := Button1.BoundsRect;
  ShowMessage(Format('Left=%d, Top=%d, Right=%d, Bottom=%d',
                     [R.Left, R.Top, R.Right, R.Bottom]));
end;
```

**Type :** TRect  
**Usage :** Obtenir toutes les coordonnées en une fois  

### SetBounds

```pascal
// Définir position et taille en un seul appel
Button1.SetBounds(Left, Top, Width, Height);

// Exemple
Button1.SetBounds(10, 50, 100, 30);
```

**Avantage :** Plus efficace que de modifier les propriétés séparément (un seul redessin).

### ClientWidth et ClientHeight

```pascal
// Zone client (intérieure, sans bordures)
var
  W, H: Integer;
begin
  W := Form1.ClientWidth;   // Largeur utilisable
  H := Form1.ClientHeight;  // Hauteur utilisable
end;
```

**Différence avec Width/Height :**
- `Width/Height` : Taille totale incluant les bordures et la barre de titre
- `ClientWidth/ClientHeight` : Taille de la zone intérieure utilisable

**Exemple pour un formulaire :**
```
┌─────────────────────────────┐  ← Width = 400
│ Titre                  _ □ ✕ │
├─────────────────────────────┤
│                             │  ← ClientHeight = 300
│      Zone client            │     (sans la barre de titre)
│                             │
│                             │
└─────────────────────────────┘
      ClientWidth = 400
```

---

## Propriétés communes : Apparence

### Name

```pascal
Button1.Name := 'ButtonCalculer';
```

**Type :** String  
**Description :** Nom du composant (identifiant dans le code)  
**Important :**  
- Doit être unique dans le formulaire
- Utilisé dans le code pour référencer le composant
- Convention : Commence par le type (Button, Edit, Label, etc.)

**Attention :** Ne changez Name qu'au design-time ! Changer Name au run-time ne change pas le nom de la variable dans votre code.

### Caption

```pascal
Button1.Caption := 'Cliquez-moi';
Label1.Caption := 'Entrez votre nom :';
Form1.Caption := 'Ma superbe application';
```

**Type :** String  
**Description :** Texte affiché sur/dans le composant  
**Utilisé par :** TButton, TLabel, TForm, TCheckBox, TGroupBox, etc.  

**Astuces :**
```pascal
// Saut de ligne
Label1.Caption := 'Ligne 1' + sLineBreak + 'Ligne 2';

// Raccourci clavier avec &
Button1.Caption := '&Valider';  // Alt+V

// Afficher &
Label1.Caption := 'Prix : 5 && 10';  // Affiche "Prix : 5 & 10"
```

### Text

```pascal
Edit1.Text := 'Contenu initial';
Memo1.Text := 'Ligne 1' + sLineBreak + 'Ligne 2';
```

**Type :** String  
**Description :** Contenu textuel modifiable  
**Utilisé par :** TEdit, TMemo, TComboBox, etc.  

**Différence avec Caption :**  
- `Caption` : Étiquette fixe (bouton, label)
- `Text` : Contenu éditable (champ de saisie)

### Color

```pascal
Button1.Color := clRed;           // Couleur prédéfinie
Panel1.Color := $00FF00;          // Valeur hexadécimale (BGR)
Label1.Color := RGBToColor(255, 128, 0);  // RGB
```

**Type :** TColor  
**Description :** Couleur de fond du composant  

**Couleurs prédéfinies courantes :**
| Constante | Couleur |
|-----------|---------|
| `clBlack` | Noir |
| `clWhite` | Blanc |
| `clRed` | Rouge |
| `clGreen` | Vert |
| `clBlue` | Bleu |
| `clYellow` | Jaune |
| `clGray` | Gris |
| `clSilver` | Gris clair |
| `clMaroon` | Bordeaux |
| `clNavy` | Bleu marine |
| `clBtnFace` | Couleur système (bouton) |
| `clWindow` | Couleur système (fenêtre) |

**Format BGR :**
```pascal
// $00BBGGRR (Bleu-Vert-Rouge)
Color := $00FF0000;  // Bleu pur
Color := $0000FF00;  // Vert pur
Color := $000000FF;  // Rouge pur
Color := $00FFFF00;  // Cyan (Bleu + Vert)
```

### Font

```pascal
// Accès aux sous-propriétés
Button1.Font.Name := 'Arial';
Button1.Font.Size := 12;
Button1.Font.Style := [fsBold, fsItalic];
Button1.Font.Color := clNavy;
```

**Type :** TFont (objet)  
**Sous-propriétés :**  

#### Font.Name
```pascal
Label1.Font.Name := 'Arial';
Label1.Font.Name := 'Courier New';
Label1.Font.Name := 'Times New Roman';
```
**Type :** String  
**Description :** Nom de la police  

**Polices courantes :**  
- **Arial** : Sans-serif moderne
- **Times New Roman** : Serif classique
- **Courier New** : Espacement fixe (code)
- **Verdana** : Lisibilité écran
- **Tahoma** : Interface système

#### Font.Size
```pascal
Label1.Font.Size := 10;   // Taille normale
Label1.Font.Size := 14;   // Grand titre
Label1.Font.Size := 8;    // Petit texte
```
**Type :** Integer
**Unité :** Points (pt)
**Valeurs courantes :** 8-12 (normal), 14-18 (titres)

#### Font.Style
```pascal
Label1.Font.Style := [];                      // Normal
Label1.Font.Style := [fsBold];                // Gras
Label1.Font.Style := [fsItalic];              // Italique
Label1.Font.Style := [fsBold, fsItalic];      // Gras + Italique
Label1.Font.Style := [fsUnderline];           // Souligné
Label1.Font.Style := [fsStrikeOut];           // Barré
```
**Type :** Set de TFontStyle  
**Valeurs possibles :**  
- `fsBold` : Gras
- `fsItalic` : Italique
- `fsUnderline` : Souligné
- `fsStrikeOut` : Barré

**Manipuler les styles :**  
```pascal
// Ajouter un style
Label1.Font.Style := Label1.Font.Style + [fsBold];

// Retirer un style
Label1.Font.Style := Label1.Font.Style - [fsBold];

// Vérifier la présence d'un style
if fsBold in Label1.Font.Style then
  ShowMessage('Le texte est en gras');
```

#### Font.Color
```pascal
Label1.Font.Color := clBlack;   // Noir (défaut)
Label1.Font.Color := clRed;     // Rouge
Label1.Font.Color := clWhite;   // Blanc
```
**Type :** TColor  
**Description :** Couleur du texte  

### Cursor

```pascal
Button1.Cursor := crHandPoint;   // Main pointée
Edit1.Cursor := crIBeam;         // Curseur texte
Panel1.Cursor := crCross;        // Croix
```

**Type :** TCursor  
**Description :** Forme du curseur souris quand il survole le composant  

**Curseurs courants :**  
| Constante | Apparence |
|-----------|-----------|
| `crDefault` | Flèche standard |
| `crHandPoint` | Main pointée (lien) |
| `crIBeam` | Barre I (texte) |
| `crCross` | Croix |
| `crSizeAll` | Flèches 4 directions |
| `crHourGlass` | Sablier (attente) |
| `crNone` | Invisible |

**Exemple d'utilisation :**  
```pascal
// Label cliquable qui ressemble à un lien
Label1.Caption := 'Cliquez ici';
Label1.Font.Color := clBlue;
Label1.Font.Style := [fsUnderline];
Label1.Cursor := crHandPoint;
```

---

## Propriétés communes : Comportement

### Enabled

```pascal
Button1.Enabled := True;   // Actif (défaut)
Button1.Enabled := False;  // Désactivé (grisé)
```

**Type :** Boolean  
**Description :** Le composant peut-il réagir aux interactions ?  

**Effet :**  
- `True` : Composant normal, interactif
- `False` : Composant grisé, ne répond pas aux clics/saisies

**Usage typique :**  
```pascal
// Désactiver le bouton Valider si le champ est vide
procedure TForm1.Edit1Change(Sender: TObject);
begin
  ButtonValider.Enabled := Edit1.Text <> '';
end;
```

**Propagation :** Si vous désactivez un conteneur (Panel, GroupBox), tous ses enfants sont désactivés aussi.

### Visible

```pascal
Panel1.Visible := True;   // Visible (défaut)
Panel1.Visible := False;  // Caché
```

**Type :** Boolean  
**Description :** Le composant est-il affiché ?  

**Effet :**  
- `True` : Composant visible
- `False` : Composant invisible (mais existe toujours en mémoire)

**Usage typique :**  
```pascal
// Afficher/Cacher un panneau d'options avancées
procedure TForm1.CheckBoxAvanceClick(Sender: TObject);
begin
  PanelAvance.Visible := CheckBoxAvance.Checked;
end;
```

**Note :** Un composant invisible ne prend pas de place dans l'interface.

### Hint (Bulle d'aide)

```pascal
Button1.Hint := 'Cliquez ici pour valider';
Button1.ShowHint := True;
```

**Type :** String  
**Description :** Texte affiché dans une bulle quand la souris survole le composant  

**Important :** N'oubliez pas `ShowHint := True` !

**Astuces :**
```pascal
// Hint sur plusieurs lignes
Button1.Hint := 'Ligne 1' + sLineBreak + 'Ligne 2';

// Activer globalement pour le formulaire
Form1.ShowHint := True;  // Tous les enfants héritent

// Hint long avec titre
Button1.Hint := 'Valider|Cliquez pour valider le formulaire et sauvegarder';
// "Valider" sera le titre en gras
```

### ShowHint

```pascal
Button1.ShowHint := True;   // Afficher la bulle d'aide
Button1.ShowHint := False;  // Ne pas afficher
```

**Type :** Boolean  
**Description :** Activer l'affichage du Hint  

**Héritage :** Si le formulaire a `ShowHint := True`, tous les composants enfants héritent cette valeur (sauf s'ils le redéfinissent).  

### PopupMenu

```pascal
Edit1.PopupMenu := PopupMenu1;
```

**Type :** TPopupMenu  
**Description :** Menu contextuel (clic droit)  

**Usage :**
```pascal
// Créer un menu contextuel au design-time
// puis l'associer à un composant
Edit1.PopupMenu := PopupMenu1;

// Le menu s'affichera automatiquement au clic droit
```

### TabStop

```pascal
Edit1.TabStop := True;   // Peut recevoir le focus avec Tab
Label1.TabStop := False; // Ne peut pas recevoir le focus
```

**Type :** Boolean  
**Description :** Le composant peut-il recevoir le focus avec la touche Tab ?  

**Par défaut :**  
- `True` : Edit, Button, CheckBox, ComboBox, etc.
- `False` : Label, Image, Panel, etc.

### TabOrder

```pascal
Edit1.TabOrder := 0;      // Premier
Edit2.TabOrder := 1;      // Deuxième
Button1.TabOrder := 2;    // Troisième
```

**Type :** Integer  
**Description :** Ordre de navigation avec la touche Tab  

**Usage :** Définit l'ordre logique de saisie dans un formulaire.  

**Conseil :** Définissez un ordre naturel (haut en bas, gauche à droite).  

### Anchors

```pascal
Button1.Anchors := [akLeft, akTop];              // Défaut
Button1.Anchors := [akRight, akBottom];          // Ancré en bas à droite
Button1.Anchors := [akLeft, akTop, akRight];     // S'étire horizontalement
```

**Type :** Set de TAnchorKind  
**Description :** Côtés ancrés lors du redimensionnement du parent  

**Valeurs possibles :**
- `akLeft` : Bord gauche
- `akTop` : Bord haut
- `akRight` : Bord droit
- `akBottom` : Bord bas

**Comportements courants :**

| Anchors | Comportement |
|---------|--------------|
| `[akLeft, akTop]` | Position fixe en haut à gauche (défaut) |
| `[akRight, akBottom]` | Position fixe en bas à droite |
| `[akLeft, akTop, akRight]` | S'étire horizontalement |
| `[akLeft, akTop, akBottom]` | S'étire verticalement |
| `[akLeft, akTop, akRight, akBottom]` | S'étire dans les deux directions |

**Exemple pratique :**
```pascal
// Bouton OK toujours en bas à droite
ButtonOK.Anchors := [akRight, akBottom];

// Memo qui s'étire avec le formulaire
Memo1.Anchors := [akLeft, akTop, akRight, akBottom];
```

### Align

```pascal
Panel1.Align := alTop;      // En haut, pleine largeur
Panel2.Align := alBottom;   // En bas, pleine largeur
Panel3.Align := alLeft;     // À gauche, pleine hauteur
Panel4.Align := alClient;   // Remplit l'espace restant
```

**Type :** TAlign  
**Description :** Alignement automatique dans le parent  

**Valeurs possibles :**
| Valeur | Description |
|--------|-------------|
| `alNone` | Pas d'alignement (défaut) |
| `alTop` | Haut, pleine largeur |
| `alBottom` | Bas, pleine largeur |
| `alLeft` | Gauche, pleine hauteur |
| `alRight` | Droite, pleine hauteur |
| `alClient` | Remplit tout l'espace disponible |

**Exemple d'interface typique :**
```pascal
// Barre d'outils en haut
ToolBar1.Align := alTop;

// Barre d'état en bas
StatusBar1.Align := alBottom;

// Zone principale au centre
Memo1.Align := alClient;
```

**Avantage :** Interface qui s'adapte automatiquement au redimensionnement !

---

## Propriétés spéciales

### Tag

```pascal
Button1.Tag := 1;
Button2.Tag := 2;
Button3.Tag := 3;
```

**Type :** PtrInt (entier)  
**Description :** Valeur numérique libre, utilisée comme vous voulez  

**Usage typique :** Identifier les composants, stocker des données associées

**Exemple :**
```pascal
// Associer des boutons à des actions
procedure TForm1.FormCreate(Sender: TObject);
begin
  ButtonNouveau.Tag := 1;
  ButtonOuvrir.Tag := 2;
  ButtonEnregistrer.Tag := 3;

  ButtonNouveau.OnClick := @BoutonClick;
  ButtonOuvrir.OnClick := @BoutonClick;
  ButtonEnregistrer.OnClick := @BoutonClick;
end;

procedure TForm1.BoutonClick(Sender: TObject);
begin
  case (Sender as TButton).Tag of
    1: NouveauFichier;
    2: OuvrirFichier;
    3: EnregistrerFichier;
  end;
end;
```

### Parent

```pascal
// Changer le parent d'un composant
Button1.Parent := Panel1;  // Button1 est maintenant dans Panel1
Button1.Parent := Form1;   // Button1 est maintenant sur Form1
```

**Type :** TWinControl  
**Description :** Le conteneur qui contient ce composant  

**Important :** Changer Parent déplace visuellement le composant et change le référentiel des coordonnées.  

### Owner

```pascal
var
  B: TButton;
begin
  B := TButton.Create(Self);  // Form1 est le Owner
  B.Parent := Panel1;         // Panel1 est le Parent
end;
```

**Type :** TComponent  
**Description :** Qui est responsable de libérer ce composant ?  

**Différence Parent/Owner :**  
- **Parent** : Où le composant est affiché (visuel)
- **Owner** : Qui libère la mémoire (gestion mémoire)

**Règle :** Owner libère automatiquement tous ses composants enfants.  

---

## Propriétés en lecture seule

Certaines propriétés ne peuvent être **que lues**, pas modifiées :

### ComponentCount et Components

```pascal
var
  i: Integer;
  Comp: TComponent;
begin
  // Nombre de composants enfants
  ShowMessage('Nombre de composants : ' + IntToStr(Form1.ComponentCount));

  // Parcourir tous les composants
  for i := 0 to Form1.ComponentCount - 1 do
  begin
    Comp := Form1.Components[i];
    ShowMessage(Comp.Name + ' : ' + Comp.ClassName);
  end;
end;
```

**Type :** Integer (Count), TComponent (Components[])  
**Description :** Liste des composants possédés  

### ControlCount et Controls

```pascal
var
  i: Integer;
  Ctrl: TControl;
begin
  // Nombre de contrôles visuels enfants
  for i := 0 to Panel1.ControlCount - 1 do
  begin
    Ctrl := Panel1.Controls[i];
    Ctrl.Visible := False;  // Cacher tous les enfants
  end;
end;
```

**Type :** Integer (Count), TControl (Controls[])  
**Description :** Liste des contrôles visuels enfants  

**Différence Components/Controls :**
- **Components** : Tous les composants (même non visuels comme Timer)  
- **Controls** : Seulement les composants visuels

### ClassName

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Type : ' + Sender.ClassName);  // Affiche "TButton"
end;
```

**Type :** String  
**Description :** Nom de la classe du composant  

**Usage :** Identifier le type d'un composant de manière sûre.

### Handle

```pascal
var
  H: THandle;
begin
  H := Button1.Handle;  // Handle Windows/Linux du composant
end;
```

**Type :** THandle  
**Description :** Identifiant système du composant  

**Usage :** Appels système de bas niveau (avancé).

---

## Propriétés et héritage

### Propriétés héritées

Grâce à l'héritage objet, un TButton hérite de toutes les propriétés de ses classes parentes :

```
TObject
  └─ TPersistent
      └─ TComponent
          ├─ Name
          ├─ Tag
          └─ TControl
              ├─ Left, Top, Width, Height
              ├─ Color, Font
              ├─ Visible, Enabled
              └─ TWinControl
                  ├─ TabStop, TabOrder
                  └─ TButtonControl
                      └─ TButton
                          ├─ Caption
                          ├─ Default
                          └─ Cancel
```

**Résultat :** TButton a TOUTES ces propriétés !

### Propriétés published

Seules les propriétés déclarées `published` apparaissent dans l'Inspecteur d'Objets :

```pascal
type
  TMyComponent = class(TComponent)
  private
    FValue: Integer;        // Invisible dans l'inspecteur
  public
    property Value: Integer read FValue write FValue;  // Invisible
  published
    property PublicValue: Integer read FValue write FValue;  // VISIBLE !
  end;
```

---

## Bonnes pratiques

### 1. Nommer clairement les composants

❌ **Mauvais :**
```pascal
Button1, Button2, Button3
Edit1, Edit2, Edit3
```

✅ **Bon :**
```pascal
ButtonValider, ButtonAnnuler, ButtonQuitter
EditNom, EditPrenom, EditEmail
```

### 2. Utiliser les propriétés plutôt que les méthodes

❌ **Moins efficace :**
```pascal
Button1.SetBounds(10, 10, Button1.Width, Button1.Height);
```

✅ **Plus clair :**
```pascal
Button1.Left := 10;
Button1.Top := 10;
```

### 3. Grouper les modifications

❌ **Plusieurs redessin :**
```pascal
Button1.Left := 10;    // Redessin
Button1.Top := 50;     // Redessin
Button1.Width := 100;  // Redessin
Button1.Height := 30;  // Redessin
```

✅ **Un seul redessin :**
```pascal
Button1.SetBounds(10, 50, 100, 30);  // Un seul redessin
```

### 4. Désactiver le redessin pour les modifications en masse

```pascal
// Bloquer le redessin
Form1.DisableAutoSizing;
try
  // Faire plein de modifications
  for i := 0 to 100 do
  begin
    Labels[i].Caption := IntToStr(i);
    Labels[i].Left := i * 20;
  end;
finally
  // Réactiver et redessiner une seule fois
  Form1.EnableAutoSizing;
end;
```

### 5. Valider les valeurs

```pascal
// Éviter les valeurs négatives
procedure TForm1.EditWidthChange(Sender: TObject);
var
  W: Integer;
begin
  if TryStrToInt(EditWidth.Text, W) then
  begin
    if W > 0 then
      Panel1.Width := W
    else
      ShowMessage('La largeur doit être positive');
  end;
end;
```

### 6. Utiliser les constantes pour les couleurs

❌ **Mauvais :**
```pascal
Button1.Color := $00FF8800;  // Pas clair
```

✅ **Bon :**
```pascal
const
  COULEUR_ACTIF = $00FF8800;
  COULEUR_INACTIF = clGray;

Button1.Color := COULEUR_ACTIF;
```

### 7. Sauvegarder/Restaurer l'état

```pascal
// Sauvegarder
var
  OldColor: TColor;
  OldEnabled: Boolean;
begin
  OldColor := Button1.Color;
  OldEnabled := Button1.Enabled;

  // Modifier temporairement
  Button1.Color := clRed;
  Button1.Enabled := False;

  // Traitement...

  // Restaurer
  Button1.Color := OldColor;
  Button1.Enabled := OldEnabled;
end;
```

---

## Propriétés et performances

### Propriétés coûteuses

Certaines propriétés déclenchent beaucoup de travail :

**Coûteux :**
- `Refresh` : Redessin complet
- `Font` : Recalcul de la taille du texte
- `Width/Height` : Réorganisation de la mise en page
- `Align` : Réorganisation de tous les enfants

**Léger :**
- `Tag` : Simple affectation
- `Enabled` : Changement d'état simple
- `Hint` : Simple affectation de texte

### Optimisation

```pascal
// ❌ Lent : modifie 100 fois
for i := 0 to 99 do
  Button1.Width := i;  // 100 redessin !

// ✅ Rapide : modifie une seule fois
Button1.Width := 99;  // 1 seul redessin
```

---

## Résumé

### Concepts clés

✅ Les **propriétés** définissent les caractéristiques d'un composant  
✅ Modification en **design-time** (Inspecteur) ou **run-time** (code)  
✅ **Héritage** : les propriétés sont transmises aux classes dérivées  
✅ Propriétés **published** apparaissent dans l'Inspecteur  
 
### Propriétés essentielles à retenir

**Position/Taille :**
- `Left`, `Top`, `Width`, `Height`
- `Anchors`, `Align`

**Apparence :**
- `Caption`, `Text`, `Color`, `Font`
- `Cursor`

**Comportement :**
- `Enabled`, `Visible`
- `Hint`, `ShowHint`
- `TabStop`, `TabOrder`

**Identification :**
- `Name`, `Tag`
- `Parent`, `Owner`

---

## Prochaines étapes

Maintenant que vous maîtrisez les propriétés des composants, vous pouvez :

- **14.7 Layouts et anchors** : Créer des interfaces adaptatives
- **14.8 Menus et barres d'outils** : Enrichir l'interface
- **14.9 Boîtes de dialogue standard** : Utiliser les dialogues système
- **15. Composants LCL Fondamentaux** : Découvrir plus de composants

Vous avez maintenant toutes les bases pour créer des interfaces professionnelles ! 🎨

---

**Point clé à retenir :** Les propriétés sont au cœur de la personnalisation des composants. Maîtrisez-les pour créer des interfaces élégantes et fonctionnelles !

⏭️ [Layouts et anchors](/14-introduction-applications-graphiques/07-layouts-anchors.md)
