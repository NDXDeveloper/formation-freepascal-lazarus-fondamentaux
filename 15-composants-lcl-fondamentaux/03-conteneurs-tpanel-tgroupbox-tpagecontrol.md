🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.3 Conteneurs (TPanel, TGroupBox, TPageControl)

## Introduction

Les **conteneurs** sont des composants spéciaux qui peuvent accueillir d'autres composants. Ils permettent d'organiser visuellement et logiquement votre interface utilisateur. Sans eux, tous vos boutons, zones de texte et étiquettes devraient être placés directement sur le formulaire, ce qui deviendrait vite chaotique !

Dans ce chapitre, nous allons explorer les trois conteneurs les plus utilisés de la LCL :

- **TPanel** : le conteneur universel et versatile
- **TGroupBox** : pour regrouper visuellement des éléments liés
- **TPageControl** : pour créer des interfaces à onglets

---

## Qu'est-ce qu'un Conteneur ?

### Définition Simple

Un conteneur est un composant qui peut **contenir** d'autres composants. C'est comme une boîte dans laquelle vous pouvez placer des objets.

### Analogie

Imaginez votre bureau :
- **Le bureau lui-même** = le formulaire (TForm)
- **Les tiroirs et organiseurs** = les conteneurs (TPanel, TGroupBox)
- **Les stylos, crayons, trombones** = les composants (TButton, TEdit, TLabel)

Les organiseurs vous permettent de ranger et de structurer votre espace de travail. De même, les conteneurs vous permettent d'organiser votre interface.

### Pourquoi Utiliser des Conteneurs ?

✅ **Organisation visuelle** : regrouper des éléments liés  
✅ **Gestion simplifiée** : déplacer un conteneur déplace tous ses enfants  
✅ **Réutilisabilité** : masquer/afficher plusieurs composants en une fois  
✅ **Layout intelligent** : alignement et redimensionnement automatique  
✅ **Séparation logique** : structurer votre code et votre interface

---

## TPanel : Le Conteneur Universel

### Présentation

`TPanel` est le conteneur le plus polyvalent de la LCL. C'est une surface rectangulaire qui peut contenir n'importe quel autre composant.

### Hiérarchie

```
TWinControl
  └─ TCustomControl
       └─ TCustomPanel
            └─ TPanel
```

`TPanel` hérite de `TCustomControl`, ce qui signifie qu'il :
- Possède un handle système (TWinControl)
- Peut dessiner sur son Canvas (TCustomControl)
- Peut recevoir le focus
- Peut contenir d'autres composants

### Propriétés Principales

#### Apparence

```pascal
property Caption: string;           // Texte affiché (souvent vide)  
property Color: TColor;             // Couleur de fond  
property BevelInner: TPanelBevel;   // Bordure intérieure (bvNone, bvLowered, bvRaised)  
property BevelOuter: TPanelBevel;   // Bordure extérieure  
property BevelWidth: Integer;       // Épaisseur de la bordure  
property BorderStyle: TBorderStyle; // Style de bordure (bsSingle, bsNone)  
```

#### Layout et Alignement

```pascal
property Align: TAlign;             // Alignement automatique  
property AutoSize: Boolean;         // Taille automatique selon le contenu  
property ChildSizing: TControlChildSizing;  // Gestion des espaces entre enfants  
```

### Exemple de Base

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  Panel1: TPanel;
  Button1: TButton;
begin
  // Créer un panel
  Panel1 := TPanel.Create(Self);
  Panel1.Parent := Self;
  Panel1.SetBounds(10, 10, 300, 200);
  Panel1.Caption := '';
  Panel1.Color := clSilver;
  Panel1.BevelOuter := bvRaised;

  // Créer un bouton dans le panel
  Button1 := TButton.Create(Self);
  Button1.Parent := Panel1;  // Important : Parent = Panel1, pas Form1 !
  Button1.SetBounds(10, 10, 100, 30);
  Button1.Caption := 'Click Me';
end;
```

### Types de Bevel (Bordures 3D)

Les bordures donnent un aspect 3D au panel :

```
bvNone     : Aucune bordure
┌──────────┐
│          │
│          │
└──────────┘

bvLowered  : Aspect enfoncé
╔══════════╗
║          ║
║          ║
╚══════════╝

bvRaised   : Aspect surélevé
╒══════════╕
│          │
│          │
╘══════════╛
```

**Combinaisons courantes :**

```pascal
// Panel plat sans bordure
Panel1.BevelOuter := bvNone;  
Panel1.BevelInner := bvNone;  

// Panel enfoncé (pour zones de saisie)
Panel1.BevelOuter := bvLowered;

// Panel surélevé (pour barres d'outils)
Panel1.BevelOuter := bvRaised;

// Panel avec double bordure
Panel1.BevelOuter := bvRaised;  
Panel1.BevelInner := bvLowered;  
```

### La Propriété Align

`Align` permet de positionner automatiquement le panel :

```pascal
type
  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);
```

**Valeurs courantes :**

| Valeur | Description | Usage typique |
|--------|-------------|---------------|
| **alNone** | Position manuelle | Position personnalisée |
| **alTop** | Haut du parent, pleine largeur | Barre d'outils supérieure |
| **alBottom** | Bas du parent, pleine largeur | Barre d'état |
| **alLeft** | Gauche du parent, pleine hauteur | Menu latéral |
| **alRight** | Droite du parent, pleine hauteur | Panneau d'outils |
| **alClient** | Remplit tout l'espace restant | Zone de contenu principale |

### Exemple avec Align

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  PanelTop, PanelBottom, PanelClient: TPanel;
begin
  // Barre supérieure
  PanelTop := TPanel.Create(Self);
  PanelTop.Parent := Self;
  PanelTop.Align := alTop;
  PanelTop.Height := 50;
  PanelTop.Caption := 'Barre d''outils';
  PanelTop.Color := clSkyBlue;

  // Barre inférieure
  PanelBottom := TPanel.Create(Self);
  PanelBottom.Parent := Self;
  PanelBottom.Align := alBottom;
  PanelBottom.Height := 30;
  PanelBottom.Caption := 'Barre d''état';
  PanelBottom.Color := clMoneyGreen;

  // Zone centrale (remplit le reste)
  PanelClient := TPanel.Create(Self);
  PanelClient.Parent := Self;
  PanelClient.Align := alClient;
  PanelClient.Caption := 'Contenu principal';
  PanelClient.Color := clWhite;
end;
```

**Résultat visuel :**

```
┌─────────────────────────────┐
│    Barre d'outils (Top)     │
├─────────────────────────────┤
│                             │
│   Contenu principal         │
│   (Client - remplit)        │
│                             │
├─────────────────────────────┤
│  Barre d'état (Bottom)      │
└─────────────────────────────┘
```

### ChildSizing : Espacement Automatique

`ChildSizing` permet de gérer automatiquement l'espacement entre les composants enfants.

**Propriétés utiles :**

```pascal
Panel1.ChildSizing.LeftRightSpacing := 10;    // Espace horizontal  
Panel1.ChildSizing.TopBottomSpacing := 10;    // Espace vertical  
Panel1.ChildSizing.HorizontalSpacing := 5;    // Entre les colonnes  
Panel1.ChildSizing.VerticalSpacing := 5;      // Entre les lignes  
Panel1.ChildSizing.Layout := cclLeftToRightThenTopToBottom;  // Disposition  
```

### Cas d'Usage Typiques de TPanel

#### 1. Barre d'outils

```pascal
ToolbarPanel.Align := alTop;  
ToolbarPanel.Height := 40;  
ToolbarPanel.BevelOuter := bvNone;  
```

#### 2. Barre d'état

```pascal
StatusPanel.Align := alBottom;  
StatusPanel.Height := 25;  
StatusPanel.Caption := 'Prêt';  
```

#### 3. Zone de contenu avec défilement

```pascal
// Note : AutoScroll est une propriété de TScrollBox, pas de TPanel
// Pour le défilement, utiliser un TScrollBox à la place de TPanel
ScrollBox1.Align := alClient;  
ScrollBox1.AutoScroll := True;  // Ajoute des barres de défilement si nécessaire  
```

#### 4. Carte d'information

```pascal
InfoPanel.Width := 200;  
InfoPanel.Height := 150;  
InfoPanel.BevelOuter := bvRaised;  
InfoPanel.Color := clInfoBk;  
```

### Masquer/Afficher Plusieurs Composants

Un avantage majeur des conteneurs : masquer le panel masque tous ses enfants !

```pascal
// Masquer tout le groupe de composants
Panel1.Visible := False;

// Réafficher tout le groupe
Panel1.Visible := True;
```

---

## TGroupBox : Regroupement Visuel

### Présentation

`TGroupBox` est un conteneur avec une **bordure visible** et un **titre**. Il est parfait pour regrouper visuellement des éléments qui ont une relation logique (comme des boutons radio).

### Hiérarchie

```
TWinControl
  └─ TCustomGroupBox
       └─ TGroupBox
```

### Apparence Visuelle

```
┌─ Options d'affichage ───────┐
│                             │
│  ○ Vue Liste                │
│  ● Vue Icônes               │
│  ○ Vue Détails              │
│                             │
└─────────────────────────────┘
```

### Propriétés Principales

```pascal
property Caption: string;           // Titre du groupe (visible)  
property Color: TColor;             // Couleur de fond  
property Font: TFont;               // Police du titre  
```

### Différences avec TPanel

| Caractéristique | TPanel | TGroupBox |
|-----------------|--------|-----------|
| Bordure | Personnalisable (Bevel) | Cadre fixe avec titre |
| Caption | Optionnel, centré | Obligatoire, en haut à gauche |
| Usage typique | Conteneur universel | Regroupement logique |
| Apparence | Peut être invisible | Toujours visible |

### Exemple avec Boutons Radio

Les boutons radio dans un même conteneur forment un groupe exclusif (un seul peut être sélectionné).

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  GroupBox1: TGroupBox;
  Radio1, Radio2, Radio3: TRadioButton;
begin
  // Créer le GroupBox
  GroupBox1 := TGroupBox.Create(Self);
  GroupBox1.Parent := Self;
  GroupBox1.SetBounds(10, 10, 200, 120);
  GroupBox1.Caption := 'Choisissez une option';

  // Premier bouton radio
  Radio1 := TRadioButton.Create(Self);
  Radio1.Parent := GroupBox1;  // Parent = GroupBox1
  Radio1.SetBounds(10, 20, 180, 20);
  Radio1.Caption := 'Option A';
  Radio1.Checked := True;  // Sélectionné par défaut

  // Deuxième bouton radio
  Radio2 := TRadioButton.Create(Self);
  Radio2.Parent := GroupBox1;
  Radio2.SetBounds(10, 50, 180, 20);
  Radio2.Caption := 'Option B';

  // Troisième bouton radio
  Radio3 := TRadioButton.Create(Self);
  Radio3.Parent := GroupBox1;
  Radio3.SetBounds(10, 80, 180, 20);
  Radio3.Caption := 'Option C';
end;
```

**Comportement :** Cliquer sur Radio2 décoche automatiquement Radio1 (et vice-versa) car ils sont dans le même conteneur.

### Plusieurs Groupes de Boutons Radio

Pour avoir plusieurs groupes indépendants, utilisez plusieurs GroupBox :

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  GroupBox1, GroupBox2: TGroupBox;
begin
  // Premier groupe
  GroupBox1 := TGroupBox.Create(Self);
  GroupBox1.Parent := Self;
  GroupBox1.SetBounds(10, 10, 200, 100);
  GroupBox1.Caption := 'Taille';

  // Radios du groupe 1 (Parent = GroupBox1)
  // ...

  // Deuxième groupe (indépendant)
  GroupBox2 := TGroupBox.Create(Self);
  GroupBox2.Parent := Self;
  GroupBox2.SetBounds(10, 120, 200, 100);
  GroupBox2.Caption := 'Couleur';

  // Radios du groupe 2 (Parent = GroupBox2)
  // ...
end;
```

### Cas d'Usage Typiques de TGroupBox

#### 1. Options de configuration

```
┌─ Paramètres d'export ───────┐
│                             │
│  ☑ Inclure les images      │
│  ☐ Compresser              │
│  ☑ Créer une table des mat.│
│                             │
└─────────────────────────────┘
```

#### 2. Choix exclusif

```
┌─ Mode d'affichage ──────────┐
│                             │
│  ● Jour                     │
│  ○ Semaine                  │
│  ○ Mois                     │
│                             │
└─────────────────────────────┘
```

#### 3. Section d'un formulaire

```
┌─ Informations personnelles ─┐
│                             │
│  Nom : [____________]       │
│  Prénom : [____________]    │
│  Email : [____________]     │
│                             │
└─────────────────────────────┘
```

### Activer/Désactiver un Groupe

Désactiver un GroupBox désactive tous ses enfants :

```pascal
GroupBox1.Enabled := False;  // Tous les contrôles à l'intérieur sont désactivés
```

---

## TPageControl et TTabSheet : Interfaces à Onglets

### Présentation

`TPageControl` permet de créer des interfaces à **onglets** (tabs). C'est très utile quand vous avez beaucoup d'informations à afficher et que vous voulez les organiser en plusieurs pages.

### Hiérarchie

```
TWinControl
  └─ TCustomTabControl
       └─ TCustomPageControl
            └─ TPageControl

TWinControl
  └─ TCustomPage
       └─ TTabSheet
```

### Relation TPageControl / TTabSheet

- **TPageControl** : le conteneur d'onglets (contient les onglets)
- **TTabSheet** : une page/onglet individuel (contient les composants)

**Analogie :** TPageControl = le classeur à onglets, TTabSheet = chaque feuille du classeur.

### Apparence Visuelle

```
┌─────────────────────────────────┐
│ Général │ Affichage │ Avancé    │  ← Onglets (TTabSheet)
├─────────────────────────────────┤
│                                 │
│  Contenu de l'onglet actif      │
│                                 │
│  [Composants de cette page]     │
│                                 │
│                                 │
└─────────────────────────────────┘
```

### Création d'un TPageControl

#### Dans l'IDE Lazarus

1. Placez un `TPageControl` sur votre formulaire
2. Faites un clic droit sur le PageControl
3. Choisissez "Ajouter une page" (ou "New Page")
4. Répétez pour créer plusieurs onglets
5. Cliquez sur un onglet pour y placer des composants

#### Par Code

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  PageControl1: TPageControl;
  TabSheet1, TabSheet2, TabSheet3: TTabSheet;
begin
  // Créer le PageControl
  PageControl1 := TPageControl.Create(Self);
  PageControl1.Parent := Self;
  PageControl1.Align := alClient;  // Remplit le formulaire

  // Créer le premier onglet
  TabSheet1 := TTabSheet.Create(PageControl1);
  TabSheet1.PageControl := PageControl1;  // Important !
  TabSheet1.Caption := 'Général';

  // Créer le deuxième onglet
  TabSheet2 := TTabSheet.Create(PageControl1);
  TabSheet2.PageControl := PageControl1;
  TabSheet2.Caption := 'Affichage';

  // Créer le troisième onglet
  TabSheet3 := TTabSheet.Create(PageControl1);
  TabSheet3.PageControl := PageControl1;
  TabSheet3.Caption := 'Avancé';
end;
```

**Note importante :** Pour un TTabSheet, on utilise `PageControl` au lieu de `Parent` !

### Ajouter des Composants à un Onglet

```pascal
var
  TabSheet1: TTabSheet;
  Button1: TButton;
  Edit1: TEdit;
begin
  // ... création du TabSheet1 ...

  // Ajouter un bouton sur le premier onglet
  Button1 := TButton.Create(Self);
  Button1.Parent := TabSheet1;  // Parent = TabSheet
  Button1.SetBounds(10, 10, 100, 30);
  Button1.Caption := 'OK';

  // Ajouter un Edit sur le premier onglet
  Edit1 := TEdit.Create(Self);
  Edit1.Parent := TabSheet1;
  Edit1.SetBounds(10, 50, 200, 25);
  Edit1.Text := 'Texte';
end;
```

### Propriétés du TPageControl

```pascal
property ActivePage: TTabSheet;      // Onglet actuellement visible  
property ActivePageIndex: Integer;   // Index de l'onglet actif (0, 1, 2...)  
property PageCount: Integer;         // Nombre d'onglets  
property Pages[Index: Integer]: TTabSheet;  // Accès aux onglets par index  
property TabPosition: TTabPosition;  // Position des onglets (tpTop, tpBottom, etc.)  
property MultiLine: Boolean;         // Plusieurs lignes d'onglets si nécessaire  
property TabHeight: Integer;         // Hauteur des onglets  
property TabWidth: Integer;          // Largeur des onglets (0 = automatique)  
```

### Propriétés du TTabSheet

```pascal
property Caption: string;            // Texte de l'onglet  
property TabVisible: Boolean;        // Visibilité de l'onglet  
property PageIndex: Integer;         // Position de l'onglet  
property ImageIndex: Integer;        // Index d'image (si ImageList associée)  
```

### Changer d'Onglet par Code

```pascal
// Par index (0 = premier onglet)
PageControl1.ActivePageIndex := 1;  // Active le 2ème onglet

// Par référence
PageControl1.ActivePage := TabSheet3;  // Active TabSheet3

// Onglet suivant
if PageControl1.ActivePageIndex < PageControl1.PageCount - 1 then
  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;

// Onglet précédent
if PageControl1.ActivePageIndex > 0 then
  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
```

### Événements

```pascal
property OnChange: TNotifyEvent;  // Déclenché quand on change d'onglet  
property OnChanging: TTabChangingEvent;  // Avant le changement (peut être annulé)  
```

#### Exemple : Valider avant de changer d'onglet

```pascal
procedure TForm1.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);  
begin  
  // Empêcher de quitter l'onglet si le nom est vide
  if (PageControl1.ActivePage = TabSheet1) and (Edit1.Text = '') then
  begin
    ShowMessage('Veuillez saisir un nom avant de continuer');
    AllowChange := False;  // Annule le changement d'onglet
  end;
end;
```

### Position des Onglets

```pascal
type
  TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);
```

```pascal
// Onglets en haut (par défaut)
PageControl1.TabPosition := tpTop;

// Onglets en bas (style navigateur web)
PageControl1.TabPosition := tpBottom;

// Onglets à gauche (navigation verticale)
PageControl1.TabPosition := tpLeft;
```

### Masquer/Afficher des Onglets

```pascal
// Masquer un onglet (reste dans le PageControl mais invisible)
TabSheet2.TabVisible := False;

// Afficher un onglet
TabSheet2.TabVisible := True;
```

### Cas d'Usage Typiques de TPageControl

#### 1. Boîte de dialogue Options/Préférences

```
┌─────────────────────────────────┐
│ Général │ Editeur │ Apparence   │
├─────────────────────────────────┤
│                                 │
│  Paramètres généraux            │
│  [Composants...]                │
│                                 │
└─────────────────────────────────┘
```

#### 2. Assistant (Wizard)

```pascal
// Bouton Suivant
procedure TForm1.BtnNextClick(Sender: TObject);  
begin  
  if PageControl1.ActivePageIndex < PageControl1.PageCount - 1 then
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1
  else
    ShowMessage('Terminé !');
end;
```

#### 3. Fiches de données multiples

```
┌─────────────────────────────────┐
│ Détails │ Historique │ Notes    │
├─────────────────────────────────┤
│  Informations détaillées        │
│  [Grilles, listes...]           │
└─────────────────────────────────┘
```

#### 4. Interface MDI (Multiple Document Interface)

Simuler plusieurs documents ouverts avec un onglet par document.

---

## Imbrication de Conteneurs

Vous pouvez **imbriquer** des conteneurs les uns dans les autres pour créer des interfaces complexes.

### Exemple : Panel dans Panel

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  PanelMain, PanelLeft, PanelRight: TPanel;
begin
  // Panel principal
  PanelMain := TPanel.Create(Self);
  PanelMain.Parent := Self;
  PanelMain.Align := alClient;

  // Panel gauche (dans PanelMain)
  PanelLeft := TPanel.Create(Self);
  PanelLeft.Parent := PanelMain;  // Parent = PanelMain
  PanelLeft.Align := alLeft;
  PanelLeft.Width := 200;
  PanelLeft.Caption := 'Menu';

  // Panel droit (dans PanelMain)
  PanelRight := TPanel.Create(Self);
  PanelRight.Parent := PanelMain;  // Parent = PanelMain
  PanelRight.Align := alClient;
  PanelRight.Caption := 'Contenu';
end;
```

**Structure :**

```
Form1
  └─ PanelMain
       ├─ PanelLeft
       └─ PanelRight
```

**Résultat visuel :**

```
┌─────────────────────────────┐
│        │                    │
│  Menu  │     Contenu        │
│        │                    │
│        │                    │
└─────────────────────────────┘
```

### Exemple : PageControl avec GroupBox

```pascal
var
  PageControl1: TPageControl;
  TabSheet1: TTabSheet;
  GroupBox1: TGroupBox;
begin
  // PageControl
  PageControl1 := TPageControl.Create(Self);
  PageControl1.Parent := Self;
  PageControl1.Align := alClient;

  // Onglet
  TabSheet1 := TTabSheet.Create(PageControl1);
  TabSheet1.PageControl := PageControl1;
  TabSheet1.Caption := 'Paramètres';

  // GroupBox dans l'onglet
  GroupBox1 := TGroupBox.Create(Self);
  GroupBox1.Parent := TabSheet1;  // Parent = TabSheet
  GroupBox1.SetBounds(10, 10, 250, 150);
  GroupBox1.Caption := 'Options de sauvegarde';

  // Composants dans le GroupBox
  // ...
end;
```

**Structure :**

```
Form1
  └─ PageControl1
       └─ TabSheet1
            └─ GroupBox1
                 └─ [Composants]
```

### Limite d'Imbrication

Il n'y a pas de limite technique, mais :
- **Trop de niveaux** rendent le code difficile à maintenir
- **3-4 niveaux maximum** est une bonne pratique
- Privilégiez la simplicité

---

## Comparaison des Trois Conteneurs

| Aspect | TPanel | TGroupBox | TPageControl |
|--------|--------|-----------|--------------|
| **Bordure** | Personnalisable | Cadre fixe | Onglets |
| **Titre** | Optionnel, centré | Obligatoire, coin | Onglets individuels |
| **Usage principal** | Conteneur universel | Regroupement logique | Pages multiples |
| **Visibilité** | Peut être invisible | Toujours visible | Onglets visibles |
| **Sous-conteneurs** | TTabSheet | Non | Oui (TTabSheet) |
| **Canvas** | Oui | Non | Non |
| **Align** | Oui | Oui | Oui |

### Quand Utiliser Quoi ?

#### Utilisez TPanel quand :
- Vous avez besoin de flexibilité maximale
- Vous voulez créer des barres d'outils ou d'état
- Vous avez besoin de dessiner sur le conteneur
- Vous voulez un conteneur invisible

#### Utilisez TGroupBox quand :
- Vous voulez un regroupement visuellement marqué
- Vous avez des boutons radio à grouper
- Vous voulez une étiquette descriptive claire
- L'organisation logique est importante

#### Utilisez TPageControl quand :
- Vous avez beaucoup d'informations à organiser
- L'espace à l'écran est limité
- Vous créez une boîte de dialogue avec plusieurs sections
- Vous voulez une navigation claire entre sections

---

## Gestion Dynamique des Conteneurs

### Créer/Détruire Dynamiquement

```pascal
procedure TForm1.BtnAjouterPanelClick(Sender: TObject);  
var  
  NewPanel: TPanel;
begin
  NewPanel := TPanel.Create(Self);  // Self = Owner (destruction auto)
  NewPanel.Parent := Self;
  NewPanel.SetBounds(Random(300), Random(300), 100, 100);
  NewPanel.Caption := 'Panel ' + IntToStr(ComponentCount);
  NewPanel.Color := RGB(Random(256), Random(256), Random(256));
end;
```

### Ajouter des Onglets Dynamiquement

```pascal
procedure TForm1.BtnAjouterOngletClick(Sender: TObject);  
var  
  NewTab: TTabSheet;
begin
  NewTab := TTabSheet.Create(PageControl1);
  NewTab.PageControl := PageControl1;
  NewTab.Caption := 'Nouvel onglet ' + IntToStr(PageControl1.PageCount);

  // Activer le nouvel onglet
  PageControl1.ActivePage := NewTab;
end;
```

### Parcourir les Enfants d'un Conteneur

```pascal
procedure TForm1.AfficherEnfants(Conteneur: TWinControl);  
var  
  i: Integer;
begin
  for i := 0 to Conteneur.ControlCount - 1 do
  begin
    ShowMessage('Enfant ' + IntToStr(i) + ': ' +
                Conteneur.Controls[i].ClassName);
  end;
end;

// Utilisation
procedure TForm1.BtnVoirEnfantsClick(Sender: TObject);  
begin  
  AfficherEnfants(Panel1);
end;
```

---

## Bonnes Pratiques

### 1. Nommage Cohérent

Utilisez des noms descriptifs pour vos conteneurs :

```pascal
// ❌ Mauvais
Panel1, Panel2, Panel3

// ✅ Bon
PanelToolbar, PanelStatus, PanelContent
```

### 2. Utiliser Align Intelligemment

```pascal
// Pour une mise en page robuste
PanelTop.Align := alTop;  
PanelBottom.Align := alBottom;  
PanelClient.Align := alClient;  // Remplit automatiquement  
```

L'ordre de création est important avec `Align` :
1. Créez d'abord les panels `alTop` et `alBottom`
2. Créez ensuite le panel `alClient`

### 3. Libération Mémoire

Si vous créez avec `Self` comme Owner, pas besoin de libérer :

```pascal
// ✅ Bon - destruction automatique
Panel1 := TPanel.Create(Self);

// ❌ Évitez - doit être libéré manuellement
Panel2 := TPanel.Create(nil);
// ... plus tard ...
Panel2.Free;
```

### 4. Organiser Logiquement

Regroupez les éléments qui ont une relation logique :

```pascal
// ✅ Bon
GroupBoxAdresse.Caption := 'Adresse';
  // Edit pour rue
  // Edit pour ville
  // Edit pour code postal

// ❌ Mauvais - tout mélangé sur le formulaire
```

### 5. Ne Pas Surcharger

Évitez trop de niveaux d'imbrication :

```pascal
// ❌ Trop complexe
Form1 → Panel1 → Panel2 → GroupBox1 → Panel3 → Button1

// ✅ Plus simple
Form1 → Panel1 → GroupBox1 → Button1
```

### 6. Utiliser les Ancres

Pour les conteneurs qui ne doivent pas remplir tout l'espace :

```pascal
Panel1.Anchors := [akLeft, akTop, akRight];  // S'étire horizontalement  
Panel1.Align := alNone;  
```

---

## Interface Typique d'Application

Voici une structure classique combinant les trois conteneurs :

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  PanelTop, PanelBottom, PanelClient: TPanel;
  PageControl1: TPageControl;
  TabGeneral, TabOptions: TTabSheet;
  GroupBox1: TGroupBox;
begin
  // Barre d'outils supérieure
  PanelTop := TPanel.Create(Self);
  PanelTop.Parent := Self;
  PanelTop.Align := alTop;
  PanelTop.Height := 50;
  PanelTop.BevelOuter := bvNone;
  // Ajouter boutons de la barre d'outils...

  // Barre d'état inférieure
  PanelBottom := TPanel.Create(Self);
  PanelBottom.Parent := Self;
  PanelBottom.Align := alBottom;
  PanelBottom.Height := 25;
  PanelBottom.Caption := 'Prêt';

  // Zone centrale avec PageControl
  PanelClient := TPanel.Create(Self);
  PanelClient.Parent := Self;
  PanelClient.Align := alClient;
  PanelClient.BevelOuter := bvNone;

  PageControl1 := TPageControl.Create(Self);
  PageControl1.Parent := PanelClient;
  PageControl1.Align := alClient;

  // Onglet Général
  TabGeneral := TTabSheet.Create(PageControl1);
  TabGeneral.PageControl := PageControl1;
  TabGeneral.Caption := 'Général';

  GroupBox1 := TGroupBox.Create(Self);
  GroupBox1.Parent := TabGeneral;
  GroupBox1.SetBounds(10, 10, 300, 150);
  GroupBox1.Caption := 'Informations';
  // Ajouter composants dans le GroupBox...

  // Onglet Options
  TabOptions := TTabSheet.Create(PageControl1);
  TabOptions.PageControl := PageControl1;
  TabOptions.Caption := 'Options';
  // Ajouter composants...
end;
```

**Structure résultante :**

```
┌─────────────────────────────────┐
│  [Barre d'outils - Boutons]     │ ← PanelTop
├─────────────────────────────────┤
│ Général │ Options               │ ← PageControl
│┌───────────────────────────────┐│
││ ┌─ Informations ────────┐     ││
││ │                       │     ││ ← GroupBox dans TabGeneral
││ │  [Composants]         │     ││
││ │                       │     ││
││ └───────────────────────┘     ││
│└───────────────────────────────┘│
├─────────────────────────────────┤
│  Prêt                           │ ← PanelBottom
└─────────────────────────────────┘
```

---

## Points Clés à Retenir

1. **TPanel** est le conteneur le plus polyvalent
   - Bordures personnalisables avec BevelOuter/BevelInner
   - Propriété Align pour mise en page automatique
   - Peut dessiner sur son Canvas

2. **TGroupBox** regroupe visuellement des éléments liés
   - Toujours visible avec titre
   - Parfait pour les boutons radio
   - Regroupement logique clair

3. **TPageControl** organise en onglets
   - Utilise des TTabSheet comme pages
   - Économise l'espace à l'écran
   - Navigation facile entre sections

4. **Imbrication** : les conteneurs peuvent contenir d'autres conteneurs
   - Maximum 3-4 niveaux recommandé
   - Structure hiérarchique claire

5. **Propriété Parent** : détermine où le composant est affiché
   - Pour TTabSheet, utilisez `PageControl` au lieu de `Parent`

6. **Align** : positionnement automatique
   - alTop, alBottom, alLeft, alRight, alClient
   - Adaptation automatique au redimensionnement

7. **Masquer/Afficher** : masquer le conteneur masque tous ses enfants

8. **Owner** : gestion automatique de la mémoire
   - Créez avec `Self` comme Owner
   - Pas besoin de libérer manuellement

---

## Conclusion

Les conteneurs sont essentiels pour créer des interfaces professionnelles et organisées. Ils vous permettent de :

- **Structurer** votre interface logiquement
- **Simplifier** la gestion de nombreux composants
- **Adapter** automatiquement la mise en page
- **Réutiliser** des groupes de composants

Maîtriser TPanel, TGroupBox et TPageControl vous donne les outils pour créer des interfaces complexes et élégantes. Dans la section suivante, nous explorerons les **listes** (TListBox, TComboBox, TTreeView) pour afficher et gérer des collections de données.

---

**Prochaine étape :** 15.4 Listes (TListBox, TComboBox, TTreeView)

⏭️ [Listes (TListBox, TComboBox, TTreeView)](/15-composants-lcl-fondamentaux/04-listes-tlistbox-tcombobox-ttreeview.md)
