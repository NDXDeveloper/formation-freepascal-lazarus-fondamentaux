🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.2 Hiérarchie des composants

## Introduction

Dans la section précédente, nous avons découvert l'architecture globale de la LCL. Maintenant, nous allons explorer en profondeur la **hiérarchie des composants**, c'est-à-dire comment les différentes classes sont organisées et héritent les unes des autres.

Comprendre cette hiérarchie est essentiel car elle explique pourquoi tous les boutons ont une propriété `Width`, pourquoi certains composants peuvent contenir d'autres composants, et comment vous pouvez créer vos propres composants personnalisés.

---

## Rappel : Qu'est-ce que l'Héritage ?

L'héritage est un concept fondamental de la Programmation Orientée Objet (POO) que vous avez découvert dans la Partie II du tutoriel.

### Principe de base

Quand une classe **hérite** d'une autre classe, elle récupère automatiquement toutes ses propriétés, méthodes et événements.

```pascal
TAnimal = class
  procedure Manger;
end;

TChien = class(TAnimal)  // TChien hérite de TAnimal
  procedure Aboyer;
end;
```

Un `TChien` peut donc :
- Appeler `Manger` (hérité de `TAnimal`)
- Appeler `Aboyer` (défini dans `TChien`)

### Application aux composants LCL

La LCL utilise massivement l'héritage. Par exemple, `TButton` hérite de `TWinControl`, qui hérite de `TControl`, qui hérite de `TComponent`, qui hérite de `TObject`.

Résultat : un `TButton` possède toutes les capacités de ses ancêtres !

---

## Vue d'Ensemble de la Hiérarchie

Voici une vue simplifiée de la hiérarchie complète des composants LCL :

```
TObject (classe racine de tout en Pascal)
  │
  └─ TPersistent (pour la persistance/streaming)
       │
       └─ TComponent (composants, gestion Owner/Name)
            │
            ├─ [Composants non-visuels]
            │    ├─ TTimer
            │    ├─ TDataSource
            │    ├─ TOpenDialog, TSaveDialog
            │    ├─ TImageList
            │    └─ ...
            │
            └─ TControl (composants visuels)
                 │
                 ├─ TGraphicControl (sans handle)
                 │    ├─ TLabel
                 │    ├─ TImage
                 │    ├─ TShape
                 │    ├─ TBevel
                 │    ├─ TSpeedButton
                 │    └─ ...
                 │
                 └─ TWinControl (avec handle)
                      │
                      ├─ TCustomControl (avec Canvas)
                      │    ├─ TPanel
                      │    ├─ TScrollBox
                      │    ├─ TCustomForm
                      │    │    └─ TForm
                      │    └─ ...
                      │
                      ├─ TButtonControl
                      │    ├─ TButton
                      │    ├─ TBitBtn
                      │    └─ TToggleBox
                      │
                      ├─ TCustomEdit
                      │    ├─ TEdit
                      │    ├─ TMemo
                      │    └─ TMaskEdit
                      │
                      ├─ TCustomListBox
                      │    └─ TListBox
                      │
                      ├─ TCustomComboBox
                      │    └─ TComboBox
                      │
                      ├─ TCustomCheckBox
                      │    ├─ TCheckBox
                      │    └─ TRadioButton
                      │
                      ├─ TScrollBar
                      ├─ TGroupBox
                      ├─ TPageControl
                      ├─ TTreeView
                      ├─ TListView
                      ├─ TStringGrid
                      └─ ...
```

---

## TObject : La Racine Universelle

### Qu'est-ce que TObject ?

`TObject` est la classe **racine** de toute la hiérarchie Pascal. Toutes les classes en héritent, directement ou indirectement.

### Méthodes importantes de TObject

```pascal
TObject = class
  constructor Create;
  destructor Destroy; virtual;
  procedure Free;
  function ClassName: string;
  function ClassType: TClass;
  // ... et d'autres
end;
```

**Méthodes clés :**

- **`Create`** : constructeur par défaut
- **`Destroy`** : destructeur (toujours virtuel)
- **`Free`** : libère l'objet de manière sécurisée (vérifie si non nil)
- **`ClassName`** : retourne le nom de la classe ('TButton', 'TEdit', etc.)
- **`ClassType`** : retourne le type de la classe

### Exemple pratique

```pascal
var
  Obj: TObject;
begin
  Obj := TButton.Create(Self);
  ShowMessage(Obj.ClassName);  // Affiche : "TButton"
  Obj.Free;
end;
```

---

## TPersistent : La Persistance des Données

### Rôle de TPersistent

`TPersistent` hérite de `TObject` et ajoute la capacité de **sauvegarder et charger** l'état d'un objet (streaming).

```pascal
TPersistent = class(TObject)
  procedure Assign(Source: TPersistent); virtual;
  // ... méthodes de streaming
end;
```

### La Méthode Assign

La méthode `Assign` permet de copier les propriétés d'un objet vers un autre :

```pascal
Button2.Assign(Button1);  // Copie les propriétés de Button1 vers Button2
```

### Pourquoi c'est important ?

- Permet de sauvegarder vos formulaires dans des fichiers `.lfm`
- Permet de copier/coller des composants dans l'IDE
- Facilite la duplication de configurations

---

## TComponent : Le Fondement des Composants

### Qu'est-ce que TComponent ?

`TComponent` est la classe de base de **tous les composants** Lazarus, qu'ils soient visuels ou non.

### Propriétés fondamentales

```pascal
TComponent = class(TPersistent)
  property Name: string;        // Nom unique du composant
  property Owner: TComponent;   // Propriétaire (pour gestion mémoire)
  property Tag: PtrInt;         // Entier libre pour vos usages
  property Components[Index]: TComponent;  // Liste des composants possédés
  property ComponentCount: Integer;        // Nombre de composants possédés
end;
```

### Le Système Owner (Propriétaire)

Le système `Owner` est crucial pour la gestion automatique de la mémoire.

**Principe :**
- Quand vous créez un composant avec un propriétaire : `TButton.Create(Form1)`
- Le propriétaire (`Form1`) garde une référence au composant
- Quand le propriétaire est détruit, il détruit automatiquement tous ses composants

**Exemple :**

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  MyButton: TButton;
begin
  // Form1 est le propriétaire du bouton
  MyButton := TButton.Create(Self);
  MyButton.Parent := Self;
  MyButton.Caption := 'Click Me';
  // Pas besoin de MyButton.Free : Form1 s'en charge automatiquement !
end;
```

### La Propriété Name

La propriété `Name` est le nom unique du composant dans son conteneur :

```pascal
Button1.Name := 'btnOk';  // Généralement défini automatiquement par l'IDE
```

Ce nom est utilisé pour :
- Identifier le composant dans le code
- Sauvegarder/charger depuis les fichiers `.lfm`
- Déboguer (afficher quel composant génère une erreur)

### La Propriété Tag

`Tag` est un entier libre que vous pouvez utiliser comme bon vous semble :

```pascal
Button1.Tag := 100;  
Button2.Tag := 200;  

procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  case (Sender as TButton).Tag of
    100: ShowMessage('Bouton 1 cliqué');
    200: ShowMessage('Bouton 2 cliqué');
  end;
end;
```

C'est pratique pour stocker une valeur associée au composant sans créer de variable supplémentaire.

---

## TControl : Les Composants Visuels

### Caractéristiques de TControl

`TControl` est la classe de base de **tous les composants visuels** (ceux qui apparaissent à l'écran).

### Propriétés Héritées par Tous les Contrôles

#### Position et Taille

```pascal
property Left: Integer;     // Position horizontale (pixels)  
property Top: Integer;      // Position verticale (pixels)  
property Width: Integer;    // Largeur (pixels)  
property Height: Integer;   // Hauteur (pixels)  
```

#### Apparence

```pascal
property Color: TColor;           // Couleur de fond  
property Font: TFont;             // Police de caractères  
property Cursor: TCursor;         // Curseur de souris  
property Visible: Boolean;        // Visibilité  
property Enabled: Boolean;        // Activation/désactivation  
property ShowHint: Boolean;       // Afficher l'infobulle  
property Hint: string;            // Texte de l'infobulle  
```

#### Ancrage et Alignement

```pascal
property Align: TAlign;           // Alignement (alTop, alClient, etc.)  
property Anchors: TAnchors;       // Points d'ancrage  
property BorderSpacing: TControlBorderSpacing;  // Marges  
```

#### Événements Souris et Clavier

```pascal
property OnClick: TNotifyEvent;  
property OnDblClick: TNotifyEvent;  
property OnMouseDown: TMouseEvent;  
property OnMouseMove: TMouseMoveEvent;  
property OnMouseUp: TMouseEvent;  
property OnKeyPress: TKeyPressEvent;  
property OnKeyDown: TKeyEvent;  
property OnKeyUp: TKeyEvent;  
```

### Méthodes Utiles

```pascal
procedure Show;                      // Rendre visible  
procedure Hide;                      // Masquer  
procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);  // Tout définir en une fois  
procedure BringToFront;              // Mettre au premier plan  
procedure SendToBack;                // Envoyer à l'arrière-plan  
function ClientToScreen(Point: TPoint): TPoint;  // Convertir coordonnées  
function ScreenToClient(Point: TPoint): TPoint;  // Convertir coordonnées  
```

### Exemple : Propriétés Communes

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Toutes ces propriétés viennent de TControl
  Button1.Left := 10;
  Button1.Top := 20;
  Button1.Width := 100;
  Button1.Height := 30;
  Button1.Enabled := True;
  Button1.Visible := True;
  Button1.Color := clYellow;
  Button1.Hint := 'Cliquez pour continuer';
  Button1.ShowHint := True;
end;
```

---

## TGraphicControl : Les Contrôles Dessinés

### Qu'est-ce qu'un TGraphicControl ?

`TGraphicControl` hérite de `TControl` et représente les composants **sans handle Windows** (sans fenêtre système propre).

### Caractéristiques

- **Pas de handle système** : plus légers en ressources
- **Dessinés par leur parent** : utilisent le Canvas du conteneur
- **Ne peuvent pas recevoir le focus clavier**
- **Ne peuvent pas contenir d'autres composants**
- **Transparence possible** : peuvent avoir un fond transparent

### Composants Typiques

| Composant | Description | Usage typique |
|-----------|-------------|---------------|
| **TLabel** | Étiquette de texte | Afficher du texte statique |
| **TImage** | Affichage d'image | Icônes, photos, logos |
| **TShape** | Formes géométriques | Rectangles, ellipses, lignes |
| **TBevel** | Bordure décorative | Séparateurs visuels |
| **TSpeedButton** | Bouton sans focus | Barres d'outils |

### Événement OnPaint

Les `TGraphicControl` ont un événement `OnPaint` pour dessiner leur contenu :

```pascal
procedure TMyLabel.Paint;  
begin  
  inherited;  // Appel du Paint parent
  Canvas.TextOut(5, 5, 'Mon texte');
end;
```

### Quand Utiliser TGraphicControl ?

✅ **Utiliser TGraphicControl quand :**
- Vous avez besoin d'affichage simple
- La légèreté est importante (beaucoup de composants)
- Pas besoin d'interaction clavier
- Transparence désirée

❌ **Ne pas utiliser TGraphicControl quand :**
- Besoin du focus clavier
- Besoin de contenir d'autres composants
- Interaction complexe requise

---

## TWinControl : Les Contrôles avec Fenêtre

### Qu'est-ce qu'un TWinControl ?

`TWinControl` hérite de `TControl` et représente les composants qui possèdent un **handle** (fenêtre système native).

### Caractéristiques

- **Handle système** : `Handle: HWND` (Windows) ou équivalent
- **Peuvent recevoir le focus** : `TabStop`, `TabOrder`
- **Peuvent contenir d'autres composants** : sont des conteneurs
- **Gérés par le système** : plus de fonctionnalités natives

### Propriétés Spécifiques

```pascal
property Handle: HWND;           // Identificateur de fenêtre système  
property TabStop: Boolean;       // Peut recevoir le focus par Tab  
property TabOrder: Integer;      // Ordre de tabulation  
property ControlCount: Integer;  // Nombre de contrôles enfants  
property Controls[Index]: TControl;  // Contrôles enfants  
```

### Événements Spécifiques

```pascal
property OnEnter: TNotifyEvent;   // Reçoit le focus  
property OnExit: TNotifyEvent;    // Perd le focus  
```

### Le Handle : Qu'est-ce que c'est ?

Le **handle** est un nombre entier qui identifie de manière unique une fenêtre dans le système d'exploitation.

**À quoi sert-il ?**
- Communiquer directement avec l'API système
- Identifier la fenêtre dans les messages système
- Permettre des opérations bas niveau

**Exemple d'utilisation avancée :**

```pascal
{$IFDEF WINDOWS}
// Faire clignoter la barre de titre (Windows uniquement)
FlashWindow(Form1.Handle, True);
{$ENDIF}
```

⚠️ **Note :** En tant que débutant, vous n'aurez presque jamais besoin d'utiliser directement le handle. La LCL s'en occupe pour vous !

### Composants Typiques

| Composant | Description | Usage typique |
|-----------|-------------|---------------|
| **TButton** | Bouton standard | Actions principales |
| **TEdit** | Zone de saisie | Saisie texte court |
| **TMemo** | Zone multiligne | Saisie texte long |
| **TListBox** | Liste de choix | Sélection dans une liste |
| **TComboBox** | Liste déroulante | Sélection compacte |
| **TPanel** | Panneau conteneur | Regrouper des contrôles |
| **TForm** | Formulaire | Fenêtre principale |

---

## TCustomControl : Les Contrôles Personnalisables

### Qu'est-ce que TCustomControl ?

`TCustomControl` hérite de `TWinControl` et ajoute la capacité de **dessiner** sur le composant via un Canvas.

### Caractéristiques Uniques

- **Canvas accessible** : possède une surface de dessin
- **OnPaint disponible** : pour dessiner du contenu personnalisé
- **Meilleur des deux mondes** : handle + dessin

### Différence avec TGraphicControl

| Caractéristique | TGraphicControl | TCustomControl |
|-----------------|-----------------|----------------|
| Handle système | ❌ Non | ✅ Oui |
| Canvas propre | ❌ Non (parent) | ✅ Oui |
| Peut recevoir focus | ❌ Non | ✅ Oui |
| Peut contenir composants | ❌ Non | ✅ Oui |
| Ressources | Légères | Plus lourdes |

### Composants Basés sur TCustomControl

- **TPanel** : conteneur avec dessin personnalisé
- **TScrollBox** : zone avec barres de défilement
- **TCustomForm** (et donc **TForm**) : formulaires

### Exemple : Dessiner sur un Panel

```pascal
procedure TForm1.Panel1Paint(Sender: TObject);  
begin  
  with Panel1.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClientRect);

    Pen.Color := clBlue;
    Pen.Width := 2;
    MoveTo(0, 0);
    LineTo(Panel1.Width, Panel1.Height);
  end;
end;
```

---

## Branches Spécialisées

### TButtonControl et Dérivés

```
TButtonControl (base abstraite pour boutons)
  ├─ TButton (bouton standard)
  ├─ TBitBtn (bouton avec icône)
  └─ TToggleBox (bouton à bascule)
```

**Propriétés communes :**
- `Caption` : texte du bouton
- `ModalResult` : résultat pour les boîtes de dialogue
- `Default` : bouton par défaut (Entrée)
- `Cancel` : bouton d'annulation (Échap)

### TCustomEdit et Dérivés

```
TCustomEdit (base pour zones de saisie)
  ├─ TEdit (saisie simple ligne)
  ├─ TMemo (saisie multiligne)
  ├─ TMaskEdit (saisie avec masque)
  └─ TSpinEdit (saisie numérique avec +/-)
```

**Propriétés communes :**
- `Text` : contenu textuel
- `ReadOnly` : lecture seule
- `MaxLength` : longueur maximale
- `PasswordChar` : caractère pour mots de passe

### TCustomListBox et Famille

```
TCustomListBox
  └─ TListBox (liste simple)

TCustomComboBox
  ├─ TComboBox (liste déroulante)
  └─ TColorBox (sélection de couleur)
```

**Propriétés communes :**
- `Items` : liste des éléments
- `ItemIndex` : élément sélectionné
- `Sorted` : tri automatique

### TCustomCheckBox et Famille

```
TCustomCheckBox
  ├─ TCheckBox (case à cocher)
  └─ TRadioButton (bouton radio)
```

**Propriétés communes :**
- `Checked` : état coché/décoché
- `Caption` : texte associé

---

## TCustomForm et TForm : Les Formulaires

### Hiérarchie

```
TWinControl
  └─ TCustomControl
       └─ TCustomForm
            └─ TForm
```

### Pourquoi TCustomForm ET TForm ?

- **TCustomForm** : classe de base, propriétés minimales, réutilisable
- **TForm** : classe complète pour l'utilisateur final, toutes les propriétés publiées

C'est un pattern courant dans la LCL : les classes `TCustomXxx` sont la base technique, les classes `TXxx` sont pour l'utilisation.

### Propriétés Spécifiques aux Formulaires

```pascal
property Caption: string;              // Titre de la fenêtre  
property Position: TPosition;          // Position à l'ouverture  
property BorderStyle: TFormBorderStyle;  // Style de bordure  
property BorderIcons: TBorderIcons;    // Icônes (min, max, close)  
property FormStyle: TFormStyle;        // Normal, MDI, etc.  
property WindowState: TWindowState;    // Normal, Minimized, Maximized  
```

### Événements Spécifiques

```pascal
property OnCreate: TNotifyEvent;    // Création du formulaire  
property OnShow: TNotifyEvent;      // Avant affichage  
property OnClose: TCloseEvent;      // Fermeture  
property OnCloseQuery: TCloseQueryEvent;  // Peut annuler la fermeture  
property OnDestroy: TNotifyEvent;   // Destruction  
```

### Cycle de Vie d'un Formulaire

```
1. Création       → Create
2. Initialisation → OnCreate
3. Affichage      → OnShow
4. Visible        → OnPaint (si nécessaire)
5. Utilisation    → Événements utilisateur
6. Demande fermeture → OnCloseQuery
7. Fermeture      → OnClose
8. Destruction    → OnDestroy → Destroy
```

---

## Les Conteneurs : Composants Parents

Certains composants peuvent **contenir** d'autres composants. Ce sont des conteneurs.

### Principaux Conteneurs

| Conteneur | Description | Usage typique |
|-----------|-------------|---------------|
| **TForm** | Fenêtre principale | Conteneur racine |
| **TPanel** | Panneau simple | Regroupement visuel |
| **TGroupBox** | Boîte avec cadre | Regroupement logique |
| **TScrollBox** | Zone avec défilement | Contenu plus grand que visible |
| **TPageControl** | Onglets | Navigation entre pages |
| **TTabSheet** | Page d'onglet | Contenu d'un onglet |
| **TSplitter** | Séparateur redimensionnable | Diviser l'espace |

### La Propriété Parent

Pour qu'un contrôle s'affiche, il doit avoir un `Parent` :

```pascal
Button1.Parent := Panel1;  // Le bouton s'affiche dans le panel
```

Sans `Parent`, le composant existe en mémoire mais est invisible !

### Hiérarchie Visuelle

```
Form1 (TForm)
  └─ Panel1 (TPanel)
       ├─ Button1 (TButton)
       ├─ Edit1 (TEdit)
       └─ Label1 (TLabel)
```

Dans cet exemple :
- `Panel1.Parent` = `Form1`
- `Button1.Parent` = `Panel1`
- `Edit1.Parent` = `Panel1`
- `Label1.Parent` = `Panel1`

---

## Classes "Custom" vs Classes Finales

Vous remarquerez beaucoup de classes préfixées par `TCustom` dans la hiérarchie.

### Le Pattern TCustomXxx

```
TCustomEdit (propriétés protected/private)
  └─ TEdit (propriétés published)
```

### Pourquoi ce Pattern ?

**TCustomXxx :**
- Implémentation complète de la fonctionnalité
- Propriétés en `protected` ou `private`
- Réutilisable pour créer des variantes
- Non directement utilisée par l'utilisateur

**TXxx :**
- Hérite de `TCustomXxx`
- Publie les propriétés en `published` (visibles dans l'inspecteur d'objets)
- Classe finale pour l'utilisateur

### Exemple Concret

```pascal
// Dans la LCL
TCustomButton = class(TButtonControl)  
private  
  FCustomProperty: Integer;
protected
  property CustomProperty: Integer read FCustomProperty write FCustomProperty;
end;

TButton = class(TCustomButton)  
published  
  // Rend visible dans l'IDE
  property Caption;
  property TabOrder;
  property OnClick;
  property CustomProperty;  // Si on veut l'exposer
end;
```

### Quand Utiliser les Classes Custom ?

- **Pour l'utilisateur normal** : utilisez toujours `TButton`, `TEdit`, etc.
- **Pour créer un composant personnalisé** : héritez de `TCustomButton`, `TCustomEdit`, etc.

---

## Composants Non-Visuels

Tous les composants n'héritent pas de `TControl`. Certains héritent directement de `TComponent`.

### Exemples de Composants Non-Visuels

```
TComponent
  ├─ TTimer (déclenchement périodique)
  ├─ TDataSource (connexion données)
  ├─ TMainMenu (menu de l'application)
  ├─ TPopupMenu (menu contextuel)
  ├─ TImageList (collection d'images)
  ├─ TOpenDialog (dialogue d'ouverture)
  ├─ TSaveDialog (dialogue de sauvegarde)
  ├─ TActionList (liste d'actions)
  └─ ... (connexions BD, etc.)
```

### Caractéristiques

- **Visibles dans l'IDE** (mode design) mais **invisibles à l'exécution**
- Pas de propriétés visuelles (Left, Top, Width, etc.)
- Souvent placés en bas du formulaire dans l'IDE
- Rôle : services, dialogues, gestion de données

### Exemple : TTimer

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  Timer1.Interval := 1000;  // 1 seconde
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);  
begin  
  Label1.Caption := TimeToStr(Now);  // Affiche l'heure actuelle
end;
```

---

## Naviguer dans la Hiérarchie : Opérateurs is et as

### L'Opérateur is : Tester le Type

L'opérateur `is` permet de vérifier si un objet est d'un certain type ou hérite de ce type :

```pascal
if Button1 is TButton then
  ShowMessage('C''est un bouton');

if Button1 is TWinControl then
  ShowMessage('C''est un TWinControl (ou dérivé)');  // TRUE car TButton hérite de TWinControl

if Button1 is TGraphicControl then
  ShowMessage('C''est un TGraphicControl');  // FALSE
```

### L'Opérateur as : Transtypage Sécurisé

L'opérateur `as` permet de convertir un objet vers un type spécifique avec vérification :

```pascal
procedure TForm1.ButtonClick(Sender: TObject);  
var  
  Btn: TButton;
begin
  // Sender est de type TObject
  // On le convertit en TButton
  Btn := Sender as TButton;
  ShowMessage('Vous avez cliqué sur : ' + Btn.Caption);
end;
```

Si l'objet n'est pas du bon type, une exception est levée.

### Pourquoi c'est Utile ?

Exemple pratique : gérer plusieurs boutons avec le même gestionnaire :

```pascal
procedure TForm1.UniversalButtonClick(Sender: TObject);  
begin  
  if Sender is TButton then
    ShowMessage('Bouton : ' + (Sender as TButton).Caption)
  else if Sender is TLabel then
    ShowMessage('Label : ' + (Sender as TLabel).Caption);
end;
```

---

## Propriétés Published : Visibilité dans l'IDE

### Les Niveaux de Visibilité

En POO Pascal, il existe plusieurs niveaux de visibilité :

| Visibilité | Accessible depuis | Inspecteur d'objets |
|------------|-------------------|---------------------|
| **private** | La classe elle-même | ❌ Non |
| **protected** | La classe et ses descendants | ❌ Non |
| **public** | Partout | ❌ Non |
| **published** | Partout | ✅ Oui |

### Published : Le Lien avec l'IDE

Les propriétés déclarées en `published` apparaissent dans **l'Inspecteur d'Objets** de Lazarus :

```pascal
TMyButton = class(TButton)  
published  
  property Color;     // Visible dans l'IDE
  property Caption;   // Visible dans l'IDE
  property OnClick;   // Visible dans l'IDE
end;
```

### Pourquoi c'est Important ?

Sans `published`, vous ne pourriez pas :
- Modifier les propriétés dans l'Inspecteur d'Objets
- Sauvegarder les propriétés dans le fichier `.lfm`
- Assigner des gestionnaires d'événements visuellement

---

## Héritage et Redéfinition de Méthodes

### Méthodes Virtual et Override

Dans la hiérarchie LCL, de nombreuses méthodes sont `virtual` (virtuelles), ce qui permet de les redéfinir dans les classes dérivées.

```pascal
// Dans TControl
procedure Paint; virtual;

// Dans TButton (descendant)
procedure Paint; override;  // Redéfinition  
begin  
  inherited Paint;  // Appel de la version parent
  // Code spécifique au bouton
end;
```

### L'Importance de inherited

Quand vous redéfinissez une méthode, appelez souvent `inherited` pour exécuter le code de la classe parent :

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  inherited;  // Important ! Initialise le formulaire

  // Votre code d'initialisation
  Label1.Caption := 'Prêt';
end;
```

### Méthodes Couramment Redéfinies

- **`Paint`** : pour dessiner du contenu personnalisé
- **`Resize`** : quand le composant change de taille
- **`Create`** : pour initialiser votre composant
- **`Destroy`** : pour nettoyer les ressources

---

## Propriétés vs Champs

### Différence Fondamentale

Dans la hiérarchie LCL, il y a une différence entre **champs** et **propriétés**.

#### Champ (Field)

```pascal
private
  FCaption: string;  // Champ privé (commence souvent par F)
```

#### Propriété (Property)

```pascal
published
  property Caption: string read FCaption write FCaption;
```

### Pourquoi des Propriétés ?

Les propriétés permettent :
- **Contrôler l'accès** : lecture seule, écriture seule
- **Valider les valeurs** : via des setters
- **Déclencher des actions** : mettre à jour l'affichage

### Exemple avec Setter

```pascal
private
  FCaption: string;
  procedure SetCaption(const Value: string);

published
  property Caption: string read FCaption write SetCaption;

// Implémentation
procedure TMyComponent.SetCaption(const Value: string);  
begin  
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;  // Redessine le composant
  end;
end;
```

Quand vous faites `Button1.Caption := 'OK';`, c'est `SetCaption` qui est appelée !

---

## Impact Pratique de la Hiérarchie

### Polymorphisme

Grâce à l'héritage, vous pouvez traiter tous les contrôles de manière uniforme :

```pascal
procedure TForm1.MasquerTout;  
var  
  i: Integer;
begin
  // Tous les Controls, quel que soit leur type réel
  for i := 0 to ControlCount - 1 do
    Controls[i].Visible := False;
end;
```

### Réutilisation du Code

Créez une fonction qui fonctionne pour tous les descendants :

```pascal
procedure DefinirCouleur(Ctrl: TControl; Couleur: TColor);  
begin  
  Ctrl.Color := Couleur;  // Fonctionne pour TButton, TPanel, TEdit, etc.
end;
```

### Création de Composants Personnalisés

Héritez d'une classe existante pour créer votre propre composant :

```pascal
type
  TMonBoutonSpecial = class(TButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TMonBoutonSpecial.Create(AOwner: TComponent);  
begin  
  inherited Create(AOwner);
  // Configuration par défaut
  Color := clYellow;
  Font.Style := [fsBold];
end;
```

---

## Points Clés à Retenir

1. **TObject** est la racine de tout en Pascal

2. **TPersistent** ajoute la persistance (sauvegarde/chargement)

3. **TComponent** est la base de tous les composants (visuels ou non)
   - Gère `Owner`, `Name`, `Tag`
   - Destruction automatique via Owner

4. **TControl** est la base de tous les composants visuels
   - Position, taille, couleur, événements souris/clavier

5. **TGraphicControl** : composants légers sans handle
   - Pas de focus, pas de conteneur
   - Exemples : TLabel, TImage

6. **TWinControl** : composants avec handle système
   - Peuvent recevoir le focus
   - Peuvent contenir d'autres composants
   - Exemples : TButton, TEdit, TPanel

7. **TCustomControl** : TWinControl + Canvas
   - Meilleur des deux mondes
   - Pour composants personnalisés complexes

8. **Pattern TCustomXxx / TXxx** : séparation implémentation / publication

9. **Opérateurs is et as** : pour naviguer dans la hiérarchie en toute sécurité

10. **Published** : rend les propriétés visibles dans l'IDE

---

## Conclusion

La hiérarchie des composants LCL est logique et bien organisée :

- Chaque niveau ajoute des fonctionnalités spécifiques
- L'héritage permet la réutilisation massive du code
- Le polymorphisme offre flexibilité et élégance
- La compréhension de cette hiérarchie vous permet de :
  - Choisir le bon composant pour chaque besoin
  - Créer vos propres composants
  - Mieux déboguer vos applications
  - Écrire du code plus générique et réutilisable

Dans la section suivante, nous explorerons les **conteneurs** (TPanel, TGroupBox, TPageControl) qui vous permettront d'organiser vos interfaces de manière professionnelle.

---

**Prochaine étape :** 15.3 Conteneurs (TPanel, TGroupBox, TPageControl)

⏭️ [Conteneurs (TPanel, TGroupBox, TPageControl)](/15-composants-lcl-fondamentaux/03-conteneurs-tpanel-tgroupbox-tpagecontrol.md)
