🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.4 Listes (TListBox, TComboBox, TTreeView)

## Introduction

Les composants de **liste** permettent d'afficher et de gérer des collections d'éléments. Ils sont essentiels dans presque toutes les applications : sélectionner un pays, choisir un fichier, naviguer dans une arborescence de dossiers, etc.

Dans ce chapitre, nous allons explorer trois composants fondamentaux pour gérer des listes :

- **TListBox** : liste verticale d'éléments
- **TComboBox** : liste déroulante avec saisie
- **TTreeView** : arborescence hiérarchique (comme l'explorateur Windows)

---

## Pourquoi Utiliser des Listes ?

### Le Problème sans Listes

Imaginez devoir créer un bouton radio pour chaque pays du monde (195 pays) ! Votre formulaire serait gigantesque et impossible à maintenir.

### La Solution : Les Listes

Les listes vous permettent de :
- Afficher des **centaines d'éléments** dans un espace restreint
- Permettre la **sélection** facile d'un ou plusieurs éléments
- **Ajouter/supprimer** dynamiquement des éléments
- **Trier** et **rechercher** dans les éléments
- **Organiser** hiérarchiquement (arborescence)

### Analogie

Une liste, c'est comme un **menu de restaurant** :
- Vous voyez plusieurs choix
- Vous en sélectionnez un (ou plusieurs)
- Le menu peut être long (scrollable)
- Les choix peuvent être organisés par catégories

---

## TListBox : La Liste Simple

### Présentation

`TListBox` affiche une liste verticale d'éléments dans laquelle l'utilisateur peut sélectionner un ou plusieurs items.

### Hiérarchie

```
TWinControl
  └─ TCustomListBox
       └─ TListBox
```

### Apparence Visuelle

```
┌────────────────┐
│ Élément 1      │
│ Élément 2      │ ← Élément sélectionné (surligné)
│ Élément 3      │
│ Élément 4      │
│ Élément 5      │
│ ...            │
└────────────────┘
```

### Propriétés Fondamentales

#### La Collection Items

`Items` est une propriété de type `TStrings` qui contient tous les éléments de la liste :

```pascal
property Items: TStrings;  // Liste des éléments
```

**Méthodes principales de Items :**

```pascal
Items.Add(s: string): Integer;           // Ajouter un élément  
Items.Insert(Index: Integer; s: string); // Insérer à une position  
Items.Delete(Index: Integer);            // Supprimer par index  
Items.Clear;                              // Vider la liste  
Items.Count: Integer;                     // Nombre d'éléments  
Items[Index]: string;                     // Accès par index  
```

#### Sélection

```pascal
property ItemIndex: Integer;      // Index de l'élément sélectionné (-1 si aucun)  
property Selected[Index: Integer]: Boolean;  // État sélectionné d'un élément  
property SelCount: Integer;       // Nombre d'éléments sélectionnés  
```

#### Mode de Sélection

```pascal
property MultiSelect: Boolean;    // Sélection multiple ?  
property ExtendedSelect: Boolean; // Sélection étendue (Ctrl, Shift) ?  
```

#### Apparence

```pascal
property Sorted: Boolean;         // Tri automatique alphabétique  
property ItemHeight: Integer;     // Hauteur de chaque élément  
property Columns: Integer;        // Nombre de colonnes (0 = 1 colonne)  
```

### Exemple de Base

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Ajouter des éléments
  ListBox1.Items.Add('Paris');
  ListBox1.Items.Add('Londres');
  ListBox1.Items.Add('Berlin');
  ListBox1.Items.Add('Madrid');
  ListBox1.Items.Add('Rome');

  // Tri automatique
  ListBox1.Sorted := True;
end;

procedure TForm1.ListBox1Click(Sender: TObject);  
begin  
  if ListBox1.ItemIndex >= 0 then
    ShowMessage('Vous avez sélectionné : ' + ListBox1.Items[ListBox1.ItemIndex]);
end;
```

### Ajouter des Éléments

#### Méthode Add

```pascal
// Ajouter à la fin
ListBox1.Items.Add('Nouvel élément');
```

#### Méthode Insert

```pascal
// Insérer à l'index 0 (début)
ListBox1.Items.Insert(0, 'Premier élément');

// Insérer à l'index 2
ListBox1.Items.Insert(2, 'Au milieu');
```

#### Ajouter Plusieurs Éléments

```pascal
// Méthode 1 : AddStrings
var
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    TempList.Add('Element 1');
    TempList.Add('Element 2');
    TempList.Add('Element 3');
    ListBox1.Items.AddStrings(TempList);
  finally
    TempList.Free;
  end;
end;

// Méthode 2 : Ajouter directement
ListBox1.Items.BeginUpdate;  // Désactive le rafraîchissement  
try  
  ListBox1.Items.Add('Element 1');
  ListBox1.Items.Add('Element 2');
  ListBox1.Items.Add('Element 3');
  // ... des centaines d'éléments
finally
  ListBox1.Items.EndUpdate;  // Rafraîchit une seule fois
end;
```

**Note :** `BeginUpdate` / `EndUpdate` améliore considérablement les performances quand vous ajoutez beaucoup d'éléments.

### Supprimer des Éléments

```pascal
// Supprimer par index
ListBox1.Items.Delete(0);  // Supprime le premier élément

// Supprimer l'élément sélectionné
if ListBox1.ItemIndex >= 0 then
  ListBox1.Items.Delete(ListBox1.ItemIndex);

// Vider complètement la liste
ListBox1.Items.Clear;
```

### Accéder aux Éléments

```pascal
// Lire un élément par index
var
  Element: string;
begin
  if ListBox1.Items.Count > 0 then
    Element := ListBox1.Items[0];  // Premier élément
end;

// Modifier un élément
ListBox1.Items[2] := 'Nouveau texte';

// Parcourir tous les éléments
var
  i: Integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ShowMessage(ListBox1.Items[i]);
end;
```

### Sélection Simple

Par défaut, la ListBox permet la sélection d'un seul élément :

```pascal
// Obtenir l'index sélectionné
var
  Index: Integer;
begin
  Index := ListBox1.ItemIndex;
  if Index = -1 then
    ShowMessage('Aucune sélection')
  else
    ShowMessage('Index : ' + IntToStr(Index) +
                ' = ' + ListBox1.Items[Index]);
end;

// Sélectionner par code (index)
ListBox1.ItemIndex := 2;  // Sélectionne le 3ème élément

// Désélectionner tout
ListBox1.ItemIndex := -1;
```

### Sélection Multiple

Pour permettre la sélection de plusieurs éléments :

```pascal
ListBox1.MultiSelect := True;  
ListBox1.ExtendedSelect := True;  // Autorise Ctrl et Shift  
```

**Comportement utilisateur :**
- **Clic simple** : sélectionne un élément, désélectionne les autres
- **Ctrl + Clic** : ajoute/retire de la sélection
- **Shift + Clic** : sélectionne une plage

#### Obtenir les Éléments Sélectionnés

```pascal
procedure TForm1.BtnAfficherSelectionClick(Sender: TObject);  
var  
  i: Integer;
  Selection: string;
begin
  Selection := '';
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    if ListBox1.Selected[i] then
      Selection := Selection + ListBox1.Items[i] + ', ';
  end;

  if Selection <> '' then
  begin
    Delete(Selection, Length(Selection) - 1, 2);  // Enlever ", " final
    ShowMessage('Sélectionnés : ' + Selection);
  end
  else
    ShowMessage('Aucune sélection');
end;
```

#### Sélectionner/Désélectionner par Code

```pascal
// Sélectionner un élément spécifique
ListBox1.Selected[3] := True;

// Désélectionner un élément
ListBox1.Selected[1] := False;

// Sélectionner tous les éléments
procedure SelectAll;  
var  
  i: Integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Selected[i] := True;
end;

// Désélectionner tout
ListBox1.ClearSelection;
```

### Recherche dans la Liste

```pascal
// Rechercher un élément (sensible à la casse)
var
  Index: Integer;
begin
  Index := ListBox1.Items.IndexOf('Paris');
  if Index >= 0 then
    ShowMessage('Trouvé à l''index : ' + IntToStr(Index))
  else
    ShowMessage('Non trouvé');
end;
```

### Tri Automatique

```pascal
ListBox1.Sorted := True;  // Tri alphabétique automatique
```

⚠️ **Attention :** Si `Sorted := True`, vous ne pouvez plus contrôler l'ordre d'insertion.

### Événements Importants

```pascal
property OnClick: TNotifyEvent;           // Clic sur un élément  
property OnDblClick: TNotifyEvent;        // Double-clic  
property OnSelectionChange: TNotifyEvent; // La sélection change  
property OnDrawItem: TDrawItemEvent;      // Dessin personnalisé (owner draw)  
```

### Exemple Complet : Gestion d'une Liste

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration
  ListBox1.MultiSelect := False;
  ListBox1.Sorted := True;

  // Remplir avec des données
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Add('Apple');
    ListBox1.Items.Add('Banana');
    ListBox1.Items.Add('Cherry');
    ListBox1.Items.Add('Date');
    ListBox1.Items.Add('Elderberry');
  finally
    ListBox1.Items.EndUpdate;
  end;
end;

// Ajouter un élément
procedure TForm1.BtnAddClick(Sender: TObject);  
begin  
  if Edit1.Text <> '' then
  begin
    ListBox1.Items.Add(Edit1.Text);
    Edit1.Clear;
  end;
end;

// Supprimer l'élément sélectionné
procedure TForm1.BtnDeleteClick(Sender: TObject);  
begin  
  if ListBox1.ItemIndex >= 0 then
    ListBox1.Items.Delete(ListBox1.ItemIndex);
end;

// Vider la liste
procedure TForm1.BtnClearClick(Sender: TObject);  
begin  
  ListBox1.Items.Clear;
end;
```

### Associer des Données aux Éléments

La propriété `Items.Objects[Index]` permet d'associer un objet à chaque élément :

```pascal
type
  TPersonne = class
    Nom: string;
    Age: Integer;
    constructor Create(ANom: string; AAge: Integer);
  end;

constructor TPersonne.Create(ANom: string; AAge: Integer);  
begin  
  Nom := ANom;
  Age := AAge;
end;

// Ajouter avec données associées
procedure TForm1.AjouterPersonne;  
var  
  P: TPersonne;
  Index: Integer;
begin
  P := TPersonne.Create('Jean Dupont', 35);
  Index := ListBox1.Items.Add('Jean Dupont');
  ListBox1.Items.Objects[Index] := P;
end;

// Récupérer les données
procedure TForm1.ListBox1Click(Sender: TObject);  
var  
  P: TPersonne;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    P := ListBox1.Items.Objects[ListBox1.ItemIndex] as TPersonne;
    ShowMessage('Âge : ' + IntToStr(P.Age));
  end;
end;

// ⚠️ IMPORTANT : Libérer les objets avant de détruire la liste
procedure TForm1.FormDestroy(Sender: TObject);  
var  
  i: Integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    if Assigned(ListBox1.Items.Objects[i]) then
      ListBox1.Items.Objects[i].Free;
  end;
end;
```

---

## TComboBox : La Liste Déroulante

### Présentation

`TComboBox` combine une zone de saisie avec une liste déroulante. C'est idéal quand vous voulez économiser de l'espace à l'écran tout en offrant des choix prédéfinis.

### Hiérarchie

```
TWinControl
  └─ TCustomComboBox
       └─ TComboBox
```

### Apparence Visuelle

**État fermé :**
```
┌──────────────┬─▼─┐
│ Paris        │   │
└──────────────┴───┘
```

**État ouvert :**
```
┌──────────────┬─▲─┐
│ Paris        │   │
├──────────────┴───┤
│ Londres          │
│ Berlin           │
│ Madrid           │
│ Rome             │
└──────────────────┘
```

### Styles de ComboBox

La propriété `Style` détermine le comportement du ComboBox :

```pascal
type
  TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable);
```

| Style | Description | Saisie libre | Liste déroulante |
|-------|-------------|--------------|------------------|
| **csDropDown** | Saisie + liste déroulante | ✅ Oui | ✅ Oui |
| **csDropDownList** | Sélection uniquement | ❌ Non | ✅ Oui |
| **csSimple** | Saisie + liste toujours visible | ✅ Oui | ❌ Non (fixe) |

#### csDropDown (par défaut)

L'utilisateur peut :
- Taper du texte librement
- Cliquer sur la flèche pour voir la liste
- Sélectionner un élément de la liste

```pascal
ComboBox1.Style := csDropDown;
```

**Usage :** Quand vous voulez suggérer des valeurs mais autoriser la saisie libre (ex: recherche avec suggestions).

#### csDropDownList

L'utilisateur peut :
- Seulement sélectionner dans la liste
- Ne peut pas taper librement

```pascal
ComboBox1.Style := csDropDownList;
```

**Usage :** Quand vous voulez restreindre les choix possibles (ex: sélection de pays).

#### csSimple

La liste est toujours visible (pas de déroulement) et l'utilisateur peut taper.

```pascal
ComboBox1.Style := csSimple;  
ComboBox1.Height := 100;  // Définir la hauteur pour voir plusieurs éléments  
```

**Usage :** Rare, préférez TListBox dans ce cas.

### Propriétés Principales

```pascal
property Items: TStrings;          // Liste des éléments (comme TListBox)  
property ItemIndex: Integer;       // Index sélectionné (-1 si aucun ou saisie libre)  
property Text: string;             // Texte affiché/saisi  
property Style: TComboBoxStyle;    // Style du ComboBox  
property Sorted: Boolean;          // Tri automatique  
property DropDownCount: Integer;   // Nombre d'éléments visibles dans la liste (défaut: 8)  
```

### Exemple de Base

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration
  ComboBox1.Style := csDropDownList;  // Sélection uniquement
  ComboBox1.Sorted := True;

  // Remplir
  ComboBox1.Items.Add('France');
  ComboBox1.Items.Add('Espagne');
  ComboBox1.Items.Add('Italie');
  ComboBox1.Items.Add('Allemagne');
  ComboBox1.Items.Add('Belgique');

  // Sélectionner le premier
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);  
begin  
  ShowMessage('Sélection : ' + ComboBox1.Text);
end;
```

### Différence Text vs Items[ItemIndex]

```pascal
// Style = csDropDown (saisie libre autorisée)
ComboBox1.Style := csDropDown;  
ComboBox1.Text := 'Nouvelle valeur';  // Peut être n'importe quoi  
// ItemIndex = -1 si 'Nouvelle valeur' n'est pas dans Items

// Style = csDropDownList (sélection uniquement)
ComboBox1.Style := csDropDownList;
// Text contient toujours Items[ItemIndex]
// ou '' si ItemIndex = -1
```

### Gestion Items (identique à TListBox)

Toutes les opérations sur `Items` fonctionnent comme pour TListBox :

```pascal
// Ajouter
ComboBox1.Items.Add('Élément');

// Supprimer
ComboBox1.Items.Delete(0);

// Vider
ComboBox1.Items.Clear;

// Compter
ShowMessage('Nombre : ' + IntToStr(ComboBox1.Items.Count));

// Parcourir
for i := 0 to ComboBox1.Items.Count - 1 do
  Memo1.Lines.Add(ComboBox1.Items[i]);
```

### Sélection

```pascal
// Sélectionner par index
ComboBox1.ItemIndex := 2;  // Sélectionne le 3ème élément

// Sélectionner par texte (trouve l'index correspondant)
ComboBox1.ItemIndex := ComboBox1.Items.IndexOf('France');

// Vérifier s'il y a une sélection
if ComboBox1.ItemIndex >= 0 then
  ShowMessage('Sélectionné : ' + ComboBox1.Items[ComboBox1.ItemIndex])
else
  ShowMessage('Aucune sélection ou saisie libre');

// Désélectionner (csDropDown uniquement)
ComboBox1.ItemIndex := -1;  
ComboBox1.Text := '';  
```

### Événements

```pascal
property OnChange: TNotifyEvent;     // Le texte ou la sélection change  
property OnSelect: TNotifyEvent;     // Un élément est sélectionné dans la liste  
property OnDropDown: TNotifyEvent;   // La liste s'ouvre  
property OnCloseUp: TNotifyEvent;    // La liste se ferme  
```

### Exemple : ComboBox Cascade (Pays → Villes)

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // ComboBox des pays
  ComboBoxPays.Items.Add('France');
  ComboBoxPays.Items.Add('Espagne');
  ComboBoxPays.Items.Add('Italie');
  ComboBoxPays.ItemIndex := 0;
  ComboBoxPaysChange(nil);  // Initialise les villes
end;

procedure TForm1.ComboBoxPaysChange(Sender: TObject);  
begin  
  ComboBoxVille.Items.Clear;

  case ComboBoxPays.ItemIndex of
    0: // France
    begin
      ComboBoxVille.Items.Add('Paris');
      ComboBoxVille.Items.Add('Lyon');
      ComboBoxVille.Items.Add('Marseille');
    end;
    1: // Espagne
    begin
      ComboBoxVille.Items.Add('Madrid');
      ComboBoxVille.Items.Add('Barcelone');
      ComboBoxVille.Items.Add('Valence');
    end;
    2: // Italie
    begin
      ComboBoxVille.Items.Add('Rome');
      ComboBoxVille.Items.Add('Milan');
      ComboBoxVille.Items.Add('Naples');
    end;
  end;

  if ComboBoxVille.Items.Count > 0 then
    ComboBoxVille.ItemIndex := 0;
end;
```

### AutoComplete (Saisie Semi-Automatique)

En mode `csDropDown`, le ComboBox peut compléter automatiquement la saisie :

```pascal
ComboBox1.Style := csDropDown;  
ComboBox1.AutoComplete := True;  // Active la complétion automatique  
```

Quand l'utilisateur tape "Pa", si "Paris" est dans la liste, il sera suggéré.

---

## TTreeView : L'Arborescence Hiérarchique

### Présentation

`TTreeView` affiche une structure hiérarchique en arbre, comme l'explorateur de fichiers Windows. C'est parfait pour organiser des données imbriquées (dossiers/fichiers, catégories/sous-catégories, etc.).

### Hiérarchie

```
TWinControl
  └─ TCustomTreeView
       └─ TTreeView
```

### Apparence Visuelle

```
┌──────────────────────┐
│ ▼ Documents          │
│   ├─ ▼ Travail       │
│   │   ├─ Rapport.doc │
│   │   └─ Budget.xls  │
│   └─ ▶ Personnel     │
│ ▶ Images             │
│ ▼ Vidéos             │
│   └─ Vacances.mp4    │
└──────────────────────┘
```

**Symboles :**
- **▼** : nœud développé (enfants visibles)
- **▶** : nœud contracté (enfants masqués)
- **├─** : branche intermédiaire
- **└─** : dernière branche

### Concepts Fondamentaux

#### Nœud (TTreeNode)

Un **nœud** est un élément de l'arbre. Il peut :
- Avoir du texte
- Avoir des enfants (sous-nœuds)
- Avoir un parent
- Être développé ou contracté
- Avoir une image associée

```pascal
TTreeNode = class
  property Text: string;           // Texte du nœud
  property Parent: TTreeNode;      // Nœud parent (nil = racine)
  property Count: Integer;         // Nombre d'enfants
  property Items[Index]: TTreeNode; // Accès aux enfants
  property Expanded: Boolean;      // Développé ?
  property Selected: Boolean;      // Sélectionné ?
  property Level: Integer;         // Niveau de profondeur (0 = racine)
  property Index: Integer;         // Position parmi les frères
  property Data: Pointer;          // Données personnalisées

  // Méthodes
  function AddChild(const S: string): TTreeNode;
  procedure Delete;
  procedure DeleteChildren;
  procedure Expand(Recursive: Boolean);
  procedure Collapse(Recursive: Boolean);
end;
```

### Propriétés du TTreeView

```pascal
property Items: TTreeNodes;        // Collection de tous les nœuds  
property Selected: TTreeNode;      // Nœud actuellement sélectionné  
property TopItem: TTreeNode;       // Premier nœud visible  
property ShowRoot: Boolean;        // Afficher les lignes de racine  
property ShowButtons: Boolean;     // Afficher les boutons +/-  
property ShowLines: Boolean;       // Afficher les lignes de connexion  
property ReadOnly: Boolean;        // Lecture seule (pas d'édition)  
property MultiSelect: Boolean;     // Sélection multiple  
property SortType: TSortType;      // Tri (stNone, stText, stData)  
```

### Ajouter des Nœuds

#### Méthode 1 : Items.Add

```pascal
// Ajouter un nœud racine
var
  Node: TTreeNode;
begin
  Node := TreeView1.Items.Add(nil, 'Racine');
  // nil = pas de parent, donc c'est une racine
end;
```

#### Méthode 2 : Items.AddChild

```pascal
var
  RootNode, ChildNode: TTreeNode;
begin
  // Créer la racine
  RootNode := TreeView1.Items.Add(nil, 'Documents');

  // Ajouter des enfants
  ChildNode := TreeView1.Items.AddChild(RootNode, 'Travail');
  TreeView1.Items.AddChild(RootNode, 'Personnel');
end;
```

#### Méthode 3 : Node.AddChild (Plus Simple)

```pascal
var
  RootNode: TTreeNode;
begin
  RootNode := TreeView1.Items.Add(nil, 'Documents');
  RootNode.AddChild('Travail');      // Plus court !
  RootNode.AddChild('Personnel');
end;
```

### Exemple : Créer une Arborescence

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  NodeDocs, NodeWork, NodePersonal: TTreeNode;
  NodeImages, NodeVideos: TTreeNode;
begin
  TreeView1.Items.Clear;

  // Documents
  NodeDocs := TreeView1.Items.Add(nil, 'Documents');

    // Documents → Travail
    NodeWork := NodeDocs.AddChild('Travail');
      NodeWork.AddChild('Rapport.doc');
      NodeWork.AddChild('Budget.xls');
      NodeWork.AddChild('Présentation.ppt');

    // Documents → Personnel
    NodePersonal := NodeDocs.AddChild('Personnel');
      NodePersonal.AddChild('CV.pdf');
      NodePersonal.AddChild('Lettre.doc');

  // Images
  NodeImages := TreeView1.Items.Add(nil, 'Images');
    NodeImages.AddChild('Photo1.jpg');
    NodeImages.AddChild('Photo2.jpg');

  // Vidéos
  NodeVideos := TreeView1.Items.Add(nil, 'Vidéos');
    NodeVideos.AddChild('Vacances.mp4');
    NodeVideos.AddChild('Mariage.avi');

  // Développer tous les nœuds racines
  TreeView1.FullExpand;
end;
```

### Naviguer dans l'Arbre

```pascal
// Obtenir le nœud sélectionné
var
  Node: TTreeNode;
begin
  Node := TreeView1.Selected;
  if Assigned(Node) then
    ShowMessage('Sélectionné : ' + Node.Text);
end;

// Obtenir le parent
if Assigned(Node.Parent) then
  ShowMessage('Parent : ' + Node.Parent.Text);

// Obtenir le premier enfant
if Node.Count > 0 then
  ShowMessage('Premier enfant : ' + Node.Items[0].Text);

// Parcourir tous les enfants
var
  i: Integer;
begin
  for i := 0 to Node.Count - 1 do
    Memo1.Lines.Add(Node.Items[i].Text);
end;

// Parcourir tous les nœuds de l'arbre
var
  i: Integer;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
    Memo1.Lines.Add(TreeView1.Items[i].Text);
end;
```

### Développer/Contracter

```pascal
// Développer un nœud
Node.Expand(False);  // False = pas récursif (seulement ce nœud)  
Node.Expand(True);   // True = récursif (tous les sous-nœuds)  

// Contracter un nœud
Node.Collapse(False);  
Node.Collapse(True);  

// Basculer (toggle)
if Node.Expanded then
  Node.Collapse(False)
else
  Node.Expand(False);

// Développer tout l'arbre
TreeView1.FullExpand;

// Contracter tout l'arbre
TreeView1.FullCollapse;
```

### Supprimer des Nœuds

```pascal
// Supprimer le nœud sélectionné
if Assigned(TreeView1.Selected) then
  TreeView1.Selected.Delete;  // Supprime aussi tous les enfants !

// Supprimer seulement les enfants
if Assigned(Node) then
  Node.DeleteChildren;

// Vider tout l'arbre
TreeView1.Items.Clear;
```

### Sélection

```pascal
// Sélectionner un nœud
Node.Selected := True;

// Obtenir la sélection actuelle
var
  Node: TTreeNode;
begin
  Node := TreeView1.Selected;
  if Assigned(Node) then
    ShowMessage(Node.Text);
end;

// Désélectionner
TreeView1.Selected := nil;
```

### Rechercher un Nœud

```pascal
function TForm1.FindNode(const SearchText: string): TTreeNode;  
var  
  i: Integer;
begin
  Result := nil;
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    if TreeView1.Items[i].Text = SearchText then
    begin
      Result := TreeView1.Items[i];
      Exit;
    end;
  end;
end;

// Utilisation
var
  Node: TTreeNode;
begin
  Node := FindNode('Rapport.doc');
  if Assigned(Node) then
  begin
    Node.Selected := True;
    Node.MakeVisible;  // Fait défiler pour rendre visible
  end;
end;
```

### Parcours Récursif de l'Arbre

```pascal
procedure TForm1.ParcoursRecursif(Node: TTreeNode; Niveau: Integer);  
var  
  i: Integer;
  Indent: string;
begin
  if not Assigned(Node) then Exit;

  // Indentation selon le niveau
  Indent := StringOfChar(' ', Niveau * 2);

  // Traiter le nœud actuel
  Memo1.Lines.Add(Indent + Node.Text);

  // Parcourir récursivement les enfants
  for i := 0 to Node.Count - 1 do
    ParcoursRecursif(Node.Items[i], Niveau + 1);
end;

// Lancer le parcours depuis les racines
procedure TForm1.BtnParcoursClick(Sender: TObject);  
var  
  i: Integer;
begin
  Memo1.Clear;
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    if TreeView1.Items[i].Level = 0 then  // Seulement les racines
      ParcoursRecursif(TreeView1.Items[i], 0);
  end;
end;
```

### Associer des Données à un Nœud

La propriété `Data` permet d'associer un pointeur à chaque nœud :

```pascal
type
  TFichierInfo = class
    Nom: string;
    Taille: Int64;
    DateModif: TDateTime;
  end;

var
  Node: TTreeNode;
  Info: TFichierInfo;
begin
  // Créer le nœud
  Node := TreeView1.Items.Add(nil, 'Document.txt');

  // Associer des données
  Info := TFichierInfo.Create;
  Info.Nom := 'Document.txt';
  Info.Taille := 1024;
  Info.DateModif := Now;
  Node.Data := Info;
end;

// Récupérer les données
procedure TForm1.TreeView1Click(Sender: TObject);  
var  
  Node: TTreeNode;
  Info: TFichierInfo;
begin
  Node := TreeView1.Selected;
  if Assigned(Node) and Assigned(Node.Data) then
  begin
    Info := TFichierInfo(Node.Data);
    ShowMessage(Format('Taille : %d octets', [Info.Taille]));
  end;
end;

// ⚠️ IMPORTANT : Libérer les objets
procedure TForm1.FormDestroy(Sender: TObject);  
var  
  i: Integer;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    if Assigned(TreeView1.Items[i].Data) then
      TObject(TreeView1.Items[i].Data).Free;
  end;
end;
```

### Images dans le TreeView

Vous pouvez associer des icônes aux nœuds via un TImageList :

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  Node: TTreeNode;
begin
  // Associer l'ImageList
  TreeView1.Images := ImageList1;

  // Ajouter des nœuds avec images
  Node := TreeView1.Items.Add(nil, 'Dossier');
  Node.ImageIndex := 0;        // Icône quand contracté
  Node.SelectedIndex := 1;     // Icône quand sélectionné
  Node.StateIndex := -1;       // Icône d'état (checkbox, etc.)
end;
```

### Tri des Nœuds

```pascal
// Tri automatique par texte
TreeView1.SortType := stText;

// Tri personnalisé
TreeView1.SortType := stData;  
TreeView1.OnCompare := @TreeView1Compare;  

procedure TForm1.TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
  var Compare: Integer);
begin
  // Compare < 0  si Node1 avant Node2
  // Compare = 0  si égaux
  // Compare > 0  si Node1 après Node2
  Compare := CompareText(Node1.Text, Node2.Text);
end;
```

### Événements Importants

```pascal
property OnClick: TNotifyEvent;                    // Clic sur un nœud  
property OnDblClick: TNotifyEvent;                 // Double-clic  
property OnChange: TTVChangedEvent;                // Sélection change  
property OnExpanding: TTVExpandingEvent;           // Avant développement  
property OnExpanded: TTVExpandedEvent;             // Après développement  
property OnCollapsing: TTVCollapsingEvent;         // Avant contraction  
property OnCollapsed: TTVCollapsedEvent;           // Après contraction  
property OnEditing: TTVEditingEvent;               // Début édition texte  
property OnEdited: TTVEditedEvent;                 // Fin édition texte  
```

### Édition des Nœuds

Permettre à l'utilisateur de modifier le texte des nœuds :

```pascal
// Activer l'édition
TreeView1.ReadOnly := False;

// Éditer par code
TreeView1.Selected.EditText;  // Lance l'édition du nœud sélectionné

// Gérer la fin de l'édition
procedure TForm1.TreeView1Edited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  if S = '' then
  begin
    ShowMessage('Le nom ne peut pas être vide');
    S := Node.Text;  // Annule la modification
  end;
end;
```

---

## Comparaison des Trois Composants

| Caractéristique | TListBox | TComboBox | TTreeView |
|-----------------|----------|-----------|-----------|
| **Structure** | Liste plate | Liste plate | Hiérarchique |
| **Espace** | Toujours visible | Compact (déroulant) | Variable |
| **Saisie libre** | ❌ Non | ✅ Oui (csDropDown) | ❌ Non |
| **Sélection multiple** | ✅ Oui | ❌ Non | ✅ Oui (avec MultiSelect) |
| **Organisation** | Séquentielle | Séquentielle | Parent/Enfant |
| **Icônes** | Possible (owner draw) | Possible (owner draw) | ✅ Natif (ImageList) |
| **Édition texte** | ❌ Non | ✅ Oui (si style le permet) | ✅ Oui (si ReadOnly=False) |
| **Usage typique** | Listes simples | Sélection compacte | Dossiers, catégories |

### Quand Utiliser Quoi ?

#### Utilisez TListBox quand :
- Vous avez une liste simple d'éléments
- Vous voulez que tous les éléments soient visibles
- Vous avez besoin de sélection multiple
- L'espace vertical est disponible

**Exemples :** Liste de fichiers, liste de contacts, historique

#### Utilisez TComboBox quand :
- L'espace est limité
- Une seule sélection suffit
- Vous voulez suggérer des valeurs mais autoriser la saisie
- La liste n'est pas consultée en permanence

**Exemples :** Sélection de pays, choix de police, filtres

#### Utilisez TTreeView quand :
- Vos données ont une structure hiérarchique
- Vous avez des catégories et sous-catégories
- Vous voulez permettre l'expansion/contraction
- L'organisation parent/enfant a du sens

**Exemples :** Explorateur de fichiers, menu de navigation, organigramme

---

## Bonnes Pratiques

### 1. Performance avec Beaucoup d'Éléments

```pascal
// ✅ BON : Désactiver le rafraîchissement
ListBox1.Items.BeginUpdate;  
try  
  for i := 1 to 10000 do
    ListBox1.Items.Add('Élément ' + IntToStr(i));
finally
  ListBox1.Items.EndUpdate;
end;

// ❌ MAUVAIS : Rafraîchissement à chaque ajout (très lent)
for i := 1 to 10000 do
  ListBox1.Items.Add('Élément ' + IntToStr(i));
```

### 2. Vérifier les Sélections

```pascal
// ✅ BON : Toujours vérifier
if ListBox1.ItemIndex >= 0 then
  ShowMessage(ListBox1.Items[ListBox1.ItemIndex]);

// ❌ MAUVAIS : Peut crasher si rien n'est sélectionné
ShowMessage(ListBox1.Items[ListBox1.ItemIndex]);  // Exception !
```

### 3. Libérer les Objets Associés

```pascal
// ✅ BON : Libérer avant de Clear
for i := 0 to ListBox1.Items.Count - 1 do  
begin  
  if Assigned(ListBox1.Items.Objects[i]) then
    ListBox1.Items.Objects[i].Free;
end;  
ListBox1.Items.Clear;  

// ❌ MAUVAIS : Fuite mémoire
ListBox1.Items.Clear;  // Les objets ne sont pas libérés !
```

### 4. Nommage Descriptif

```pascal
// ✅ BON
ListBoxPays, ComboBoxVilles, TreeViewDossiers

// ❌ MAUVAIS
ListBox1, ComboBox2, TreeView3
```

### 5. TreeView : Éviter les Profondeurs Excessives

```pascal
// ✅ BON : Maximum 4-5 niveaux
Racine → Catégorie → Sous-catégorie → Élément

// ❌ MAUVAIS : Trop profond, difficile à naviguer
Niveau1 → Niveau2 → Niveau3 → ... → Niveau10
```

### 6. Utiliser Assigned

```pascal
// ✅ BON
if Assigned(TreeView1.Selected) then
  ShowMessage(TreeView1.Selected.Text);

// ❌ MAUVAIS : Peut crasher
ShowMessage(TreeView1.Selected.Text);  // Exception si nil !
```

---

## Exemples Pratiques Complets

### Exemple 1 : Liste de Courses (TListBox)

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  ListBoxCourses.Items.Clear;
  EditProduit.Clear;
end;

procedure TForm1.BtnAjouterClick(Sender: TObject);  
begin  
  if EditProduit.Text <> '' then
  begin
    ListBoxCourses.Items.Add(EditProduit.Text);
    EditProduit.Clear;
    EditProduit.SetFocus;
  end;
end;

procedure TForm1.BtnSupprimerClick(Sender: TObject);  
begin  
  if ListBoxCourses.ItemIndex >= 0 then
    ListBoxCourses.Items.Delete(ListBoxCourses.ItemIndex);
end;

procedure TForm1.BtnViderClick(Sender: TObject);  
begin  
  if MessageDlg('Vider la liste ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ListBoxCourses.Items.Clear;
end;
```

### Exemple 2 : Sélection Pays/Ville (TComboBox)

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  ComboBoxPays.Style := csDropDownList;
  ComboBoxVille.Style := csDropDownList;

  ComboBoxPays.Items.Add('France');
  ComboBoxPays.Items.Add('Espagne');
  ComboBoxPays.Items.Add('Italie');

  if ComboBoxPays.Items.Count > 0 then
  begin
    ComboBoxPays.ItemIndex := 0;
    ComboBoxPaysChange(nil);
  end;
end;

procedure TForm1.ComboBoxPaysChange(Sender: TObject);  
begin  
  ComboBoxVille.Items.Clear;

  case ComboBoxPays.ItemIndex of
    0: // France
    begin
      ComboBoxVille.Items.Add('Paris');
      ComboBoxVille.Items.Add('Lyon');
      ComboBoxVille.Items.Add('Marseille');
    end;
    1: // Espagne
    begin
      ComboBoxVille.Items.Add('Madrid');
      ComboBoxVille.Items.Add('Barcelone');
    end;
    2: // Italie
    begin
      ComboBoxVille.Items.Add('Rome');
      ComboBoxVille.Items.Add('Milan');
    end;
  end;

  if ComboBoxVille.Items.Count > 0 then
    ComboBoxVille.ItemIndex := 0;
end;

procedure TForm1.BtnValiderClick(Sender: TObject);  
begin  
  ShowMessage(Format('Pays : %s, Ville : %s',
    [ComboBoxPays.Text, ComboBoxVille.Text]));
end;
```

### Exemple 3 : Explorateur de Projet (TTreeView)

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var  
  NodeProjet, NodeSrc, NodeBin, NodeDoc: TTreeNode;
begin
  TreeView1.Items.Clear;

  // Racine du projet
  NodeProjet := TreeView1.Items.Add(nil, 'MonProjet');

  // Dossier Source
  NodeSrc := NodeProjet.AddChild('src');
    NodeSrc.AddChild('main.pas');
    NodeSrc.AddChild('unit1.pas');
    NodeSrc.AddChild('unit2.pas');

  // Dossier Binaires
  NodeBin := NodeProjet.AddChild('bin');
    NodeBin.AddChild('MonProjet.exe');
    NodeBin.AddChild('MonProjet.dll');

  // Documentation
  NodeDoc := NodeProjet.AddChild('docs');
    NodeDoc.AddChild('README.md');
    NodeDoc.AddChild('CHANGELOG.md');

  // Développer le projet
  NodeProjet.Expand(True);
end;

procedure TForm1.TreeView1DblClick(Sender: TObject);  
var  
  Node: TTreeNode;
begin
  Node := TreeView1.Selected;
  if Assigned(Node) then
  begin
    if Node.Count > 0 then
      Node.Expanded := not Node.Expanded
    else
      ShowMessage('Ouvrir : ' + Node.Text);
  end;
end;
```

---

## Points Clés à Retenir

1. **TListBox** : liste verticale simple
   - `Items` : collection d'éléments (TStrings)
   - `ItemIndex` : élément sélectionné
   - `MultiSelect` : sélection multiple possible
   - `BeginUpdate/EndUpdate` : pour les performances

2. **TComboBox** : liste déroulante avec saisie optionnelle
   - `Style` : csDropDown (saisie), csDropDownList (sélection)
   - `Text` : texte affiché/saisi
   - Même interface `Items` que TListBox
   - Économise l'espace à l'écran

3. **TTreeView** : arborescence hiérarchique
   - Structure parent/enfant (TTreeNode)
   - `Items.Add(nil, ...)` : nœud racine
   - `Node.AddChild(...)` : ajouter un enfant
   - `Expand/Collapse` : développer/contracter
   - `Data` : associer des données à chaque nœud

4. **Performances** : toujours utiliser `BeginUpdate/EndUpdate` pour beaucoup d'éléments

5. **Sécurité** : vérifier avec `Assigned()` et `ItemIndex >= 0`

6. **Mémoire** : libérer les objets associés via `Items.Objects[]` ou `Node.Data`

7. **Choisir le bon composant** selon la structure des données et l'espace disponible

---

## Conclusion

Les composants de liste sont des outils puissants pour gérer des collections de données dans vos interfaces. Chacun a ses forces :

- **TListBox** pour la simplicité et la visibilité complète
- **TComboBox** pour l'économie d'espace et la saisie libre
- **TTreeView** pour l'organisation hiérarchique

Maîtriser ces trois composants vous permet de créer des interfaces riches et intuitives, capables de gérer des données complexes de manière élégante.

Dans la section suivante, nous explorerons les **grilles** (TStringGrid, TDrawGrid) pour afficher des données sous forme tabulaire.

---

**Prochaine étape :** 15.5 Grilles (TStringGrid, TDrawGrid)

⏭️ [Grilles (TStringGrid, TDrawGrid)](/15-composants-lcl-fondamentaux/05-grilles-tstringgrid-tdrawgrid.md)
