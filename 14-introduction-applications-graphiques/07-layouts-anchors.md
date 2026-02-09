🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.7 Layouts et anchors

## Introduction

Créer une interface belle et fonctionnelle, c'est bien. Créer une interface qui **s'adapte** au redimensionnement de la fenêtre, c'est encore mieux !

Imaginez : l'utilisateur agrandit votre fenêtre, et... tous vos composants restent coincés dans le coin supérieur gauche. Pas très professionnel, n'est-ce pas ?

Dans cette section, nous allons apprendre à créer des **interfaces adaptatives** qui :
- S'ajustent automatiquement à la taille de la fenêtre
- Restent utilisables quelle que soit la résolution d'écran
- Offrent une expérience professionnelle

Nous explorerons :
- Les **Anchors** (ancrages)
- La propriété **Align** (alignement automatique)
- Les **Constraints** (contraintes de taille)
- Les **conteneurs** (Panels, GroupBox)
- Les techniques de mise en page

---

## Le problème : Interface fixe

### Situation de départ

Vous créez une fenêtre avec des composants positionnés manuellement :

```
┌────────────────────────────────────┐
│ Form1                        _ □ ✕ │
├────────────────────────────────────┤
│                                    │
│  [Label1]                          │
│  [Edit1________]                   │
│                                    │
│  [Button1]                         │
│                                    │
└────────────────────────────────────┘
Largeur: 400 pixels
```

### L'utilisateur agrandit la fenêtre...

```
┌────────────────────────────────────────────────────────────┐
│ Form1                                            _ □ ✕     │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  [Label1]                                                  │
│  [Edit1________]                                           │
│                                                            │
│  [Button1]                                                 │
│                                                            │
│                                                            │
│                                                            │
│                    Espace vide inutilisé                   │
│                                                            │
└────────────────────────────────────────────────────────────┘
Largeur: 800 pixels - Mais les composants n'ont pas bougé !
```

**Problème :** L'interface ne profite pas de l'espace disponible.

### Solution

Utiliser **Anchors** et **Align** pour créer une interface qui s'adapte :

```
┌────────────────────────────────────────────────────────────┐
│ Form1                                            _ □ ✕     │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  [Label1]                                                  │
│  [Edit1____________________________________________]       │
│                                                            │
│                                          [Button1]         │
│                                                            │
│                                                            │
│                                                            │
│                                                            │
│                                                            │
└────────────────────────────────────────────────────────────┘
Edit1 s'étire, Button1 reste à droite !
```

---

## Les Anchors (Ancrages)

### Concept

Les **anchors** (ancrages) définissent quels **côtés** d'un composant restent à une distance fixe des côtés de son parent.

**Métaphore :** Imaginez des élastiques attachant votre composant aux bords de la fenêtre.

### Propriété Anchors

```pascal
Button1.Anchors := [akLeft, akTop];  // Défaut
```

**Type :** Set de TAnchorKind  
**Valeurs possibles :**  
- `akLeft` : Ancré au bord gauche
- `akTop` : Ancré au bord haut
- `akRight` : Ancré au bord droit
- `akBottom` : Ancré au bord bas

### Comportements selon les anchors

#### 1. Anchors par défaut : [akLeft, akTop]

```pascal
Button1.Anchors := [akLeft, akTop];
```

**Comportement :** Position fixe en haut à gauche.

```
┌─────────────────────────┐         ┌──────────────────────────────────┐
│ Fenêtre normale         │         │ Fenêtre agrandie                 │
│                         │         │                                  │
│ [Button1]               │   →     │ [Button1]                        │
│                         │         │                                  │
│                         │         │                                  │
└─────────────────────────┘         └──────────────────────────────────┘
   Button reste en place              Button reste en place
```

**Usage :** Composants qui doivent rester en haut à gauche.

#### 2. Anchors droit bas : [akRight, akBottom]

```pascal
Button1.Anchors := [akRight, akBottom];
```

**Comportement :** Position fixe en bas à droite.

```
┌─────────────────────────┐         ┌──────────────────────────────────┐
│ Fenêtre normale         │         │ Fenêtre agrandie                 │
│                         │         │                                  │
│                         │         │                                  │
│              [Button1]  │   →     │                       [Button1]  │
└─────────────────────────┘         └──────────────────────────────────┘
   Button en bas à droite             Button reste en bas à droite
```

**Usage :** Boutons OK/Annuler, barres d'état, etc.

**Exemple pratique :**
```pascal
// Bouton OK toujours en bas à droite
ButtonOK.Anchors := [akRight, akBottom];
ButtonAnnuler.Anchors := [akRight, akBottom];
```

#### 3. Étirement horizontal : [akLeft, akTop, akRight]

```pascal
Edit1.Anchors := [akLeft, akTop, akRight];
```

**Comportement :** Le composant s'étire horizontalement.

```
┌─────────────────────────┐         ┌──────────────────────────────────┐
│ Fenêtre normale         │         │ Fenêtre agrandie                 │
│                         │         │                                  │
│ [Edit1_________]        │   →     │ [Edit1____________________]      │
│                         │         │                                  │
└─────────────────────────┘         └──────────────────────────────────┘
   Edit1 a une largeur fixe           Edit1 s'étire pour remplir
```

**Comment ça marche :**
- Distance au bord gauche : **fixe** (akLeft)
- Distance au bord droit : **fixe** (akRight)
- Résultat : La largeur change pour maintenir les deux distances

**Usage :** Champs de saisie, barres de recherche, etc.

**Exemple pratique :**
```pascal
// Champ de recherche qui s'étire
EditRecherche.Anchors := [akLeft, akTop, akRight];
EditRecherche.Left := 10;
EditRecherche.Top := 10;
// La distance de 10 pixels à gauche et à droite sera maintenue
```

#### 4. Étirement vertical : [akLeft, akTop, akBottom]

```pascal
Memo1.Anchors := [akLeft, akTop, akBottom];
```

**Comportement :** Le composant s'étire verticalement.

```
┌─────────────────────────┐         ┌──────────────────────────────────┐
│ Fenêtre normale         │         │ Fenêtre agrandie                 │
│                         │         │                                  │
│ ┌───────┐               │         │ ┌───────┐                        │
│ │Memo1  │               │   →     │ │Memo1  │                        │
│ │       │               │         │ │       │                        │
│ └───────┘               │         │ │       │                        │
└─────────────────────────┘         │ │       │                        │
                                    │ └───────┘                        │
                                    └──────────────────────────────────┘
```

**Usage :** Listes, mémos, zones de texte multiligne.

#### 5. Étirement dans les deux directions : [akLeft, akTop, akRight, akBottom]

```pascal
Memo1.Anchors := [akLeft, akTop, akRight, akBottom];
```

**Comportement :** Le composant s'étire horizontalement ET verticalement.

```
┌─────────────────────────┐         ┌──────────────────────────────────┐
│ Fenêtre normale         │         │ Fenêtre agrandie                 │
│                         │         │                                  │
│ ┌───────────────────┐   │         │ ┌──────────────────────────────┐ │
│ │ Memo1             │   │   →     │ │ Memo1                        │ │
│ │                   │   │         │ │                              │ │
│ └───────────────────┘   │         │ │                              │ │
└─────────────────────────┘         │ │                              │ │
                                    │ └──────────────────────────────┘ │
                                    └──────────────────────────────────┘
```

**Usage :** Zone principale de l'application (éditeur de texte, navigateur web, etc.)

**Exemple pratique :**
```pascal
// Éditeur de texte qui occupe tout l'espace disponible
MemoEditeur.Anchors := [akLeft, akTop, akRight, akBottom];
```

### Tableau récapitulatif des Anchors

| Anchors | Comportement | Usage typique |
|---------|--------------|---------------|
| `[akLeft, akTop]` | Position fixe haut-gauche | Labels, icônes |
| `[akRight, akTop]` | Position fixe haut-droite | Boutons de fermeture |
| `[akLeft, akBottom]` | Position fixe bas-gauche | Infos de statut |
| `[akRight, akBottom]` | Position fixe bas-droite | Boutons OK/Annuler |
| `[akLeft, akTop, akRight]` | Étirement horizontal | Champs de saisie, barres |
| `[akLeft, akTop, akBottom]` | Étirement vertical | Menus, listes latérales |
| `[akLeft, akTop, akRight, akBottom]` | Étirement total | Zone de contenu principal |
| `[akRight, akTop, akBottom]` | Fixe à droite, s'étire verticalement | Barres d'outils latérales |

### Exemple complet d'interface avec Anchors

```pascal
type
  TForm1 = class(TForm)
    LabelTitre: TLabel;
    EditRecherche: TEdit;
    ButtonRechercher: TButton;
    MemoResultats: TMemo;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    procedure FormCreate(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Titre : reste en haut à gauche
  LabelTitre.Anchors := [akLeft, akTop];

  // Champ de recherche : s'étire horizontalement
  EditRecherche.Anchors := [akLeft, akTop, akRight];

  // Bouton : reste en haut à droite
  ButtonRechercher.Anchors := [akRight, akTop];

  // Zone de résultats : s'étire dans les 2 directions
  MemoResultats.Anchors := [akLeft, akTop, akRight, akBottom];

  // Boutons de validation : restent en bas à droite
  ButtonOK.Anchors := [akRight, akBottom];
  ButtonAnnuler.Anchors := [akRight, akBottom];
end;
```

**Résultat visuel :**

```
┌────────────────────────────────────────────────┐
│ Form1                                   _ □ ✕  │
├────────────────────────────────────────────────┤
│                                                │
│  Titre de l'application                        │
│  [Recherche________________] [Rechercher]      │
│                                                │
│  ┌──────────────────────────────────────────┐  │
│  │ Résultats de la recherche                │  │
│  │                                          │  │
│  │                                          │  │
│  │                                          │  │
│  │                                          │  │
│  └──────────────────────────────────────────┘  │
│                                                │
│                           [OK]  [Annuler]      │
└────────────────────────────────────────────────┘
```

Si la fenêtre est agrandie, tout s'adapte parfaitement !

---

## La propriété Align

### Concept

`Align` est une approche **encore plus automatique** que les Anchors. Au lieu de définir des distances fixes, vous dites : "ce composant doit remplir tout le haut" ou "tout l'espace restant".

### Valeurs de Align

```pascal
Panel1.Align := alTop;
```

**Type :** TAlign  
**Valeurs possibles :**  

| Valeur | Description |
|--------|-------------|
| `alNone` | Pas d'alignement automatique (défaut) |
| `alTop` | S'aligne en haut, occupe toute la largeur |
| `alBottom` | S'aligne en bas, occupe toute la largeur |
| `alLeft` | S'aligne à gauche, occupe toute la hauteur |
| `alRight` | S'aligne à droite, occupe toute la hauteur |
| `alClient` | Remplit **tout** l'espace disponible restant |
| `alCustom` | Alignement personnalisé (avancé) |

### Comportements détaillés

#### alTop : Bande horizontale en haut

```pascal
Panel1.Align := alTop;
Panel1.Height := 50;  // La largeur est automatique
```

**Résultat :**
```
┌────────────────────────────────────┐
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │ ← Panel1 (alTop)
│                                    │
│                                    │
│         Zone libre                 │
│                                    │
│                                    │
└────────────────────────────────────┘
```

**Usage :** Barres d'outils, en-têtes, barres de titre personnalisées.

**Exemple :**
```pascal
// Barre d'outils en haut
ToolBar1.Align := alTop;
ToolBar1.Height := 40;
```

#### alBottom : Bande horizontale en bas

```pascal
Panel1.Align := alBottom;
Panel1.Height := 30;
```

**Résultat :**
```
┌────────────────────────────────────┐
│                                    │
│                                    │
│         Zone libre                 │
│                                    │
│                                    │
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │ ← Panel1 (alBottom)
└────────────────────────────────────┘
```

**Usage :** Barres d'état, pieds de page.

**Exemple :**
```pascal
// Barre d'état en bas
StatusBar1.Align := alBottom;
```

#### alLeft : Bande verticale à gauche

```pascal
Panel1.Align := alLeft;
Panel1.Width := 200;
```

**Résultat :**
```
┌─────────────────────────────────────┐
│ ▓▓▓▓│                               │
│ ▓▓▓▓│                               │
│ ▓▓▓▓│     Zone libre                │
│ ▓▓▓▓│                               │
│ ▓▓▓▓│                               │
└─────────────────────────────────────┘
  ↑
Panel1 (alLeft)
```

**Usage :** Menus latéraux, explorateurs de fichiers, palettes d'outils.

**Exemple :**
```pascal
// Menu de navigation à gauche
PanelMenu.Align := alLeft;
PanelMenu.Width := 200;
```

#### alRight : Bande verticale à droite

```pascal
Panel1.Align := alRight;
Panel1.Width := 150;
```

**Résultat :**
```
┌─────────────────────────────────────┐
│                            │▓▓▓▓▓▓▓ │
│                            │▓▓▓▓▓▓▓ │
│     Zone libre             │▓▓▓▓▓▓▓ │
│                            │▓▓▓▓▓▓▓ │
│                            │▓▓▓▓▓▓▓ │
└─────────────────────────────────────┘
                               ↑
                          Panel1 (alRight)
```

**Usage :** Panneaux de propriétés, informations contextuelles.

#### alClient : Remplit tout l'espace restant

```pascal
Panel1.Align := alClient;
```

**Résultat :**
```
┌────────────────────────────────────┐
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │
└────────────────────────────────────┘
         Panel1 remplit tout
```

**Usage :** Zone de contenu principal de l'application.

**Important :** Un seul composant devrait avoir `alClient` dans un conteneur donné !

### Combinaison de plusieurs Align

L'ordre de création importe ! Lazarus remplit l'espace dans l'ordre où les composants ont été créés.

**Exemple : Interface classique**

```pascal
// 1. Barre d'outils en haut
ToolBar1.Align := alTop;
ToolBar1.Height := 40;

// 2. Barre d'état en bas
StatusBar1.Align := alBottom;
StatusBar1.Height := 25;

// 3. Menu latéral à gauche
PanelMenu.Align := alLeft;
PanelMenu.Width := 200;

// 4. Zone principale remplit le reste
MemoContenu.Align := alClient;
```

**Résultat :**
```
┌────────────────────────────────────────┐
│ ▓▓▓▓▓▓▓▓▓▓ ToolBar1 ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓  │ ← alTop
├─────┬──────────────────────────────────┤
│ ▓▓▓ │                                  │
│ M ▓ │                                  │
│ e ▓ │       MemoContenu                │ ← alClient
│ n ▓ │       (remplit le reste)         │
│ u ▓ │                                  │
├─────┴──────────────────────────────────┤
│ ▓▓▓▓▓▓▓ StatusBar1 ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓  │ ← alBottom
└────────────────────────────────────────┘
  ↑
alLeft
```

**Cette mise en page s'adapte automatiquement à toutes les tailles de fenêtre !**

### Ordre de superposition (Z-Order)

Si plusieurs composants ont le même Align, le **dernier créé** est au-dessus.

**Exemple :**
```pascal
Panel1.Align := alClient;
Panel2.Align := alClient;  // Panel2 sera au-dessus
```

Pour changer l'ordre :
```pascal
Panel1.BringToFront;  // Amener devant
Panel2.SendToBack;    // Envoyer derrière
```

---

## Splitters : Divisions redimensionnables

Un **Splitter** permet à l'utilisateur de redimensionner les zones avec la souris.

### Utilisation d'un TSplitter

```pascal
// 1. Panneau gauche
PanelGauche.Align := alLeft;
PanelGauche.Width := 200;

// 2. Splitter (séparateur)
Splitter1.Align := alLeft;  // Même alignement que le panneau
Splitter1.Width := 5;

// 3. Zone principale
PanelDroit.Align := alClient;
```

**Résultat :**
```
┌────────────────────────────────────────┐
│           │┃│                          │
│  Panneau  │┃│   Zone principale        │
│  Gauche   │┃│                          │
│           │┃│                          │
└────────────────────────────────────────┘
            ↑
      Splitter (redimensionnable)
```

L'utilisateur peut glisser le splitter pour ajuster les tailles !

**Exemple complet :**
```pascal
// Explorateur de fichiers à gauche
TreeView1.Parent := PanelGauche;
TreeView1.Align := alClient;
PanelGauche.Align := alLeft;
PanelGauche.Width := 200;

// Splitter
Splitter1.Align := alLeft;
Splitter1.Left := PanelGauche.Width + 1;  // Juste après le panneau

// Visualiseur de fichiers à droite
Memo1.Parent := PanelDroit;
Memo1.Align := alClient;
PanelDroit.Align := alClient;
```

### Position du Splitter

**Important :** Le splitter doit avoir le **même Align** que le composant qu'il sépare.

| Composant avec | Splitter avec | Résultat |
|----------------|---------------|----------|
| `alLeft` | `alLeft` | Redimensionne horizontalement |
| `alTop` | `alTop` | Redimensionne verticalement |
| `alRight` | `alRight` | Redimensionne horizontalement |
| `alBottom` | `alBottom` | Redimensionne verticalement |

---

## Constraints : Contraintes de taille

Les **Constraints** (contraintes) limitent les dimensions minimales et maximales d'un composant.

### Propriété Constraints

```pascal
Form1.Constraints.MinWidth := 400;
Form1.Constraints.MinHeight := 300;
Form1.Constraints.MaxWidth := 1920;
Form1.Constraints.MaxHeight := 1080;
```

**Type :** TSizeConstraints  
**Sous-propriétés :**  
- `MinWidth` : Largeur minimale
- `MinHeight` : Hauteur minimale
- `MaxWidth` : Largeur maximale
- `MaxHeight` : Hauteur maximale

### Cas d'usage

#### 1. Empêcher une fenêtre de devenir trop petite

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // L'utilisateur ne peut pas réduire en dessous de 600x400
  Form1.Constraints.MinWidth := 600;
  Form1.Constraints.MinHeight := 400;
end;
```

**Pourquoi ?** Éviter que l'interface devienne inutilisable.

#### 2. Limiter la taille maximale

```pascal
// Boîte de dialogue : pas besoin d'être énorme
FormDialogue.Constraints.MaxWidth := 800;
FormDialogue.Constraints.MaxHeight := 600;
```

#### 3. Rapport d'aspect fixe

```pascal
// Forcer un carré (pas directement supporté, mais possible par code)
procedure TForm1.FormResize(Sender: TObject);
begin
  if Width <> Height then
    Height := Width;  // Forcer la hauteur = largeur
end;
```

#### 4. Contraintes sur les composants

```pascal
// Un panneau ne peut pas être trop étroit
Panel1.Constraints.MinWidth := 150;

// Un mémo ne peut pas être trop petit
Memo1.Constraints.MinHeight := 100;
```

---

## Conteneurs et organisation

### Panels (TPanels)

Les **Panels** sont des conteneurs qui regroupent d'autres composants.

**Avantages :**  
- Organiser visuellement l'interface
- Appliquer Align/Anchors à un groupe
- Créer des zones distinctes
- Faciliter le déplacement de groupes de composants

#### Exemple : Formulaire de saisie

```pascal
// Panneau supérieur : formulaire
PanelFormulaire.Align := alTop;
PanelFormulaire.Height := 200;

// À l'intérieur du PanelFormulaire
LabelNom.Parent := PanelFormulaire;
EditNom.Parent := PanelFormulaire;
LabelPrenom.Parent := PanelFormulaire;
EditPrenom.Parent := PanelFormulaire;

// Panneau inférieur : boutons
PanelBoutons.Align := alBottom;
PanelBoutons.Height := 50;

// À l'intérieur du PanelBoutons
ButtonOK.Parent := PanelBoutons;
ButtonAnnuler.Parent := PanelBoutons;

// Zone centrale : résultats
MemoResultats.Align := alClient;
```

**Structure visuelle :**
```
┌────────────────────────────────────┐
│ ╔════════════════════════════════╗ │
│ ║ PanelFormulaire (alTop)        ║ │
│ ║   Nom: [_________]             ║ │
│ ║   Prénom: [_________]          ║ │
│ ╚════════════════════════════════╝ │
├────────────────────────────────────┤
│                                    │
│ MemoResultats (alClient)           │
│                                    │
├────────────────────────────────────┤
│ ╔════════════════════════════════╗ │
│ ║ PanelBoutons (alBottom)        ║ │
│ ║         [OK]  [Annuler]        ║ │
│ ╚════════════════════════════════╝ │
└────────────────────────────────────┘
```

#### Propriétés utiles des Panels

**BevelInner et BevelOuter**
```pascal
Panel1.BevelOuter := bvRaised;   // Bordure extérieure relevée
Panel1.BevelInner := bvLowered;  // Bordure intérieure enfoncée
```

**Valeurs :**
- `bvNone` : Pas de bordure
- `bvLowered` : Enfoncé
- `bvRaised` : Relevé
- `bvSpace` : Espace

**BorderWidth**
```pascal
Panel1.BorderWidth := 10;  // Espacement interne de 10 pixels
```

**Caption**
```pascal
Panel1.Caption := '';  // Généralement vide (pas de texte affiché)
```

### GroupBox (TGroupBox)

Les **GroupBox** sont comme les Panels, mais avec un cadre et un titre.

```pascal
GroupBox1.Caption := 'Informations personnelles';
GroupBox1.Align := alTop;
GroupBox1.Height := 150;

// Composants à l'intérieur
LabelNom.Parent := GroupBox1;
EditNom.Parent := GroupBox1;
```

**Apparence :**
```
┌─ Informations personnelles ────────┐
│                                    │
│  Nom: [_______________]            │
│  Prénom: [_______________]         │
│                                    │
└────────────────────────────────────┘
```

**Différence Panel/GroupBox :**
- **Panel** : Invisible ou avec bordure simple
- **GroupBox** : Cadre visible avec titre

---

## Techniques avancées de mise en page

### 1. Marges et espacement avec BorderSpacing

```pascal
Button1.BorderSpacing.Around := 10;  // 10 pixels tout autour
Button1.BorderSpacing.Left := 5;
Button1.BorderSpacing.Top := 5;
Button1.BorderSpacing.Right := 5;
Button1.BorderSpacing.Bottom := 5;
```

**BorderSpacing** définit l'espace minimal entre le composant et son parent ou ses voisins.

### 2. AutoSize : Ajustement automatique

```pascal
Label1.AutoSize := True;  // La taille s'ajuste au contenu
Panel1.AutoSize := True;  // La taille s'ajuste aux enfants
```

**Utile pour :** Labels dont le texte change, panels qui s'adaptent à leur contenu.

### 3. ChildSizing : Disposition automatique des enfants

Les Panels ont une propriété `ChildSizing` qui permet de disposer automatiquement les composants enfants en grille.

```pascal
Panel1.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
Panel1.ChildSizing.ControlsPerLine := 3;  // 3 composants par ligne
Panel1.ChildSizing.HorizontalSpacing := 10;
Panel1.ChildSizing.VerticalSpacing := 10;
```

**Layouts possibles :**
- `cclNone` : Pas de disposition automatique
- `cclLeftToRightThenTopToBottom` : De gauche à droite, puis de haut en bas
- `cclTopToBottomThenLeftToRight` : De haut en bas, puis de gauche à droite

**Résultat :** Les boutons se disposent automatiquement en grille !

```
┌────────────────────────────────────┐
│ [Btn1] [Btn2] [Btn3]               │
│ [Btn4] [Btn5] [Btn6]               │
│ [Btn7] [Btn8] [Btn9]               │
└────────────────────────────────────┘
```

### 4. ScrollBox : Zone défilante

Si votre contenu dépasse la taille disponible, utilisez un **TScrollBox** :

```pascal
ScrollBox1.Align := alClient;
ScrollBox1.AutoScroll := True;

// Placer les composants dans le ScrollBox
Panel1.Parent := ScrollBox1;
Panel2.Parent := ScrollBox1;
// etc.
```

Des barres de défilement apparaissent automatiquement si nécessaire !

---

## Exemples de mises en page courantes

### 1. Éditeur de texte simple

```pascal
// Menu en haut
MainMenu1.Align := alTop;

// Barre d'outils
ToolBar1.Align := alTop;

// Zone de texte (remplit le reste)
Memo1.Align := alClient;
Memo1.ScrollBars := ssBoth;

// Barre d'état en bas
StatusBar1.Align := alBottom;
```

### 2. Navigateur web / Lecteur PDF

```pascal
// Barre d'adresse/outils en haut
PanelTop.Align := alTop;
PanelTop.Height := 40;

// Zone de visualisation
WebBrowser1.Align := alClient;

// Optionnel : panneau latéral pour signets
PanelSignets.Align := alLeft;
PanelSignets.Width := 200;

Splitter1.Align := alLeft;
```

### 3. Application de gestion (type CRUD)

```pascal
// Panneau gauche : liste des éléments
PanelListe.Align := alLeft;
PanelListe.Width := 250;
ListBox1.Parent := PanelListe;
ListBox1.Align := alClient;

// Séparateur
Splitter1.Align := alLeft;

// Panneau droit : détails
PanelDetails.Align := alClient;

  // Dans PanelDetails :
  // Formulaire en haut
  PanelFormulaire.Parent := PanelDetails;
  PanelFormulaire.Align := alTop;
  PanelFormulaire.Height := 200;

  // Boutons en bas
  PanelBoutons.Parent := PanelDetails;
  PanelBoutons.Align := alBottom;
  PanelBoutons.Height := 50;

  // Notes au centre
  MemoNotes.Parent := PanelDetails;
  MemoNotes.Align := alClient;
```

### 4. Calculatrice

```pascal
// Écran en haut
EditEcran.Align := alTop;
EditEcran.Height := 40;
EditEcran.ReadOnly := True;
EditEcran.Alignment := taRightJustify;

// Panneau pour les boutons
PanelBoutons.Align := alClient;

// Utiliser ChildSizing pour disposer les boutons en grille
PanelBoutons.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
PanelBoutons.ChildSizing.ControlsPerLine := 4;  // 4 boutons par ligne
PanelBoutons.ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
PanelBoutons.ChildSizing.EnlargeVertical := crsHomogenousChildResize;

// Créer les boutons dynamiquement
for i := 0 to 15 do
begin
  Btn := TButton.Create(Self);
  Btn.Parent := PanelBoutons;
  Btn.Caption := IntToStr(i);
end;
```

---

## Bonnes pratiques

### 1. Planifier la structure avant de coder

✅ **Dessinez** votre interface sur papier ou dans un outil de maquette

✅ **Identifiez** les zones : haut, bas, gauche, droite, centre

✅ **Choisissez** la stratégie : Align ou Anchors ?

### 2. Utiliser des conteneurs (Panels)

❌ **Mauvais :** Tous les composants directement sur le formulaire
```pascal
Button1.Anchors := [akRight, akBottom];
Button2.Anchors := [akRight, akBottom];
Button3.Anchors := [akRight, akBottom];
// Difficile à gérer !
```

✅ **Bon :** Grouper dans des Panels
```pascal
PanelBoutons.Align := alBottom;
Button1.Parent := PanelBoutons;
Button2.Parent := PanelBoutons;
Button3.Parent := PanelBoutons;
// Facile à déplacer et gérer !
```

### 3. Align pour les grandes zones, Anchors pour les détails

**Stratégie recommandée :**
- Utilisez **Align** pour la structure principale (barres, zones)
- Utilisez **Anchors** pour les composants individuels à l'intérieur

```pascal
// Structure avec Align
PanelTop.Align := alTop;
PanelBottom.Align := alBottom;
PanelCenter.Align := alClient;

// Détails avec Anchors
ButtonOK.Parent := PanelBottom;
ButtonOK.Anchors := [akRight, akBottom];
```

### 4. Tester à différentes tailles

✅ Testez votre interface en redimensionnant la fenêtre

✅ Testez sur différentes résolutions (petit écran, grand écran)

✅ Vérifiez que rien ne disparaît ou ne se chevauche

### 5. Définir des tailles minimales

```pascal
// Toujours définir une taille minimale raisonnable
Form1.Constraints.MinWidth := 640;
Form1.Constraints.MinHeight := 480;
```

### 6. Ordre de création important avec Align

Créez les composants dans l'ordre logique :
1. D'abord les barres (Top, Bottom)
2. Ensuite les côtés (Left, Right)
3. Enfin la zone centrale (Client)

### 7. Utiliser BorderSpacing pour les marges

```pascal
// Ajouter des marges autour des composants
Button1.BorderSpacing.Around := 5;
Edit1.BorderSpacing.Around := 5;
```

**Résultat :** Interface plus aérée et professionnelle.

---

## Débogage des problèmes de mise en page

### Problème : Composant invisible

**Cause possible :** Un autre composant le recouvre (Z-Order)

**Solution :**
```pascal
MonComposant.BringToFront;
```

### Problème : Composant ne se redimensionne pas

**Vérifiez :**
1. Les Anchors sont-ils corrects ?
2. Le parent est-il redimensionnable ?
3. Y a-t-il des Constraints qui bloquent ?

### Problème : Align ne fonctionne pas comme attendu

**Cause :** Ordre de création incorrect

**Solution :** Recréez les composants dans le bon ordre ou utilisez :
```pascal
Panel1.Top := 0;  // Forcer l'ordre
Panel2.Top := Panel1.Height;
```

### Problème : Espace vide inexpliqué

**Cause :** BorderWidth ou BorderSpacing

**Vérifiez :**
```pascal
Panel1.BorderWidth := 0;
Panel1.BorderSpacing.Around := 0;
```

---

## Résumé

### Concepts clés

✅ **Anchors** : Définissent les côtés ancrés (distances fixes)  
✅ **Align** : Alignement automatique (Top, Bottom, Left, Right, Client)  
✅ **Constraints** : Limites de taille (min/max)  
✅ **Panels** : Conteneurs pour organiser  
✅ **Splitters** : Divisions redimensionnables  

### Tableau de décision : Quand utiliser quoi ?

| Besoin | Solution |
|--------|----------|
| Barre en haut/bas | `Align := alTop / alBottom` |
| Menu latéral | `Align := alLeft / alRight` |
| Zone principale | `Align := alClient` |
| Bouton en bas à droite | `Anchors := [akRight, akBottom]` |
| Champ qui s'étire | `Anchors := [akLeft, akTop, akRight]` |
| Limite de taille | `Constraints.MinWidth/MinHeight` |
| Grouper des composants | Utiliser un `TPanel` |
| Zone redimensionnable | Utiliser un `TSplitter` |

### Checklist pour une bonne interface

✅ Tous les composants visibles à toutes les tailles  
✅ Taille minimale définie (Constraints)  
✅ Zones principales utilisent Align  
✅ Détails utilisent Anchors  
✅ Marges et espacement cohérents  
✅ Testée à différentes résolutions  

---

## Prochaines étapes

Maintenant que vous maîtrisez les layouts et anchors, vous pouvez créer des interfaces professionnelles et adaptatives !

Dans les prochaines sections :
- **14.8 Menus et barres d'outils** : Ajouter des menus complets
- **14.9 Boîtes de dialogue standard** : Utiliser les dialogues système
- **15. Composants LCL Fondamentaux** : Explorer d'autres composants

Félicitations ! Vous savez maintenant créer des interfaces qui s'adaptent à toutes les situations ! 🎨✨

---

**Point clé à retenir :** Une bonne mise en page utilise Align pour la structure globale et Anchors pour les ajustements fins. Testez toujours à différentes tailles !

⏭️ [Menus et barres d'outils](/14-introduction-applications-graphiques/08-menus-barres-outils.md)
