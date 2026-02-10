🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.8 Menus et barres d'outils

## Introduction

Les **menus** et **barres d'outils** sont des éléments essentiels des applications modernes. Ils permettent à l'utilisateur d'accéder facilement à toutes les fonctionnalités de votre application.

**Menus :** Listes déroulantes organisées hiérarchiquement (Fichier, Édition, Affichage, etc.)

**Barres d'outils :** Boutons avec icônes pour un accès rapide aux fonctions courantes

Dans cette section, nous allons explorer :
- Les menus principaux (TMainMenu)
- Les menus contextuels (TPopupMenu)
- Les barres d'outils (TToolBar)
- Les actions réutilisables (TActionList)
- Les raccourcis clavier
- L'intégration d'icônes

---

## Les menus principaux (TMainMenu)

### Présentation

Le **TMainMenu** est la barre de menu classique en haut de la fenêtre :

```
┌─────────────────────────────────────────────┐
│ Fichier  Édition  Affichage  Aide     _ □ ✕ │
├─────────────────────────────────────────────┤
│                                             │
│                                             │
│                                             │
└─────────────────────────────────────────────┘
```

Cliquer sur "Fichier" ouvre un menu déroulant :

```
┌───────────────────────────────────────────────┐
│ Fichier  Édition  Affichage  Aide      _ □ ✕  │
├───────────────────────────────────────────────┤
│ ┌───────────────────┐                         │
│ │ Nouveau     Ctrl+N│                         │
│ │ Ouvrir...   Ctrl+O│                         │
│ │ Enregistrer Ctrl+S│                         │
│ ├───────────────────┤                         │
│ │ Quitter           │                         │
│ └───────────────────┘                         │
│                                               │
└───────────────────────────────────────────────┘
```

### Ajouter un TMainMenu

**Étape 1 : Ajouter le composant**
1. Palette de composants → Onglet **"Standard"**
2. Cliquez sur **TMainMenu**
3. Cliquez sur le formulaire

Un composant **MainMenu1** apparaît en bas du formulaire (zone des composants non visuels).

**Étape 2 : Concevoir le menu**
1. Double-cliquez sur **MainMenu1**
2. L'éditeur de menu s'ouvre

### L'éditeur de menu

L'éditeur de menu vous permet de créer visuellement votre structure de menu :

```
┌─────────────────────────────┐
│ Menu Editor                 │
├─────────────────────────────┤
│ [             ]             │ ← Premier item (vide)
│                             │
└─────────────────────────────┘
```

**Pour créer un menu :**

1. **Cliquez sur le premier rectangle vide**
2. **Dans l'Inspecteur d'Objets**, modifiez la propriété `Caption` : `'&Fichier'`
3. **Appuyez sur Entrée**
4. Un nouvel item apparaît en dessous et à droite

**Structure créée :**

```
Fichier
  └─ [nouvel item]
```

### Créer une structure complète

**Menu Fichier :**

```pascal
Fichier
├─ Nouveau     (Caption: '&Nouveau')
├─ Ouvrir...   (Caption: '&Ouvrir...')
├─ Enregistrer (Caption: '&Enregistrer')
├─ ───────     (Caption: '-')  // Séparateur
└─ Quitter     (Caption: '&Quitter')
```

**Comment faire :**

1. Créez "Fichier" au premier niveau
2. Avec "Fichier" sélectionné, cliquez sur la flèche droite ➡ (ou la case à droite)
3. Vous créez maintenant des sous-items
4. Créez "Nouveau", puis "Ouvrir...", etc.
5. Pour un séparateur, mettez simplement `'-'` dans Caption

**Menu Édition :**

```pascal
Édition
├─ Couper     (Caption: 'Co&uper')
├─ Copier     (Caption: '&Copier')
├─ Coller     (Caption: 'C&oller')
├─ ───────
└─ Supprimer  (Caption: '&Supprimer')
```

**Menu complet typique :**

```
Fichier        Édition       Affichage     Aide
├─ Nouveau     ├─ Couper     ├─ Zoom+      ├─ Aide
├─ Ouvrir...   ├─ Copier     ├─ Zoom-      ├─ À propos...
├─ Enregistrer ├─ Coller     └─ Normal
├─ ─────       └─ Supprimer
└─ Quitter
```

### Propriétés importantes d'un item de menu

#### Caption

```pascal
MenuItem1.Caption := '&Nouveau';
```

**Description :** Texte affiché dans le menu

**Astuce :** Le caractère `&` crée un raccourci clavier (Alt+N pour "**N**ouveau")

**Cas spéciaux :**
```pascal
Caption := '-';       // Séparateur  
Caption := '&Fichier';  // F souligné, Alt+F pour ouvrir  
Caption := 'A && B';    // Affiche "A & B" (double &&)  
```

#### ShortCut

```pascal
MenuItem1.ShortCut := TextToShortCut('Ctrl+N');
```

**Description :** Raccourci clavier direct (sans passer par le menu)

**Raccourcis courants :**
```pascal
MenuNouveau.ShortCut := TextToShortCut('Ctrl+N');  
MenuOuvrir.ShortCut := TextToShortCut('Ctrl+O');  
MenuEnregistrer.ShortCut := TextToShortCut('Ctrl+S');  
MenuQuitter.ShortCut := TextToShortCut('Alt+F4');  
MenuCouper.ShortCut := TextToShortCut('Ctrl+X');  
MenuCopier.ShortCut := TextToShortCut('Ctrl+C');  
MenuColler.ShortCut := TextToShortCut('Ctrl+V');  
MenuSupprimer.ShortCut := TextToShortCut('Del');  
```

**Le raccourci s'affiche automatiquement dans le menu :**
```
Nouveau     Ctrl+N  
Ouvrir...   Ctrl+O  
```

#### Checked

```pascal
MenuItem1.Checked := True;  // Coche visible  
MenuItem1.Checked := False; // Pas de coche  
```

**Description :** Affiche une coche ✓ devant l'item

**Usage :** Options activables/désactivables

**Exemple :**
```pascal
// Menu Affichage
MenuBarreOutils.Checked := True;   // ✓ Barre d'outils  
MenuBarreEtat.Checked := True;     // ✓ Barre d'état  
MenuReglages.Checked := False;     //   Réglages  
```

#### Enabled

```pascal
MenuItem1.Enabled := True;   // Actif  
MenuItem1.Enabled := False;  // Grisé, non cliquable  
```

**Usage :** Désactiver les options non disponibles

**Exemple :**
```pascal
// Désactiver "Enregistrer" si aucun document ouvert
MenuEnregistrer.Enabled := DocumentOuvert;

// Désactiver "Coller" si le presse-papiers est vide
// Clipboard et CF_TEXT nécessitent : uses Clipbrd
MenuColler.Enabled := Clipboard.HasFormat(CF_TEXT);
```

#### Visible

```pascal
MenuItem1.Visible := True;   // Visible  
MenuItem1.Visible := False;  // Caché  
```

**Usage :** Cacher complètement un item (vs Enabled qui le grise)

#### RadioItem

```pascal
MenuItem1.RadioItem := True;  
MenuItem1.GroupIndex := 1;  
```

**Description :** Crée des items mutuellement exclusifs (boutons radio)

**Exemple : Tailles de police**
```pascal
// Tous dans le même GroupIndex
MenuPetit.RadioItem := True;  
MenuPetit.GroupIndex := 1;  
MenuPetit.Checked := False;  

MenuMoyen.RadioItem := True;  
MenuMoyen.GroupIndex := 1;  
MenuMoyen.Checked := True;  // Sélectionné par défaut  

MenuGrand.RadioItem := True;  
MenuGrand.GroupIndex := 1;  
MenuGrand.Checked := False;  
```

Quand on clique sur un, les autres se décochent automatiquement !

#### ImageIndex (pour les icônes)

```pascal
MenuItem1.ImageIndex := 0;  // Première image de l'ImageList
```

**Description :** Index de l'icône à afficher (nécessite un TImageList)

### Événement principal : OnClick

```pascal
procedure TForm1.MenuNouveauClick(Sender: TObject);  
begin  
  // Code exécuté quand on clique sur "Nouveau"
  NouveauDocument;
  StatusBar1.SimpleText := 'Nouveau document créé';
end;

procedure TForm1.MenuOuvrirClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    OuvrirFichier(OpenDialog1.FileName);
    StatusBar1.SimpleText := 'Fichier ouvert : ' + OpenDialog1.FileName;
  end;
end;

procedure TForm1.MenuQuitterClick(Sender: TObject);  
begin  
  Close;
end;
```

**Pour créer le handler :**
1. Dans l'éditeur de menu, sélectionnez l'item
2. Double-cliquez dessus
3. Lazarus crée automatiquement le handler OnClick

### Sous-menus (menus imbriqués)

Vous pouvez créer des menus à plusieurs niveaux :

```
Fichier
├─ Nouveau
│  ├─ Document texte
│  ├─ Feuille de calcul
│  └─ Présentation
├─ Ouvrir...
└─ Récents
   ├─ fichier1.txt
   ├─ fichier2.txt
   └─ fichier3.txt
```

**Dans l'éditeur de menu :**
1. Sélectionnez "Nouveau"
2. Cliquez sur la flèche droite ➡
3. Créez les sous-items

**Illimité en profondeur** (mais restez raisonnable : 2-3 niveaux max pour l'ergonomie)

---

## Les menus contextuels (TPopupMenu)

### Présentation

Un **TPopupMenu** est un menu qui apparaît avec un clic droit :

```
          Utilisateur fait clic droit
                    ↓
          ┌───────────────────┐
          │ Couper      Ctrl+X│
          │ Copier      Ctrl+C│
          │ Coller      Ctrl+V│
          ├───────────────────┤
          │ Supprimer   Del   │
          └───────────────────┘
```

### Créer un TPopupMenu

**Étape 1 : Ajouter le composant**
1. Palette → Onglet **"Standard"**
2. Cliquez sur **TPopupMenu**
3. Cliquez sur le formulaire

**Étape 2 : Concevoir le menu**
1. Double-cliquez sur **PopupMenu1**
2. Créez les items comme pour TMainMenu

**Exemple simple :**
```pascal
PopupMenu1
├─ Couper
├─ Copier
├─ Coller
├─ ───────
└─ Supprimer
```

**Étape 3 : Associer à un composant**

```pascal
Edit1.PopupMenu := PopupMenu1;  
Memo1.PopupMenu := PopupMenu1;  
Panel1.PopupMenu := PopupMenu1;  
```

Maintenant, un clic droit sur Edit1 affichera le menu !

### Exemple complet : Menu contextuel pour un Memo

```pascal
type
  TForm1 = class(TForm)
    Memo1: TMemo;
    PopupMenu1: TPopupMenu;
    MenuCouper: TMenuItem;
    MenuCopier: TMenuItem;
    MenuColler: TMenuItem;
    MenuSeparateur: TMenuItem;
    MenuSupprimer: TMenuItem;
    MenuSelectionneTout: TMenuItem;
    procedure MenuCouperClick(Sender: TObject);
    procedure MenuCopierClick(Sender: TObject);
    procedure MenuCollerClick(Sender: TObject);
    procedure MenuSupprimerClick(Sender: TObject);
    procedure MenuSelectionneToutClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Associer le menu contextuel au Memo
  Memo1.PopupMenu := PopupMenu1;

  // Définir les raccourcis
  MenuCouper.ShortCut := TextToShortCut('Ctrl+X');
  MenuCopier.ShortCut := TextToShortCut('Ctrl+C');
  MenuColler.ShortCut := TextToShortCut('Ctrl+V');
  MenuSelectionneTout.ShortCut := TextToShortCut('Ctrl+A');
end;

procedure TForm1.MenuCouperClick(Sender: TObject);  
begin  
  Memo1.CutToClipboard;
end;

procedure TForm1.MenuCopierClick(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;

procedure TForm1.MenuCollerClick(Sender: TObject);  
begin  
  Memo1.PasteFromClipboard;
end;

procedure TForm1.MenuSupprimerClick(Sender: TObject);  
begin  
  Memo1.SelText := '';
end;

procedure TForm1.MenuSelectionneToutClick(Sender: TObject);  
begin  
  Memo1.SelectAll;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);  
begin  
  // Activer/désactiver selon le contexte
  MenuCouper.Enabled := Memo1.SelLength > 0;
  MenuCopier.Enabled := Memo1.SelLength > 0;
  MenuColler.Enabled := Clipboard.HasFormat(CF_TEXT);
  MenuSupprimer.Enabled := Memo1.SelLength > 0;
end;
```

### Événement OnPopup

```pascal
procedure TForm1.PopupMenu1Popup(Sender: TObject);  
begin  
  // Appelé AVANT l'affichage du menu
  // Parfait pour activer/désactiver des items selon le contexte

  MenuCopier.Enabled := Memo1.SelLength > 0;
  MenuColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;
```

**Usage :** Adapter le menu au contexte actuel

### Afficher un PopupMenu par code

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  // Afficher à la position de la souris
  PopupMenu1.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);

  // Ou à une position spécifique
  PopupMenu1.Popup(100, 100);
end;
```

---

## Les barres d'outils (TToolBar)

### Présentation

Une **TToolBar** est une barre horizontale contenant des boutons avec icônes :

```
┌─────────────────────────────────────────────┐
│ [📄] [📁] [💾] │ [✂] [📋] [📌] │ [▶] [⏸]    │
├─────────────────────────────────────────────┤
│                                             │
│                                             │
└─────────────────────────────────────────────┘
  Fichier        Édition         Lecture
```

### Créer une TToolBar

**Étape 1 : Ajouter le composant**
1. Palette → Onglet **"Common Controls"**
2. Cliquez sur **TToolBar**
3. Cliquez sur le formulaire

**Étape 2 : Positionner**
```pascal
ToolBar1.Align := alTop;  // En haut, pleine largeur
```

**Étape 3 : Ajouter des boutons**
1. Clic droit sur la ToolBar
2. **"Éditeur de boutons de barre d'outils..."**

Ou :
1. Cliquez sur la ToolBar
2. Dans l'Inspecteur d'Objets, trouvez la propriété `Buttons`
3. Cliquez sur `[...]`

### Éditeur de boutons

L'éditeur vous permet d'ajouter des **TToolButton** :

```
┌─────────────────────────────┐
│ ToolBar Button Editor       │
├─────────────────────────────┤
│ Buttons:                    │
│ ┌─────────────────────────┐ │
│ │ [Nouveau]               │ │
│ │ [Ouvrir]                │ │
│ │ [Enregistrer]           │ │
│ └─────────────────────────┘ │
│                             │
│ [Ajouter] [Supprimer]       │
└─────────────────────────────┘
```

**Ajouter un bouton :**
1. Cliquez sur **"Ajouter"**
2. Un nouveau bouton apparaît
3. Configurez ses propriétés dans l'Inspecteur d'Objets

### Propriétés d'un TToolButton

#### Style

```pascal
ToolButton1.Style := tbsButton;  // Bouton normal (défaut)
```

**Valeurs possibles :**

| Style | Description |
|-------|-------------|
| `tbsButton` | Bouton normal (cliquable) |
| `tbsCheck` | Bouton avec état on/off (reste enfoncé) |
| `tbsDropDown` | Bouton avec menu déroulant |
| `tbsSeparator` | Séparateur vertical |
| `tbsDivider` | Ligne de séparation |

**Exemple :**
```pascal
// Bouton normal
BtnNouveau.Style := tbsButton;

// Séparateur
ToolButton1.Style := tbsSeparator;  
ToolButton1.Width := 8;  // Largeur du séparateur  

// Bouton toggle (gras, italique, etc.)
BtnGras.Style := tbsCheck;
```

#### Caption

```pascal
ToolButton1.Caption := 'Nouveau';
```

**Description :** Texte affiché sur le bouton

**Note :** Si vous avez des icônes, le texte est souvent omis ou affiché en dessous

#### Hint et ShowHint

```pascal
ToolButton1.Hint := 'Créer un nouveau document (Ctrl+N)';  
ToolButton1.ShowHint := True;  
```

**Important :** Les hints sont essentiels pour les barres d'outils avec icônes !

#### ImageIndex

```pascal
ToolButton1.ImageIndex := 0;  // Première image
```

**Description :** Index de l'icône dans le TImageList associé

#### Enabled et Visible

Comme tous les composants :
```pascal
ToolButton1.Enabled := DocumentOuvert;  
ToolButton1.Visible := ModeAvance;  
```

#### Grouped et GroupIndex

Pour créer des boutons mutuellement exclusifs :
```pascal
BtnGauche.Grouped := True;  
BtnGauche.GroupIndex := 1;  

BtnCentre.Grouped := True;  
BtnCentre.GroupIndex := 1;  

BtnDroite.Grouped := True;  
BtnDroite.GroupIndex := 1;  
```

Un seul peut être enfoncé à la fois (comme des boutons radio).

### Événement OnClick

```pascal
procedure TForm1.ToolButton1Click(Sender: TObject);  
begin  
  // Action du bouton
  NouveauDocument;
end;
```

### Associer un TImageList

Pour avoir des icônes sur vos boutons :

**Étape 1 : Ajouter un TImageList**
1. Palette → Onglet **"Common Controls"**
2. Ajoutez **TImageList** au formulaire

**Étape 2 : Charger des images**
1. Double-cliquez sur ImageList1
2. Cliquez sur **"Ajouter"**
3. Sélectionnez vos images (PNG, BMP, ICO)
4. Répétez pour toutes vos icônes

**Étape 3 : Associer à la ToolBar**
```pascal
ToolBar1.Images := ImageList1;
```

**Étape 4 : Définir les ImageIndex**
```pascal
BtnNouveau.ImageIndex := 0;    // Première image  
BtnOuvrir.ImageIndex := 1;     // Deuxième image  
BtnEnregistrer.ImageIndex := 2; // Troisième image  
```

### Exemple complet de ToolBar

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration de la ToolBar
  ToolBar1.Align := alTop;
  ToolBar1.Images := ImageList1;
  ToolBar1.ShowCaptions := False;  // Cacher les textes (uniquement icônes)

  // Bouton Nouveau
  BtnNouveau.ImageIndex := 0;
  BtnNouveau.Hint := 'Nouveau document (Ctrl+N)';

  // Bouton Ouvrir
  BtnOuvrir.ImageIndex := 1;
  BtnOuvrir.Hint := 'Ouvrir un document (Ctrl+O)';

  // Séparateur
  ToolButton1.Style := tbsSeparator;

  // Bouton Couper
  BtnCouper.ImageIndex := 2;
  BtnCouper.Hint := 'Couper (Ctrl+X)';

  // Bouton Copier
  BtnCopier.ImageIndex := 3;
  BtnCopier.Hint := 'Copier (Ctrl+C)';

  // Bouton Coller
  BtnColler.ImageIndex := 4;
  BtnColler.Hint := 'Coller (Ctrl+V)';
end;

procedure TForm1.BtnNouveauClick(Sender: TObject);  
begin  
  NouveauDocument;
end;

procedure TForm1.BtnOuvrirClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
    OuvrirDocument(OpenDialog1.FileName);
end;
```

---

## Les Actions (TActionList)

### Le problème

Imaginez : vous avez une fonction "Copier" accessible par :
- Un item de menu (Menu → Édition → Copier)
- Un bouton de barre d'outils
- Un menu contextuel (clic droit)
- Un raccourci clavier (Ctrl+C)

**Sans TActionList :**
```pascal
// 4 handlers différents qui font la même chose !
procedure TForm1.MenuCopierClick(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;

procedure TForm1.BtnCopierClick(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;

procedure TForm1.PopupCopierClick(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;
// etc.
```

**Problème :** Duplication de code, difficile à maintenir !

### La solution : TActionList

Une **Action** centralise :
- Le code à exécuter (OnExecute)
- Le texte (Caption)
- Le raccourci (ShortCut)
- L'état (Enabled, Checked)
- L'icône (ImageIndex)

**Avec TActionList :**
```pascal
// UN SEUL handler !
procedure TForm1.ActionCopierExecute(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;

// Et tous les composants utilisent cette action :
MenuCopier.Action := ActionCopier;  
BtnCopier.Action := ActionCopier;  
PopupCopier.Action := ActionCopier;  
```

### Créer un TActionList

**Étape 1 : Ajouter le composant**
1. Palette → Onglet **"Standard"**
2. Ajoutez **TActionList** au formulaire

**Étape 2 : Créer des actions**
1. Double-cliquez sur **ActionList1**
2. L'éditeur d'actions s'ouvre

**Étape 3 : Nouvelle action**
1. Cliquez sur **"Nouvelle action"** (icône +)
2. Une nouvelle action **Action1** est créée
3. Configurez-la dans l'Inspecteur d'Objets

### Propriétés d'une Action

#### Caption

```pascal
ActionNouveau.Caption := '&Nouveau';
```

**Description :** Texte affiché (avec & pour raccourci Alt)

#### ShortCut

```pascal
ActionNouveau.ShortCut := TextToShortCut('Ctrl+N');
```

**Description :** Raccourci clavier

#### Hint

```pascal
ActionNouveau.Hint := 'Créer un nouveau document';
```

**Description :** Bulle d'aide

#### Enabled

```pascal
ActionCopier.Enabled := Memo1.SelLength > 0;
```

**Description :** Actif/désactivé

**Important :** Quand vous changez `ActionCopier.Enabled`, **tous** les composants liés sont automatiquement mis à jour !

#### Checked

```pascal
ActionGras.Checked := True;
```

**Description :** Pour les actions toggle (marche/arrêt)

#### ImageIndex

```pascal
ActionNouveau.ImageIndex := 0;
```

**Description :** Index de l'icône

#### Category

```pascal
ActionNouveau.Category := 'Fichier';  
ActionCopier.Category := 'Édition';  
```

**Description :** Catégorie pour organiser les actions

### Événement OnExecute

```pascal
procedure TForm1.ActionNouveauExecute(Sender: TObject);  
begin  
  // Code exécuté quand l'action est déclenchée
  NouveauDocument;
  StatusBar1.SimpleText := 'Nouveau document créé';
end;
```

**Déclenché par :**
- Clic sur un menu lié
- Clic sur un bouton lié
- Raccourci clavier
- Appel par code : `ActionNouveau.Execute;`

### Événement OnUpdate

```pascal
procedure TForm1.ActionCopierUpdate(Sender: TObject);  
begin  
  // Appelé régulièrement (souvent à chaque mouvement de souris)
  // Permet de mettre à jour l'état de l'action
  ActionCopier.Enabled := Memo1.SelLength > 0;
end;
```

**Usage :** Mise à jour automatique de l'état selon le contexte

**Attention :** OnUpdate est appelé très fréquemment, gardez le code léger !

### Associer une action à un composant

**Dans l'Inspecteur d'Objets :**
1. Sélectionnez le composant (TMenuItem, TToolButton, TButton)
2. Trouvez la propriété `Action`
3. Choisissez l'action dans la liste déroulante

**Par code :**
```pascal
MenuNouveau.Action := ActionNouveau;  
BtnNouveau.Action := ActionNouveau;  
```

**Résultat :** Le composant hérite automatiquement de :
- Caption
- ShortCut
- Hint
- Enabled
- Checked
- ImageIndex
- OnClick → OnExecute

### Exemple complet avec TActionList

```pascal
type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    ActionNouveau: TAction;
    ActionOuvrir: TAction;
    ActionEnregistrer: TAction;
    ActionCopier: TAction;
    ActionCouper: TAction;
    ActionColler: TAction;

    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuNouveau: TMenuItem;
    MenuOuvrir: TMenuItem;
    MenuEnregistrer: TMenuItem;
    MenuEdition: TMenuItem;
    MenuCouper: TMenuItem;
    MenuCopier: TMenuItem;
    MenuColler: TMenuItem;

    ToolBar1: TToolBar;
    BtnNouveau: TToolButton;
    BtnOuvrir: TToolButton;
    BtnEnregistrer: TToolButton;
    BtnCouper: TToolButton;
    BtnCopier: TToolButton;
    BtnColler: TToolButton;

    Memo1: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure ActionNouveauExecute(Sender: TObject);
    procedure ActionOuvrirExecute(Sender: TObject);
    procedure ActionEnregistrerExecute(Sender: TObject);
    procedure ActionCouperExecute(Sender: TObject);
    procedure ActionCopierExecute(Sender: TObject);
    procedure ActionCollerExecute(Sender: TObject);
    procedure ActionCopierUpdate(Sender: TObject);
    procedure ActionCouperUpdate(Sender: TObject);
    procedure ActionCollerUpdate(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration des actions
  ActionNouveau.Caption := '&Nouveau';
  ActionNouveau.ShortCut := TextToShortCut('Ctrl+N');
  ActionNouveau.Hint := 'Créer un nouveau document';
  ActionNouveau.ImageIndex := 0;

  ActionOuvrir.Caption := '&Ouvrir...';
  ActionOuvrir.ShortCut := TextToShortCut('Ctrl+O');
  ActionOuvrir.Hint := 'Ouvrir un document';
  ActionOuvrir.ImageIndex := 1;

  ActionCouper.Caption := 'Co&uper';
  ActionCouper.ShortCut := TextToShortCut('Ctrl+X');
  ActionCouper.Hint := 'Couper';
  ActionCouper.ImageIndex := 2;

  ActionCopier.Caption := '&Copier';
  ActionCopier.ShortCut := TextToShortCut('Ctrl+C');
  ActionCopier.Hint := 'Copier';
  ActionCopier.ImageIndex := 3;

  ActionColler.Caption := 'C&oller';
  ActionColler.ShortCut := TextToShortCut('Ctrl+V');
  ActionColler.Hint := 'Coller';
  ActionColler.ImageIndex := 4;

  // Associer les actions aux menus
  MenuNouveau.Action := ActionNouveau;
  MenuOuvrir.Action := ActionOuvrir;
  MenuCouper.Action := ActionCouper;
  MenuCopier.Action := ActionCopier;
  MenuColler.Action := ActionColler;

  // Associer les actions aux boutons de la toolbar
  BtnNouveau.Action := ActionNouveau;
  BtnOuvrir.Action := ActionOuvrir;
  BtnCouper.Action := ActionCouper;
  BtnCopier.Action := ActionCopier;
  BtnColler.Action := ActionColler;

  // Configuration de la toolbar
  ToolBar1.Images := ImageList1;
end;

// UN SEUL handler par action !
procedure TForm1.ActionNouveauExecute(Sender: TObject);  
begin  
  Memo1.Clear;
end;

procedure TForm1.ActionOuvrirExecute(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.ActionCouperExecute(Sender: TObject);  
begin  
  Memo1.CutToClipboard;
end;

procedure TForm1.ActionCopierExecute(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;

procedure TForm1.ActionCollerExecute(Sender: TObject);  
begin  
  Memo1.PasteFromClipboard;
end;

// Mise à jour automatique de l'état
procedure TForm1.ActionCopierUpdate(Sender: TObject);  
begin  
  ActionCopier.Enabled := Memo1.SelLength > 0;
end;

procedure TForm1.ActionCouperUpdate(Sender: TObject);  
begin  
  ActionCouper.Enabled := Memo1.SelLength > 0;
end;

procedure TForm1.ActionCollerUpdate(Sender: TObject);  
begin  
  ActionColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;
```

**Avantages :**
- Un seul OnExecute par action
- Mise à jour automatique de tous les composants liés
- Facile à maintenir
- Raccourcis centralisés
- Icônes centralisées

---

## Raccourcis clavier avancés

### Combinaisons courantes

```pascal
// Ctrl
ShortCut := TextToShortCut('Ctrl+S');  
ShortCut := TextToShortCut('Ctrl+Shift+S');  

// Alt
ShortCut := TextToShortCut('Alt+F4');  
ShortCut := TextToShortCut('Alt+Enter');  

// Touches de fonction
ShortCut := TextToShortCut('F1');  
ShortCut := TextToShortCut('Shift+F1');  
ShortCut := TextToShortCut('Ctrl+F5');  

// Autres touches
ShortCut := TextToShortCut('Del');  
ShortCut := TextToShortCut('Shift+Del');  
ShortCut := TextToShortCut('Ins');  
```

### Raccourcis standards à respecter

| Fonction | Raccourci | Standard |
|----------|-----------|----------|
| Nouveau | Ctrl+N | ✅ Universel |
| Ouvrir | Ctrl+O | ✅ Universel |
| Enregistrer | Ctrl+S | ✅ Universel |
| Quitter | Alt+F4 | ✅ Windows |
| Couper | Ctrl+X | ✅ Universel |
| Copier | Ctrl+C | ✅ Universel |
| Coller | Ctrl+V | ✅ Universel |
| Annuler | Ctrl+Z | ✅ Universel |
| Rétablir | Ctrl+Y | ✅ Universel |
| Tout sélectionner | Ctrl+A | ✅ Universel |
| Rechercher | Ctrl+F | ✅ Universel |
| Imprimer | Ctrl+P | ✅ Universel |
| Aide | F1 | ✅ Universel |

**Important :** Respecter ces conventions améliore l'ergonomie !

### Raccourcis personnalisés

Pour des fonctions spécifiques à votre application :
```pascal
// Fonctions avancées
ActionCompiler.ShortCut := TextToShortCut('F9');  
ActionDeboguer.ShortCut := TextToShortCut('F5');  
ActionFormat.ShortCut := TextToShortCut('Ctrl+Shift+F');  
```

**Conseil :** Documentez les raccourcis non-standards dans l'aide ou un menu "Raccourcis clavier".

---

## Icônes et images

### Où trouver des icônes ?

**Sources gratuites :**
- Icons8 (https://icons8.com)
- Flaticon (https://www.flaticon.com)
- Font Awesome (version icônes PNG)
- Material Icons
- Lazarus lui-même (dossier images/)

**Formats recommandés :**
- **PNG** : Transparence, bonne qualité
- **ICO** : Multi-résolutions
- **BMP** : Simple mais pas de transparence

**Tailles courantes :**
- 16×16 : Menus, petites toolbars
- 24×24 : Toolbars standards
- 32×32 : Grandes toolbars
- 48×48 : Très grandes icônes

### Utiliser des icônes cohérentes

**Thèmes populaires :**
- **Flat** : Moderne, minimaliste
- **3D** : Réaliste, classique
- **Line Art** : Contours uniquement
- **Material Design** : Style Google

**Conseil :** Choisissez un thème et restez-y cohérent !

### Gérer plusieurs tailles

Créez plusieurs TImageList pour différentes tailles :

```pascal
ImageList16.Width := 16;  
ImageList16.Height := 16;  

ImageList24.Width := 24;  
ImageList24.Height := 24;  

// Associer selon le contexte
MainMenu1.Images := ImageList16;  // Petites icônes pour menus  
ToolBar1.Images := ImageList24;   // Grandes icônes pour toolbar  
```

---

## Exemple complet : Application avec menus complets

```pascal
type
  TFormPrincipal = class(TForm)
    // Actions
    ActionList1: TActionList;
    ActionNouveau: TAction;
    ActionOuvrir: TAction;
    ActionEnregistrer: TAction;
    ActionEnregistrerSous: TAction;
    ActionQuitter: TAction;
    ActionCouper: TAction;
    ActionCopier: TAction;
    ActionColler: TAction;
    ActionSelectionnerTout: TAction;
    ActionRechercher: TAction;
    ActionAPropos: TAction;

    // Menu principal
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuNouveau: TMenuItem;
    MenuOuvrir: TMenuItem;
    MenuEnregistrer: TMenuItem;
    MenuEnregistrerSous: TMenuItem;
    MenuSep1: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuEdition: TMenuItem;
    MenuCouper: TMenuItem;
    MenuCopier: TMenuItem;
    MenuColler: TMenuItem;
    MenuSep2: TMenuItem;
    MenuSelectionnerTout: TMenuItem;
    MenuRechercher: TMenuItem;
    MenuAide: TMenuItem;
    MenuAPropos: TMenuItem;

    // Toolbar
    ToolBar1: TToolBar;
    BtnNouveau: TToolButton;
    BtnOuvrir: TToolButton;
    BtnEnregistrer: TToolButton;
    ToolButton1: TToolButton;  // Séparateur
    BtnCouper: TToolButton;
    BtnCopier: TToolButton;
    BtnColler: TToolButton;

    // Composants
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ImageList1: TImageList;

    procedure FormCreate(Sender: TObject);
    procedure ConfigurerActions;
    procedure ConfigurerMenus;
    procedure ConfigurerToolbar;

    // Handlers des actions
    procedure ActionNouveauExecute(Sender: TObject);
    procedure ActionOuvrirExecute(Sender: TObject);
    procedure ActionEnregistrerExecute(Sender: TObject);
    procedure ActionQuitterExecute(Sender: TObject);
    procedure ActionCouperExecute(Sender: TObject);
    procedure ActionCopierExecute(Sender: TObject);
    procedure ActionCollerExecute(Sender: TObject);
    procedure ActionSelectionnerToutExecute(Sender: TObject);
    procedure ActionAProposExecute(Sender: TObject);

    // Updates
    procedure ActionCouperUpdate(Sender: TObject);
    procedure ActionCopierUpdate(Sender: TObject);
    procedure ActionCollerUpdate(Sender: TObject);
  end;

procedure TFormPrincipal.FormCreate(Sender: TObject);  
begin  
  ConfigurerActions;
  ConfigurerMenus;
  ConfigurerToolbar;

  // Configuration générale
  Memo1.Align := alClient;
  StatusBar1.SimpleText := 'Prêt';

  Form1.Caption := 'Éditeur de texte';
  Form1.Width := 800;
  Form1.Height := 600;
end;

procedure TFormPrincipal.ConfigurerActions;  
begin  
  // Fichier
  ActionNouveau.Caption := '&Nouveau';
  ActionNouveau.ShortCut := TextToShortCut('Ctrl+N');
  ActionNouveau.Hint := 'Nouveau document';
  ActionNouveau.ImageIndex := 0;

  ActionOuvrir.Caption := '&Ouvrir...';
  ActionOuvrir.ShortCut := TextToShortCut('Ctrl+O');
  ActionOuvrir.Hint := 'Ouvrir un fichier';
  ActionOuvrir.ImageIndex := 1;

  ActionEnregistrer.Caption := '&Enregistrer';
  ActionEnregistrer.ShortCut := TextToShortCut('Ctrl+S');
  ActionEnregistrer.Hint := 'Enregistrer';
  ActionEnregistrer.ImageIndex := 2;

  ActionQuitter.Caption := '&Quitter';
  ActionQuitter.ShortCut := TextToShortCut('Alt+F4');

  // Édition
  ActionCouper.Caption := 'Co&uper';
  ActionCouper.ShortCut := TextToShortCut('Ctrl+X');
  ActionCouper.Hint := 'Couper';
  ActionCouper.ImageIndex := 3;

  ActionCopier.Caption := '&Copier';
  ActionCopier.ShortCut := TextToShortCut('Ctrl+C');
  ActionCopier.Hint := 'Copier';
  ActionCopier.ImageIndex := 4;

  ActionColler.Caption := 'C&oller';
  ActionColler.ShortCut := TextToShortCut('Ctrl+V');
  ActionColler.Hint := 'Coller';
  ActionColler.ImageIndex := 5;

  ActionSelectionnerTout.Caption := '&Tout sélectionner';
  ActionSelectionnerTout.ShortCut := TextToShortCut('Ctrl+A');
end;

procedure TFormPrincipal.ConfigurerMenus;  
begin  
  // Structure du menu Fichier
  MenuNouveau.Action := ActionNouveau;
  MenuOuvrir.Action := ActionOuvrir;
  MenuEnregistrer.Action := ActionEnregistrer;
  MenuSep1.Caption := '-';
  MenuQuitter.Action := ActionQuitter;

  // Structure du menu Édition
  MenuCouper.Action := ActionCouper;
  MenuCopier.Action := ActionCopier;
  MenuColler.Action := ActionColler;
  MenuSep2.Caption := '-';
  MenuSelectionnerTout.Action := ActionSelectionnerTout;
end;

procedure TFormPrincipal.ConfigurerToolbar;  
begin  
  ToolBar1.Align := alTop;
  ToolBar1.Images := ImageList1;
  ToolBar1.ShowCaptions := False;

  // Boutons
  BtnNouveau.Action := ActionNouveau;
  BtnOuvrir.Action := ActionOuvrir;
  BtnEnregistrer.Action := ActionEnregistrer;

  // Séparateur
  ToolButton1.Style := tbsSeparator;

  BtnCouper.Action := ActionCouper;
  BtnCopier.Action := ActionCopier;
  BtnColler.Action := ActionColler;
end;

// Implémentation des actions
procedure TFormPrincipal.ActionNouveauExecute(Sender: TObject);  
begin  
  if Memo1.Modified then
  begin
    case MessageDlg('Enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ActionEnregistrer.Execute;
      mrCancel: Exit;
    end;
  end;

  Memo1.Clear;
  Memo1.Modified := False;
  StatusBar1.SimpleText := 'Nouveau document';
end;

procedure TFormPrincipal.ActionOuvrirExecute(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    Memo1.Modified := False;
    StatusBar1.SimpleText := 'Fichier ouvert : ' + OpenDialog1.FileName;
  end;
end;

procedure TFormPrincipal.ActionEnregistrerExecute(Sender: TObject);  
begin  
  if SaveDialog1.Execute then
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
    Memo1.Modified := False;
    StatusBar1.SimpleText := 'Fichier enregistré : ' + SaveDialog1.FileName;
  end;
end;

procedure TFormPrincipal.ActionQuitterExecute(Sender: TObject);  
begin  
  Close;
end;

procedure TFormPrincipal.ActionCouperExecute(Sender: TObject);  
begin  
  Memo1.CutToClipboard;
end;

procedure TFormPrincipal.ActionCopierExecute(Sender: TObject);  
begin  
  Memo1.CopyToClipboard;
end;

procedure TFormPrincipal.ActionCollerExecute(Sender: TObject);  
begin  
  Memo1.PasteFromClipboard;
end;

procedure TFormPrincipal.ActionSelectionnerToutExecute(Sender: TObject);  
begin  
  Memo1.SelectAll;
end;

// Mise à jour de l'état des actions
procedure TFormPrincipal.ActionCouperUpdate(Sender: TObject);  
begin  
  ActionCouper.Enabled := Memo1.SelLength > 0;
end;

procedure TFormPrincipal.ActionCopierUpdate(Sender: TObject);  
begin  
  ActionCopier.Enabled := Memo1.SelLength > 0;
end;

procedure TFormPrincipal.ActionCollerUpdate(Sender: TObject);  
begin  
  ActionColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;
```

---

## Bonnes pratiques

### 1. Utilisez TActionList systématiquement

✅ **Une action = une fonction**

✅ Centralisez le code dans OnExecute

✅ Liez tous les composants (menus, boutons) à l'action

### 2. Organisez vos menus logiquement

**Structure classique :**
```
Fichier | Édition | Affichage | Outils | Fenêtre | Aide
```

**Dans Fichier :**
- Actions sur les fichiers
- Quitter en dernier

**Dans Édition :**
- Annuler/Rétablir en premier
- Copier/Coller/Couper
- Rechercher/Remplacer

**Dans Aide :**
- Aide en premier
- À propos en dernier

### 3. Raccourcis cohérents

✅ Respectez les standards (Ctrl+C = Copier)

✅ Documentez les raccourcis personnalisés

✅ Évitez les conflits (ne réassignez pas Ctrl+S)

### 4. Hints informatifs

```pascal
Action.Hint := 'Copier la sélection (Ctrl+C)';
```

✅ Décrivez l'action

✅ Indiquez le raccourci

✅ Soyez concis

### 5. Désactivez les actions non disponibles

```pascal
procedure ActionUpdate(Sender: TObject);  
begin  
  ActionCopier.Enabled := SelLength > 0;
  ActionEnregistrer.Enabled := Modified;
  ActionAnnuler.Enabled := CanUndo;
end;
```

**Ne cachez pas**, désactivez (grisé) pour que l'utilisateur sache que la fonction existe.

### 6. Séparateurs dans les menus

Groupez les fonctions similaires :
```
Nouveau  
Ouvrir  
Enregistrer  
────────    ← Séparateur
Imprimer
────────    ← Séparateur
Quitter
```

### 7. Icônes cohérentes

✅ Même style pour toutes les icônes

✅ Même taille (16×16 ou 24×24)

✅ Utilisez des icônes universelles (disquette = enregistrer)

### 8. Testez l'accessibilité

✅ Tous les menus accessibles au clavier (Alt+F)

✅ Raccourcis pour les fonctions courantes

✅ Hints sur les boutons de toolbar

---

## Résumé

### Composants principaux

| Composant | Usage |
|-----------|-------|
| **TMainMenu** | Menu principal en haut de la fenêtre |
| **TPopupMenu** | Menu contextuel (clic droit) |
| **TToolBar** | Barre d'outils avec boutons |
| **TActionList** | Centralisation des actions |
| **TImageList** | Collection d'icônes |

### Workflow recommandé

1. **Créer les actions** (TActionList)
   - Définir Caption, ShortCut, Hint
   - Implémenter OnExecute et OnUpdate

2. **Créer les menus** (TMainMenu)
   - Structure hiérarchique
   - Associer aux actions

3. **Créer la toolbar** (TToolBar)
   - Ajouter des boutons
   - Associer aux mêmes actions

4. **Ajouter les icônes** (TImageList)
   - Charger les images
   - Définir ImageIndex

**Résultat :** Interface cohérente, facile à maintenir !

### Checklist

✅ Toutes les fonctions accessibles par menu  
✅ Fonctions courantes dans la toolbar  
✅ Raccourcis clavier standards respectés  
✅ Hints sur tous les boutons de toolbar  
✅ Actions activées/désactivées selon le contexte  
✅ Icônes cohérentes et lisibles  
✅ Séparateurs pour organiser les menus  

---

## Prochaines étapes

Maintenant que vous maîtrisez les menus et barres d'outils, vous pouvez créer des interfaces professionnelles !

Dans les prochaines sections :
- **14.9 Boîtes de dialogue standard** : Ouvrir, Enregistrer, Couleurs, etc.
- **15. Composants LCL Fondamentaux** : Explorer d'autres composants

Félicitations ! Vous savez maintenant créer des applications avec une interface complète et professionnelle ! 🎉

---

**Point clé à retenir :** Utilisez TActionList pour centraliser la logique et maintenir la cohérence entre menus, toolbars et raccourcis !

⏭️ [Boîtes de dialogue standard](/14-introduction-applications-graphiques/09-boites-dialogue-standard.md)
