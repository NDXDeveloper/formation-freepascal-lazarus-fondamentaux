🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.9 Actions et TActionList

## Introduction

Dans une application, certaines **commandes** sont utilisées à plusieurs endroits : le menu "Copier" peut être accessible via un menu, un bouton de barre d'outils, un menu contextuel, et un raccourci clavier (Ctrl+C). Sans organisation, vous devriez coder la même fonctionnalité plusieurs fois et maintenir la cohérence entre tous ces éléments.

Les **Actions** (TAction) et les **listes d'actions** (TActionList) résolvent ce problème en **centralisant** les commandes de votre application.

---

## Le Problème sans Actions

### Exemple Classique : Commande "Copier"

Sans actions, pour implémenter "Copier" partout :

```pascal
// Note : l'utilisation de Clipboard nécessite uses Clipbrd
// Menu principal
procedure TForm1.MenuItemCopierClick(Sender: TObject);
begin
  Clipboard.AsText := Memo1.SelText;
end;

// Bouton barre d'outils
procedure TForm1.BtnCopierClick(Sender: TObject);
begin
  Clipboard.AsText := Memo1.SelText;  // Code dupliqué !
end;

// Menu contextuel
procedure TForm1.PopupMenuCopierClick(Sender: TObject);
begin
  Clipboard.AsText := Memo1.SelText;  // Encore dupliqué !
end;

// Raccourci clavier
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_C) then
    Clipboard.AsText := Memo1.SelText;  // Toujours dupliqué !
end;
```

**Problèmes :**
- ❌ Code **dupliqué** (4 fois la même chose)
- ❌ Difficile à **maintenir** (modifier à 4 endroits)
- ❌ Risque d'**incohérence** (oublier un endroit)
- ❌ Gestion manuelle de l'**état** (activer/désactiver partout)

### La Solution : Actions

Avec une Action, vous écrivez le code **une seule fois** et le liez à tous les contrôles :

```pascal
// UNE SEULE procédure
procedure TForm1.ActCopierExecute(Sender: TObject);
begin
  Clipboard.AsText := Memo1.SelText;
end;

// Tous les contrôles utilisent automatiquement cette action !
```

---

## Qu'est-ce qu'une Action ?

### Définition

Une **Action** (TAction) est un objet qui représente une **commande** de votre application. Elle encapsule :
- Le **code à exécuter** (événement OnExecute)
- Le **nom/libellé** (Caption)
- Le **raccourci clavier** (ShortCut)
- L'**état** (Enabled, Checked, Visible)
- L'**icône** (ImageIndex)
- L'**infobulle** (Hint)

### Analogie

Imaginez une Action comme une **télécommande** :
- La télécommande peut être **copiée** (plusieurs boutons font la même chose)
- Elle a un **nom** ("Volume +")
- Elle peut être **activée ou désactivée** (si la TV est éteinte, les boutons sont désactivés)
- Elle exécute une **commande** unique quand on appuie dessus

Peu importe le bouton que vous pressez (physique, sur l'application mobile, vocal), c'est toujours la **même commande** qui s'exécute.

---

## TActionList : Le Conteneur d'Actions

### Présentation

`TActionList` est un composant **non-visuel** qui contient et gère une **collection d'actions** (TAction).

### Hiérarchie

```
TObject
  └─ TPersistent
       └─ TComponent
            └─ TCustomActionList
                 └─ TActionList

TObject
  └─ TPersistent
       └─ TComponent
            └─ TBasicAction
                 └─ TContainedAction
                      └─ TCustomAction
                           └─ TAction
```

### Structure

```
Formulaire
  └─ ActionList1 (TActionList)
       ├─ ActNouveau (TAction)
       ├─ ActOuvrir (TAction)
       ├─ ActSauvegarder (TAction)
       ├─ ActCopier (TAction)
       ├─ ActColler (TAction)
       └─ ActQuitter (TAction)
```

---

## Créer et Utiliser des Actions

### Étape 1 : Placer un TActionList

1. Dans l'IDE Lazarus, placez un composant `TActionList` sur votre formulaire
2. Il apparaît dans la zone non-visuelle (bas du formulaire)

### Étape 2 : Créer des Actions

1. **Double-cliquez** sur le TActionList (ou clic droit → "Éditeur d'éléments de liste")
2. L'**Éditeur d'Actions** s'ouvre
3. Cliquez sur "Ajouter" (ou icône +) pour créer une nouvelle action
4. Configurez la nouvelle action :
   - **Name** : `ActCopier` (nom dans le code)
   - **Caption** : `&Copier` (texte affiché, & pour raccourci Alt)
   - **ShortCut** : `Ctrl+C` (raccourci clavier)
   - **Hint** : `Copier la sélection`

### Étape 3 : Coder l'Événement OnExecute

Double-cliquez sur l'action dans l'éditeur pour créer l'événement `OnExecute` :

```pascal
procedure TForm1.ActCopierExecute(Sender: TObject);
begin
  if Memo1.SelLength > 0 then
    Clipboard.AsText := Memo1.SelText;
end;
```

### Étape 4 : Lier l'Action aux Contrôles

#### Lier à un Menu

```pascal
// Dans l'éditeur de menu
MenuItem1.Caption := 'Copier';
MenuItem1.Action := ActCopier;  // Lie l'action

// Le Caption, ShortCut, Enabled seront gérés automatiquement !
```

#### Lier à un Bouton

```pascal
Button1.Action := ActCopier;
// Le Caption du bouton devient automatiquement "Copier"
// Le bouton sera activé/désactivé automatiquement
```

#### Lier à un SpeedButton (barre d'outils)

```pascal
SpeedButton1.Action := ActCopier;
SpeedButton1.Flat := True;  // Style plat pour barre d'outils
```

### Exemple Complet : Actions de Base

```pascal
type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    ActCopier: TAction;
    ActColler: TAction;
    ActCouper: TAction;
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    MenuItemEdition: TMenuItem;
    MenuItemCouper: TMenuItem;
    MenuItemCopier: TMenuItem;
    MenuItemColler: TMenuItem;
    ToolBar1: TToolBar;
    BtnCouper: TSpeedButton;
    BtnCopier: TSpeedButton;
    BtnColler: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ActCopierExecute(Sender: TObject);
    procedure ActCollerExecute(Sender: TObject);
    procedure ActCouperExecute(Sender: TObject);
    procedure ActCopierUpdate(Sender: TObject);
    procedure ActCollerUpdate(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration des actions
  ActCouper.Caption := 'Co&uper';
  ActCouper.ShortCut := TextToShortCut('Ctrl+X');
  ActCouper.Hint := 'Couper la sélection';

  ActCopier.Caption := '&Copier';
  ActCopier.ShortCut := TextToShortCut('Ctrl+C');
  ActCopier.Hint := 'Copier la sélection';

  ActColler.Caption := 'C&oller';
  ActColler.ShortCut := TextToShortCut('Ctrl+V');
  ActColler.Hint := 'Coller';

  // Lier aux menus
  MenuItemCouper.Action := ActCouper;
  MenuItemCopier.Action := ActCopier;
  MenuItemColler.Action := ActColler;

  // Lier aux boutons
  BtnCouper.Action := ActCouper;
  BtnCopier.Action := ActCopier;
  BtnColler.Action := ActColler;
end;

procedure TForm1.ActCopierExecute(Sender: TObject);
begin
  Clipboard.AsText := Memo1.SelText;
end;

procedure TForm1.ActCollerExecute(Sender: TObject);
begin
  Memo1.SelText := Clipboard.AsText;
end;

procedure TForm1.ActCouperExecute(Sender: TObject);
begin
  Clipboard.AsText := Memo1.SelText;
  Memo1.SelText := '';
end;

// Mise à jour automatique de l'état
procedure TForm1.ActCopierUpdate(Sender: TObject);
begin
  ActCopier.Enabled := Memo1.SelLength > 0;
end;

procedure TForm1.ActCollerUpdate(Sender: TObject);
begin
  ActColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;
```

---

## Propriétés des Actions

### Propriétés Visuelles

```pascal
property Caption: string;        // Texte affiché (avec & pour raccourci Alt)
property Hint: string;           // Infobulle
property ImageIndex: Integer;    // Index de l'icône (si ImageList)
property ShortCut: TShortCut;    // Raccourci clavier
```

### Propriétés d'État

```pascal
property Enabled: Boolean;       // Action activée/désactivée
property Checked: Boolean;       // Action cochée (pour menus)
property Visible: Boolean;       // Action visible/invisible
```

### Propriétés de Catégorie

```pascal
property Category: string;       // Catégorie pour organisation
property GroupIndex: Integer;    // Groupe pour actions mutuellement exclusives
```

---

## Événements des Actions

### OnExecute

L'événement **principal** déclenché quand l'action est exécutée :

```pascal
procedure TForm1.ActNouveauExecute(Sender: TObject);
begin
  // Code de la commande "Nouveau"
  Memo1.Clear;
end;
```

### OnUpdate

Événement appelé **périodiquement** pour mettre à jour l'état de l'action :

```pascal
procedure TForm1.ActSauvegarderUpdate(Sender: TObject);
begin
  // Désactiver si rien à sauvegarder
  ActSauvegarder.Enabled := Memo1.Modified;
end;
```

**Note** : OnUpdate est appelé automatiquement par l'application au moment opportun (généralement quand l'interface est inactive). Vous n'avez pas besoin de l'appeler manuellement.

### OnHint

Événement appelé pour afficher l'infobulle :

```pascal
procedure TForm1.ActCopierHint(var HintStr: string; var CanShow: Boolean);
begin
  HintStr := 'Copier : ' + IntToStr(Memo1.SelLength) + ' caractères sélectionnés';
  CanShow := True;
end;
```

---

## Raccourcis Clavier

### Définir un Raccourci

#### Méthode 1 : Dans l'IDE

Dans l'Inspecteur d'Objets, propriété `ShortCut` :
- Cliquez sur la valeur
- Une fenêtre s'ouvre
- Pressez la combinaison de touches (ex: Ctrl+N)

#### Méthode 2 : Par Code

```pascal
ActNouveau.ShortCut := TextToShortCut('Ctrl+N');
ActOuvrir.ShortCut := TextToShortCut('Ctrl+O');
ActSauvegarder.ShortCut := TextToShortCut('Ctrl+S');
ActQuitter.ShortCut := TextToShortCut('Alt+F4');

// Avec touches de fonction
ActAide.ShortCut := VK_F1;
ActRechercher.ShortCut := TextToShortCut('Ctrl+F');

// Avec Shift
ActSelectionnerTout.ShortCut := TextToShortCut('Ctrl+A');
ActRechercherSuivant.ShortCut := TextToShortCut('F3');
```

### Raccourcis Standards

| Commande | Raccourci Windows | Raccourci macOS |
|----------|-------------------|-----------------|
| **Nouveau** | Ctrl+N | Cmd+N |
| **Ouvrir** | Ctrl+O | Cmd+O |
| **Sauvegarder** | Ctrl+S | Cmd+S |
| **Sauvegarder sous** | Ctrl+Shift+S | Cmd+Shift+S |
| **Imprimer** | Ctrl+P | Cmd+P |
| **Quitter** | Alt+F4 | Cmd+Q |
| **Annuler** | Ctrl+Z | Cmd+Z |
| **Refaire** | Ctrl+Y ou Ctrl+Shift+Z | Cmd+Shift+Z |
| **Couper** | Ctrl+X | Cmd+X |
| **Copier** | Ctrl+C | Cmd+C |
| **Coller** | Ctrl+V | Cmd+V |
| **Sélectionner tout** | Ctrl+A | Cmd+A |
| **Rechercher** | Ctrl+F | Cmd+F |
| **Remplacer** | Ctrl+H | Cmd+Alt+F |
| **Aide** | F1 | - |

---

## Gestion Automatique de l'État

### Le Pouvoir de OnUpdate

L'événement `OnUpdate` est appelé **automatiquement** et permet de synchroniser l'état de l'action avec l'état de l'application :

```pascal
procedure TForm1.ActCouperUpdate(Sender: TObject);
begin
  // Active uniquement si du texte est sélectionné
  ActCouper.Enabled := Memo1.SelLength > 0;
end;

procedure TForm1.ActAnnulerUpdate(Sender: TObject);
begin
  // Active uniquement si on peut annuler
  ActAnnuler.Enabled := Memo1.CanUndo;
end;

procedure TForm1.ActRefaireUpdate(Sender: TObject);
begin
  // Active uniquement si on peut refaire
  ActRefaire.Enabled := Memo1.CanRedo;
end;
```

**Résultat** : Tous les contrôles liés (menu, bouton, etc.) sont **automatiquement** activés/désactivés selon l'état !

### Exemple : Mode Lecture Seule

```pascal
procedure TForm1.ActModeEditionUpdate(Sender: TObject);
begin
  ActModeEdition.Checked := not Memo1.ReadOnly;

  // Activer/désactiver les actions d'édition
  ActCouper.Enabled := not Memo1.ReadOnly;
  ActColler.Enabled := not Memo1.ReadOnly;
end;

procedure TForm1.ActModeEditionExecute(Sender: TObject);
begin
  // Basculer le mode lecture seule
  Memo1.ReadOnly := not Memo1.ReadOnly;
end;
```

---

## Actions avec Images (Icônes)

### Utiliser un ImageList

Pour afficher des icônes sur les menus et boutons :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Associer l'ImageList à l'ActionList
  ActionList1.Images := ImageList1;

  // Définir l'index de l'image pour chaque action
  ActNouveau.ImageIndex := 0;    // Icône "nouveau document"
  ActOuvrir.ImageIndex := 1;     // Icône "dossier ouvert"
  ActSauvegarder.ImageIndex := 2; // Icône "disquette"
  ActCopier.ImageIndex := 3;     // Icône "copier"
  ActColler.ImageIndex := 4;     // Icône "coller"

  // Les menus et boutons afficheront automatiquement les icônes
end;
```

### Icônes dans les Menus

```pascal
// Le menu principal peut aussi avoir l'ImageList
MainMenu1.Images := ImageList1;

// Les items de menu liés aux actions afficheront automatiquement les icônes
```

---

## Catégories d'Actions

Les actions peuvent être **organisées en catégories** pour faciliter la gestion :

```pascal
procedure TForm1.OrganiserActions;
begin
  // Catégorie "Fichier"
  ActNouveau.Category := 'Fichier';
  ActOuvrir.Category := 'Fichier';
  ActSauvegarder.Category := 'Fichier';
  ActQuitter.Category := 'Fichier';

  // Catégorie "Édition"
  ActCouper.Category := 'Édition';
  ActCopier.Category := 'Édition';
  ActColler.Category := 'Édition';
  ActAnnuler.Category := 'Édition';

  // Catégorie "Affichage"
  ActZoomPlus.Category := 'Affichage';
  ActZoomMoins.Category := 'Affichage';
end;
```

Dans l'éditeur d'actions, vous pouvez alors **filtrer** par catégorie.

---

## Actions Mutuellement Exclusives (Groupe Radio)

Pour créer des actions comme des boutons radio (une seule cochée à la fois) :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Définir le même GroupIndex
  ActAffichageListe.GroupIndex := 1;
  ActAffichageIcone.GroupIndex := 1;
  ActAffichageDetails.GroupIndex := 1;

  // Cocher l'option par défaut
  ActAffichageListe.Checked := True;
end;

procedure TForm1.ActAffichageListeExecute(Sender: TObject);
begin
  // Basculer l'état coché
  ActAffichageListe.Checked := True;
  // Les autres actions du même groupe seront automatiquement décochées

  // Appliquer le mode d'affichage
  ListView1.ViewStyle := vsReport;
end;

procedure TForm1.ActAffichageIconeExecute(Sender: TObject);
begin
  ActAffichageIcone.Checked := True;
  ListView1.ViewStyle := vsIcon;
end;

procedure TForm1.ActAffichageDetailsExecute(Sender: TObject);
begin
  ActAffichageDetails.Checked := True;
  ListView1.ViewStyle := vsReport;
end;
```

---

## Exemple Complet : Éditeur de Texte Simple

```pascal
type
  TFormEditeur = class(TForm)
    // Actions
    ActionList1: TActionList;
    ActNouveau: TAction;
    ActOuvrir: TAction;
    ActSauvegarder: TAction;
    ActQuitter: TAction;
    ActCouper: TAction;
    ActCopier: TAction;
    ActColler: TAction;
    ActSelectionnerTout: TAction;
    ActRechercher: TAction;

    // Interface
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

    // Événements
    procedure FormCreate(Sender: TObject);

    procedure ActNouveauExecute(Sender: TObject);
    procedure ActOuvrirExecute(Sender: TObject);
    procedure ActSauvegarderExecute(Sender: TObject);
    procedure ActQuitterExecute(Sender: TObject);

    procedure ActCouperExecute(Sender: TObject);
    procedure ActCopierExecute(Sender: TObject);
    procedure ActCollerExecute(Sender: TObject);
    procedure ActSelectionnerToutExecute(Sender: TObject);

    procedure ActCouperUpdate(Sender: TObject);
    procedure ActCopierUpdate(Sender: TObject);
    procedure ActCollerUpdate(Sender: TObject);
    procedure ActSauvegarderUpdate(Sender: TObject);
  private
    FFichierCourant: string;
    procedure MettreAJourTitre;
  end;

procedure TFormEditeur.FormCreate(Sender: TObject);
begin
  // Configuration des actions Fichier
  ActNouveau.Caption := '&Nouveau';
  ActNouveau.ShortCut := TextToShortCut('Ctrl+N');
  ActNouveau.Hint := 'Nouveau document';
  ActNouveau.Category := 'Fichier';

  ActOuvrir.Caption := '&Ouvrir...';
  ActOuvrir.ShortCut := TextToShortCut('Ctrl+O');
  ActOuvrir.Hint := 'Ouvrir un fichier';
  ActOuvrir.Category := 'Fichier';

  ActSauvegarder.Caption := '&Sauvegarder';
  ActSauvegarder.ShortCut := TextToShortCut('Ctrl+S');
  ActSauvegarder.Hint := 'Sauvegarder le document';
  ActSauvegarder.Category := 'Fichier';

  ActQuitter.Caption := '&Quitter';
  ActQuitter.ShortCut := TextToShortCut('Alt+F4');
  ActQuitter.Hint := 'Quitter l''application';
  ActQuitter.Category := 'Fichier';

  // Configuration des actions Édition
  ActCouper.Caption := 'Co&uper';
  ActCouper.ShortCut := TextToShortCut('Ctrl+X');
  ActCouper.Hint := 'Couper la sélection';
  ActCouper.Category := 'Édition';

  ActCopier.Caption := '&Copier';
  ActCopier.ShortCut := TextToShortCut('Ctrl+C');
  ActCopier.Hint := 'Copier la sélection';
  ActCopier.Category := 'Édition';

  ActColler.Caption := 'C&oller';
  ActColler.ShortCut := TextToShortCut('Ctrl+V');
  ActColler.Hint := 'Coller';
  ActColler.Category := 'Édition';

  ActSelectionnerTout.Caption := 'Sélectionner &tout';
  ActSelectionnerTout.ShortCut := TextToShortCut('Ctrl+A');
  ActSelectionnerTout.Hint := 'Sélectionner tout le texte';
  ActSelectionnerTout.Category := 'Édition';

  // Initialisation
  FFichierCourant := '';
  Memo1.Clear;
  MettreAJourTitre;
end;

// --- Actions Fichier ---

procedure TFormEditeur.ActNouveauExecute(Sender: TObject);
begin
  if Memo1.Modified then
  begin
    case MessageDlg('Sauvegarder les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ActSauvegarder.Execute;
      mrCancel: Exit;
    end;
  end;

  Memo1.Clear;
  FFichierCourant := '';
  Memo1.Modified := False;
  MettreAJourTitre;
end;

procedure TFormEditeur.ActOuvrirExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
      FFichierCourant := OpenDialog1.FileName;
      Memo1.Modified := False;
      MettreAJourTitre;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''ouverture : ' + E.Message);
    end;
  end;
end;

procedure TFormEditeur.ActSauvegarderExecute(Sender: TObject);
begin
  if FFichierCourant = '' then
  begin
    // Demander le nom du fichier
    if SaveDialog1.Execute then
      FFichierCourant := SaveDialog1.FileName
    else
      Exit;
  end;

  try
    Memo1.Lines.SaveToFile(FFichierCourant);
    Memo1.Modified := False;
    StatusBar1.SimpleText := 'Fichier sauvegardé : ' + ExtractFileName(FFichierCourant);
    MettreAJourTitre;
  except
    on E: Exception do
      ShowMessage('Erreur lors de la sauvegarde : ' + E.Message);
  end;
end;

procedure TFormEditeur.ActQuitterExecute(Sender: TObject);
begin
  Close;
end;

// --- Actions Édition ---

procedure TFormEditeur.ActCouperExecute(Sender: TObject);
begin
  Memo1.CutToClipboard;
end;

procedure TFormEditeur.ActCopierExecute(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TFormEditeur.ActCollerExecute(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;

procedure TFormEditeur.ActSelectionnerToutExecute(Sender: TObject);
begin
  Memo1.SelectAll;
end;

// --- Mise à jour automatique ---

procedure TFormEditeur.ActCouperUpdate(Sender: TObject);
begin
  ActCouper.Enabled := Memo1.SelLength > 0;
end;

procedure TFormEditeur.ActCopierUpdate(Sender: TObject);
begin
  ActCopier.Enabled := Memo1.SelLength > 0;
end;

procedure TFormEditeur.ActCollerUpdate(Sender: TObject);
begin
  ActColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TFormEditeur.ActSauvegarderUpdate(Sender: TObject);
begin
  ActSauvegarder.Enabled := Memo1.Modified;
end;

// --- Utilitaires ---

procedure TFormEditeur.MettreAJourTitre;
begin
  if FFichierCourant = '' then
    Caption := 'Éditeur - Sans titre'
  else
    Caption := 'Éditeur - ' + ExtractFileName(FFichierCourant);

  if Memo1.Modified then
    Caption := Caption + ' *';
end;
```

---

## Actions Standards

Lazarus fournit des **actions standards** prêtes à l'emploi pour les opérations courantes :

### TEditCopy, TEditCut, TEditPaste

Actions d'édition standard :

```pascal
uses
  StdActns;  // Unité des actions standard

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    EditCopy1: TEditCopy;      // Action standard "Copier"
    EditCut1: TEditCut;        // Action standard "Couper"
    EditPaste1: TEditPaste;    // Action standard "Coller"
    EditSelectAll1: TEditSelectAll; // Action standard "Sélectionner tout"
  end;

// Ces actions fonctionnent automatiquement avec les composants d'édition !
// Pas besoin d'écrire OnExecute
```

### TFileOpen, TFileSaveAs

Actions de fichiers :

```pascal
type
  TForm1 = class(TForm)
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
  end;

// Ouvrent automatiquement les dialogues appropriés
```

**Avantage** : Comportement standard et testé, gain de temps.
**Inconvénient** : Moins de contrôle, parfois trop générique.

---

## Menu Contextuel avec Actions

Créer un menu contextuel est très simple avec les actions :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  PopupMenu: TPopupMenu;
  MenuItem: TMenuItem;
begin
  // Créer le menu contextuel
  PopupMenu := TPopupMenu.Create(Self);

  // Ajouter des items liés aux actions
  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Action := ActCouper;
  PopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Action := ActCopier;
  PopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Action := ActColler;
  PopupMenu.Items.Add(MenuItem);

  // Séparateur
  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Caption := '-';
  PopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Action := ActSelectionnerTout;
  PopupMenu.Items.Add(MenuItem);

  // Associer au Memo
  Memo1.PopupMenu := PopupMenu;
end;
```

**Résultat** : Menu contextuel avec toutes les fonctionnalités, raccourcis et états des actions !

---

## Avantages des Actions

### 1. Centralisation du Code

✅ **Une seule implémentation** de chaque commande
✅ **Maintenance simplifiée** : modifier à un seul endroit
✅ **Moins de bugs** : pas de code dupliqué

### 2. Cohérence Automatique

✅ **Même libellé** partout (menu, bouton, tooltip)
✅ **Même raccourci** clavier
✅ **Même icône** si ImageList utilisée

### 3. Gestion Automatique de l'État

✅ **OnUpdate** synchronise automatiquement
✅ **Activation/désactivation** propagée à tous les contrôles
✅ **État coché** (Checked) géré uniformément

### 4. Facilité d'Extension

✅ **Ajouter un nouveau bouton** : une ligne de code (`Button.Action := ...`)
✅ **Créer un menu contextuel** : réutiliser les actions existantes
✅ **Ajouter un raccourci clavier** : défini dans l'action uniquement

### 5. Organisation

✅ **Catégories** pour grouper logiquement
✅ **Liste centralisée** de toutes les commandes
✅ **Plus facile à documenter** et à comprendre

---

## Quand Utiliser des Actions ?

### ✅ Utilisez des Actions quand :

- Une commande est accessible depuis **plusieurs endroits** (menu + bouton + raccourci)
- Vous devez gérer **l'état** d'une commande (enabled, checked)
- Vous voulez des **raccourcis clavier** propres
- Vous créez une **application professionnelle** avec de nombreuses commandes
- Vous voulez **centraliser** la logique métier

### ⚠️ Pas forcément nécessaire si :

- Vous avez une **interface très simple** (1-2 boutons)
- Les commandes sont **uniques** (un seul bouton, pas de menu)
- C'est un **prototype rapide** ou une démonstration

---

## Bonnes Pratiques

### 1. Nommage Cohérent

```pascal
// ✅ BON : Préfixe Act + verbe d'action
ActNouveau, ActOuvrir, ActSauvegarder, ActQuitter
ActCopier, ActColler, ActCouper
ActRechercher, ActRemplacer

// ❌ MAUVAIS : Noms vagues
Action1, Action2, Action3
BoutonCopier, MenuCopier  // Ce ne sont pas des actions !
```

### 2. Catégoriser

```pascal
// ✅ BON : Organiser par catégories logiques
ActNouveau.Category := 'Fichier';
ActCopier.Category := 'Édition';
ActZoomPlus.Category := 'Affichage';
```

### 3. Raccourcis Standards

```pascal
// ✅ BON : Utiliser les raccourcis conventionnels
ActCopier.ShortCut := TextToShortCut('Ctrl+C');
ActSauvegarder.ShortCut := TextToShortCut('Ctrl+S');

// ❌ MAUVAIS : Raccourcis non-conventionnels
ActCopier.ShortCut := TextToShortCut('Ctrl+K');  // Déroutant !
```

### 4. Toujours Implémenter OnUpdate

```pascal
// ✅ BON : Synchroniser l'état automatiquement
procedure TForm1.ActCopierUpdate(Sender: TObject);
begin
  ActCopier.Enabled := Memo1.SelLength > 0;
end;

// ❌ MAUVAIS : Gérer manuellement partout
procedure TForm1.Memo1SelectionChange(Sender: TObject);
begin
  ActCopier.Enabled := Memo1.SelLength > 0;
  MenuItemCopier.Enabled := Memo1.SelLength > 0;
  BtnCopier.Enabled := Memo1.SelLength > 0;
  // Oubli facile, code dupliqué
end;
```

### 5. Utiliser ImageList pour les Icônes

```pascal
// ✅ BON : ImageList centralisé
ActionList1.Images := ImageList1;
ActNouveau.ImageIndex := 0;

// Tous les contrôles liés auront automatiquement l'icône
```

### 6. Gérer les Erreurs

```pascal
// ✅ BON : Gestion d'erreur dans l'action
procedure TForm1.ActOuvrirExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

---

## Comparaison : Avec et Sans Actions

### Sans Actions

```pascal
// Menu
procedure TForm1.MenuCopierClick(Sender: TObject);
begin
  if Memo1.SelLength > 0 then
    Clipboard.AsText := Memo1.SelText;
end;

// Bouton
procedure TForm1.BtnCopierClick(Sender: TObject);
begin
  if Memo1.SelLength > 0 then
    Clipboard.AsText := Memo1.SelText;
end;

// Mise à jour manuelle
procedure TForm1.Memo1Change(Sender: TObject);
begin
  MenuCopier.Enabled := Memo1.SelLength > 0;
  BtnCopier.Enabled := Memo1.SelLength > 0;
end;
```

**Total** : 3 procédures, code dupliqué, gestion manuelle complexe.

### Avec Actions

```pascal
// Une seule procédure Execute
procedure TForm1.ActCopierExecute(Sender: TObject);
begin
  Clipboard.AsText := Memo1.SelText;
end;

// Une seule procédure Update
procedure TForm1.ActCopierUpdate(Sender: TObject);
begin
  ActCopier.Enabled := Memo1.SelLength > 0;
end;

// Liaison
MenuCopier.Action := ActCopier;
BtnCopier.Action := ActCopier;
```

**Total** : 2 procédures, pas de duplication, gestion automatique !

---

## Points Clés à Retenir

1. **TAction** : représente une commande de l'application
   - OnExecute : code de la commande
   - OnUpdate : mise à jour de l'état
   - Caption, ShortCut, Hint, ImageIndex

2. **TActionList** : conteneur d'actions
   - Organise toutes les actions
   - Peut avoir un ImageList pour les icônes

3. **Centralisation** : écrire le code une fois
   - Lier plusieurs contrôles à la même action
   - Automatique : Caption, ShortCut, Enabled, Checked

4. **OnUpdate** : synchronisation automatique
   - Appelé périodiquement par l'application
   - Met à jour Enabled, Checked, Visible

5. **Raccourcis clavier** : gérés par l'action
   - TextToShortCut('Ctrl+C')
   - Fonctionnent partout automatiquement

6. **Avantages** :
   - Moins de code
   - Meilleure maintenance
   - Cohérence garantie
   - Interface professionnelle

7. **Bonnes pratiques** :
   - Nommage avec préfixe Act
   - Catégoriser les actions
   - Raccourcis standards
   - Toujours implémenter OnUpdate

---

## Conclusion

Les **Actions** et **TActionList** sont des outils essentiels pour créer des applications professionnelles. Ils permettent de :

- **Centraliser** les commandes de votre application
- **Réduire** considérablement la duplication de code
- **Maintenir** facilement votre interface
- **Garantir** la cohérence entre menus, boutons et raccourcis
- **Gérer automatiquement** l'état des commandes

Une fois que vous aurez pris l'habitude d'utiliser les Actions, vous ne pourrez plus vous en passer ! Elles simplifient énormément le développement et rendent votre code plus propre et maintenable.

C'est la fin de la Partie II - Programmation Orientée Objet (Niveau Intermédiaire) et du chapitre 15 sur les Composants LCL Fondamentaux. Vous avez maintenant une base solide pour créer des interfaces graphiques riches et professionnelles avec Lazarus !

---

**Prochaine étape :** Partie III - Consolidation des Compétences Intermédiaires

⏭️ [Bases de Données - Maîtrise Approfondie](/16-bases-donnees-maitrise-approfondie/README.md)
