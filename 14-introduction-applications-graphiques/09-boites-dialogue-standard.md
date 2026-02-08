🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.9 Boîtes de dialogue standard

## Introduction

Les **boîtes de dialogue standard** sont des fenêtres prêtes à l'emploi fournies par le système d'exploitation (Windows, Linux, macOS). Elles offrent une interface familière à l'utilisateur pour effectuer des tâches courantes.

**Avantages :**
- **Apparence native** : S'intègrent parfaitement au système
- **Prêtes à l'emploi** : Pas besoin de créer vos propres formulaires
- **Familières** : L'utilisateur connaît déjà leur fonctionnement
- **Accessibilité** : Supportent les standards du système

Dans cette section, nous allons explorer :
- Les dialogues de fichiers (Ouvrir, Enregistrer)
- Les dialogues de sélection (Couleur, Police, Dossier)
- Les messages et confirmations
- Les dialogues de recherche

---

## Les dialogues de fichiers

### TOpenDialog : Ouvrir un fichier

Le **TOpenDialog** permet à l'utilisateur de sélectionner un fichier à ouvrir.

**Apparence typique :**
```
┌────────────────────────────────────────────┐
│ Ouvrir                               _ □ ✕ │
├────────────────────────────────────────────┤
│ Emplacement: ▼ Documents                   │
│ ┌────────────────────────────────────────┐ │
│ │ 📁 Images                              │ │
│ │ 📁 Musique                             │ │
│ │ 📄 document1.txt                       │ │
│ │ 📄 document2.txt                       │ │
│ │ 📄 rapport.pdf                         │ │
│ └────────────────────────────────────────┘ │
│                                            │
│ Nom du fichier: [__________________] [▼]   │
│ Type de fichier: Fichiers texte (*.txt) ▼  │
│                                            │
│              [Ouvrir]  [Annuler]           │
└────────────────────────────────────────────┘
```

#### Ajouter le composant

1. Palette → Onglet **"Dialogs"**
2. Cliquez sur **TOpenDialog**
3. Cliquez sur le formulaire

Le composant apparaît dans la zone des composants non visuels.

#### Utilisation de base

```pascal
procedure TForm1.ButtonOuvrirClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    // L'utilisateur a sélectionné un fichier
    ShowMessage('Fichier sélectionné : ' + OpenDialog1.FileName);
  end
  else
  begin
    // L'utilisateur a annulé
    ShowMessage('Opération annulée');
  end;
end;
```

**Méthode `Execute` :**
- Retourne `True` si l'utilisateur clique sur "Ouvrir"
- Retourne `False` si l'utilisateur clique sur "Annuler"

#### Propriétés importantes

##### FileName
```pascal
// Lire le fichier sélectionné
var
  Fichier: string;
begin
  if OpenDialog1.Execute then
  begin
    Fichier := OpenDialog1.FileName;
    // Fichier contient le chemin complet : "C:\Users\...\document.txt"
  end;
end;
```

**Type :** String
**Description :** Chemin complet du fichier sélectionné

##### InitialDir
```pascal
// Définir le dossier d'ouverture par défaut
OpenDialog1.InitialDir := 'C:\Users\Public\Documents';

// Ou utiliser le dossier Documents de l'utilisateur
OpenDialog1.InitialDir := GetUserDir + 'Documents';
```

**Type :** String
**Description :** Dossier affiché à l'ouverture du dialogue

##### Filter
```pascal
// Filtrer les types de fichiers
OpenDialog1.Filter := 'Fichiers texte (*.txt)|*.txt|Tous les fichiers (*.*)|*.*';
```

**Format :** `'Description1|Masque1|Description2|Masque2|...'`

**Exemples de filtres :**
```pascal
// Un seul type
OpenDialog1.Filter := 'Fichiers texte (*.txt)|*.txt';

// Plusieurs types
OpenDialog1.Filter := 'Images (*.png;*.jpg;*.bmp)|*.png;*.jpg;*.bmp|Tous les fichiers (*.*)|*.*';

// Filtres séparés
OpenDialog1.Filter :=
  'Fichiers texte (*.txt)|*.txt|' +
  'Fichiers Word (*.doc;*.docx)|*.doc;*.docx|' +
  'Fichiers PDF (*.pdf)|*.pdf|' +
  'Tous les fichiers (*.*)|*.*';

// Fichiers Pascal
OpenDialog1.Filter :=
  'Fichiers Pascal (*.pas;*.pp)|*.pas;*.pp|' +
  'Fichiers projet (*.lpr;*.dpr)|*.lpr;*.dpr|' +
  'Tous les fichiers (*.*)|*.*';
```

##### FilterIndex
```pascal
OpenDialog1.FilterIndex := 1;  // Premier filtre par défaut (1-based)
OpenDialog1.FilterIndex := 2;  // Deuxième filtre par défaut
```

**Type :** Integer
**Description :** Index du filtre sélectionné par défaut (commence à 1)

##### Title
```pascal
OpenDialog1.Title := 'Ouvrir un document';
OpenDialog1.Title := 'Sélectionner une image';
```

**Type :** String
**Description :** Titre de la fenêtre de dialogue

##### Options
```pascal
OpenDialog1.Options := [ofFileMustExist, ofEnableSizing];
```

**Type :** Set de TOpenOption
**Options courantes :**

| Option | Description |
|--------|-------------|
| `ofAllowMultiSelect` | Permettre la sélection multiple |
| `ofFileMustExist` | Le fichier doit exister |
| `ofPathMustExist` | Le chemin doit exister |
| `ofEnableSizing` | Dialogue redimensionnable |
| `ofReadOnly` | Afficher case "Lecture seule" |
| `ofHideReadOnly` | Cacher case "Lecture seule" |
| `ofNoChangeDir` | Ne pas changer le dossier courant |

**Exemple d'utilisation :**
```pascal
// Configuration complète
OpenDialog1.Options := [
  ofFileMustExist,      // Le fichier doit exister
  ofPathMustExist,      // Le chemin doit exister
  ofEnableSizing,       // Redimensionnable
  ofNoChangeDir         // Ne pas changer le dossier courant
];
```

#### Sélection multiple

```pascal
// Activer la sélection multiple
OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];

procedure TForm1.ButtonOuvrirClick(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialog1.Execute then
  begin
    // Nombre de fichiers sélectionnés
    ShowMessage('Fichiers sélectionnés : ' + IntToStr(OpenDialog1.Files.Count));

    // Parcourir tous les fichiers
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      Memo1.Lines.Add(OpenDialog1.Files[i]);
    end;
  end;
end;
```

**Propriété Files :** TStrings contenant la liste de tous les fichiers sélectionnés

#### Exemple complet

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du dialogue
  OpenDialog1.Title := 'Ouvrir un document';
  OpenDialog1.InitialDir := GetUserDir + 'Documents';
  OpenDialog1.Filter :=
    'Fichiers texte (*.txt)|*.txt|' +
    'Tous les fichiers (*.*)|*.*';
  OpenDialog1.FilterIndex := 1;
  OpenDialog1.Options := [ofFileMustExist, ofEnableSizing];
end;

procedure TForm1.MenuOuvrirClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      // Charger le fichier
      Memo1.Lines.LoadFromFile(OpenDialog1.FileName);

      // Mettre à jour l'interface
      Form1.Caption := 'Éditeur - ' + ExtractFileName(OpenDialog1.FileName);
      StatusBar1.SimpleText := 'Fichier ouvert : ' + OpenDialog1.FileName;

    except
      on E: Exception do
        ShowMessage('Erreur lors de l''ouverture : ' + E.Message);
    end;
  end;
end;
```

---

### TSaveDialog : Enregistrer un fichier

Le **TSaveDialog** permet à l'utilisateur de choisir où enregistrer un fichier.

**Très similaire à TOpenDialog**, mais avec quelques différences :

```pascal
procedure TForm1.MenuEnregistrerClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    try
      Memo1.Lines.SaveToFile(SaveDialog1.FileName);
      StatusBar1.SimpleText := 'Fichier enregistré : ' + SaveDialog1.FileName;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''enregistrement : ' + E.Message);
    end;
  end;
end;
```

#### Différences avec OpenDialog

**Options spécifiques à TSaveDialog :**
```pascal
SaveDialog1.Options := [
  ofOverwritePrompt,    // Confirmer avant d'écraser un fichier existant
  ofEnableSizing,
  ofNoChangeDir
];
```

| Option | Description |
|--------|-------------|
| `ofOverwritePrompt` | Demander confirmation si le fichier existe |
| `ofNoReadOnlyReturn` | Ne pas accepter fichiers en lecture seule |

#### Ajouter l'extension automatiquement

```pascal
// Ajouter .txt si aucune extension
SaveDialog1.DefaultExt := 'txt';

// Exemple
if SaveDialog1.Execute then
begin
  // Si l'utilisateur tape "document", ça devient "document.txt"
  Memo1.Lines.SaveToFile(SaveDialog1.FileName);
end;
```

#### Exemple complet

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du dialogue
  SaveDialog1.Title := 'Enregistrer le document';
  SaveDialog1.InitialDir := GetUserDir + 'Documents';
  SaveDialog1.Filter :=
    'Fichiers texte (*.txt)|*.txt|' +
    'Tous les fichiers (*.*)|*.*';
  SaveDialog1.FilterIndex := 1;
  SaveDialog1.DefaultExt := 'txt';
  SaveDialog1.Options := [
    ofOverwritePrompt,  // Confirmer écrasement
    ofEnableSizing
  ];
end;

procedure TForm1.MenuEnregistrerClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    try
      Memo1.Lines.SaveToFile(SaveDialog1.FileName);

      // Mettre à jour l'interface
      Form1.Caption := 'Éditeur - ' + ExtractFileName(SaveDialog1.FileName);
      StatusBar1.SimpleText := 'Enregistré : ' + SaveDialog1.FileName;
      Memo1.Modified := False;

    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

---

### TSelectDirectoryDialog : Sélectionner un dossier

Le **TSelectDirectoryDialog** permet de choisir un dossier (pas un fichier).

```pascal
procedure TForm1.ButtonChoisirDossierClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    ShowMessage('Dossier sélectionné : ' + SelectDirectoryDialog1.FileName);
    Edit1.Text := SelectDirectoryDialog1.FileName;
  end;
end;
```

#### Propriétés

```pascal
SelectDirectoryDialog1.Title := 'Sélectionner un dossier de destination';
SelectDirectoryDialog1.InitialDir := GetUserDir;
```

#### Exemple : Sélectionner un dossier de sauvegarde

```pascal
procedure TForm1.ButtonDestinationClick(Sender: TObject);
begin
  SelectDirectoryDialog1.Title := 'Choisir le dossier de sauvegarde';
  SelectDirectoryDialog1.InitialDir := ExtractFilePath(Application.ExeName);

  if SelectDirectoryDialog1.Execute then
  begin
    EditDestination.Text := SelectDirectoryDialog1.FileName;
    LabelInfo.Caption := 'Les fichiers seront sauvegardés dans : ' +
                         SelectDirectoryDialog1.FileName;
  end;
end;
```

---

## Les dialogues de sélection

### TColorDialog : Choisir une couleur

Le **TColorDialog** affiche une palette de couleurs.

**Apparence typique :**
```
┌──────────────────────────────────┐
│ Couleur                _ □ ✕     │
├──────────────────────────────────┤
│ Couleurs de base:                │
│ ┌─┬─┬─┬─┬─┬─┬─┬─┐                │
│ │■│■│■│■│■│■│■│■│                │
│ ├─┼─┼─┼─┼─┼─┼─┼─┤                │
│ │■│■│■│■│■│■│■│■│                │
│ └─┴─┴─┴─┴─┴─┴─┴─┘                │
│                                  │
│ Couleurs personnalisées:         │
│ ┌─┬─┬─┬─┬─┬─┬─┬─┐                │
│ │ │ │ │ │ │ │ │ │                │
│ └─┴─┴─┴─┴─┴─┴─┴─┘                │
│                                  │
│ [Définir couleurs personnalisées]│
│                                  │
│         [OK]  [Annuler]          │
└──────────────────────────────────┘
```

#### Utilisation simple

```pascal
procedure TForm1.ButtonCouleurClick(Sender: TObject);
begin
  // Définir la couleur actuelle
  ColorDialog1.Color := Panel1.Color;

  if ColorDialog1.Execute then
  begin
    // Appliquer la couleur sélectionnée
    Panel1.Color := ColorDialog1.Color;
  end;
end;
```

#### Propriétés

##### Color
```pascal
// Couleur actuelle (avant et après le dialogue)
ColorDialog1.Color := clRed;

if ColorDialog1.Execute then
  Button1.Color := ColorDialog1.Color;
```

##### CustomColors
```pascal
// Sauvegarder/restaurer les couleurs personnalisées
ColorDialog1.CustomColors.CommaText := IniFile.ReadString('Config', 'CustomColors', '');

// Après le dialogue, sauvegarder
if ColorDialog1.Execute then
begin
  IniFile.WriteString('Config', 'CustomColors', ColorDialog1.CustomColors.CommaText);
end;
```

#### Exemples pratiques

**Choisir la couleur de fond :**
```pascal
procedure TForm1.MenuCouleurFondClick(Sender: TObject);
begin
  ColorDialog1.Color := Memo1.Color;

  if ColorDialog1.Execute then
  begin
    Memo1.Color := ColorDialog1.Color;
    StatusBar1.SimpleText := 'Couleur de fond modifiée';
  end;
end;
```

**Choisir la couleur du texte :**
```pascal
procedure TForm1.MenuCouleurTexteClick(Sender: TObject);
begin
  ColorDialog1.Color := Memo1.Font.Color;

  if ColorDialog1.Execute then
  begin
    Memo1.Font.Color := ColorDialog1.Color;
  end;
end;
```

**Palette de couleurs rapide :**
```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  Btn: TButton;
  i: Integer;
  Couleurs: array[0..7] of TColor;
begin
  // Couleurs prédéfinies
  Couleurs[0] := clRed;
  Couleurs[1] := clBlue;
  Couleurs[2] := clGreen;
  Couleurs[3] := clYellow;
  Couleurs[4] := clBlack;
  Couleurs[5] := clWhite;
  Couleurs[6] := clGray;
  Couleurs[7] := clPurple;

  for i := 0 to 7 do
  begin
    Btn := TButton.Create(Self);
    Btn.Parent := PanelCouleurs;
    Btn.Width := 30;
    Btn.Height := 30;
    Btn.Left := i * 35 + 5;
    Btn.Top := 5;
    Btn.Color := Couleurs[i];
    Btn.Tag := Integer(Couleurs[i]);
    Btn.OnClick := @BoutonCouleurClick;
  end;
end;

procedure TForm1.BoutonCouleurClick(Sender: TObject);
begin
  Panel1.Color := TColor((Sender as TButton).Tag);
end;
```

---

### TFontDialog : Choisir une police

Le **TFontDialog** permet de sélectionner une police de caractères.

**Apparence typique :**
```
┌────────────────────────────────────┐
│ Police                      _ □ ✕ │
├────────────────────────────────────┤
│ Police:          Style:     Taille:│
│ ┌──────────┐   ┌──────┐   ┌────┐   │
│ │Arial     │   │Normal│   │10  │   │
│ │Courier   │   │Gras  │   │12  │   │
│ │Times     │   │Italique  │14  │   │
│ │Verdana   │   │      │   │18  │   │
│ └──────────┘   └──────┘   └────┘   │
│                                    │
│ Aperçu:                            │
│ ┌──────────────────────────────┐   │
│ │ AaBbYyZz                     │   │
│ └──────────────────────────────┘   │
│                                    │
│          [OK]  [Annuler]           │
└────────────────────────────────────┘
```

#### Utilisation simple

```pascal
procedure TForm1.ButtonPoliceClick(Sender: TObject);
begin
  // Définir la police actuelle
  FontDialog1.Font := Memo1.Font;

  if FontDialog1.Execute then
  begin
    // Appliquer la police sélectionnée
    Memo1.Font := FontDialog1.Font;
  end;
end;
```

#### Propriétés

##### Font
```pascal
// Accès à la police complète
FontDialog1.Font.Name := 'Arial';
FontDialog1.Font.Size := 12;
FontDialog1.Font.Color := clBlack;
FontDialog1.Font.Style := [fsBold];

if FontDialog1.Execute then
begin
  // Récupérer tous les attributs
  Label1.Font := FontDialog1.Font;
end;
```

##### Options
```pascal
FontDialog1.Options := [fdEffects, fdFixedPitchOnly];
```

**Options courantes :**

| Option | Description |
|--------|-------------|
| `fdEffects` | Afficher couleur et effets (souligné, barré) |
| `fdFixedPitchOnly` | Seulement polices à espacement fixe |
| `fdNoVectorFonts` | Exclure polices vectorielles |
| `fdNoSimulations` | Pas de simulation gras/italique |
| `fdAnsiOnly` | Seulement polices ANSI |
| `fdApplyButton` | Bouton "Appliquer" (avancé) |

#### Exemples pratiques

**Changer la police d'un memo :**
```pascal
procedure TForm1.MenuPoliceClick(Sender: TObject);
begin
  FontDialog1.Font := Memo1.Font;
  FontDialog1.Options := [fdEffects];  // Avec couleur

  if FontDialog1.Execute then
  begin
    Memo1.Font := FontDialog1.Font;
    StatusBar1.SimpleText := Format('Police : %s, Taille : %d',
                                     [Memo1.Font.Name, Memo1.Font.Size]);
  end;
end;
```

**Polices à espacement fixe uniquement (code) :**
```pascal
procedure TForm1.MenuPoliceCodeClick(Sender: TObject);
begin
  FontDialog1.Font := MemoCode.Font;
  FontDialog1.Options := [fdFixedPitchOnly];  // Courier, Consolas, etc.

  if FontDialog1.Execute then
    MemoCode.Font := FontDialog1.Font;
end;
```

**Sauvegarder les préférences :**
```pascal
// Sauvegarder
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IniFile.WriteString('Font', 'Name', Memo1.Font.Name);
  IniFile.WriteInteger('Font', 'Size', Memo1.Font.Size);
  IniFile.WriteInteger('Font', 'Color', Memo1.Font.Color);
  // etc.
end;

// Charger
procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Font.Name := IniFile.ReadString('Font', 'Name', 'Arial');
  Memo1.Font.Size := IniFile.ReadInteger('Font', 'Size', 10);
  Memo1.Font.Color := IniFile.ReadInteger('Font', 'Color', clBlack);
end;
```

---

## Les messages et confirmations

### ShowMessage : Message simple

La fonction **ShowMessage** affiche un message simple avec un bouton OK.

```pascal
ShowMessage('Fichier enregistré avec succès !');
ShowMessage('Erreur : Fichier introuvable');
```

**Apparence :**
```
┌─────────────────────────────┐
│ Information           _ □ ✕ │
├─────────────────────────────┤
│                             │
│  Fichier enregistré         │
│  avec succès !              │
│                             │
│          [OK]               │
└─────────────────────────────┘
```

**Usage :** Informations simples qui ne nécessitent pas de réponse

### MessageDlg : Boîte de dialogue personnalisée

La fonction **MessageDlg** offre plus de contrôle.

```pascal
function MessageDlg(const Msg: string;
                    DlgType: TMsgDlgType;
                    Buttons: TMsgDlgButtons;
                    HelpCtx: Longint): Integer;
```

#### Types de dialogues (DlgType)

| Type | Icône | Usage |
|------|-------|-------|
| `mtInformation` | ℹ️ | Information générale |
| `mtWarning` | ⚠️ | Avertissement |
| `mtError` | ❌ | Erreur |
| `mtConfirmation` | ❓ | Question, confirmation |
| `mtCustom` | Aucune | Personnalisé |

#### Boutons disponibles (Buttons)

| Bouton | Description | Retour |
|--------|-------------|--------|
| `mbYes` | Oui | `mrYes` |
| `mbNo` | Non | `mrNo` |
| `mbOK` | OK | `mrOK` |
| `mbCancel` | Annuler | `mrCancel` |
| `mbAbort` | Abandonner | `mrAbort` |
| `mbRetry` | Réessayer | `mrRetry` |
| `mbIgnore` | Ignorer | `mrIgnore` |
| `mbAll` | Tout | `mrAll` |
| `mbYesToAll` | Oui pour tout | `mrYesToAll` |
| `mbNoToAll` | Non pour tout | `mrNoToAll` |
| `mbHelp` | Aide | - |
| `mbClose` | Fermer | `mrClose` |

#### Exemples d'utilisation

**Confirmation simple :**
```pascal
procedure TForm1.MenuQuitterClick(Sender: TObject);
begin
  if MessageDlg('Voulez-vous vraiment quitter ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Close;
  end;
end;
```

**Confirmation avec 3 options :**
```pascal
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Reponse: Integer;
begin
  if Memo1.Modified then
  begin
    Reponse := MessageDlg('Le document a été modifié.' + sLineBreak +
                          'Voulez-vous enregistrer les modifications ?',
                          mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    case Reponse of
      mrYes:
        begin
          MenuEnregistrer.Click;  // Enregistrer
          CanClose := True;
        end;
      mrNo:
        CanClose := True;         // Fermer sans enregistrer
      mrCancel:
        CanClose := False;        // Annuler la fermeture
    end;
  end
  else
    CanClose := True;
end;
```

**Message d'erreur :**
```pascal
procedure TForm1.OuvrirFichier(const NomFichier: string);
begin
  if not FileExists(NomFichier) then
  begin
    MessageDlg('Erreur : Le fichier n''existe pas !' + sLineBreak +
               NomFichier,
               mtError, [mbOK], 0);
    Exit;
  end;

  try
    Memo1.Lines.LoadFromFile(NomFichier);
  except
    on E: Exception do
      MessageDlg('Erreur lors de l''ouverture :' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
  end;
end;
```

**Avertissement :**
```pascal
procedure TForm1.ButtonSupprimerClick(Sender: TObject);
begin
  if MessageDlg('Attention : Cette action est irréversible !' + sLineBreak +
                'Continuer ?',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    SupprimerDonnees;
  end;
end;
```

**Information :**
```pascal
procedure TForm1.MenuAProposClick(Sender: TObject);
begin
  MessageDlg('Mon Application v1.0' + sLineBreak +
             'Par Votre Nom' + sLineBreak +
             '© 2025',
             mtInformation, [mbOK], 0);
end;
```

**Opérations multiples (Oui pour tout / Non pour tout) :**
```pascal
procedure TForm1.TraiterFichiers;
var
  i: Integer;
  Reponse: Integer;
  AppliquerATous: Boolean;
begin
  AppliquerATous := False;
  Reponse := mrYes;

  for i := 0 to ListeFichiers.Count - 1 do
  begin
    if not AppliquerATous then
    begin
      Reponse := MessageDlg('Écraser le fichier ' + ListeFichiers[i] + ' ?',
                            mtConfirmation,
                            [mbYes, mbNo, mbYesToAll, mbNoToAll, mbCancel], 0);

      if Reponse in [mrYesToAll, mrNoToAll] then
        AppliquerATous := True;

      if Reponse = mrCancel then
        Break;
    end;

    if Reponse in [mrYes, mrYesToAll] then
      CopierFichier(ListeFichiers[i]);
  end;
end;
```

### InputBox et InputQuery : Saisie simple

**InputBox** demande une valeur à l'utilisateur.

```pascal
function InputBox(const ACaption, APrompt, ADefault: string): string;
```

**Exemple :**
```pascal
procedure TForm1.ButtonNomClick(Sender: TObject);
var
  Nom: string;
begin
  Nom := InputBox('Votre nom', 'Entrez votre nom :', 'Jean Dupont');

  if Nom <> '' then
    ShowMessage('Bonjour ' + Nom + ' !');
end;
```

**InputQuery** avec validation :
```pascal
function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean;
```

**Exemple :**
```pascal
procedure TForm1.ButtonAgeClick(Sender: TObject);
var
  AgeStr: string;
  Age: Integer;
begin
  AgeStr := '18';

  if InputQuery('Votre âge', 'Entrez votre âge :', AgeStr) then
  begin
    if TryStrToInt(AgeStr, Age) then
    begin
      if (Age >= 0) and (Age <= 120) then
        ShowMessage('Vous avez ' + IntToStr(Age) + ' ans')
      else
        ShowMessage('Âge invalide');
    end
    else
      ShowMessage('Veuillez entrer un nombre');
  end;
end;
```

**Exemple : Renommer un élément**
```pascal
procedure TForm1.MenuRenommerClick(Sender: TObject);
var
  NouveauNom: string;
begin
  if ListBox1.ItemIndex = -1 then
  begin
    ShowMessage('Sélectionnez d''abord un élément');
    Exit;
  end;

  NouveauNom := ListBox1.Items[ListBox1.ItemIndex];

  if InputQuery('Renommer', 'Nouveau nom :', NouveauNom) then
  begin
    if NouveauNom <> '' then
    begin
      ListBox1.Items[ListBox1.ItemIndex] := NouveauNom;
      StatusBar1.SimpleText := 'Élément renommé';
    end;
  end;
end;
```

---

## Les dialogues de recherche

### TFindDialog : Rechercher

Le **TFindDialog** offre une interface de recherche standard.

```
┌──────────────────────────────┐
│ Rechercher             _ □ ✕ │
├──────────────────────────────┤
│ Rechercher: [___________]    │
│                              │
│ ☐ Mot entier                 │
│ ☐ Respecter la casse         │
│ ○ Vers le haut               │
│ ⦿ Vers le bas               │
│                              │
│   [Suivant]  [Annuler]       │
└──────────────────────────────┘
```

#### Utilisation

```pascal
type
  TForm1 = class(TForm)
    Memo1: TMemo;
    FindDialog1: TFindDialog;
    procedure MenuRechercherClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
  end;

procedure TForm1.MenuRechercherClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TForm1.FindDialog1Find(Sender: TObject);
var
  FoundPos, StartPos: Integer;
  SearchText: string;
begin
  SearchText := FindDialog1.FindText;

  // Position de départ selon la direction
  if frDown in FindDialog1.Options then
    StartPos := Memo1.SelStart + Memo1.SelLength
  else
    StartPos := Memo1.SelStart - 1;

  // Effectuer la recherche avec PosEx (uses StrUtils)
  FoundPos := PosEx(SearchText, Memo1.Text, StartPos + 1);

  if FoundPos > 0 then
  begin
    // Texte trouvé (PosEx est 1-based, SelStart est 0-based)
    Memo1.SelStart := FoundPos - 1;
    Memo1.SelLength := Length(SearchText);
    Memo1.SetFocus;
  end
  else
  begin
    // Texte non trouvé
    ShowMessage('Texte introuvable : "' + SearchText + '"');
  end;
end;
```

#### Options de recherche

```pascal
FindDialog1.Options := [frDown, frMatchCase];
```

| Option | Description |
|--------|-------------|
| `frDown` | Rechercher vers le bas |
| `frFindNext` | Rechercher le suivant |
| `frWholeWord` | Mot entier uniquement |
| `frMatchCase` | Respecter la casse |

### TReplaceDialog : Rechercher et remplacer

Le **TReplaceDialog** étend TFindDialog avec le remplacement.

```pascal
type
  TForm1 = class(TForm)
    Memo1: TMemo;
    ReplaceDialog1: TReplaceDialog;
    procedure MenuRemplacerClick(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
  end;

procedure TForm1.MenuRemplacerClick(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;

procedure TForm1.ReplaceDialog1Replace(Sender: TObject);
var
  Pos: Integer;
  TexteComplet: string;
begin
  TexteComplet := Memo1.Text;

  if frReplaceAll in ReplaceDialog1.Options then
  begin
    // Remplacer tout
    TexteComplet := StringReplace(TexteComplet,
                                  ReplaceDialog1.FindText,
                                  ReplaceDialog1.ReplaceText,
                                  [rfReplaceAll, rfIgnoreCase]);
    Memo1.Text := TexteComplet;
    ShowMessage('Tous les remplacements effectués');
  end
  else
  begin
    // Remplacer l'occurrence actuelle
    if Memo1.SelLength > 0 then
    begin
      Memo1.SelText := ReplaceDialog1.ReplaceText;

      // Chercher la suivante
      FindDialog1Find(Sender);
    end;
  end;
end;
```

---

## Bonnes pratiques

### 1. Toujours vérifier le résultat de Execute

❌ **Mauvais :**
```pascal
OpenDialog1.Execute;
Memo1.Lines.LoadFromFile(OpenDialog1.FileName);  // Et si annulé ?
```

✅ **Bon :**
```pascal
if OpenDialog1.Execute then
  Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
```

### 2. Gérer les exceptions

```pascal
if OpenDialog1.Execute then
begin
  try
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
  except
    on E: Exception do
      MessageDlg('Erreur : ' + E.Message, mtError, [mbOK], 0);
  end;
end;
```

### 3. Configurer InitialDir intelligemment

```pascal
// Se souvenir du dernier dossier
var
  DernierDossier: string = '';

procedure TForm1.MenuOuvrirClick(Sender: TObject);
begin
  if DernierDossier <> '' then
    OpenDialog1.InitialDir := DernierDossier
  else
    OpenDialog1.InitialDir := GetUserDir + 'Documents';

  if OpenDialog1.Execute then
  begin
    DernierDossier := ExtractFilePath(OpenDialog1.FileName);
    // ...
  end;
end;
```

### 4. Filtres clairs et complets

✅ **Bon :**
```pascal
OpenDialog1.Filter :=
  'Images (*.png;*.jpg;*.bmp;*.gif)|*.png;*.jpg;*.bmp;*.gif|' +
  'PNG (*.png)|*.png|' +
  'JPEG (*.jpg;*.jpeg)|*.jpg;*.jpeg|' +
  'Tous les fichiers (*.*)|*.*';
```

❌ **Mauvais :**
```pascal
OpenDialog1.Filter := '*.png';  // Format incorrect
```

### 5. Messages clairs et utiles

❌ **Mauvais :**
```pascal
MessageDlg('Erreur', mtError, [mbOK], 0);
```

✅ **Bon :**
```pascal
MessageDlg('Impossible d''ouvrir le fichier :' + sLineBreak +
           OpenDialog1.FileName + sLineBreak + sLineBreak +
           'Vérifiez que le fichier existe et que vous avez les permissions.',
           mtError, [mbOK], 0);
```

### 6. Confirmer les actions destructives

```pascal
procedure TForm1.MenuEffacerToutClick(Sender: TObject);
begin
  if MessageDlg('Êtes-vous sûr de vouloir tout effacer ?' + sLineBreak +
                'Cette action est irréversible.',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    Memo1.Clear;
  end;
end;
```

### 7. Sauvegarder les préférences

```pascal
// Sauvegarder le dernier dossier utilisé
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IniFile.WriteString('Paths', 'LastOpenDir', OpenDialog1.InitialDir);
  IniFile.WriteString('Paths', 'LastSaveDir', SaveDialog1.InitialDir);
end;

// Restaurer au démarrage
procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenDialog1.InitialDir := IniFile.ReadString('Paths', 'LastOpenDir', GetUserDir);
  SaveDialog1.InitialDir := IniFile.ReadString('Paths', 'LastSaveDir', GetUserDir);
end;
```

### 8. Options appropriées

```pascal
// Pour OpenDialog : le fichier doit exister
OpenDialog1.Options := [ofFileMustExist, ofPathMustExist];

// Pour SaveDialog : confirmer écrasement
SaveDialog1.Options := [ofOverwritePrompt];
```

---

## Exemple complet : Éditeur de texte

```pascal
type
  TFormEditeur = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;

    procedure FormCreate(Sender: TObject);
    procedure MenuNouveauClick(Sender: TObject);
    procedure MenuOuvrirClick(Sender: TObject);
    procedure MenuEnregistrerClick(Sender: TObject);
    procedure MenuEnregistrerSousClick(Sender: TObject);
    procedure MenuQuitterClick(Sender: TObject);
    procedure MenuCouleurFondClick(Sender: TObject);
    procedure MenuPoliceClick(Sender: TObject);
    procedure MenuRechercherClick(Sender: TObject);
    procedure MenuRemplacerClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FFichierCourant: string;
    procedure NouveauDocument;
    procedure OuvrirDocument(const Fichier: string);
    procedure EnregistrerDocument(const Fichier: string);
  end;

var
  Form1: TFormEditeur;

implementation

procedure TFormEditeur.FormCreate(Sender: TObject);
begin
  // Configuration des dialogues
  OpenDialog1.Filter := 'Fichiers texte (*.txt)|*.txt|Tous les fichiers (*.*)|*.*';
  OpenDialog1.Options := [ofFileMustExist, ofEnableSizing];

  SaveDialog1.Filter := OpenDialog1.Filter;
  SaveDialog1.Options := [ofOverwritePrompt, ofEnableSizing];
  SaveDialog1.DefaultExt := 'txt';

  FontDialog1.Options := [fdEffects];

  // État initial
  NouveauDocument;
end;

procedure TFormEditeur.NouveauDocument;
begin
  Memo1.Clear;
  Memo1.Modified := False;
  FFichierCourant := '';
  Caption := 'Éditeur - Sans titre';
end;

procedure TFormEditeur.MenuNouveauClick(Sender: TObject);
begin
  if Memo1.Modified then
  begin
    case MessageDlg('Enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: MenuEnregistrer.Click;
      mrCancel: Exit;
    end;
  end;

  NouveauDocument;
end;

procedure TFormEditeur.MenuOuvrirClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    OuvrirDocument(OpenDialog1.FileName);
  end;
end;

procedure TFormEditeur.OuvrirDocument(const Fichier: string);
begin
  try
    Memo1.Lines.LoadFromFile(Fichier);
    Memo1.Modified := False;
    FFichierCourant := Fichier;
    Caption := 'Éditeur - ' + ExtractFileName(Fichier);
  except
    on E: Exception do
      MessageDlg('Impossible d''ouvrir le fichier :' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
  end;
end;

procedure TFormEditeur.MenuEnregistrerClick(Sender: TObject);
begin
  if FFichierCourant = '' then
    MenuEnregistrerSous.Click
  else
    EnregistrerDocument(FFichierCourant);
end;

procedure TFormEditeur.MenuEnregistrerSousClick(Sender: TObject);
begin
  if FFichierCourant <> '' then
    SaveDialog1.FileName := FFichierCourant;

  if SaveDialog1.Execute then
  begin
    EnregistrerDocument(SaveDialog1.FileName);
  end;
end;

procedure TFormEditeur.EnregistrerDocument(const Fichier: string);
begin
  try
    Memo1.Lines.SaveToFile(Fichier);
    Memo1.Modified := False;
    FFichierCourant := Fichier;
    Caption := 'Éditeur - ' + ExtractFileName(Fichier);
  except
    on E: Exception do
      MessageDlg('Impossible d''enregistrer :' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
  end;
end;

procedure TFormEditeur.MenuQuitterClick(Sender: TObject);
begin
  Close;
end;

procedure TFormEditeur.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Memo1.Modified then
  begin
    case MessageDlg('Enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          MenuEnregistrer.Click;
          CanClose := not Memo1.Modified;
        end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end
  else
    CanClose := True;
end;

procedure TFormEditeur.MenuCouleurFondClick(Sender: TObject);
begin
  ColorDialog1.Color := Memo1.Color;

  if ColorDialog1.Execute then
    Memo1.Color := ColorDialog1.Color;
end;

procedure TFormEditeur.MenuPoliceClick(Sender: TObject);
begin
  FontDialog1.Font := Memo1.Font;

  if FontDialog1.Execute then
    Memo1.Font := FontDialog1.Font;
end;

procedure TFormEditeur.MenuRechercherClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TFormEditeur.MenuRemplacerClick(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;

procedure TFormEditeur.FindDialog1Find(Sender: TObject);
var
  Pos, StartPos: Integer;
  SearchText: string;
begin
  SearchText := FindDialog1.FindText;

  if frDown in FindDialog1.Options then
    StartPos := Memo1.SelStart + Memo1.SelLength
  else
    StartPos := Memo1.SelStart - 1;

  Pos := PosEx(SearchText, Memo1.Text, StartPos + 1);  // uses StrUtils

  if Pos > 0 then
  begin
    Memo1.SelStart := Pos - 1;
    Memo1.SelLength := Length(SearchText);
    Memo1.SetFocus;
  end
  else
    MessageDlg('Texte introuvable : "' + SearchText + '"',
               mtInformation, [mbOK], 0);
end;
```

---

## Résumé

### Dialogues de fichiers

| Dialogue | Usage | Méthode |
|----------|-------|---------|
| **TOpenDialog** | Ouvrir un fichier | `Execute` → `FileName` |
| **TSaveDialog** | Enregistrer un fichier | `Execute` → `FileName` |
| **TSelectDirectoryDialog** | Choisir un dossier | `Execute` → `FileName` |

### Dialogues de sélection

| Dialogue | Usage | Propriété |
|----------|-------|-----------|
| **TColorDialog** | Choisir une couleur | `Color` |
| **TFontDialog** | Choisir une police | `Font` |

### Messages

| Fonction | Usage |
|----------|-------|
| `ShowMessage` | Message simple |
| `MessageDlg` | Message avec choix multiples |
| `InputBox` | Demander une valeur |
| `InputQuery` | Demander avec validation |

### Dialogues de recherche

| Dialogue | Usage |
|----------|-------|
| **TFindDialog** | Rechercher |
| **TReplaceDialog** | Rechercher et remplacer |

### Checklist

✅ Toujours vérifier `Execute` avant d'utiliser le résultat
✅ Gérer les exceptions (try-except)
✅ Filtres clairs et complets
✅ Messages informatifs
✅ Confirmer les actions destructives
✅ Options appropriées (ofFileMustExist, ofOverwritePrompt, etc.)
✅ Sauvegarder les préférences (InitialDir, couleurs, polices)

---

## Prochaines étapes

Vous maîtrisez maintenant tous les dialogues standard ! Avec ces outils, vous pouvez créer des applications complètes et professionnelles.

Dans les prochaines sections :
- **15. Composants LCL Fondamentaux** : Découvrir d'autres composants
- **16. Bases de données** : Connecter à des bases de données

Félicitations ! Vous avez maintenant toutes les bases pour créer des interfaces utilisateur riches et intuitives ! 🎉

---

**Point clé à retenir :** Les boîtes de dialogue standard offrent une expérience utilisateur cohérente et professionnelle. Utilisez-les systématiquement au lieu de créer vos propres formulaires !

⏭️ [Composants LCL Fondamentaux](/15-composants-lcl-fondamentaux/README.md)
