🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.4 Composants de base (TButton, TEdit, TLabel)

## Introduction

Les composants sont les **briques de construction** de votre interface graphique. Ce sont les éléments avec lesquels l'utilisateur interagit : boutons, zones de texte, étiquettes, etc.

Dans cette section, nous allons découvrir les trois composants les plus fondamentaux :
- **TLabel** : pour afficher du texte
- **TEdit** : pour saisir du texte
- **TButton** : pour déclencher des actions

Ces trois composants suffisent à créer des applications simples mais fonctionnelles !

---

## Comment ajouter un composant ?

### La palette de composants

En haut de l'IDE Lazarus, vous voyez plusieurs onglets :

```
┌─────────────────────────────────────────────────┐
│ Standard | Additional | Common | Dialogs | ...  │
├─────────────────────────────────────────────────┤
│ [📋] [📝] [🔘] [☑] [📻] [📋] [📊] [📁] ...     │
└─────────────────────────────────────────────────┘
```

L'onglet **"Standard"** contient les composants de base.

### Ajouter un composant en 3 étapes

**1. Cliquez sur le composant dans la palette**
   - Exemple : cliquez sur l'icône du bouton 🔘

**2. Cliquez sur le formulaire**
   - Où vous voulez placer le composant

**3. Le composant apparaît !**
   - Il est automatiquement sélectionné
   - Vous pouvez le déplacer et le redimensionner

### Manipuler un composant

**Sélectionner :** Cliquez dessus  
**Déplacer :** Cliquez et glissez  
**Redimensionner :** Utilisez les poignées (petits carrés noirs) aux coins et sur les bords  
**Supprimer :** Sélectionnez puis appuyez sur `Suppr` ou `Delete`  

---

## TLabel : Afficher du texte

### Présentation

`TLabel` est un composant qui affiche du texte. Il n'est **pas interactif** (l'utilisateur ne peut pas le modifier directement).

**Utilisations courantes :**
- Titres et sous-titres
- Descriptions de champs de saisie
- Messages d'information
- Résultats de calculs

### Ajouter un TLabel

1. Onglet **"Standard"** de la palette
2. Cliquez sur l'icône **TLabel** (représentée par "Label")
3. Cliquez sur le formulaire

Un label apparaît avec le texte "Label1".

### Propriétés essentielles

#### Caption
```pascal
Label1.Caption := 'Bonjour le monde !';
```
**Description :** Le texte affiché  
**Type :** String  
**Modification :** Inspecteur d'Objets ou par code  

**Astuce :** Utilisez `#13#10` ou `sLineBreak` pour créer des sauts de ligne :  
```pascal
Label1.Caption := 'Ligne 1' + sLineBreak + 'Ligne 2';
```

#### Font
```pascal
Label1.Font.Name := 'Arial';
Label1.Font.Size := 12;
Label1.Font.Style := [fsBold];
Label1.Font.Color := clBlue;
```
**Description :** Police de caractères du texte

**Styles disponibles :**
| Style | Description |
|-------|-------------|
| `fsBold` | Gras |
| `fsItalic` | Italique |
| `fsUnderline` | Souligné |
| `fsStrikeOut` | Barré |

**Exemple de combinaison :**
```pascal
Label1.Font.Style := [fsBold, fsItalic];  // Gras ET italique
```

#### Color
```pascal
Label1.Color := clYellow;      // Couleur de fond jaune
Label1.Transparent := False;   // Doit être False pour voir la couleur
```
**Description :** Couleur de fond du label  
**Attention :** Par défaut, `Transparent := True` (le fond est transparent)  

#### Transparent
```pascal
Label1.Transparent := True;   // Fond transparent (par défaut)
Label1.Transparent := False;  // Fond opaque avec la couleur définie
```

#### Alignment
```pascal
Label1.Alignment := taLeftJustify;   // Gauche (défaut)
Label1.Alignment := taCenter;        // Centre
Label1.Alignment := taRightJustify;  // Droite
```
**Description :** Alignement horizontal du texte  

#### Layout
```pascal
Label1.Layout := tlTop;     // Haut (défaut)
Label1.Layout := tlCenter;  // Centre vertical
Label1.Layout := tlBottom;  // Bas
```
**Description :** Alignement vertical du texte  

#### WordWrap
```pascal
Label1.WordWrap := True;   // Le texte revient à la ligne automatiquement
Label1.WordWrap := False;  // Le texte reste sur une ligne
```
**Description :** Activer le retour à la ligne automatique  

#### AutoSize
```pascal
Label1.AutoSize := True;   // Taille ajustée au contenu (défaut)
Label1.AutoSize := False;  // Taille fixe
```
**Description :** Ajuster automatiquement la taille au texte  

**Important :** Si `AutoSize = True`, le label s'agrandira automatiquement. Si vous voulez un label de taille fixe avec retour à la ligne, mettez :  
```pascal
Label1.AutoSize := False;
Label1.WordWrap := True;
```

#### Visible
```pascal
Label1.Visible := True;   // Visible
Label1.Visible := False;  // Caché
```

#### Enabled
```pascal
Label1.Enabled := True;   // Normal
Label1.Enabled := False;  // Grisé
```

### Événements courants

#### OnClick
```pascal
procedure TForm1.Label1Click(Sender: TObject);
begin
  ShowMessage('Vous avez cliqué sur le label !');
end;
```

**Usage :** Bien que rare, on peut utiliser un label cliquable comme un "lien" visuel.  

#### OnDblClick
```pascal
procedure TForm1.Label1DblClick(Sender: TObject);
begin
  Label1.Font.Size := Label1.Font.Size + 2;  // Augmente la taille
end;
```

### Exemples pratiques

**Titre d'application :**
```pascal
LabelTitre.Caption := 'Gestionnaire de Clients';
LabelTitre.Font.Size := 18;
LabelTitre.Font.Style := [fsBold];
LabelTitre.Font.Color := clNavy;
LabelTitre.Alignment := taCenter;
```

**Label d'information :**
```pascal
LabelInfo.Caption := 'Remplissez tous les champs obligatoires';
LabelInfo.Font.Color := clGray;
LabelInfo.Font.Style := [fsItalic];
```

**Label de résultat :**
```pascal
LabelResultat.Caption := 'Total : 150,00 €';
LabelResultat.Font.Size := 14;
LabelResultat.Font.Style := [fsBold];
LabelResultat.Font.Color := clGreen;
```

**Label qui accompagne un champ de saisie :**
```pascal
LabelNom.Caption := 'Nom :';
LabelNom.Left := 10;
LabelNom.Top := 50;

EditNom.Left := 80;  // Aligné avec le label
EditNom.Top := 47;   // Légèrement ajusté pour être aligné verticalement
```

---

## TEdit : Saisir du texte

### Présentation

`TEdit` est un champ de saisie de texte sur **une seule ligne**. C'est l'un des composants les plus utilisés dans les formulaires.

**Utilisations courantes :**
- Nom, prénom, adresse
- Numéros (téléphone, code postal)
- Mots de passe
- Recherche
- Valeurs numériques

### Ajouter un TEdit

1. Onglet **"Standard"** de la palette
2. Cliquez sur l'icône **TEdit** (représentée par une zone de texte)
3. Cliquez sur le formulaire

Un champ de saisie apparaît avec le texte "Edit1".

### Propriétés essentielles

#### Text
```pascal
// Définir le texte
Edit1.Text := 'Contenu initial';

// Lire le texte
var
  Contenu: string;
begin
  Contenu := Edit1.Text;
  ShowMessage('Vous avez saisi : ' + Contenu);
end;
```
**Description :** Le contenu du champ  
**Type :** String  
**Note :** C'est la propriété la plus importante du TEdit !  

#### MaxLength
```pascal
Edit1.MaxLength := 50;  // Limite à 50 caractères
Edit1.MaxLength := 0;   // Pas de limite (défaut)
```
**Description :** Nombre maximum de caractères autorisés

**Exemple pour un code postal :**
```pascal
EditCodePostal.MaxLength := 5;
```

#### PasswordChar
```pascal
Edit1.PasswordChar := '*';   // Affiche des astérisques
Edit1.PasswordChar := '•';   // Affiche des points
Edit1.PasswordChar := #0;    // Texte normal (défaut)
```
**Description :** Caractère de masquage pour les mots de passe

**Exemple :**
```pascal
EditMotDePasse.PasswordChar := '*';
EditMotDePasse.MaxLength := 20;
```

#### ReadOnly
```pascal
Edit1.ReadOnly := True;   // Lecture seule (non modifiable)
Edit1.ReadOnly := False;  // Modifiable (défaut)
```
**Description :** Empêche la modification du contenu

**Différence avec Enabled :**
- `ReadOnly = True` : Le texte reste noir, on peut le sélectionner et le copier
- `Enabled = False` : Le texte est grisé, on ne peut rien faire

#### Alignment
```pascal
Edit1.Alignment := taLeftJustify;   // Gauche (défaut)
Edit1.Alignment := taCenter;        // Centre
Edit1.Alignment := taRightJustify;  // Droite
```

**Usage :** `taRightJustify` est utile pour les montants :
```pascal
EditMontant.Alignment := taRightJustify;
EditMontant.Text := '150,00';
```

#### CharCase
```pascal
Edit1.CharCase := ecNormal;      // Mixte (défaut)
Edit1.CharCase := ecUpperCase;   // TOUT EN MAJUSCULES
Edit1.CharCase := ecLowerCase;   // tout en minuscules
```
**Description :** Conversion automatique de la casse

**Exemple :**
```pascal
EditCodeClient.CharCase := ecUpperCase;  // Les codes clients sont toujours en majuscules
```

#### NumbersOnly
```pascal
Edit1.NumbersOnly := True;   // Accepte uniquement les chiffres
Edit1.NumbersOnly := False;  // Accepte tous les caractères (défaut)
```
**Description :** Restreindre la saisie aux chiffres

**Note :** Disponible principalement sous Windows. Sous Linux, vous devrez valider dans l'événement OnKeyPress.

#### Color
```pascal
Edit1.Color := clWhite;    // Blanc (défaut)
Edit1.Color := clYellow;   // Jaune
Edit1.Color := clMoneyGreen;  // Vert pâle
```

**Usage :** Mettre en évidence un champ :
```pascal
// Champ obligatoire non rempli
if Edit1.Text = '' then
  Edit1.Color := clRed
else
  Edit1.Color := clWhite;
```

#### Font
```pascal
Edit1.Font.Name := 'Courier New';  // Police à espacement fixe
Edit1.Font.Size := 10;
Edit1.Font.Style := [fsBold];
```

### Propriétés de sélection

#### SelStart
```pascal
Edit1.SelStart := 0;  // Position du curseur (0 = début)
```
**Description :** Position de départ de la sélection

#### SelLength
```pascal
Edit1.SelLength := 5;  // Nombre de caractères sélectionnés
```
**Description :** Longueur de la sélection

#### SelText
```pascal
// Obtenir le texte sélectionné
var
  TexteSelectionne: string;
begin
  TexteSelectionne := Edit1.SelText;
end;

// Remplacer le texte sélectionné
Edit1.SelText := 'Nouveau texte';
```

**Exemple : Sélectionner tout le texte**
```pascal
procedure TForm1.Edit1Enter(Sender: TObject);
begin
  // Sélectionne tout quand on entre dans le champ
  Edit1.SelectAll;
  // Ou :
  // Edit1.SelStart := 0;
  // Edit1.SelLength := Length(Edit1.Text);
end;
```

### Méthodes utiles

#### Clear
```pascal
Edit1.Clear;  // Équivalent à Edit1.Text := '';
```

#### SelectAll
```pascal
Edit1.SelectAll;  // Sélectionne tout le texte
```

#### CopyToClipboard, CutToClipboard, PasteFromClipboard
```pascal
Edit1.CopyToClipboard;   // Copier
Edit1.CutToClipboard;    // Couper
Edit1.PasteFromClipboard;  // Coller
```

#### SetFocus
```pascal
Edit1.SetFocus;  // Place le curseur dans ce champ
```

**Usage :** Mettre le focus sur un champ au démarrage :
```pascal
procedure TForm1.FormShow(Sender: TObject);
begin
  EditNom.SetFocus;
end;
```

### Événements courants

#### OnChange
```pascal
procedure TForm1.Edit1Change(Sender: TObject);
begin
  // Déclenché à CHAQUE modification du texte
  Label1.Caption := 'Vous avez saisi : ' + Edit1.Text;

  // Compter les caractères
  LabelNbCaracteres.Caption := IntToStr(Length(Edit1.Text)) + ' caractères';
end;
```
**Quand :** Chaque fois que le contenu du champ change  
**Usage :** Validation en temps réel, compteurs de caractères  

#### OnKeyPress
```pascal
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  // Accepter uniquement les chiffres
  if not (Key in ['0'..'9', #8, #13]) then  // #8=Backspace, #13=Enter
  begin
    Key := #0;  // Annule la touche
    Beep;       // Signal sonore
  end;

  // Enter = passer au champ suivant
  if Key = #13 then
  begin
    Key := #0;
    Edit2.SetFocus;
  end;
end;
```
**Quand :** Quand une touche caractère est pressée  
**Usage :** Validation de saisie, navigation au clavier  

#### OnKeyDown
```pascal
procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Ctrl+A pour tout sélectionner
  if (ssCtrl in Shift) and (Key = Ord('A')) then
  begin
    Edit1.SelectAll;
    Key := 0;
  end;
end;
```
**Quand :** Quand une touche est enfoncée (y compris touches spéciales)  
**Usage :** Raccourcis clavier complexes  

#### OnEnter / OnExit
```pascal
procedure TForm1.Edit1Enter(Sender: TObject);
begin
  // Quand on entre dans le champ
  Edit1.Color := clYellow;  // Surligner le champ actif
  Edit1.SelectAll;          // Sélectionner tout
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
  // Quand on sort du champ
  Edit1.Color := clWhite;   // Retour à la normale

  // Validation
  if Edit1.Text = '' then
  begin
    ShowMessage('Ce champ est obligatoire !');
    Edit1.SetFocus;  // Retour au champ
  end;
end;
```
**Quand :** Quand le champ reçoit/perd le focus  
**Usage :** Mise en évidence, validation  

### Exemples pratiques

**Champ de saisie de nom :**
```pascal
EditNom.Text := '';
EditNom.MaxLength := 50;
EditNom.CharCase := ecNormal;
EditNom.TabOrder := 0;  // Premier champ dans l'ordre de tabulation
```

**Champ de saisie de code postal (France) :**
```pascal
EditCodePostal.Text := '';
EditCodePostal.MaxLength := 5;
EditCodePostal.NumbersOnly := True;  // Sous Windows
```

**Champ de mot de passe :**
```pascal
EditMotDePasse.PasswordChar := '*';
EditMotDePasse.MaxLength := 50;
```

**Champ de montant :**
```pascal
EditMontant.Alignment := taRightJustify;
EditMontant.Text := '0,00';
```

**Validation simple de nombre :**
```pascal
procedure TForm1.EditAgeKeyPress(Sender: TObject; var Key: Char);
begin
  // Accepter uniquement chiffres, backspace, enter
  if not (Key in ['0'..'9', #8, #13]) then
    Key := #0;
end;

procedure TForm1.ButtonValiderClick(Sender: TObject);
var
  Age: Integer;
begin
  if TryStrToInt(EditAge.Text, Age) then
  begin
    if (Age >= 0) and (Age <= 120) then
      ShowMessage('Âge valide : ' + IntToStr(Age))
    else
      ShowMessage('Âge invalide !');
  end
  else
    ShowMessage('Veuillez entrer un nombre !');
end;
```

---

## TButton : Déclencher des actions

### Présentation

`TButton` est un bouton cliquable qui déclenche une action. C'est **LE** composant interactif par excellence.

**Utilisations courantes :**
- Valider un formulaire
- Annuler une opération
- Calculer un résultat
- Ouvrir une autre fenêtre
- Lancer un processus

### Ajouter un TButton

1. Onglet **"Standard"** de la palette
2. Cliquez sur l'icône **TButton** (représentée par un bouton)
3. Cliquez sur le formulaire

Un bouton apparaît avec le texte "Button1".

### Propriétés essentielles

#### Caption
```pascal
Button1.Caption := 'Valider';
Button1.Caption := 'Annuler';
Button1.Caption := 'OK';
Button1.Caption := 'Calculer';
```
**Description :** Le texte affiché sur le bouton
**Type :** String

**Astuce :** Utilisez le caractère `&` pour créer un raccourci clavier :
```pascal
Button1.Caption := '&Valider';    // Alt+V pour cliquer
Button2.Caption := '&Annuler';    // Alt+A pour cliquer
Button3.Caption := '&Quitter';    // Alt+Q pour cliquer
```
La lettre suivant `&` sera soulignée et deviendra le raccourci.

#### Enabled
```pascal
Button1.Enabled := True;   // Actif, cliquable (défaut)
Button1.Enabled := False;  // Désactivé, grisé, non cliquable
```

**Usage :** Désactiver un bouton jusqu'à ce qu'une condition soit remplie :
```pascal
procedure TForm1.Edit1Change(Sender: TObject);
begin
  // Activer le bouton uniquement si le champ n'est pas vide
  ButtonValider.Enabled := Edit1.Text <> '';
end;
```

#### Visible
```pascal
Button1.Visible := True;   // Visible
Button1.Visible := False;  // Caché
```

#### Default
```pascal
Button1.Default := True;  // Ce bouton sera cliqué quand on appuie sur Enter
```
**Description :** Bouton par défaut du formulaire

**Important :** Un seul bouton peut être `Default` par formulaire.

**Usage :** Bouton "OK" ou "Valider" :
```pascal
ButtonOK.Default := True;      // Enter = cliquer sur OK
ButtonOK.Caption := '&OK';
```

#### Cancel
```pascal
Button1.Cancel := True;  // Ce bouton sera cliqué quand on appuie sur Échap
```
**Description :** Bouton d'annulation du formulaire

**Important :** Un seul bouton peut être `Cancel` par formulaire.

**Usage :** Bouton "Annuler" ou "Fermer" :
```pascal
ButtonAnnuler.Cancel := True;      // Échap = cliquer sur Annuler
ButtonAnnuler.Caption := '&Annuler';
```

#### ModalResult
```pascal
ButtonOK.ModalResult := mrOK;
ButtonAnnuler.ModalResult := mrCancel;
```
**Description :** Valeur retournée quand le bouton est cliqué dans un formulaire modal

**Valeurs courantes :**
| Valeur | Description |
|--------|-------------|
| `mrNone` | Aucun (défaut) |
| `mrOK` | OK |
| `mrCancel` | Annuler |
| `mrYes` | Oui |
| `mrNo` | Non |
| `mrAbort` | Abandonner |
| `mrRetry` | Réessayer |
| `mrIgnore` | Ignorer |

**Usage dans un formulaire modal :**
```pascal
// Dans le formulaire de dialogue :
ButtonOK.ModalResult := mrOK;
ButtonAnnuler.ModalResult := mrCancel;

// Dans le formulaire appelant :
if FormDialogue.ShowModal = mrOK then
  ShowMessage('Utilisateur a cliqué OK')
else
  ShowMessage('Utilisateur a cliqué Annuler');
```

**Important :** Quand `ModalResult <> mrNone`, le bouton ferme automatiquement le formulaire modal sans avoir besoin d'écrire de code !

#### TabOrder
```pascal
Edit1.TabOrder := 0;     // Premier
Edit2.TabOrder := 1;     // Deuxième
Button1.TabOrder := 2;   // Troisième
```
**Description :** Ordre de navigation avec la touche Tab

#### Font et Color
Comme les autres composants, vous pouvez personnaliser la police et la couleur :
```pascal
Button1.Font.Size := 12;
Button1.Font.Style := [fsBold];
Button1.Font.Color := clWhite;
Button1.Color := clGreen;
```

**Note :** L'apparence exacte dépend du système d'exploitation et du thème.

### Événements

#### OnClick
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bouton cliqué !');

  // Effectuer une action
  CalculerTotal;

  // Modifier l'interface
  Label1.Caption := 'Action effectuée';
  Button1.Enabled := False;
end;
```
**Quand :** Quand le bouton est cliqué  
**Usage :** C'est L'événement principal du bouton !  

**Note :** OnClick est aussi déclenché par :
- Clic de souris
- Espace (si le bouton a le focus)
- Enter (si Default = True)
- Échap (si Cancel = True)
- Alt+Lettre (si Caption contient &)

### Méthodes

#### Click
```pascal
Button1.Click;  // Simule un clic (déclenche OnClick)
```

#### SetFocus
```pascal
Button1.SetFocus;  // Donne le focus au bouton
```

### Exemples pratiques

**Bouton de validation standard :**
```pascal
ButtonValider.Caption := '&Valider';
ButtonValider.Default := True;
ButtonValider.TabOrder := 10;  // Dernier dans l'ordre de tabulation
```

**Bouton d'annulation standard :**
```pascal
ButtonAnnuler.Caption := '&Annuler';
ButtonAnnuler.Cancel := True;
ButtonAnnuler.TabOrder := 11;
```

**Bouton de calcul :**
```pascal
procedure TForm1.ButtonCalculerClick(Sender: TObject);
var
  A, B, Resultat: Double;
begin
  // Validation et conversion
  if not TryStrToFloat(Edit1.Text, A) then
  begin
    ShowMessage('Entrez un nombre valide dans le premier champ');
    Edit1.SetFocus;
    Exit;
  end;

  if not TryStrToFloat(Edit2.Text, B) then
  begin
    ShowMessage('Entrez un nombre valide dans le deuxième champ');
    Edit2.SetFocus;
    Exit;
  end;

  // Calcul
  Resultat := A + B;

  // Affichage
  LabelResultat.Caption := 'Résultat : ' + FloatToStr(Resultat);
end;
```

**Bouton pour ouvrir une autre fenêtre :**
```pascal
procedure TForm1.ButtonParametresClick(Sender: TObject);
begin
  FormParametres.ShowModal;
end;
```

**Bouton qui se désactive après utilisation :**
```pascal
procedure TForm1.ButtonEnvoyerClick(Sender: TObject);
begin
  // Désactiver pour éviter les double-clics
  ButtonEnvoyer.Enabled := False;

  try
    // Traitement long
    EnvoyerDonnees;
    ShowMessage('Données envoyées avec succès !');
  finally
    // Réactiver
    ButtonEnvoyer.Enabled := True;
  end;
end;
```

---

## Organiser les composants

### Alignement et espacement

**Conseils visuels :**
- **Alignement** : Les composants de même type devraient être alignés
- **Espacement** : Laissez de l'espace (au moins 10 pixels) entre les composants
- **Marges** : Laissez une marge avec les bords du formulaire (au moins 10 pixels)

**Exemple de disposition standard :**
```
┌─────────────────────────────────┐
│ Form                            │
│                                 │
│  Nom :        [_______________] │  ← Label aligné à gauche, Edit aligné
│                                 │
│  Prénom :     [_______________] │
│                                 │
│  Email :      [_______________] │
│                                 │
│       [Valider]  [Annuler]      │  ← Boutons centrés ou à droite
│                                 │
└─────────────────────────────────┘
```

### Utiliser les outils d'alignement de Lazarus

**Menu : Édition → Aligner**

Sélectionnez plusieurs composants (maintenez Ctrl enfoncé) puis :
- **Aligner à gauche** : tous les composants s'alignent sur le plus à gauche
- **Aligner en haut** : tous s'alignent sur le plus haut
- **Espacer horizontalement** : espace uniforme entre les composants
- **Même largeur** : tous prennent la largeur du premier sélectionné

### TabOrder (ordre de tabulation)

L'ordre dans lequel les composants reçoivent le focus quand on appuie sur Tab est important pour l'ergonomie.

**Définir l'ordre :**
1. Clic droit sur le formulaire
2. **Ordre de tabulation...**
3. Glissez-déposez pour réorganiser

**Ordre recommandé :**
```
0. EditNom
1. EditPrenom
2. EditEmail
3. EditTelephone
4. ButtonValider
5. ButtonAnnuler
```

---

## Association Label + Edit : Bonne pratique

### Le problème

Quand vous cliquez sur un label, rien ne se passe. Ce serait pratique que cliquer sur "Nom :" place le curseur dans le champ de saisie du nom.

### La solution : FocusControl

```pascal
// Associer un label à un Edit
LabelNom.FocusControl := EditNom;
```

Maintenant, cliquer sur le label donnera le focus à l'Edit associé !

**Configuration complète :**
```pascal
// Dans OnCreate ou visuellement dans l'Inspecteur d'Objets
LabelNom.Caption := '&Nom :';         // & pour le raccourci Alt+N
LabelNom.FocusControl := EditNom;     // Clic sur label = focus sur Edit
```

**Résultat :**
- Cliquer sur "Nom :" → curseur dans EditNom
- Appuyer sur Alt+N → curseur dans EditNom

---

## Validation complète d'un formulaire

Voici un exemple complet de formulaire avec validation :

```pascal
type
  TForm1 = class(TForm)
    LabelNom: TLabel;
    EditNom: TEdit;
    LabelEmail: TLabel;
    EditEmail: TEdit;
    LabelAge: TLabel;
    EditAge: TEdit;
    ButtonValider: TButton;
    ButtonAnnuler: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure ButtonValiderClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration des labels
  LabelNom.Caption := '&Nom :';
  LabelNom.FocusControl := EditNom;

  LabelEmail.Caption := '&Email :';
  LabelEmail.FocusControl := EditEmail;

  LabelAge.Caption := '&Âge :';
  LabelAge.FocusControl := EditAge;

  // Configuration des edits
  EditNom.MaxLength := 50;
  EditEmail.MaxLength := 100;
  EditAge.MaxLength := 3;

  // Configuration des boutons
  ButtonValider.Caption := '&Valider';
  ButtonValider.Default := True;
  ButtonValider.Enabled := False;  // Désactivé au départ

  ButtonAnnuler.Caption := '&Annuler';
  ButtonAnnuler.Cancel := True;

  // Focus sur le premier champ
  EditNom.SetFocus;
end;

procedure TForm1.EditChange(Sender: TObject);
begin
  // Activer le bouton Valider uniquement si tous les champs sont remplis
  ButtonValider.Enabled := (EditNom.Text <> '') and
                           (EditEmail.Text <> '') and
                           (EditAge.Text <> '');
end;

procedure TForm1.ButtonValiderClick(Sender: TObject);
var
  Age: Integer;
begin
  // Validation de l'âge
  if not TryStrToInt(EditAge.Text, Age) then
  begin
    ShowMessage('L''âge doit être un nombre !');
    EditAge.SetFocus;
    Exit;
  end;

  if (Age < 0) or (Age > 120) then
  begin
    ShowMessage('L''âge doit être entre 0 et 120 !');
    EditAge.SetFocus;
    Exit;
  end;

  // Validation simple de l'email (contient @)
  if Pos('@', EditEmail.Text) = 0 then
  begin
    ShowMessage('L''email doit contenir un @');
    EditEmail.SetFocus;
    Exit;
  end;

  // Tout est valide
  ShowMessage('Formulaire valide !' + sLineBreak +
              'Nom : ' + EditNom.Text + sLineBreak +
              'Email : ' + EditEmail.Text + sLineBreak +
              'Âge : ' + EditAge.Text);

  // Fermer le formulaire ou continuer le traitement...
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);
begin
  Close;
end;
```

---

## Résumé des composants

### TLabel

| Aspect | Description |
|--------|-------------|
| **But** | Afficher du texte non modifiable |
| **Propriété clé** | `Caption` |
| **Astuce** | Utilisez `FocusControl` pour associer à un Edit |
| **Usage courant** | Titres, descriptions, résultats |

### TEdit

| Aspect | Description |
|--------|-------------|
| **But** | Saisir du texte sur une ligne |
| **Propriété clé** | `Text` |
| **Événement clé** | `OnChange` |
| **Astuces** | `MaxLength`, `PasswordChar`, `ReadOnly` |
| **Usage courant** | Formulaires de saisie |

### TButton

| Aspect | Description |
|--------|-------------|
| **But** | Déclencher une action |
| **Propriété clé** | `Caption` |
| **Événement clé** | `OnClick` |
| **Astuces** | `Default`, `Cancel`, `ModalResult` |
| **Usage courant** | Valider, annuler, calculer, ouvrir |

---

## Bonnes pratiques

### 1. Nommage cohérent

❌ **Mauvais :**
```pascal
Label1, Label2, Label3
Edit1, Edit2, Edit3
Button1, Button2
```

✅ **Bon :**
```pascal
LabelNom, LabelPrenom, LabelEmail
EditNom, EditPrenom, EditEmail
ButtonValider, ButtonAnnuler
```

### 2. Donnez toujours un Caption significatif

```pascal
// Remplacez les Caption par défaut
LabelNom.Caption := 'Nom :';  // Au lieu de 'Label1'
ButtonValider.Caption := 'Valider';  // Au lieu de 'Button1'
```

### 3. Utilisez les raccourcis clavier

```pascal
LabelNom.Caption := '&Nom :';        // Alt+N
ButtonValider.Caption := '&Valider';  // Alt+V
ButtonAnnuler.Caption := '&Annuler';  // Alt+A
```

### 4. Définissez Default et Cancel

```pascal
ButtonValider.Default := True;   // Enter pour valider
ButtonAnnuler.Cancel := True;    // Échap pour annuler
```

### 5. Validez les saisies

```pascal
// Toujours valider avant d'utiliser
if TryStrToInt(EditAge.Text, Age) then
  // OK, utiliser Age
else
  // Erreur, informer l'utilisateur
```

### 6. Donnez du feedback à l'utilisateur

```pascal
// Désactiver les boutons pendant les traitements
ButtonEnvoyer.Enabled := False;
try
  TraiterDonnees;
finally
  ButtonEnvoyer.Enabled := True;
end;
```

### 7. Gérez les champs obligatoires

```pascal
// Activez le bouton uniquement si tous les champs sont remplis
procedure VerifierChamps;
begin
  ButtonValider.Enabled := (EditNom.Text <> '') and
                           (EditPrenom.Text <> '') and
                           (EditEmail.Text <> '');
end;
```

---

## Ce que vous avez appris

✅ Ajouter des composants visuellement dans Lazarus  
✅ Configurer les propriétés des TLabel, TEdit et TButton  
✅ Gérer les événements (OnClick, OnChange, OnKeyPress)  
✅ Valider les saisies utilisateur  
✅ Organiser les composants de manière ergonomique  
✅ Créer des formulaires interactifs complets  

---

## Prochaines étapes

Avec ces trois composants de base, vous pouvez déjà créer de nombreuses applications ! Dans les prochaines sections, vous découvrirez :

- **14.5 Événements et handlers** : Comprendre en profondeur la gestion des événements
- **14.6 Propriétés des composants** : Explorer les propriétés communes à tous les composants
- **15. Composants LCL Fondamentaux** : Découvrir d'autres composants (listes, grilles, images, etc.)

Félicitations ! Vous savez maintenant créer des interfaces graphiques interactives ! 🎉

⏭️ [Événements et handlers](/14-introduction-applications-graphiques/05-evenements-handlers.md)
