🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.6 Composants de saisie avancés

## Introduction

Dans les sections précédentes, nous avons vu les composants de saisie de base comme `TEdit` (zone de texte), `TMemo` (texte multiligne), `TCheckBox` et `TRadioButton`. Maintenant, nous allons explorer des composants plus spécialisés qui facilitent la saisie de types de données particuliers.

Ces composants **avancés** offrent :
- ✅ **Validation automatique** : empêchent les saisies invalides
- ✅ **Interface intuitive** : boutons, curseurs, calendriers
- ✅ **Formatage** : affichent les données de manière appropriée
- ✅ **Ergonomie** : réduisent les erreurs de saisie

---

## Vue d'Ensemble des Composants

| Composant | Type de données | Usage typique |
|-----------|-----------------|---------------|
| **TSpinEdit** | Nombres entiers | Quantités, âges, compteurs |
| **TFloatSpinEdit** | Nombres décimaux | Prix, pourcentages, mesures |
| **TMaskEdit** | Texte formaté | Téléphones, codes postaux |
| **TDateEdit** | Dates | Dates de naissance, échéances |
| **TTimeEdit** | Heures | Horaires, durées |
| **TTrackBar** | Valeurs continues | Volume, luminosité, zoom |
| **TUpDown** | Incrémentation | Associé à un TEdit |
| **TColorButton** | Couleurs | Sélection de couleurs |
| **TCalendar** | Dates visuelles | Calendrier mensuel |

---

## TSpinEdit : Saisie Numérique avec Boutons

### Présentation

`TSpinEdit` permet de saisir des **nombres entiers** avec des boutons **+** et **-** pour incrémenter/décrémenter la valeur.

### Apparence Visuelle

```
┌──────────┬─┐
│   42     │▲│
│          │▼│
└──────────┴─┘
```

### Hiérarchie

```
TWinControl
  └─ TCustomEdit
       └─ TCustomSpinEdit
            └─ TSpinEdit
```

### Propriétés Principales

```pascal
property Value: Integer;        // Valeur actuelle  
property MinValue: Integer;     // Valeur minimum  
property MaxValue: Integer;     // Valeur maximum  
property Increment: Integer;    // Pas d'incrémentation (défaut: 1)  
property EditorEnabled: Boolean; // Autoriser la saisie directe  
```

### Exemple de Base

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration de l'âge
  SpinEditAge.MinValue := 0;
  SpinEditAge.MaxValue := 120;
  SpinEditAge.Value := 18;
  SpinEditAge.Increment := 1;
end;

procedure TForm1.SpinEditAgeChange(Sender: TObject);  
begin  
  LabelResultat.Caption := 'Âge : ' + IntToStr(SpinEditAge.Value) + ' ans';
end;
```

### Événements

```pascal
property OnChange: TNotifyEvent;  // La valeur change
```

### Cas d'Usage Typiques

#### 1. Quantité de produits

```pascal
SpinEditQuantite.MinValue := 1;  
SpinEditQuantite.MaxValue := 99;  
SpinEditQuantite.Value := 1;  
```

#### 2. Réglage de volume (0-100)

```pascal
SpinEditVolume.MinValue := 0;  
SpinEditVolume.MaxValue := 100;  
SpinEditVolume.Increment := 5;  // Incrémente par 5  
SpinEditVolume.Value := 50;  
```

#### 3. Année

```pascal
SpinEditAnnee.MinValue := 1900;  
SpinEditAnnee.MaxValue := 2100;  
SpinEditAnnee.Value := 2025;  
```

### Validation

Le SpinEdit **empêche automatiquement** les valeurs hors limites :

```pascal
// Si MinValue = 0 et MaxValue = 100
SpinEdit1.Value := 150;  // Sera ramené automatiquement à 100  
SpinEdit1.Value := -10;  // Sera ramené automatiquement à 0  
```

### Désactiver la Saisie Directe

```pascal
SpinEdit1.EditorEnabled := False;  // Seuls les boutons fonctionnent
```

---

## TFloatSpinEdit : Nombres Décimaux

### Présentation

`TFloatSpinEdit` est comme `TSpinEdit` mais pour les **nombres décimaux** (nombres à virgule).

### Propriétés Principales

```pascal
property Value: Double;           // Valeur actuelle (décimale)  
property MinValue: Double;        // Minimum  
property MaxValue: Double;        // Maximum  
property Increment: Double;       // Pas d'incrémentation  
property DecimalPlaces: Integer;  // Nombre de décimales affichées  
```

### Exemple : Prix

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  FloatSpinEditPrix.MinValue := 0.00;
  FloatSpinEditPrix.MaxValue := 9999.99;
  FloatSpinEditPrix.Increment := 0.10;  // Incrémente par 10 centimes
  FloatSpinEditPrix.DecimalPlaces := 2;  // Affiche 2 décimales
  FloatSpinEditPrix.Value := 19.99;
end;

procedure TForm1.FloatSpinEditPrixChange(Sender: TObject);  
begin  
  LabelPrix.Caption := Format('Prix : %.2f €', [FloatSpinEditPrix.Value]);
end;
```

### Cas d'Usage Typiques

#### 1. Pourcentages

```pascal
FloatSpinEditPourcent.MinValue := 0.0;  
FloatSpinEditPourcent.MaxValue := 100.0;  
FloatSpinEditPourcent.Increment := 0.5;  
FloatSpinEditPourcent.DecimalPlaces := 1;  
```

#### 2. Mesures (longueur, poids)

```pascal
FloatSpinEditPoids.MinValue := 0.0;  
FloatSpinEditPoids.MaxValue := 500.0;  
FloatSpinEditPoids.Increment := 0.1;  
FloatSpinEditPoids.DecimalPlaces := 2;  
```

#### 3. Température

```pascal
FloatSpinEditTemp.MinValue := -50.0;  
FloatSpinEditTemp.MaxValue := 50.0;  
FloatSpinEditTemp.Increment := 0.5;  
FloatSpinEditTemp.DecimalPlaces := 1;  
```

---

## TMaskEdit : Saisie avec Masque de Format

### Présentation

`TMaskEdit` permet de saisir du texte selon un **masque prédéfini**. C'est très utile pour les numéros de téléphone, codes postaux, numéros de sécurité sociale, etc.

### Hiérarchie

```
TWinControl
  └─ TCustomEdit
       └─ TCustomMaskEdit
            └─ TMaskEdit
```

### Propriété Fondamentale

```pascal
property EditMask: string;  // Le masque de saisie  
property Text: string;      // Le texte saisi  
```

### Syntaxe des Masques

| Caractère | Signification | Exemple |
|-----------|---------------|---------|
| **0** | Chiffre obligatoire (0-9) | `00` = 00 à 99 |
| **9** | Chiffre optionnel (0-9 ou espace) | `999` = peut être vide |
| **#** | Chiffre ou signe +/- optionnel | `###` |
| **L** | Lettre obligatoire (A-Z, a-z) | `LL` = 2 lettres |
| **l** | Lettre optionnelle | `lll` |
| **A** | Lettre ou chiffre obligatoire | `AAA` |
| **a** | Lettre ou chiffre optionnel | `aaa` |
| **C** | Caractère obligatoire quelconque | `CC` |
| **c** | Caractère optionnel quelconque | `ccc` |
| **\\** | Échappement (le caractère suivant est littéral) | `\\(` = parenthèse |
| **Autres** | Caractères littéraux (tirets, espaces, etc.) | `-`, `.`, `/`, `(`, `)` |

### Exemples de Masques Courants

#### 1. Numéro de téléphone français (format: 01 23 45 67 89)

```pascal
MaskEditTelephone.EditMask := '00 00 00 00 00;1;_';
// Explications :
// '00 00 00 00 00' = masque (les espaces sont littéraux)
// ';1;' = enregistre les caractères littéraux dans Text
// '_' = caractère de remplacement affiché
```

**Résultat :**
```
┌──────────────────┐
│ __ __ __ __ __   │
└──────────────────┘
```

#### 2. Code postal français (5 chiffres)

```pascal
MaskEditCodePostal.EditMask := '00000;1;_';
```

#### 3. Date (JJ/MM/AAAA)

```pascal
MaskEditDate.EditMask := '00/00/0000;1;_';
```

#### 4. Heure (HH:MM)

```pascal
MaskEditHeure.EditMask := '00:00;1;_';
```

#### 5. Plaque d'immatriculation (XX-000-XX)

```pascal
MaskEditPlaque.EditMask := 'LL-000-LL;1;_';
```

#### 6. Numéro de carte bancaire (0000 0000 0000 0000)

```pascal
MaskEditCarte.EditMask := '0000 0000 0000 0000;1;_';
```

#### 7. Numéro de sécurité sociale (1 23 45 67 890 123 45)

```pascal
MaskEditSecu.EditMask := '0 00 00 00 000 000 00;1;_';
```

### Structure du Masque

Le masque complet a trois parties séparées par des points-virgules :

```
'masque;sauvegarde;caractère_remplacement'
```

- **masque** : définit le format
- **sauvegarde** :
  - `0` = ne sauvegarde pas les caractères littéraux dans Text
  - `1` = sauvegarde tout dans Text
- **caractère_remplacement** : caractère affiché pour les positions non saisies (souvent `_` ou espace)

### Exemple Complet

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Téléphone
  MaskEditTel.EditMask := '00 00 00 00 00;1;_';
  MaskEditTel.Text := '';

  // Code postal
  MaskEditCP.EditMask := '00000;1;_';

  // Date
  MaskEditDate.EditMask := '00/00/0000;1;_';
end;

procedure TForm1.BtnValiderClick(Sender: TObject);  
begin  
  // Vérifier si la saisie est complète
  if Pos('_', MaskEditTel.Text) > 0 then
    ShowMessage('Numéro de téléphone incomplet')
  else
    ShowMessage('Téléphone : ' + MaskEditTel.Text);
end;
```

### Validation de la Saisie

```pascal
function TForm1.TelephoneComplet: Boolean;  
begin  
  // Vérifier qu'il n'y a pas de caractères de remplacement
  Result := (Pos('_', MaskEditTel.Text) = 0) and
            (Length(MaskEditTel.Text) > 0);
end;
```

### Cas d'Usage Typiques

- **Numéros de téléphone** : formatage automatique avec espaces
- **Dates et heures** : format JJ/MM/AAAA, HH:MM:SS
- **Codes** : codes postaux, codes produits
- **Identifiants** : numéros de sécurité sociale, NIR
- **Plaques d'immatriculation** : format contrôlé

---

## TDateEdit : Saisie de Dates

### Présentation

`TDateEdit` est un composant spécialisé pour saisir des **dates** avec un bouton calendrier intégré.

### Apparence Visuelle

```
┌──────────────┬─┐
│ 14/10/2025   │▼│  ← Bouton pour ouvrir le calendrier
└──────────────┴─┘
```

### Propriétés Principales

```pascal
property Date: TDateTime;       // Date actuelle  
property Text: string;          // Texte affiché  
property ButtonWidth: Integer;  // Largeur du bouton calendrier  
property DialogTitle: string;   // Titre du calendrier  
property DateOrder: TDateOrder; // Ordre d'affichage (doJJ/MM/AAAA, etc.)  
```

### Exemple de Base

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Date du jour par défaut
  DateEdit1.Date := Now;

  // Ou une date spécifique
  DateEdit2.Date := EncodeDate(2025, 12, 25);  // Noël 2025
end;

procedure TForm1.DateEdit1Change(Sender: TObject);  
begin  
  ShowMessage('Date sélectionnée : ' + DateToStr(DateEdit1.Date));
end;
```

### Manipulation des Dates

```pascal
// Obtenir la date
var
  MaDate: TDateTime;
begin
  MaDate := DateEdit1.Date;
end;

// Définir une date
DateEdit1.Date := EncodeDate(2025, 10, 14);  // 14 octobre 2025

// Date du jour
DateEdit1.Date := Date;  // ou Now

// Calculer des dates
DateEdit1.Date := Date + 7;  // Dans 7 jours  
DateEdit1.Date := Date - 30; // Il y a 30 jours  
```

### Validation

```pascal
procedure TForm1.BtnValiderClick(Sender: TObject);  
var  
  Aujourd_hui: TDateTime;
begin
  Aujourd_hui := Date;

  if DateEdit1.Date > Aujourd_hui then
    ShowMessage('La date est dans le futur')
  else if DateEdit1.Date < Aujourd_hui then
    ShowMessage('La date est dans le passé')
  else
    ShowMessage('C''est aujourd''hui !');
end;
```

### Calculer un Âge

```pascal
function TForm1.CalculerAge(DateNaissance: TDateTime): Integer;  
var  
  Aujourd_hui: TDateTime;
begin
  Aujourd_hui := Date;
  Result := YearsBetween(DateNaissance, Aujourd_hui);  // Nécessite uses DateUtils
end;

// Utilisation
procedure TForm1.DateEditNaissanceChange(Sender: TObject);  
begin  
  LabelAge.Caption := 'Âge : ' + IntToStr(CalculerAge(DateEditNaissance.Date)) + ' ans';
end;
```

### Cas d'Usage Typiques

- **Dates de naissance** : calcul d'âge automatique
- **Dates d'échéance** : gestion de délais
- **Réservations** : dates de début et fin
- **Historique** : dates d'événements

---

## TTimeEdit : Saisie d'Heures

### Présentation

`TTimeEdit` permet de saisir des **heures** au format HH:MM:SS avec des boutons d'incrémentation.

### Apparence Visuelle

```
┌──────────┬─┐
│ 14:30:00 │▲│
│          │▼│
└──────────┴─┘
```

### Propriétés Principales

```pascal
property Time: TDateTime;  // Heure actuelle  
property Text: string;     // Texte affiché  
```

### Exemple de Base

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Heure actuelle
  TimeEdit1.Time := Now;

  // Ou une heure spécifique
  TimeEdit2.Time := EncodeTime(14, 30, 0, 0);  // 14:30:00
end;

procedure TForm1.TimeEdit1Change(Sender: TObject);  
begin  
  ShowMessage('Heure : ' + TimeToStr(TimeEdit1.Time));
end;
```

### Manipulation des Heures

```pascal
// Extraire les composants
var
  H, M, S, MS: Word;
begin
  DecodeTime(TimeEdit1.Time, H, M, S, MS);
  ShowMessage(Format('Il est %d heures et %d minutes', [H, M]));
end;

// Créer une heure
TimeEdit1.Time := EncodeTime(9, 0, 0, 0);  // 09:00:00

// Ajouter du temps
TimeEdit1.Time := TimeEdit1.Time + EncodeTime(1, 30, 0, 0);  // +1h30
```

### Calculer une Durée

```pascal
function TForm1.CalculerDuree(Debut, Fin: TDateTime): string;  
var  
  Duree: TDateTime;
  H, M, S, MS: Word;
begin
  Duree := Fin - Debut;
  DecodeTime(Duree, H, M, S, MS);
  Result := Format('%d heures %d minutes', [H, M]);
end;

// Utilisation
procedure TForm1.BtnCalculerClick(Sender: TObject);  
var  
  Duree: string;
begin
  Duree := CalculerDuree(TimeEditDebut.Time, TimeEditFin.Time);
  ShowMessage('Durée : ' + Duree);
end;
```

### Cas d'Usage Typiques

- **Horaires** : heures d'ouverture/fermeture
- **Rendez-vous** : planification
- **Durées** : temps de travail, durées de projet
- **Alarmes** : réglage d'heures de réveil

---

## TTrackBar : Curseur de Valeur

### Présentation

`TTrackBar` (barre de défilement) permet de sélectionner une valeur dans une plage en déplaçant un **curseur**.

### Apparence Visuelle

```
Horizontal:
┌────────●─────────────┐
└──────────────────────┘
      Curseur

Vertical:
┌──┐
│  │
│  │
│●─┤
│  │
│  │
└──┘
```

### Hiérarchie

```
TWinControl
  └─ TCustomTrackBar
       └─ TTrackBar
```

### Propriétés Principales

```pascal
property Min: Integer;          // Valeur minimum  
property Max: Integer;          // Valeur maximum  
property Position: Integer;     // Position actuelle du curseur  
property Frequency: Integer;    // Espacement des graduations  
property TickMarks: TTickMark;  // Position des graduations (tmBottomRight, tmTopLeft, tmBoth)  
property TickStyle: TTickStyle; // Style des graduations (tsAuto, tsNone, tsManual)  
property Orientation: TTrackBarOrientation;  // Horizontal ou Vertical  
property LineSize: Integer;     // Déplacement par flèches clavier  
property PageSize: Integer;     // Déplacement par PageUp/PageDown  
property ShowSelRange: Boolean; // Afficher plage de sélection  
```

### Exemple : Volume Audio

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration du curseur de volume
  TrackBarVolume.Min := 0;
  TrackBarVolume.Max := 100;
  TrackBarVolume.Position := 50;
  TrackBarVolume.Frequency := 10;  // Graduation tous les 10
  TrackBarVolume.Orientation := trHorizontal;
end;

procedure TForm1.TrackBarVolumeChange(Sender: TObject);  
begin  
  LabelVolume.Caption := 'Volume : ' + IntToStr(TrackBarVolume.Position) + '%';
  // Ici, appliquer le volume réel à un lecteur audio
end;
```

### Orientation

```pascal
// Horizontal (par défaut)
TrackBar1.Orientation := trHorizontal;  
TrackBar1.Width := 200;  
TrackBar1.Height := 30;  

// Vertical
TrackBar2.Orientation := trVertical;  
TrackBar2.Width := 30;  
TrackBar2.Height := 200;  
```

### Graduations

```pascal
// Graduations en bas/droite
TrackBar1.TickMarks := tmBottomRight;

// Graduations des deux côtés
TrackBar1.TickMarks := tmBoth;

// Sans graduations
TrackBar1.TickStyle := tsNone;

// Espacement des graduations
TrackBar1.Frequency := 5;  // Tous les 5 unités
```

### Cas d'Usage Typiques

#### 1. Contrôle de volume

```pascal
TrackBarVolume.Min := 0;  
TrackBarVolume.Max := 100;  
TrackBarVolume.Position := 50;  
```

#### 2. Luminosité

```pascal
TrackBarLuminosite.Min := 0;  
TrackBarLuminosite.Max := 255;  
TrackBarLuminosite.Position := 128;  
```

#### 3. Zoom

```pascal
TrackBarZoom.Min := 10;   // 10%  
TrackBarZoom.Max := 400;  // 400%  
TrackBarZoom.Position := 100;  // 100%  
TrackBarZoom.Frequency := 10;  
```

#### 4. Sélection d'année

```pascal
TrackBarAnnee.Min := 1900;  
TrackBarAnnee.Max := 2100;  
TrackBarAnnee.Position := 2025;  
TrackBarAnnee.Frequency := 10;  
```

### Combinaison avec un Label

```pascal
procedure TForm1.TrackBar1Change(Sender: TObject);  
begin  
  LabelValeur.Caption := IntToStr(TrackBar1.Position);
end;
```

---

## TUpDown : Boutons Haut/Bas

### Présentation

`TUpDown` (aussi appelé "Spin Button") fournit deux boutons (▲▼) pour incrémenter/décrémenter une valeur. Il est souvent **associé à un TEdit**.

### Apparence Visuelle

```
┌──────────┐┌─┐
│   42     ││▲│
└──────────┘│▼│
            └─┘
```

### Propriétés Principales

```pascal
property Min: SmallInt;         // Valeur minimum  
property Max: SmallInt;         // Valeur maximum  
property Position: SmallInt;    // Position actuelle  
property Increment: SmallInt;   // Pas d'incrémentation  
property Associate: TWinControl; // Contrôle associé (souvent TEdit)  
property Orientation: TUDOrientation;  // Horizontal ou Vertical  
property Wrap: Boolean;         // Boucler (Max → Min)  
```

### Exemple : Associé à un TEdit

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Associer l'UpDown à l'Edit
  UpDown1.Associate := Edit1;

  // Configuration
  UpDown1.Min := 0;
  UpDown1.Max := 100;
  UpDown1.Position := 50;

  // La valeur sera automatiquement affichée dans Edit1
end;

procedure TForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);  
begin  
  LabelResultat.Caption := 'Valeur : ' + IntToStr(UpDown1.Position);
end;
```

### Sans Association

Vous pouvez aussi utiliser TUpDown seul :

```pascal
procedure TForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);  
begin  
  // Button = btNext (▲) ou btPrev (▼)
  if Button = btNext then
    ShowMessage('Incrémenter')
  else
    ShowMessage('Décrémenter');

  Label1.Caption := IntToStr(UpDown1.Position);
end;
```

### Wrap (Bouclage)

```pascal
UpDown1.Min := 0;  
UpDown1.Max := 10;  
UpDown1.Wrap := True;  // 10 → 0 et 0 → 10  
```

### Orientation

```pascal
// Vertical (par défaut)
UpDown1.Orientation := udVertical;

// Horizontal
UpDown1.Orientation := udHorizontal;  // ◄ ►
```

---

## TColorButton : Sélecteur de Couleur

### Présentation

`TColorButton` affiche un bouton avec une couleur et ouvre un **sélecteur de couleur** au clic.

### Apparence Visuelle

```
┌────────────┐
│   ████     │ ← Bouton coloré
└────────────┘
```

### Propriétés Principales

```pascal
property ButtonColor: TColor;  // Couleur sélectionnée
```

### Exemple

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  ColorButton1.ButtonColor := clRed;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);  
begin  
  // Appliquer la couleur à un autre composant
  Panel1.Color := ColorButton1.ButtonColor;

  LabelCouleur.Caption := 'Couleur : ' +
    ColorToString(ColorButton1.ButtonColor);
end;
```

### Cas d'Usage

- **Personnalisation** : choix de couleur de fond, texte
- **Éditeurs graphiques** : palette de couleurs
- **Thèmes** : configuration de l'apparence

---

## TCalendar : Calendrier Visuel

### Présentation

`TCalendar` affiche un **calendrier mensuel** complet, permettant de sélectionner une date visuellement.

### Apparence Visuelle

```
┌──────────────────────────┐
│   Octobre 2025       ◄ ► │
├──┬──┬──┬──┬──┬──┬──┤
│ L│ M│ M│ J│ V│ S│ D│
├──┼──┼──┼──┼──┼──┼──┤
│  │  │ 1│ 2│ 3│ 4│ 5│
│ 6│ 7│ 8│ 9│10│11│12│
│13│14│15│16│17│18│19│  ← Jour sélectionné
│20│21│22│23│24│25│26│
│27│28│29│30│31│  │  │
└──┴──┴──┴──┴──┴──┴──┘
```

### Propriétés Principales

```pascal
property Date: TDateTime;     // Date sélectionnée  
property DisplaySettings: TDisplaySettings;  // Configuration affichage  
```

### Exemple

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Date du jour par défaut
  Calendar1.Date := Date;
end;

procedure TForm1.Calendar1Click(Sender: TObject);  
begin  
  ShowMessage('Date sélectionnée : ' + DateToStr(Calendar1.Date));
end;

procedure TForm1.Calendar1DblClick(Sender: TObject);  
begin  
  // Double-clic pour valider
  DateEdit1.Date := Calendar1.Date;
  Panel1.Visible := False;  // Masquer le calendrier
end;
```

### Cas d'Usage

- **Sélection de date** : alternative visuelle à TDateEdit
- **Planning** : visualiser les jours d'un mois
- **Réservations** : afficher les disponibilités

---

## Comparaison des Composants

| Besoin | Composant recommandé | Raison |
|--------|---------------------|---------|
| Nombre entier 0-100 | TSpinEdit | Simple et direct |
| Prix 0.00-999.99 | TFloatSpinEdit | Gestion décimales |
| Téléphone | TMaskEdit | Format contrôlé |
| Date de naissance | TDateEdit | Calendrier intégré |
| Heure de RDV | TTimeEdit | Format horaire |
| Volume 0-100 | TTrackBar | Visuel et intuitif |
| Quantité avec Edit | TUpDown + TEdit | Combine saisie et boutons |
| Couleur de fond | TColorButton | Sélecteur visuel |
| Date visuelle | TCalendar | Vue mensuelle complète |

---

## Validation Multi-Composants

### Exemple : Formulaire d'Inscription

```pascal
type
  TFormInscription = class(TForm)
    EditNom: TEdit;
    EditPrenom: TEdit;
    DateEditNaissance: TDateEdit;
    MaskEditTelephone: TMaskEdit;
    SpinEditAge: TSpinEdit;
    CheckBoxConditions: TCheckBox;
    BtnValider: TButton;
    procedure BtnValiderClick(Sender: TObject);
  private
    function FormulaireValide: Boolean;
  end;

function TFormInscription.FormulaireValide: Boolean;  
var  
  Age: Integer;
begin
  Result := False;

  // Vérifier le nom
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    EditNom.SetFocus;
    Exit;
  end;

  // Vérifier le prénom
  if Trim(EditPrenom.Text) = '' then
  begin
    ShowMessage('Le prénom est obligatoire');
    EditPrenom.SetFocus;
    Exit;
  end;

  // Vérifier l'âge (via date de naissance)
  Age := YearsBetween(DateEditNaissance.Date, Date);  // Nécessite uses DateUtils
  if Age < 18 then
  begin
    ShowMessage('Vous devez avoir au moins 18 ans');
    DateEditNaissance.SetFocus;
    Exit;
  end;

  // Vérifier le téléphone (pas de caractères de remplacement)
  if Pos('_', MaskEditTelephone.Text) > 0 then
  begin
    ShowMessage('Le numéro de téléphone est incomplet');
    MaskEditTelephone.SetFocus;
    Exit;
  end;

  // Vérifier les conditions
  if not CheckBoxConditions.Checked then
  begin
    ShowMessage('Vous devez accepter les conditions');
    CheckBoxConditions.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TFormInscription.BtnValiderClick(Sender: TObject);  
begin  
  if FormulaireValide then
  begin
    ShowMessage('Inscription validée !');
    // Enregistrer les données...
  end;
end;
```

---

## Événements Communs

### OnChange

La plupart des composants de saisie ont un événement `OnChange` déclenché quand la valeur change :

```pascal
procedure TForm1.SpinEdit1Change(Sender: TObject);  
begin  
  // Réagir au changement
  UpdateInterface;
end;
```

### OnEnter / OnExit

Pour faire des actions quand le composant reçoit ou perd le focus :

```pascal
procedure TForm1.Edit1Enter(Sender: TObject);  
begin  
  // Composant actif
  Edit1.Color := clYellow;
end;

procedure TForm1.Edit1Exit(Sender: TObject);  
begin  
  // Composant quitté
  Edit1.Color := clWhite;
  // Valider la saisie
  if Edit1.Text = '' then
    ShowMessage('Ce champ est obligatoire');
end;
```

---

## Disposition et Organisation

### Exemple : Interface Complète

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Grouper dans un GroupBox "Informations personnelles"
  GroupBox1.Caption := 'Informations personnelles';

  // Nom, Prénom (TEdit standards)
  // ...

  // Date de naissance (TDateEdit)
  DateEditNaissance.Date := EncodeDate(1990, 1, 1);

  // Âge (TSpinEdit en lecture seule)
  SpinEditAge.Enabled := False;  // Calculé automatiquement

  // Téléphone (TMaskEdit)
  MaskEditTel.EditMask := '00 00 00 00 00;1;_';

  // Grouper dans un GroupBox "Préférences"
  GroupBox2.Caption := 'Préférences';

  // Volume notification (TTrackBar)
  TrackBarVolume.Min := 0;
  TrackBarVolume.Max := 100;
  TrackBarVolume.Position := 50;

  // Couleur thème (TColorButton)
  ColorButtonTheme.ButtonColor := clBlue;
end;

procedure TForm1.DateEditNaissanceChange(Sender: TObject);  
begin  
  // Calculer l'âge automatiquement
  SpinEditAge.Value := YearsBetween(DateEditNaissance.Date, Date);  // Nécessite uses DateUtils
end;
```

---

## Accessibilité et Ergonomie

### 1. TabOrder (Ordre de Tabulation)

Définissez l'ordre de navigation avec la touche **Tab** :

```pascal
EditNom.TabOrder := 0;  
EditPrenom.TabOrder := 1;  
DateEditNaissance.TabOrder := 2;  
MaskEditTel.TabOrder := 3;  
BtnValider.TabOrder := 4;  
```

### 2. Touches de Raccourci (Accelerators)

Utilisez `&` dans les Label pour créer des raccourcis :

```pascal
Label1.Caption := '&Nom :';  // Alt+N active EditNom  
Label1.FocusControl := EditNom;  
```

### 3. Hints (Infobulles)

Ajoutez des infobulles explicatives :

```pascal
MaskEditTel.Hint := 'Format : 01 23 45 67 89';  
MaskEditTel.ShowHint := True;  

DateEditNaissance.Hint := 'Cliquez sur ▼ pour ouvrir le calendrier';  
DateEditNaissance.ShowHint := True;  
```

### 4. Valeurs par Défaut Intelligentes

```pascal
// Date du jour par défaut
DateEditDate.Date := Date;

// Heure actuelle
TimeEditHeure.Time := Now;

// Valeur médiane
SpinEditQuantite.Value := (SpinEditQuantite.MinValue + SpinEditQuantite.MaxValue) div 2;
```

---

## Bonnes Pratiques

### 1. Validation Progressive

Validez pendant la saisie, pas seulement à la fin :

```pascal
procedure TForm1.SpinEditAgeChange(Sender: TObject);  
begin  
  if SpinEditAge.Value < 18 then
    LabelAvertissement.Caption := 'Âge insuffisant'
  else
    LabelAvertissement.Caption := '';
end;
```

### 2. Feedback Visuel

Indiquez visuellement l'état :

```pascal
procedure TForm1.EditChange(Sender: TObject);  
begin  
  if Edit1.Text = '' then
    Edit1.Color := clYellow  // Champ vide = jaune
  else
    Edit1.Color := clWhite;  // Champ rempli = blanc
end;
```

### 3. Désactiver Plutôt que Masquer

```pascal
// ✅ BON : Désactiver (l'utilisateur voit pourquoi c'est grisé)
BtnValider.Enabled := CheckBoxConditions.Checked;

// ❌ MAUVAIS : Masquer (l'utilisateur ne sait pas où est le bouton)
BtnValider.Visible := CheckBoxConditions.Checked;
```

### 4. Messages d'Erreur Clairs

```pascal
// ✅ BON
ShowMessage('Le numéro de téléphone doit contenir 10 chiffres (format : 01 23 45 67 89)');

// ❌ MAUVAIS
ShowMessage('Erreur');
```

### 5. Valeurs Limites Cohérentes

```pascal
// ✅ BON : Limites logiques
SpinEditAge.MinValue := 0;  
SpinEditAge.MaxValue := 120;  // Âge maximum réaliste  

// ❌ MAUVAIS : Limites absurdes
SpinEditAge.MinValue := -100;  
SpinEditAge.MaxValue := 9999;  
```

---

## Exemple Complet : Calculateur de Prix

```pascal
type
  TFormCalculateur = class(TForm)
    SpinEditQuantite: TSpinEdit;
    FloatSpinEditPrixUnitaire: TFloatSpinEdit;
    TrackBarRemise: TTrackBar;
    LabelRemise: TLabel;
    LabelPrixTotal: TLabel;
    DateEditLivraison: TDateEdit;
    TimeEditHeureLivraison: TTimeEdit;
    BtnCalculer: TButton;
    procedure BtnCalculerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBarRemiseChange(Sender: TObject);
  end;

procedure TFormCalculateur.FormCreate(Sender: TObject);  
begin  
  // Quantité
  SpinEditQuantite.MinValue := 1;
  SpinEditQuantite.MaxValue := 999;
  SpinEditQuantite.Value := 1;

  // Prix unitaire
  FloatSpinEditPrixUnitaire.MinValue := 0.01;
  FloatSpinEditPrixUnitaire.MaxValue := 9999.99;
  FloatSpinEditPrixUnitaire.DecimalPlaces := 2;
  FloatSpinEditPrixUnitaire.Value := 10.00;

  // Remise (0-50%)
  TrackBarRemise.Min := 0;
  TrackBarRemise.Max := 50;
  TrackBarRemise.Position := 0;
  TrackBarRemise.Frequency := 5;

  // Livraison demain par défaut
  DateEditLivraison.Date := Date + 1;
  TimeEditHeureLivraison.Time := EncodeTime(10, 0, 0, 0);  // 10:00
end;

procedure TFormCalculateur.TrackBarRemiseChange(Sender: TObject);  
begin  
  LabelRemise.Caption := 'Remise : ' + IntToStr(TrackBarRemise.Position) + '%';
end;

procedure TFormCalculateur.BtnCalculerClick(Sender: TObject);  
var  
  PrixBase, Remise, PrixFinal: Double;
  DateLivraison: string;
begin
  // Calculs
  PrixBase := SpinEditQuantite.Value * FloatSpinEditPrixUnitaire.Value;
  Remise := PrixBase * (TrackBarRemise.Position / 100);
  PrixFinal := PrixBase - Remise;

  // Date de livraison
  DateLivraison := DateToStr(DateEditLivraison.Date) + ' à ' +
                   TimeToStr(TimeEditHeureLivraison.Time);

  // Affichage
  LabelPrixTotal.Caption := Format(
    'Prix de base : %.2f €'#13#10 +
    'Remise (-%d%%) : -%.2f €'#13#10 +
    'Prix final : %.2f €'#13#10 +
    'Livraison prévue : %s',
    [PrixBase, TrackBarRemise.Position, Remise, PrixFinal, DateLivraison]
  );
end;
```

---

## Points Clés à Retenir

1. **TSpinEdit / TFloatSpinEdit** : saisie numérique avec validation automatique
   - MinValue, MaxValue, Increment
   - Évite les erreurs de saisie

2. **TMaskEdit** : format de saisie contrôlé
   - EditMask définit le format ('00 00 00 00 00')
   - Parfait pour téléphones, codes, dates

3. **TDateEdit / TTimeEdit** : saisie de dates et heures
   - Calendrier intégré pour TDateEdit
   - Manipulation avec TDateTime

4. **TTrackBar** : sélection visuelle d'une valeur
   - Min, Max, Position
   - Idéal pour volumes, zoom, etc.

5. **TUpDown** : boutons +/- souvent avec TEdit
   - Associate pour lier à un Edit

6. **TColorButton** : sélection de couleur visuelle

7. **TCalendar** : vue mensuelle complète

8. **Validation** : vérifier les saisies avant traitement

9. **Ergonomie** : TabOrder, Hints, feedback visuel

10. **Messages clairs** : guider l'utilisateur en cas d'erreur

---

## Conclusion

Les composants de saisie avancés améliorent considérablement l'expérience utilisateur en :
- **Prévenant les erreurs** : validation automatique
- **Guidant l'utilisateur** : formats visuels, calendriers
- **Simplifiant la saisie** : boutons, curseurs
- **Rendant l'interface intuitive** : composants spécialisés

Choisir le bon composant pour chaque type de donnée rend vos applications plus professionnelles et agréables à utiliser.

Dans la section suivante, nous explorerons les **composants d'affichage** (TImage, TShape) pour enrichir visuellement vos interfaces.

---

**Prochaine étape :** 15.7 Composants d'affichage (TImage, TShape)

⏭️ [Composants d'affichage (TImage, TShape)](/15-composants-lcl-fondamentaux/07-composants-affichage-timage-tshape.md)
