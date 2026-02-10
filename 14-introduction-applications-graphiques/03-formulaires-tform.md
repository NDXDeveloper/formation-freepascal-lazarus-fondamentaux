🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.3 Formulaires (TForm)

## Introduction

Le **formulaire** (Form) est l'élément fondamental de toute application graphique. C'est la fenêtre qui contient tous vos composants (boutons, zones de texte, etc.) et qui interagit avec l'utilisateur.

Dans cette section, nous allons explorer en profondeur :
- Ce qu'est réellement un TForm
- Ses propriétés essentielles
- Ses méthodes principales
- Son cycle de vie
- Comment gérer plusieurs formulaires

---

## Qu'est-ce qu'un TForm ?

### Définition

`TForm` est une **classe** fournie par la LCL (Lazarus Component Library) qui représente une fenêtre de votre application.

```pascal
type
  TForm1 = class(TForm)
    // Vos composants et méthodes ici
  end;
```

### Hiérarchie de classes

Comprendre d'où vient TForm aide à comprendre ses capacités :

```
TObject                    (classe racine de tous les objets)
  └─ TPersistent          (objets qui peuvent être sauvegardés)
      └─ TComponent       (composants avec propriétés et événements)
          └─ TControl     (éléments visuels)
              └─ TWinControl    (contrôles qui peuvent contenir d'autres contrôles)
                  └─ TCustomForm    (formulaire de base)
                      └─ TForm          (formulaire complet)
                          └─ TForm1     (VOTRE formulaire)
```

**Ce que cela signifie :**
- Votre `TForm1` **hérite** de toutes les capacités de `TForm`
- `TForm` hérite des capacités de `TWinControl` (peut contenir des composants)
- `TWinControl` hérite de `TControl` (est visible et interactif)
- Et ainsi de suite...

### Deux rôles du formulaire

Le formulaire joue deux rôles :

**1. Conteneur visuel**
- C'est la fenêtre que l'utilisateur voit
- Il contient et organise les composants

**2. Classe de code**
- C'est là que vous écrivez la logique de votre application
- Il gère les événements et les interactions

---

## Propriétés essentielles du formulaire

### Propriétés d'apparence

#### Caption
```pascal
Form1.Caption := 'Gestionnaire de clients';
```
**Description :** Le texte affiché dans la barre de titre  
**Type :** String  
**Valeur par défaut :** 'Form1'  

#### Width et Height
```pascal
Form1.Width := 800;   // Largeur en pixels  
Form1.Height := 600;  // Hauteur en pixels  
```
**Description :** Dimensions de la fenêtre  
**Type :** Integer  
**Conseil :** Pensez à différentes résolutions d'écran !  

#### Color
```pascal
Form1.Color := clWhite;      // Blanc  
Form1.Color := clBtnFace;    // Couleur système (recommandé)  
Form1.Color := $00FF8800;    // Couleur personnalisée (BGR)  
```
**Description :** Couleur de fond du formulaire  
**Type :** TColor  

**Couleurs courantes :**
| Constante | Couleur |
|-----------|---------|
| `clWhite` | Blanc |
| `clBlack` | Noir |
| `clBtnFace` | Couleur système (gris clair) |
| `clWindow` | Couleur de fond système |
| `clSkyBlue` | Bleu ciel |

#### Font
```pascal
Form1.Font.Name := 'Arial';  
Form1.Font.Size := 10;  
Form1.Font.Style := [fsBold];  
Form1.Font.Color := clNavy;  
```
**Description :** Police par défaut pour le formulaire et ses composants  
**Type :** TFont  

### Propriétés de comportement

#### BorderStyle
```pascal
Form1.BorderStyle := bsSizeable;  // Valeur par défaut
```

**Valeurs possibles :**

| Valeur | Description | Redimensionnable | Boutons |
|--------|-------------|------------------|---------|
| `bsNone` | Pas de bordure | Non | Aucun |
| `bsSingle` | Bordure simple | Non | Min, Max, Fermer |
| `bsSizeable` | Bordure redimensionnable | Oui | Min, Max, Fermer |
| `bsDialog` | Bordure de dialogue | Non | Fermer uniquement |
| `bsToolWindow` | Petite fenêtre outil | Non | Fermer uniquement |
| `bsSizeToolWin` | Fenêtre outil redimensionnable | Oui | Fermer uniquement |

**Quand utiliser quoi :**
- `bsSizeable` : Fenêtre principale (par défaut)
- `bsDialog` : Boîtes de dialogue, fenêtres de paramètres
- `bsNone` : Splash screens, fenêtres personnalisées
- `bsToolWindow` : Palettes d'outils, fenêtres flottantes

#### Position
```pascal
Form1.Position := poScreenCenter;
```

**Valeurs possibles :**

| Valeur | Description |
|--------|-------------|
| `poDesigned` | Position définie dans le designer (par défaut) |
| `poDefault` | Position choisie par le système |
| `poDefaultPosOnly` | Position par défaut, taille définie |
| `poDefaultSizeOnly` | Taille par défaut, position définie |
| `poScreenCenter` | **Centré sur l'écran** (recommandé) |
| `poDesktopCenter` | Centré sur le bureau |
| `poMainFormCenter` | Centré sur le formulaire principal |
| `poOwnerFormCenter` | Centré sur le formulaire propriétaire |

**Recommandation :** `poScreenCenter` pour le formulaire principal

#### WindowState
```pascal
Form1.WindowState := wsNormal;
```

**Valeurs possibles :**

| Valeur | Description |
|--------|-------------|
| `wsNormal` | Taille normale (par défaut) |
| `wsMinimized` | Réduit dans la barre des tâches |
| `wsMaximized` | Agrandi en plein écran |
| `wsFullScreen` | Plein écran (masque la barre des tâches) |

**Usage :**
```pascal
// Démarrer maximisé
Form1.WindowState := wsMaximized;

// Basculer en plein écran
if Form1.WindowState = wsFullScreen then
  Form1.WindowState := wsNormal
else
  Form1.WindowState := wsFullScreen;
```

#### FormStyle
```pascal
Form1.FormStyle := fsNormal;
```

**Valeurs possibles :**

| Valeur | Description |
|--------|-------------|
| `fsNormal` | Formulaire normal (par défaut) |
| `fsMDIForm` | Formulaire parent MDI |
| `fsMDIChild` | Formulaire enfant MDI |
| `fsStayOnTop` | Toujours au premier plan |
| `fsSplash` | Écran de démarrage |
| `fsSystemStayOnTop` | Toujours au premier plan (système) |

**Note :** MDI (Multiple Document Interface) est un style moins utilisé aujourd'hui.

### Propriétés de contraintes

#### Constraints
```pascal
// Taille minimale
Form1.Constraints.MinWidth := 400;  
Form1.Constraints.MinHeight := 300;  

// Taille maximale
Form1.Constraints.MaxWidth := 1920;  
Form1.Constraints.MaxHeight := 1080;  
```

**Description :** Limites de redimensionnement  
**Utile pour :** Empêcher l'utilisateur de rendre la fenêtre trop petite ou trop grande  

### Propriétés de visibilité

#### Visible
```pascal
Form1.Visible := True;   // Visible  
Form1.Visible := False;  // Caché  
```
**Description :** Contrôle si le formulaire est affiché  
**Type :** Boolean  

#### Enabled
```pascal
Form1.Enabled := True;   // Actif (par défaut)  
Form1.Enabled := False;  // Désactivé (grisé, n'accepte plus d'événements)  
```
**Description :** Contrôle si le formulaire répond aux interactions  
**Type :** Boolean  

---

## Méthodes essentielles du formulaire

### Méthodes de visibilité

#### Show et ShowModal

**Show : Affichage non-modal**
```pascal
Form1.Show;
```
- Le formulaire s'affiche
- L'utilisateur peut interagir avec d'autres fenêtres
- Le code continue immédiatement après l'appel

**ShowModal : Affichage modal**
```pascal
var
  Resultat: Integer;
begin
  Resultat := Form1.ShowModal;
  if Resultat = mrOK then
    ShowMessage('Utilisateur a cliqué OK');
end;
```
- Le formulaire s'affiche
- L'utilisateur **DOIT** fermer ce formulaire avant de pouvoir interagir avec d'autres
- Le code attend que le formulaire soit fermé
- Retourne une valeur (mrOK, mrCancel, etc.)

**Quand utiliser quoi :**
- `Show` : Fenêtres indépendantes, outils, palettes
- `ShowModal` : Boîtes de dialogue, formulaires de saisie obligatoire

#### Hide
```pascal
Form1.Hide;
// Équivalent à :
Form1.Visible := False;
```
**Description :** Cache le formulaire sans le détruire

#### Close
```pascal
Form1.Close;
```
**Description :** Ferme le formulaire (déclenche l'événement OnClose)  
**Note :** Le formulaire peut empêcher sa fermeture dans OnClose  

### Méthodes de gestion de la position

#### SetBounds
```pascal
// Définir position et taille en une seule fois
Form1.SetBounds(100, 100, 800, 600);
//               Left Top  Width Height
```

#### CenterScreen
```pascal
// Centrer le formulaire sur l'écran
Form1.Left := (Screen.Width - Form1.Width) div 2;  
Form1.Top := (Screen.Height - Form1.Height) div 2;  

// Ou plus simplement :
Form1.Position := poScreenCenter;
```

### Méthodes de dessin

#### Refresh
```pascal
Form1.Refresh;
```
**Description :** Force le redessin complet du formulaire  

#### Invalidate
```pascal
Form1.Invalidate;
```
**Description :** Marque le formulaire comme devant être redessiné  
**Différence avec Refresh :** Plus léger, le redessin peut être différé  

#### Update
```pascal
Form1.Update;
```
**Description :** Force le traitement immédiat des messages de dessin

### Méthodes de gestion des composants

#### FindComponent
```pascal
var
  Comp: TComponent;
begin
  Comp := Form1.FindComponent('Button1');
  if Comp <> nil then
    TButton(Comp).Caption := 'Trouvé !';
end;
```
**Description :** Recherche un composant par son nom  

---

## Événements principaux du formulaire

### Événements de cycle de vie

#### OnCreate
```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Code exécuté UNE SEULE FOIS à la création du formulaire
  Caption := 'Application démarrée à ' + TimeToStr(Now);

  // Initialisation des variables
  FCompteur := 0;

  // Chargement de configuration
  LoadConfig;
end;
```
**Quand :** À la création du formulaire (avant affichage)  
**Usage :** Initialisation, chargement de données  

#### OnShow
```pascal
procedure TForm1.FormShow(Sender: TObject);  
begin  
  // Code exécuté CHAQUE FOIS que le formulaire est affiché
  Edit1.SetFocus;  // Met le focus sur Edit1
  RefreshData;     // Rafraîchit les données
end;
```
**Quand :** Juste avant que le formulaire devienne visible  
**Usage :** Mise à jour de l'affichage, définition du focus  

#### OnActivate
```pascal
procedure TForm1.FormActivate(Sender: TObject);  
begin  
  // Code exécuté quand le formulaire devient actif
  StatusBar1.SimpleText := 'Fenêtre active';
end;
```
**Quand :** Quand le formulaire reçoit le focus  
**Usage :** Mise à jour d'état, rafraîchissement  

#### OnDeactivate
```pascal
procedure TForm1.FormDeactivate(Sender: TObject);  
begin  
  // Code exécuté quand le formulaire perd le focus
  StatusBar1.SimpleText := 'Fenêtre inactive';
end;
```
**Quand :** Quand le formulaire perd le focus

#### OnClose
```pascal
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  // Demander confirmation
  if MessageDlg('Voulez-vous vraiment quitter ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrNo then
  begin
    Action := caNone;  // Empêche la fermeture
    Exit;
  end;

  // Sauvegarder avant de fermer
  SaveData;

  Action := caFree;  // Libère le formulaire
end;
```

**Actions possibles :**
| Action | Description |
|--------|-------------|
| `caNone` | Annule la fermeture |
| `caHide` | Cache le formulaire |
| `caFree` | Libère le formulaire de la mémoire |
| `caMinimize` | Réduit le formulaire |

**Quand :** Quand l'utilisateur tente de fermer le formulaire  
**Usage :** Confirmation, sauvegarde, nettoyage  

#### OnCloseQuery
```pascal
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin  
  // Vérifier si on peut fermer
  if DataModified then
  begin
    case MessageDlg('Les données ont été modifiées. Sauvegarder ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          SaveData;
          CanClose := True;
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
```
**Quand :** Avant OnClose  
**Usage :** Validation avant fermeture  

#### OnDestroy
```pascal
procedure TForm1.FormDestroy(Sender: TObject);  
begin  
  // Code exécuté à la destruction du formulaire
  // Libération des ressources
  FMyObject.Free;
  FMyList.Free;
end;
```
**Quand :** Juste avant la destruction du formulaire  
**Usage :** Libération de ressources, nettoyage mémoire  

### Événements de redimensionnement

#### OnResize
```pascal
procedure TForm1.FormResize(Sender: TObject);  
begin  
  // Ajuster les composants
  Panel1.Width := Form1.ClientWidth div 2;

  // Afficher la nouvelle taille
  StatusBar1.SimpleText := Format('Taille: %d x %d',
                                   [ClientWidth, ClientHeight]);
end;
```
**Quand :** Quand le formulaire est redimensionné  
**Usage :** Ajustement dynamique de la mise en page  

### Événements de clavier

#### OnKeyPress
```pascal
procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);  
begin  
  // Réagir aux touches
  if Key = #27 then  // Échap
    Close;

  if Key = #13 then  // Entrée
    Button1.Click;
end;
```
**Quand :** Quand une touche caractère est pressée  
**Usage :** Raccourcis clavier simples  

#### OnKeyDown / OnKeyUp
```pascal
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);  
begin  
  // Touches spéciales et modificateurs
  if (ssCtrl in Shift) and (Key = VK_S) then
  begin
    SaveData;  // Ctrl+S pour sauvegarder
    Key := 0;  // Consomme l'événement
  end;

  if Key = VK_F1 then
    ShowHelp;  // F1 pour l'aide
end;
```
**Quand :** Quand une touche est enfoncée/relâchée  
**Usage :** Raccourcis complexes, touches spéciales  

### Événements de souris

#### OnClick / OnDblClick
```pascal
procedure TForm1.FormClick(Sender: TObject);  
begin  
  ShowMessage('Vous avez cliqué sur le formulaire');
end;

procedure TForm1.FormDblClick(Sender: TObject);  
begin  
  ShowMessage('Double-clic détecté !');
end;
```

#### OnMouseMove
```pascal
procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);  
begin  
  // Afficher les coordonnées
  StatusBar1.SimpleText := Format('Position: X=%d, Y=%d', [X, Y]);
end;
```

---

## Cycle de vie d'un formulaire

### Création et affichage

```
1. Constructor Create
   │
   ↓
2. Lecture du fichier .lfm
   │ (création des composants)
   ↓
3. OnCreate
   │ (initialisation du code utilisateur)
   ↓
4. OnShow
   │
   ↓
5. Formulaire visible
   │
   ↓
6. OnActivate
   │
   ↓
7. Le formulaire est actif et attend des événements
```

### Fermeture et destruction

```
1. Utilisateur clique sur X ou appelle Close
   │
   ↓
2. OnCloseQuery
   │ (peut annuler la fermeture)
   ↓
3. OnClose
   │ (définit l'action: Hide, Free, etc.)
   ↓
4. OnDeactivate
   │
   ↓
5. OnHide (si caHide)
   │
   ↓
6. OnDestroy
   │ (libération des ressources)
   ↓
7. Destructor Destroy
   │
   ↓
8. Formulaire détruit
```

---

## Formulaires multiples

### Créer un second formulaire

**Via le menu Lazarus :**
1. **Fichier → Nouveau → Formulaire**
2. Sauvegarder : `unit2.pas`
3. Un nouveau formulaire `TForm2` est créé

### Afficher un second formulaire

**Méthode 1 : Formulaire non-modal**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Form2.Show;
end;
```

**Méthode 2 : Formulaire modal**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Resultat: Integer;
begin
  Resultat := Form2.ShowModal;

  if Resultat = mrOK then
    ShowMessage('Utilisateur a validé')
  else
    ShowMessage('Utilisateur a annulé');
end;
```

**Méthode 3 : Création dynamique**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  MonForm: TForm2;
begin
  MonForm := TForm2.Create(Self);
  try
    MonForm.ShowModal;
  finally
    MonForm.Free;
  end;
end;
```

### Communication entre formulaires

#### Méthode 1 : Accès direct (déconseillé)
```pascal
// Dans Form2
procedure TForm2.Button1Click(Sender: TObject);  
begin  
  Form1.Caption := 'Modifié depuis Form2';  // Couplage fort !
end;
```
**Problème :** Couplage fort, difficile à maintenir

#### Méthode 2 : Propriétés publiques (recommandé)
```pascal
// Dans Unit2
type
  TForm2 = class(TForm)
    Edit1: TEdit;
  public
    function GetNom: string;
  end;

function TForm2.GetNom: string;  
begin  
  Result := Edit1.Text;
end;

// Dans Form1
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nom: string;
begin
  if Form2.ShowModal = mrOK then
  begin
    Nom := Form2.GetNom;
    Label1.Caption := 'Bonjour ' + Nom;
  end;
end;
```

#### Méthode 3 : Paramètres et résultats
```pascal
// Dans Unit2
type
  TForm2 = class(TForm)
  private
    FResultat: string;
  public
    procedure SetInitialValue(const Value: string);
    function GetResult: string;
  end;

// Dans Form1
var
  MonForm: TForm2;
begin
  MonForm := TForm2.Create(Self);
  try
    MonForm.SetInitialValue('Valeur initiale');

    if MonForm.ShowModal = mrOK then
      ShowMessage(MonForm.GetResult);
  finally
    MonForm.Free;
  end;
end;
```

---

## Bonnes pratiques

### 1. Nommage des formulaires

❌ **Mauvais :** `Form1`, `Form2`, `Form3`

✅ **Bon :** `FormPrincipal`, `FormParametres`, `FormAPropos`

```pascal
type
  TFormPrincipal = class(TForm)
  TFormParametres = class(TForm)
  TFormAPropos = class(TForm)
```

### 2. Gestion de la mémoire

**Pour le formulaire principal :**
```pascal
// Créé automatiquement par Application.CreateForm
// Libéré automatiquement à la fin
```

**Pour les formulaires secondaires modaux :**
```pascal
var
  F: TFormSecondaire;
begin
  F := TFormSecondaire.Create(Self);
  try
    F.ShowModal;
  finally
    F.Free;  // TOUJOURS libérer !
  end;
end;
```

**Pour les formulaires non-modaux :**
```pascal
// Dans OnClose du formulaire secondaire :
procedure TFormSecondaire.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  Action := caFree;  // Se libère automatiquement
end;
```

### 3. Initialisation

**Utilisez OnCreate pour :**
- Initialiser les variables
- Charger la configuration
- Préparer les données

**Utilisez OnShow pour :**
- Rafraîchir l'affichage
- Définir le focus
- Mettre à jour les données volatiles

### 4. Séparation des responsabilités

❌ **Mauvais :** Tout dans le formulaire
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  // 200 lignes de logique métier ici...
end;
```

✅ **Bon :** Séparer interface et logique
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  FGestionnaireClients.AjouterClient(Edit1.Text);
  RefreshList;
end;
```

### 5. Gestion des erreurs

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  try
    // Code qui peut échouer
    LoadData;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur: ' + E.Message);
      // Log l'erreur
    end;
  end;
end;
```

---

## Propriétés utiles supplémentaires

### AlphaBlend et AlphaBlendValue
```pascal
Form1.AlphaBlend := True;  
Form1.AlphaBlendValue := 200;  // 0 (transparent) à 255 (opaque)  
```
**Description :** Rendre le formulaire semi-transparent  

### DoubleBuffered
```pascal
Form1.DoubleBuffered := True;
```
**Description :** Active le double-buffering (réduit le scintillement)  
**Recommandation :** True pour les formulaires avec beaucoup de dessin personnalisé  

### KeyPreview
```pascal
Form1.KeyPreview := True;
```
**Description :** Le formulaire reçoit les événements clavier avant les composants  
**Usage :** Raccourcis globaux, gestion centralisée du clavier  

---

## Résumé

Le formulaire (TForm) est :

✅ La base de toute application GUI  
✅ Un conteneur pour les composants  
✅ Un gestionnaire d'événements  
✅ Une classe personnalisable par héritage  

**Propriétés clés à retenir :**
- `Caption`, `Width`, `Height`, `Color`
- `BorderStyle`, `Position`, `WindowState`
- `FormStyle`, `Visible`, `Enabled`

**Méthodes clés à retenir :**
- `Show`, `ShowModal`, `Hide`, `Close`
- `SetBounds`, `Refresh`, `Invalidate`

**Événements clés à retenir :**
- `OnCreate`, `OnShow`, `OnClose`, `OnDestroy`
- `OnResize`, `OnKeyPress`, `OnClick`

---

## Prochaines étapes

Maintenant que vous maîtrisez les formulaires, vous êtes prêt à :

- **14.4** : Découvrir les composants de base (TButton, TEdit, TLabel)
- **14.5** : Gérer les événements et créer des handlers
- **14.6** : Explorer les propriétés des composants

Votre fenêtre vide va enfin prendre vie ! 🎨

⏭️ [Composants de base (TButton, TEdit, TLabel)](/14-introduction-applications-graphiques/04-composants-base-tbutton-tedit-tlabel.md)
