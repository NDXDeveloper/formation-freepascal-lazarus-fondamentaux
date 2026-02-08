🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.5 Événements et handlers

## Introduction

Nous avons déjà utilisé des événements dans les sections précédentes (OnClick, OnChange, etc.), mais maintenant il est temps de **vraiment comprendre** ce qui se passe sous le capot.

Dans cette section, nous allons explorer en profondeur :
- Ce qu'est réellement un événement
- Comment fonctionne un gestionnaire d'événement (handler)
- Comment créer et associer des événements
- Les différents types d'événements
- Les astuces et bonnes pratiques

Cette compréhension vous permettra de maîtriser pleinement la programmation événementielle !

---

## Rappel : La programmation événementielle

Dans une application graphique, votre code ne s'exécute **pas** de façon linéaire du début à la fin. Au lieu de cela :

1. L'application démarre et affiche l'interface
2. Elle entre dans une **boucle d'attente**
3. Quand quelque chose se produit (clic, frappe clavier, etc.), un **événement** est généré
4. Le système appelle votre **gestionnaire d'événement** (handler)
5. Votre code s'exécute
6. Retour à la boucle d'attente

```
        ┌─────────────────────────┐
        │  Application tourne     │
        │  Boucle d'événements    │
        └────────┬────────────────┘
                 │
                 ↓
        ┌─────────────────────────┐
        │  Événement détecté !    │
        │  (ex: clic souris)      │
        └────────┬────────────────┘
                 │
                 ↓
        ┌─────────────────────────┐
        │  Quel composant ?       │
        │  Quel événement ?       │
        └────────┬────────────────┘
                 │
                 ↓
        ┌─────────────────────────┐
        │  Appel du handler       │
        │  ButtonClick()          │
        └────────┬────────────────┘
                 │
                 ↓
        ┌─────────────────────────┐
        │  Code utilisateur       │
        │  s'exécute              │
        └────────┬────────────────┘
                 │
                 ↓
        ┌─────────────────────────┐
        │  Retour à la boucle     │
        └─────────────────────────┘
```

---

## Qu'est-ce qu'un événement ?

### Définition technique

Un **événement** est une **propriété spéciale** d'un composant qui peut être associée à une procédure de votre code.

Techniquement, c'est un **pointeur de fonction** (ou plus exactement, une **méthode**).

### Exemple concret

Quand vous créez un bouton :

```pascal
type
  TButton = class(TWinControl)
    // ... autres propriétés ...
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;
```

`OnClick` est une **propriété** de type `TNotifyEvent`.

Quand vous associez votre code à OnClick :

```pascal
Button1.OnClick := @ButtonClick;
// ou visuellement dans l'Inspecteur d'Objets
```

Vous dites : "Quand ce bouton sera cliqué, appelle MA procédure ButtonClick".

---

## Qu'est-ce qu'un handler (gestionnaire) ?

### Définition

Un **handler** (gestionnaire d'événement) est une **procédure ou méthode** que vous écrivez pour répondre à un événement.

### Anatomie d'un handler

```pascal
procedure TForm1.Button1Click(Sender: TObject);
//        ^^^^^  ^^^^^^^^^^^^  ^^^^^^^^^^^^^^
//         |          |              |
//      Classe    Nom du         Paramètres
//                handler
begin
  // Votre code ici
  ShowMessage('Bouton cliqué !');
end;
```

**Décortiquons :**

| Élément | Description |
|---------|-------------|
| `TForm1` | La classe qui contient ce handler (votre formulaire) |
| `Button1Click` | Le nom du handler (vous pouvez le choisir) |
| `Sender: TObject` | Le composant qui a déclenché l'événement |

---

## Créer un handler : Les méthodes

### Méthode 1 : Double-clic (la plus simple)

**Dans le designer de formulaire :**
1. Double-cliquez sur le composant
2. Lazarus crée automatiquement le handler pour l'événement par défaut
   - Bouton → OnClick
   - Edit → OnChange
   - Form → OnCreate

**Lazarus génère :**
```pascal
// Dans la section type
type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);  // Déclaration
  private
  public
  end;

// Dans la section implementation
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Votre code ici
end;
```

### Méthode 2 : Via l'Inspecteur d'Objets

**Plus de contrôle sur l'événement :**

1. Sélectionnez le composant
2. Dans l'Inspecteur d'Objets, cliquez sur l'onglet **"Événements"**
3. Trouvez l'événement souhaité (ex: OnClick)
4. Double-cliquez dans la case vide à droite
5. Lazarus crée le handler

**Vous voyez maintenant :**
```
┌───────────────────────────────┐
│ Événements                    │
├───────────────────────────────┤
│ OnClick    Button1Click    ⚙  │
│ OnDblClick                    │
│ OnEnter                       │
│ OnExit                        │
│ OnKeyPress                    │
│ ...                           │
└───────────────────────────────┘
```

### Méthode 3 : Créer manuellement

**Pour les développeurs avancés :**

```pascal
type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure MonHandler(Sender: TObject);  // 1. Déclarer
  end;

implementation

procedure TForm1.MonHandler(Sender: TObject);  // 2. Implémenter
begin
  ShowMessage('Mon handler personnalisé');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.OnClick := @MonHandler;  // 3. Associer
end;
```

---

## Le paramètre Sender

### À quoi sert Sender ?

`Sender` est le composant qui a **déclenché** l'événement. Il permet de :
- Savoir quel composant a été activé
- Réutiliser un même handler pour plusieurs composants
- Accéder aux propriétés du composant

### Type de Sender

`Sender` est de type `TObject`, le type le plus générique. Pour l'utiliser, vous devez souvent le **transtyper** :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  MonBouton: TButton;
begin
  // Transtypage sûr avec "as"
  MonBouton := Sender as TButton;
  MonBouton.Caption := 'Cliqué !';

  // Ou directement
  (Sender as TButton).Caption := 'Cliqué !';
end;
```

### Pourquoi TObject et pas directement TButton ?

Parce que **tous** les composants peuvent déclencher des événements, pas seulement les boutons. Le système utilise TObject pour être universel.

### Vérification du type avec "is"

Avant de transtyper, vous pouvez vérifier le type :

```pascal
procedure TForm1.ComposantClick(Sender: TObject);
begin
  if Sender is TButton then
    ShowMessage('C''est un bouton')
  else if Sender is TLabel then
    ShowMessage('C''est un label')
  else
    ShowMessage('C''est autre chose');
end;
```

### Exemple pratique : Handler partagé

**Un seul handler pour plusieurs boutons :**

```pascal
type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure BoutonClick(Sender: TObject);  // Handler unique
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Associer le même handler aux 3 boutons
  Button1.OnClick := @BoutonClick;
  Button2.OnClick := @BoutonClick;
  Button3.OnClick := @BoutonClick;
end;

procedure TForm1.BoutonClick(Sender: TObject);
var
  Bouton: TButton;
begin
  Bouton := Sender as TButton;

  // Identifier quel bouton a été cliqué
  if Bouton = Button1 then
    ShowMessage('Bouton 1 cliqué')
  else if Bouton = Button2 then
    ShowMessage('Bouton 2 cliqué')
  else if Bouton = Button3 then
    ShowMessage('Bouton 3 cliqué');

  // Ou utiliser la propriété Name
  ShowMessage('Bouton ' + Bouton.Name + ' cliqué');
end;
```

---

## Types d'événements courants

### TNotifyEvent - L'événement simple

**Signature :**
```pascal
TNotifyEvent = procedure(Sender: TObject) of object;
```

**Utilisé par :**
- OnClick
- OnDblClick
- OnCreate
- OnDestroy
- OnShow
- OnEnter
- OnExit
- Et beaucoup d'autres...

**Exemple :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Événement simple');
end;
```

### TKeyEvent - Événements clavier

**Signature :**
```pascal
TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of object;
```

**Paramètres :**
- `Sender` : Le composant
- `Key` : Le code de la touche (VK_F1, VK_ESCAPE, etc.)
- `Shift` : État des touches modificatrices (Ctrl, Alt, Shift)

**Utilisé par :**
- OnKeyDown
- OnKeyUp

**Exemple :**
```pascal
procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Détecter Ctrl+S
  if (ssCtrl in Shift) and (Key = VK_S) then
  begin
    SaveData;
    Key := 0;  // Consommer l'événement
  end;

  // Détecter F1
  if Key = VK_F1 then
    ShowHelp;

  // Détecter Alt+F4
  if (ssAlt in Shift) and (Key = VK_F4) then
    Close;
end;
```

**Codes de touches courants :**
| Constante | Touche |
|-----------|--------|
| `VK_RETURN` ou `VK_ENTER` | Entrée |
| `VK_ESCAPE` | Échap |
| `VK_SPACE` | Espace |
| `VK_TAB` | Tab |
| `VK_BACK` | Backspace |
| `VK_DELETE` | Delete |
| `VK_F1` à `VK_F12` | Touches F1 à F12 |
| `VK_LEFT`, `VK_RIGHT`, `VK_UP`, `VK_DOWN` | Flèches |

**États de Shift :**
| Constante | Touche |
|-----------|--------|
| `ssShift` | Shift enfoncée |
| `ssCtrl` | Ctrl enfoncée |
| `ssAlt` | Alt enfoncée |
| `ssLeft` | Bouton gauche souris enfoncé |
| `ssRight` | Bouton droit souris enfoncé |
| `ssMiddle` | Bouton milieu souris enfoncé |

### TKeyPressEvent - Frappe clavier

**Signature :**
```pascal
TKeyPressEvent = procedure(Sender: TObject; var Key: Char) of object;
```

**Paramètres :**
- `Sender` : Le composant
- `Key` : Le caractère saisi

**Utilisé par :**
- OnKeyPress

**Différence avec OnKeyDown :**
- OnKeyPress : Pour les **caractères** (a, b, 1, 2, etc.)
- OnKeyDown : Pour **toutes les touches** (F1, Ctrl, flèches, etc.)

**Exemple :**
```pascal
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  // Accepter uniquement les chiffres
  if not (Key in ['0'..'9', #8, #13]) then  // #8=Backspace, #13=Enter
  begin
    Key := #0;  // Annuler la touche
    Beep;       // Signal sonore
  end;

  // Convertir en majuscules
  Key := UpCase(Key);

  // Enter = passer au suivant
  if Key = #13 then
  begin
    Key := #0;
    SelectNext(Sender as TWinControl, True, True);
  end;
end;
```

**Codes de caractères spéciaux :**
| Code | Signification |
|------|---------------|
| `#8` | Backspace |
| `#9` | Tab |
| `#13` | Enter/Return |
| `#27` | Escape |
| `#32` | Space |

### TMouseEvent - Événements souris

**Signature :**
```pascal
TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
                        Shift: TShiftState; X, Y: Integer) of object;
```

**Paramètres :**
- `Sender` : Le composant
- `Button` : Quel bouton (mbLeft, mbRight, mbMiddle)
- `Shift` : État des modificateurs
- `X, Y` : Coordonnées du curseur (relatives au composant)

**Utilisé par :**
- OnMouseDown
- OnMouseUp

**Exemple :**
```pascal
procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
begin
  // Clic gauche
  if Button = mbLeft then
    ShowMessage('Clic gauche à ' + IntToStr(X) + ', ' + IntToStr(Y));

  // Clic droit
  if Button = mbRight then
    PopupMenu1.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);

  // Clic avec Ctrl
  if ssCtrl in Shift then
    ShowMessage('Clic avec Ctrl');
end;
```

### TMouseMoveEvent - Mouvement souris

**Signature :**
```pascal
TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
                           X, Y: Integer) of object;
```

**Utilisé par :**
- OnMouseMove

**Exemple :**
```pascal
procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  // Afficher les coordonnées
  StatusBar1.SimpleText := Format('Position: %d, %d', [X, Y]);

  // Dessiner pendant le glissement (bouton gauche enfoncé)
  if ssLeft in Shift then
    Canvas.Pixels[X, Y] := clBlack;
end;
```

### TCloseEvent - Fermeture

**Signature :**
```pascal
TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
```

**Paramètres :**
- `Sender` : Le formulaire
- `Action` : Que faire (caNone, caHide, caFree, caMinimize)

**Utilisé par :**
- OnClose

**Exemple :**
```pascal
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if MessageDlg('Voulez-vous vraiment quitter ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrNo then
  begin
    Action := caNone;  // Empêcher la fermeture
    Exit;
  end;

  SaveData;           // Sauvegarder
  Action := caFree;   // Libérer le formulaire
end;
```

### TCloseQueryEvent - Vérification avant fermeture

**Signature :**
```pascal
TCloseQueryEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
```

**Paramètres :**
- `Sender` : Le formulaire
- `CanClose` : Autoriser la fermeture (True/False)

**Utilisé par :**
- OnCloseQuery

**Exemple :**
```pascal
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
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

---

## Ordre d'exécution des événements

### Exemple : Cliquer sur un bouton

Quand l'utilisateur clique sur un bouton, plusieurs événements se déclenchent dans un ordre précis :

```
1. OnMouseDown    (bouton souris enfoncé)
   ↓
2. OnClick        (clic détecté)
   ↓
3. OnMouseUp      (bouton souris relâché)
```

### Exemple : Saisir du texte dans un Edit

```
1. OnEnter        (le focus entre dans le champ)
   ↓
2. OnKeyDown      (touche enfoncée)
   ↓
3. OnKeyPress     (caractère saisi)
   ↓
4. OnChange       (contenu modifié)
   ↓
5. OnKeyUp        (touche relâchée)
   ↓
   ... (répétition pour chaque touche)
   ↓
6. OnExit         (le focus sort du champ)
```

### Exemple : Démarrage d'un formulaire

```
1. Constructor Create
   ↓
2. Lecture du .lfm (création des composants)
   ↓
3. OnCreate       (votre code d'initialisation)
   ↓
4. OnShow         (juste avant l'affichage)
   ↓
5. Le formulaire devient visible
   ↓
6. OnActivate     (le formulaire est actif)
```

### Importance de l'ordre

**Exemple pratique :**
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialiser des données
  LoadConfiguration;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Utiliser les données chargées
  Edit1.Text := FConfiguration.LastValue;
  Edit1.SetFocus;
end;
```

**OnCreate** vient avant **OnShow**, donc les données sont prêtes pour l'affichage.

---

## Associer/Dissocier des événements par code

### Associer dynamiquement

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer un bouton dynamiquement
  MyButton := TButton.Create(Self);
  MyButton.Parent := Self;
  MyButton.Caption := 'Cliquez-moi';
  MyButton.Left := 10;
  MyButton.Top := 10;

  // Associer l'événement
  MyButton.OnClick := @MonHandler;
end;

procedure TForm1.MonHandler(Sender: TObject);
begin
  ShowMessage('Bouton dynamique cliqué !');
end;
```

### Changer le handler

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.OnClick := @Handler1;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  // Changer le comportement selon la case à cocher
  if CheckBox1.Checked then
    Button1.OnClick := @Handler2
  else
    Button1.OnClick := @Handler1;
end;

procedure TForm1.Handler1(Sender: TObject);
begin
  ShowMessage('Handler 1');
end;

procedure TForm1.Handler2(Sender: TObject);
begin
  ShowMessage('Handler 2');
end;
```

### Dissocier un événement

```pascal
// Supprimer l'événement
Button1.OnClick := nil;

// Maintenant cliquer sur Button1 ne fait rien
```

### Réutiliser un handler existant

```pascal
// Plusieurs composants partagent le même handler
Button1.OnClick := @MonHandler;
Button2.OnClick := @MonHandler;
Button3.OnClick := @MonHandler;
MenuItem1.OnClick := @MonHandler;
```

---

## Bloquer la propagation d'événements

### Consommer un événement

Certains événements peuvent être "consommés" pour empêcher leur traitement par défaut :

**OnKeyPress :**
```pascal
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then  // Enter
  begin
    ProcessData;
    Key := #0;  // Consommer = empêcher le "beep" par défaut
  end;
end;
```

**OnKeyDown :**
```pascal
procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Edit1.Text := '';
    Key := 0;  // Consommer
  end;
end;
```

**OnCloseQuery :**
```pascal
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not DataSaved then
  begin
    ShowMessage('Sauvegardez d''abord !');
    CanClose := False;  // Bloquer la fermeture
  end;
end;
```

---

## Événements personnalisés

Vous pouvez créer vos propres événements !

### Définir un type d'événement

```pascal
type
  // Type d'événement avec données personnalisées
  TMonEvenement = procedure(Sender: TObject; const Valeur: Integer) of object;

  // Classe avec événement personnalisé
  TMaClasse = class
  private
    FOnMonEvenement: TMonEvenement;
  public
    procedure Declencher(V: Integer);
    property OnMonEvenement: TMonEvenement read FOnMonEvenement write FOnMonEvenement;
  end;

procedure TMaClasse.Declencher(V: Integer);
begin
  // Vérifier qu'un handler est associé
  if Assigned(FOnMonEvenement) then
    FOnMonEvenement(Self, V);
end;
```

### Utiliser l'événement personnalisé

```pascal
type
  TForm1 = class(TForm)
  private
    FMaClasse: TMaClasse;
    procedure MonHandler(Sender: TObject; const Valeur: Integer);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMaClasse := TMaClasse.Create;
  FMaClasse.OnMonEvenement := @MonHandler;
end;

procedure TForm1.MonHandler(Sender: TObject; const Valeur: Integer);
begin
  ShowMessage('Valeur reçue : ' + IntToStr(Valeur));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FMaClasse.Declencher(42);  // Déclenche l'événement
end;
```

---

## Vérifier si un événement est associé

### Avec Assigned

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Vérifier avant d'appeler
  if Assigned(Button2.OnClick) then
    Button2.OnClick(Button2)  // Déclencher manuellement
  else
    ShowMessage('Pas de handler associé');
end;
```

### Appel sécurisé d'événement

```pascal
procedure TForm1.Declencher;
begin
  if Assigned(FOnMonEvenement) then
    FOnMonEvenement(Self);
end;
```

---

## Bonnes pratiques

### 1. Nommage des handlers

**Convention recommandée :**
```pascal
ComposantÉvénement
```

**Exemples :**
```pascal
Button1Click
Edit1Change
FormCreate
Timer1Timer
ListBox1Click
```

**Évitez :**
```pascal
btn1  // Pas clair
HandleButton  // Trop générique
DoSomething  // Pas descriptif
```

### 2. Un handler = Une responsabilité

❌ **Mauvais :** Handler trop complexe
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // 200 lignes de code...
  ValidateInput;
  ProcessData;
  UpdateDatabase;
  RefreshDisplay;
  SendEmail;
  GenerateReport;
  // etc.
end;
```

✅ **Bon :** Handler délègue
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  if not ValidateInput then
    Exit;

  ProcessData;
  RefreshDisplay;
end;
```

### 3. Gérer les erreurs

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    // Code qui peut échouer
    ProcessData;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur : ' + E.Message);
      LogError(E);
    end;
  end;
end;
```

### 4. Éviter les boucles infinies

❌ **Danger :**
```pascal
procedure TForm1.Edit1Change(Sender: TObject);
begin
  Edit1.Text := UpperCase(Edit1.Text);  // Déclenche OnChange !
  // Boucle infinie !
end;
```

✅ **Solution :**
```pascal
procedure TForm1.Edit1Change(Sender: TObject);
var
  OldText: string;
begin
  OldText := Edit1.Text;
  Edit1.Text := UpperCase(OldText);

  // Restaurer la position du curseur
  Edit1.SelStart := Length(Edit1.Text);
end;
```

✅ **Meilleure solution :**
```pascal
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  Key := UpCase(Key);  // Convertir directement, pas de boucle
end;
```

### 5. Désactiver temporairement les handlers

```pascal
procedure TForm1.LoadData;
var
  OldHandler: TNotifyEvent;
begin
  // Sauvegarder le handler
  OldHandler := Edit1.OnChange;

  try
    // Désactiver temporairement
    Edit1.OnChange := nil;

    // Charger les données sans déclencher OnChange
    Edit1.Text := DataFromDatabase;

  finally
    // Restaurer le handler
    Edit1.OnChange := OldHandler;
  end;
end;
```

### 6. Utiliser les tags pour identifier les composants

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Tag := 1;
  Button2.Tag := 2;
  Button3.Tag := 3;

  Button1.OnClick := @BoutonClick;
  Button2.OnClick := @BoutonClick;
  Button3.OnClick := @BoutonClick;
end;

procedure TForm1.BoutonClick(Sender: TObject);
begin
  case (Sender as TButton).Tag of
    1: ShowMessage('Action 1');
    2: ShowMessage('Action 2');
    3: ShowMessage('Action 3');
  end;
end;
```

### 7. Libérer les ressources

```pascal
procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Dissocier les événements avant de détruire
  Button1.OnClick := nil;

  // Libérer les ressources
  if Assigned(FMyObject) then
    FreeAndNil(FMyObject);
end;
```

---

## Débogage des événements

### Afficher quand un événement se déclenche

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Message de debug
  ShowMessage('Button1Click déclenché');

  // Ou dans la console (si application console/debug)
  WriteLn('Button1Click appelé à ', TimeToStr(Now));

  // Votre code ici
end;
```

### Points d'arrêt

Dans Lazarus, placez un point d'arrêt (F5) dans votre handler pour inspecter :
- Les valeurs des variables
- L'état de Sender
- La pile d'appels (qui a déclenché cet événement ?)

### Vérifier l'association

```pascal
procedure TForm1.FormShow(Sender: TObject);
begin
  if Assigned(Button1.OnClick) then
    ShowMessage('Handler associé')
  else
    ShowMessage('Pas de handler !');
end;
```

---

## Événements et threads

### ⚠️ Attention : Thread safety

Les événements GUI doivent être appelés depuis le **thread principal** uniquement !

❌ **Dangereux :**
```pascal
// Dans un thread secondaire
procedure MonThread.Execute;
begin
  Form1.Label1.Caption := 'Terminé';  // ERREUR !
end;
```

✅ **Correct :**
```pascal
// Dans un thread secondaire
procedure MonThread.Execute;
begin
  Synchronize(@UpdateUI);
end;

procedure MonThread.UpdateUI;
begin
  Form1.Label1.Caption := 'Terminé';  // OK dans le thread principal
end;
```

---

## Résumé

### Concepts clés

✅ Un **événement** est une propriété qui peut pointer vers une méthode
✅ Un **handler** est la méthode que vous écrivez pour répondre
✅ **Sender** identifie qui a déclenché l'événement
✅ Les événements ont des **signatures** spécifiques (paramètres)
✅ L'**ordre** des événements est important
✅ On peut **associer/dissocier** des événements dynamiquement

### Types d'événements principaux

| Type | Utilisé pour |
|------|--------------|
| `TNotifyEvent` | Événements simples (OnClick, OnCreate) |
| `TKeyEvent` | Touches spéciales (OnKeyDown/Up) |
| `TKeyPressEvent` | Caractères (OnKeyPress) |
| `TMouseEvent` | Souris (OnMouseDown/Up) |
| `TMouseMoveEvent` | Mouvement souris (OnMouseMove) |
| `TCloseEvent` | Fermeture (OnClose) |

### Bonnes pratiques à retenir

1. **Nommer clairement** : ComposantÉvénement
2. **Une responsabilité** par handler
3. **Gérer les erreurs** avec try-except
4. **Éviter les boucles** infinies
5. **Utiliser Sender** intelligemment
6. **Désactiver** temporairement si besoin
7. **Déboguer** avec points d'arrêt

---

## Prochaines étapes

Maintenant que vous maîtrisez les événements et handlers, vous pouvez :

- **14.6 Propriétés des composants** : Explorer les propriétés communes
- **14.7 Layouts et anchors** : Organiser l'interface
- **14.8 Menus et barres d'outils** : Ajouter des menus
- **15. Composants LCL Fondamentaux** : Découvrir d'autres composants

Vous avez maintenant tous les outils pour créer des applications interactives sophistiquées ! 🚀

---

**Point clé à retenir :** Les événements sont le cœur de la programmation GUI. Maîtriser leur fonctionnement vous permet de créer des applications réactives et intuitives !

⏭️ [Propriétés des composants](/14-introduction-applications-graphiques/06-proprietes-composants.md)
