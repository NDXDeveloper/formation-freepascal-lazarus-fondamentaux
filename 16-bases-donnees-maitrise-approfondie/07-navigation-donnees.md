🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.7 Navigation dans les données

## Introduction : se déplacer dans les enregistrements

Maintenant que vous savez afficher des données avec les composants data-aware, il est temps d'approfondir la **navigation** dans ces données. Comment passer d'un enregistrement à l'autre ? Comment aller directement au premier ou au dernier ? Comment savoir où vous êtes dans l'ensemble des données ?

### L'analogie du livre

Imaginez votre base de données comme un **livre** :

- Chaque **enregistrement** est une **page**
- Le **curseur** est le **marque-page** qui indique la page courante
- **Naviguer** = tourner les pages
- **First** = aller à la première page
- **Last** = aller à la dernière page
- **Next** = tourner la page suivante
- **Prior** = revenir à la page précédente
- **EOF** (End Of File) = vous êtes après la dernière page
- **BOF** (Beginning Of File) = vous êtes avant la première page

Votre dataset (`TSQLQuery`) maintient toujours un **enregistrement courant**, celui qui est affiché dans vos composants data-aware.

## Le concept d'enregistrement courant

### Qu'est-ce que l'enregistrement courant ?

À tout moment, votre dataset pointe vers **un seul enregistrement** : l'**enregistrement courant**.

- C'est celui affiché dans vos `TDBEdit`, `TDBLabel`, etc.
- C'est celui qui sera modifié si vous changez les valeurs
- C'est celui qui sera supprimé si vous cliquez sur le bouton Supprimer

```pascal
// L'enregistrement courant est celui accessible par :
SQLQuery1.FieldByName('nom').AsString  // Nom de l'enregistrement courant  
SQLQuery1.FieldByName('id').AsInteger  // ID de l'enregistrement courant  
```

### Visualiser dans le DBGrid

Dans un `TDBGrid`, l'enregistrement courant est celui :
- Surligné (ligne sélectionnée)
- Marqué par une flèche dans la colonne indicateur (si activée)

Cliquer sur une ligne du `DBGrid` **change** l'enregistrement courant.

## Les méthodes de navigation de base

### First : Aller au premier enregistrement

```pascal
SQLQuery1.First;
```

**Effet :**
- Positionne le curseur sur le **premier** enregistrement
- Met à jour tous les composants data-aware
- `BOF` devient `True` (vous êtes au début)

**Exemple :**
```pascal
procedure TForm1.ButtonPremierClick(Sender: TObject);  
begin  
  if SQLQuery1.Active then
    SQLQuery1.First;
end;
```

### Last : Aller au dernier enregistrement

```pascal
SQLQuery1.Last;
```

**Effet :**
- Positionne le curseur sur le **dernier** enregistrement
- Met à jour tous les composants data-aware
- `EOF` devient `True` après un `Next`

**Exemple :**
```pascal
procedure TForm1.ButtonDernierClick(Sender: TObject);  
begin  
  if SQLQuery1.Active then
    SQLQuery1.Last;
end;
```

### Next : Avancer d'un enregistrement

```pascal
SQLQuery1.Next;
```

**Effet :**
- Passe à l'enregistrement **suivant**
- Si vous êtes sur le dernier, `EOF` devient `True`
- Les composants data-aware se mettent à jour

**Exemple avec vérification :**
```pascal
procedure TForm1.ButtonSuivantClick(Sender: TObject);  
begin  
  if SQLQuery1.Active and not SQLQuery1.EOF then
    SQLQuery1.Next
  else
    ShowMessage('Vous êtes déjà au dernier enregistrement');
end;
```

### Prior : Reculer d'un enregistrement

```pascal
SQLQuery1.Prior;
```

**Effet :**
- Revient à l'enregistrement **précédent**
- Si vous êtes sur le premier, `BOF` devient `True`
- Les composants data-aware se mettent à jour

**Exemple avec vérification :**
```pascal
procedure TForm1.ButtonPrecedentClick(Sender: TObject);  
begin  
  if SQLQuery1.Active and not SQLQuery1.BOF then
    SQLQuery1.Prior
  else
    ShowMessage('Vous êtes déjà au premier enregistrement');
end;
```

## Les propriétés de position

### BOF : Beginning Of File

**Type :** `Boolean`

**Signification :** `True` si le curseur est **avant** le premier enregistrement.

```pascal
if SQLQuery1.BOF then
  ShowMessage('Vous êtes au début du dataset');
```

**Cas d'usage :**
- Désactiver le bouton "Précédent" quand `BOF = True`
- Savoir qu'on ne peut plus reculer

**Attention :** `BOF` ne signifie pas que vous êtes **sur** le premier enregistrement, mais **avant** !

Pour savoir si vous êtes sur le premier :
```pascal
if SQLQuery1.RecNo = 1 then
  ShowMessage('Premier enregistrement');
```

### EOF : End Of File

**Type :** `Boolean`

**Signification :** `True` si le curseur est **après** le dernier enregistrement.

```pascal
if SQLQuery1.EOF then
  ShowMessage('Vous êtes à la fin du dataset');
```

**Cas d'usage :**
- Condition de fin dans les boucles `while`
- Désactiver le bouton "Suivant" quand `EOF = True`

**Exemple de parcours classique :**
```pascal
SQLQuery1.First;  
while not SQLQuery1.EOF do  
begin  
  // Traiter l'enregistrement courant
  Memo1.Lines.Add(SQLQuery1.FieldByName('nom').AsString);

  SQLQuery1.Next;  // Important : avancer !
end;
```

### RecNo : Numéro de l'enregistrement courant

**Type :** `Integer`

**Signification :** Position de l'enregistrement courant (commence à 1).

```pascal
ShowMessage('Enregistrement n° ' + IntToStr(SQLQuery1.RecNo));
```

**Utilisation :**
```pascal
// Afficher la position
LabelPosition.Caption :=
  Format('Enregistrement %d sur %d',
         [SQLQuery1.RecNo, SQLQuery1.RecordCount]);
```

**Attention :**
- `RecNo` peut ne pas correspondre à l'ID de la base de données
- C'est juste la position dans le résultat de la requête
- Peut changer si vous triez ou filtrez

### RecordCount : Nombre total d'enregistrements

**Type :** `Integer`

**Signification :** Nombre total d'enregistrements dans le dataset.

```pascal
ShowMessage('Total : ' + IntToStr(SQLQuery1.RecordCount) + ' enregistrements');
```

**Utilisation :**
```pascal
if SQLQuery1.RecordCount = 0 then
  ShowMessage('Aucun enregistrement')
else
  ShowMessage(Format('%d enregistrement(s) trouvé(s)',
                     [SQLQuery1.RecordCount]));
```

**Note importante :**
Pour obtenir le compte exact avec certaines bases de données, vous devez parfois forcer le chargement complet :
```pascal
SQLQuery1.Last;   // Force le chargement complet  
SQLQuery1.First;  // Retour au début  
ShowMessage(IntToStr(SQLQuery1.RecordCount));  
```

## Navigation avec TDBNavigator

Le composant `TDBNavigator` fournit une interface complète de navigation.

### Les boutons de navigation

**⏮ First (nbFirst)**
- Va au premier enregistrement
- Équivalent à `SQLQuery1.First`

**◀ Prior (nbPrior)**
- Va à l'enregistrement précédent
- Équivalent à `SQLQuery1.Prior`
- Désactivé automatiquement si `BOF = True`

**▶ Next (nbNext)**
- Va à l'enregistrement suivant
- Équivalent à `SQLQuery1.Next`
- Désactivé automatiquement si `EOF = True`

**⏭ Last (nbLast)**
- Va au dernier enregistrement
- Équivalent à `SQLQuery1.Last`

### Personnaliser le Navigator

**Afficher uniquement les boutons de navigation :**
```pascal
DBNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];
```

**Afficher navigation + rafraîchissement :**
```pascal
DBNavigator1.VisibleButtons :=
  [nbFirst, nbPrior, nbNext, nbLast, nbRefresh];
```

**Avec des hints (infobulles) personnalisés :**
```pascal
DBNavigator1.Hints.Clear;  
DBNavigator1.Hints.Add('Premier enregistrement');  
DBNavigator1.Hints.Add('Enregistrement précédent');  
DBNavigator1.Hints.Add('Enregistrement suivant');  
DBNavigator1.Hints.Add('Dernier enregistrement');  
// ... et ainsi de suite pour chaque bouton
DBNavigator1.ShowHint := True;
```

## Navigation programmatique avancée

### MoveBy : Avancer/reculer de plusieurs enregistrements

```pascal
// Avancer de 5 enregistrements
SQLQuery1.MoveBy(5);

// Reculer de 3 enregistrements
SQLQuery1.MoveBy(-3);
```

**Exemple : pagination :**
```pascal
const
  LIGNES_PAR_PAGE = 10;

procedure TForm1.ButtonPageSuivanteClick(Sender: TObject);  
begin  
  SQLQuery1.MoveBy(LIGNES_PAR_PAGE);

  // Si on dépasse la fin, revenir au dernier
  if SQLQuery1.EOF then
    SQLQuery1.Last;
end;

procedure TForm1.ButtonPagePrecedenteClick(Sender: TObject);  
begin  
  SQLQuery1.MoveBy(-LIGNES_PAR_PAGE);

  // Si on dépasse le début, revenir au premier
  if SQLQuery1.BOF then
    SQLQuery1.First;
end;
```

### Locate : Rechercher et positionner

La méthode `Locate` permet de **rechercher** un enregistrement et de s'y positionner.

**Syntaxe :**
```pascal
function Locate(const KeyFields: string;
                const KeyValues: Variant;
                Options: TLocateOptions): Boolean;
```

**Paramètres :**
- `KeyFields` : nom du/des champ(s) à rechercher
- `KeyValues` : valeur(s) recherchée(s)
- `Options` : options de recherche

**Options disponibles :**
- `[]` : recherche exacte, sensible à la casse
- `[loCaseInsensitive]` : ignorer la casse (majuscules/minuscules)
- `[loPartialKey]` : recherche partielle (début de chaîne)

**Exemple 1 : Recherche par ID**
```pascal
procedure TForm1.ButtonChercherIDClick(Sender: TObject);  
var  
  ClientID: Integer;
begin
  ClientID := StrToInt(EditID.Text);

  if SQLQuery1.Locate('id', ClientID, []) then
    ShowMessage('Client trouvé !')
  else
    ShowMessage('Client non trouvé');
end;
```

**Exemple 2 : Recherche par nom (insensible à la casse)**
```pascal
procedure TForm1.ButtonChercherNomClick(Sender: TObject);  
var  
  Nom: string;
begin
  Nom := EditNom.Text;

  if SQLQuery1.Locate('nom', Nom, [loCaseInsensitive]) then
    ShowMessage('Client trouvé : ' + SQLQuery1.FieldByName('prenom').AsString)
  else
    ShowMessage('Aucun client nommé ' + Nom);
end;
```

**Exemple 3 : Recherche partielle**
```pascal
// Rechercher les noms commençant par "Dup"
if SQLQuery1.Locate('nom', 'Dup', [loCaseInsensitive, loPartialKey]) then
  ShowMessage('Trouvé : ' + SQLQuery1.FieldByName('nom').AsString);
```

**Exemple 4 : Recherche sur plusieurs champs**
```pascal
// Rechercher par nom ET prénom
if SQLQuery1.Locate('nom;prenom',
                    VarArrayOf(['Dupont', 'Pierre']),
                    [loCaseInsensitive]) then
  ShowMessage('Client trouvé !');
```

### Lookup : Rechercher sans changer de position

Contrairement à `Locate`, `Lookup` **ne change pas** la position du curseur. Elle retourne simplement une valeur.

**Syntaxe :**
```pascal
function Lookup(const KeyFields: string;
                const KeyValues: Variant;
                const ResultFields: string): Variant;
```

**Exemple :**
```pascal
// Obtenir l'email d'un client sans changer la position
var
  Email: Variant;
begin
  Email := SQLQuery1.Lookup('id', 5, 'email');

  if not VarIsNull(Email) then
    ShowMessage('Email du client 5 : ' + string(Email))
  else
    ShowMessage('Client non trouvé');
end;
```

## Signets (Bookmarks)

Les **signets** (bookmarks) permettent de **mémoriser** une position et d'y revenir plus tard.

### Créer et utiliser un signet

```pascal
var
  Signet: TBookmark;
begin
  // Mémoriser la position actuelle
  Signet := SQLQuery1.GetBookmark;

  try
    // Naviguer ailleurs
    SQLQuery1.First;
    // ... faire des choses ...
    SQLQuery1.Last;
    // ... faire d'autres choses ...

    // Revenir à la position mémorisée
    SQLQuery1.GotoBookmark(Signet);
    ShowMessage('Retour à l''enregistrement mémorisé !');
  finally
    // Important : libérer le signet
    SQLQuery1.FreeBookmark(Signet);
  end;
end;
```

### Cas d'usage typique

**Parcourir sans perdre la position :**
```pascal
procedure TForm1.ButtonCalculerTotalClick(Sender: TObject);  
var  
  Signet: TBookmark;
  Total: Double;
begin
  // Mémoriser où on est
  Signet := SQLQuery1.GetBookmark;

  try
    Total := 0;

    // Parcourir tous les enregistrements
    SQLQuery1.First;
    while not SQLQuery1.EOF do
    begin
      Total := Total + SQLQuery1.FieldByName('montant').AsFloat;
      SQLQuery1.Next;
    end;

    ShowMessage('Total : ' + FormatFloat('#,##0.00', Total) + ' €');

    // Revenir à l'enregistrement d'origine
    SQLQuery1.GotoBookmark(Signet);
  finally
    SQLQuery1.FreeBookmark(Signet);
  end;
end;
```

### Vérifier la validité d'un signet

```pascal
if SQLQuery1.BookmarkValid(Signet) then
  SQLQuery1.GotoBookmark(Signet)
else
  ShowMessage('Le signet n''est plus valide');
```

**Note :** Un signet devient invalide si :
- L'enregistrement a été supprimé
- Le dataset a été refermé
- La requête a changé

## Désactiver temporairement l'affichage

Lors de navigations intensives, les mises à jour visuelles peuvent ralentir. Utilisez `DisableControls` et `EnableControls`.

### DisableControls et EnableControls

```pascal
procedure TForm1.ButtonTraiterTousClick(Sender: TObject);  
begin  
  SQLQuery1.DisableControls;  // Geler l'affichage
  try
    SQLQuery1.First;
    while not SQLQuery1.EOF do
    begin
      // Traiter l'enregistrement
      // Les TDBEdit ne se mettent pas à jour = PLUS RAPIDE

      SQLQuery1.Next;
    end;
  finally
    SQLQuery1.EnableControls;  // Réactiver l'affichage
  end;
end;
```

**Avantages :**
- ✅ Performance : beaucoup plus rapide pour les traitements de masse
- ✅ Visuel : évite le clignotement des composants
- ✅ Expérience : pas de "flash" dérangeant pour l'utilisateur

**Exemple concret : export vers CSV**
```pascal
procedure TForm1.ButtonExporterCSVClick(Sender: TObject);  
var  
  Fichier: TextFile;
  Ligne: string;
begin
  AssignFile(Fichier, 'export.csv');
  Rewrite(Fichier);

  try
    // En-tête
    WriteLn(Fichier, 'ID;Nom;Prenom;Email');

    // Désactiver l'affichage pour la vitesse
    SQLQuery1.DisableControls;
    try
      SQLQuery1.First;
      while not SQLQuery1.EOF do
      begin
        Ligne := Format('%d;%s;%s;%s',
          [SQLQuery1.FieldByName('id').AsInteger,
           SQLQuery1.FieldByName('nom').AsString,
           SQLQuery1.FieldByName('prenom').AsString,
           SQLQuery1.FieldByName('email').AsString]);
        WriteLn(Fichier, Ligne);

        SQLQuery1.Next;
      end;
    finally
      SQLQuery1.EnableControls;
    end;

    CloseFile(Fichier);
    ShowMessage('Export terminé !');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

## Afficher la position courante

### Dans un Label

```pascal
procedure TForm1.SQLQuery1AfterScroll(DataSet: TDataSet);  
begin  
  if SQLQuery1.RecordCount > 0 then
    LabelPosition.Caption :=
      Format('Enregistrement %d / %d',
             [SQLQuery1.RecNo, SQLQuery1.RecordCount])
  else
    LabelPosition.Caption := 'Aucun enregistrement';
end;
```

**Note :** L'événement `AfterScroll` est déclenché chaque fois que le curseur se déplace.

### Dans une barre de progression

```pascal
procedure TForm1.SQLQuery1AfterScroll(DataSet: TDataSet);  
begin  
  if SQLQuery1.RecordCount > 0 then
  begin
    ProgressBar1.Max := SQLQuery1.RecordCount;
    ProgressBar1.Position := SQLQuery1.RecNo;
  end;
end;
```

### Avec un TrackBar (curseur)

```pascal
// Configuration initiale
procedure TForm1.SQLQuery1AfterOpen(DataSet: TDataSet);  
begin  
  TrackBar1.Min := 1;
  TrackBar1.Max := SQLQuery1.RecordCount;
  TrackBar1.Position := 1;
end;

// Synchroniser la position
procedure TForm1.SQLQuery1AfterScroll(DataSet: TDataSet);  
begin  
  TrackBar1.Position := SQLQuery1.RecNo;
end;

// Naviguer avec le TrackBar
procedure TForm1.TrackBar1Change(Sender: TObject);  
var  
  Deplacement: Integer;
begin
  Deplacement := TrackBar1.Position - SQLQuery1.RecNo;
  if Deplacement <> 0 then
    SQLQuery1.MoveBy(Deplacement);
end;
```

## Navigation par recherche incrémentale

Créez une recherche "au fur et à mesure de la frappe" :

```pascal
procedure TForm1.EditRechercheKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Texte: string;
begin
  Texte := Trim(EditRecherche.Text);

  if Texte <> '' then
  begin
    // Chercher dès le premier enregistrement
    SQLQuery1.First;

    // Localiser
    if not SQLQuery1.Locate('nom', Texte,
                            [loCaseInsensitive, loPartialKey]) then
    begin
      // Pas trouvé, retour au début
      SQLQuery1.First;
      LabelResultat.Caption := 'Non trouvé';
    end
    else
    begin
      LabelResultat.Caption := 'Trouvé !';
    end;
  end;
end;
```

## Navigation dans de grands datasets

### Problème de performance

Avec des milliers ou millions d'enregistrements, charger tout en mémoire est inefficace.

### Solution 1 : Limiter les résultats

```pascal
// Charger seulement les 100 premiers
SQLQuery1.SQL.Text := 'SELECT * FROM Clients ORDER BY nom LIMIT 100';  
SQLQuery1.Open;  
```

### Solution 2 : Pagination SQL

```pascal
const
  LIGNES_PAR_PAGE = 50;

var
  PageActuelle: Integer = 0;

procedure TForm1.ChargerPage(NumPage: Integer);  
begin  
  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'SELECT * FROM Clients ' +
    'ORDER BY nom ' +
    'LIMIT :limite OFFSET :offset';

  SQLQuery1.ParamByName('limite').AsInteger := LIGNES_PAR_PAGE;
  SQLQuery1.ParamByName('offset').AsInteger := NumPage * LIGNES_PAR_PAGE;

  SQLQuery1.Open;
  PageActuelle := NumPage;

  LabelPage.Caption := Format('Page %d', [PageActuelle + 1]);
end;

procedure TForm1.ButtonPageSuivanteClick(Sender: TObject);  
begin  
  ChargerPage(PageActuelle + 1);
end;

procedure TForm1.ButtonPagePrecedenteClick(Sender: TObject);  
begin  
  if PageActuelle > 0 then
    ChargerPage(PageActuelle - 1);
end;
```

### Solution 3 : Chargement à la demande

Certains datasets supportent le chargement progressif (lazy loading), où les données sont chargées par blocs au fur et à mesure de la navigation.

## Événements liés à la navigation

### AfterScroll

Déclenché **après** chaque déplacement du curseur.

```pascal
procedure TForm1.SQLQuery1AfterScroll(DataSet: TDataSet);  
begin  
  // Mise à jour de l'interface après navigation
  LabelPosition.Caption :=
    Format('Enregistrement %d / %d',
           [SQLQuery1.RecNo, SQLQuery1.RecordCount]);

  // Activer/désactiver des boutons
  ButtonPrecedent.Enabled := not SQLQuery1.BOF;
  ButtonSuivant.Enabled := not SQLQuery1.EOF;
end;
```

### BeforeScroll

Déclenché **avant** le déplacement (rare).

```pascal
procedure TForm1.SQLQuery1BeforeScroll(DataSet: TDataSet);  
begin  
  // Sauvegarder quelque chose avant de changer
end;
```

## Patterns de navigation courants

### Pattern 1 : Parcours complet avec traitement

```pascal
procedure TForm1.TraiterTousLesEnregistrements;  
begin  
  if not SQLQuery1.Active then Exit;

  SQLQuery1.DisableControls;
  try
    SQLQuery1.First;
    while not SQLQuery1.EOF do
    begin
      // Traiter l'enregistrement
      TraiterEnregistrement(SQLQuery1);

      SQLQuery1.Next;
    end;
  finally
    SQLQuery1.EnableControls;
  end;
end;
```

### Pattern 2 : Navigation sécurisée avec vérifications

```pascal
procedure TForm1.NaviguerVers(Direction: string);  
begin  
  if not SQLQuery1.Active then
  begin
    ShowMessage('Aucune donnée chargée');
    Exit;
  end;

  if SQLQuery1.IsEmpty then
  begin
    ShowMessage('Aucun enregistrement');
    Exit;
  end;

  case Direction of
    'First':
      SQLQuery1.First;
    'Prior':
      if not SQLQuery1.BOF then
        SQLQuery1.Prior;
    'Next':
      if not SQLQuery1.EOF then
        SQLQuery1.Next;
    'Last':
      SQLQuery1.Last;
  end;
end;
```

### Pattern 3 : Recherche avec retour à la position

```pascal
function TForm1.RechercherSansDeplacer(
  const Champ: string;
  const Valeur: Variant): Boolean;
var
  Signet: TBookmark;
begin
  Result := False;

  if not SQLQuery1.Active then Exit;

  Signet := SQLQuery1.GetBookmark;
  try
    Result := SQLQuery1.Locate(Champ, Valeur, [loCaseInsensitive]);

    // Si trouvé, afficher puis revenir
    if Result then
    begin
      ShowMessage('Trouvé : ' + SQLQuery1.FieldByName('nom').AsString);
      SQLQuery1.GotoBookmark(Signet);
    end;
  finally
    SQLQuery1.FreeBookmark(Signet);
  end;
end;
```

## Bonnes pratiques

### 1. Toujours vérifier si le dataset est actif

```pascal
if SQLQuery1.Active then
  SQLQuery1.First;
```

### 2. Vérifier BOF et EOF avant navigation

```pascal
// MAL
SQLQuery1.Prior;  // Peut causer une erreur si on est au début

// BIEN
if not SQLQuery1.BOF then
  SQLQuery1.Prior;
```

### 3. Utiliser DisableControls pour les traitements de masse

```pascal
SQLQuery1.DisableControls;  
try  
  // Traitement
finally
  SQLQuery1.EnableControls;
end;
```

### 4. Libérer les signets

```pascal
Signet := SQLQuery1.GetBookmark;  
try  
  // Utilisation
finally
  SQLQuery1.FreeBookmark(Signet);  // IMPORTANT
end;
```

### 5. Utiliser AfterScroll pour synchroniser l'interface

```pascal
procedure TForm1.SQLQuery1AfterScroll(DataSet: TDataSet);  
begin  
  // Mettre à jour tous les indicateurs de position
  MettreAJourInterface;
end;
```

### 6. Gérer les datasets vides

```pascal
if SQLQuery1.IsEmpty then  
begin  
  ShowMessage('Aucune donnée');
  Exit;
end;
```

### 7. Ne pas modifier pendant la navigation

```pascal
// MAL - Modifier en naviguant peut causer des problèmes
SQLQuery1.First;  
while not SQLQuery1.EOF do  
begin  
  SQLQuery1.Edit;  // Attention !
  SQLQuery1.FieldByName('statut').AsString := 'Traité';
  SQLQuery1.Post;
  SQLQuery1.Next;
end;

// BIEN - Utiliser une requête UPDATE
SQLQuery2.SQL.Text := 'UPDATE Clients SET statut = ''Traité''';  
SQLQuery2.ExecSQL;  
SQLTransaction1.Commit;  
SQLQuery1.Refresh;  
```

## Résumé

**Méthodes de navigation :**
- `First` : premier enregistrement
- `Last` : dernier enregistrement
- `Next` : suivant
- `Prior` : précédent
- `MoveBy(n)` : avancer/reculer de n

**Propriétés de position :**
- `BOF` : avant le premier
- `EOF` : après le dernier
- `RecNo` : numéro courant (1..n)
- `RecordCount` : total d'enregistrements

**Recherche :**
- `Locate` : chercher et se positionner
- `Lookup` : chercher sans se déplacer

**Signets :**
- `GetBookmark` : mémoriser la position
- `GotoBookmark` : revenir à la position
- `FreeBookmark` : libérer (important !)

**Performance :**
- `DisableControls` / `EnableControls` pour les traitements de masse
- Pagination pour les grands datasets
- LIMIT dans les requêtes SQL

**Événements :**
- `AfterScroll` : après chaque déplacement

La navigation est au cœur de toute application de gestion de données. Maîtriser ces techniques vous permet de créer des interfaces fluides et réactives !

---

*Naviguer dans les données devient une seconde nature avec la pratique !*

⏭️ [Ajout, modification, suppression](/16-bases-donnees-maitrise-approfondie/08-ajout-modification-suppression.md)
