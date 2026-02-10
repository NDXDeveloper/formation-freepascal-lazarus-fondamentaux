🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.6 Composants data-aware

## Introduction : l'interface automatique avec les données

Dans les sections précédentes, vous avez appris à récupérer des données avec `TSQLQuery` et à les afficher manuellement dans un `TMemo` en parcourant les enregistrements avec des boucles. Cette approche fonctionne, mais elle nécessite beaucoup de code.

Et s'il existait une façon **automatique** de lier vos composants visuels à la base de données, où l'affichage, la navigation et même la modification se feraient sans écrire de code ? C'est exactement ce que font les **composants data-aware** !

### L'analogie du tableau blanc magique

Imaginez un tableau blanc qui :
- Affiche automatiquement les informations d'un contact
- Se met à jour tout seul quand vous changez de contact
- Enregistre automatiquement vos modifications quand vous écrivez dessus
- Vous permet de naviguer entre les contacts avec des boutons

C'est ce que font les composants data-aware : ils sont **conscients des données** (data-aware) et se synchronisent automatiquement avec votre base de données.

## Qu'est-ce qu'un composant data-aware ?

Un composant **data-aware** (ou "sensible aux données") est un composant visuel qui :

1. **Se lie** automatiquement à une source de données
2. **Affiche** le contenu du champ de base de données correspondant
3. **Se met à jour** automatiquement quand on change d'enregistrement
4. **Envoie les modifications** à la base de données quand on modifie le contenu

**Exemples :**
- `TEdit` → `TDBEdit` (champ de saisie lié à la base)
- `TLabel` → `TDBLabel` (étiquette affichant une valeur de la base)
- `TMemo` → `TDBMemo` (zone de texte liée à la base)
- `TImage` → `TDBImage` (image stockée dans la base)

## Le composant central : TDataSource

Pour comprendre les composants data-aware, il faut d'abord comprendre `TDataSource`.

### Le rôle de TDataSource

`TDataSource` est le **pont** entre vos données (`TSQLQuery`) et vos composants visuels.

```
┌──────────────┐
│  TSQLQuery   │  ← Récupère les données de la base
└──────┬───────┘
       │
┌──────▼───────┐
│ TDataSource  │  ← Pont/Intermédiaire
└──────┬───────┘
       │
   ┌───┴─────┬────────┬──────────┐
   │         │        │          │
┌──▼───┐  ┌──▼────┐ ┌──▼────┐ ┌──▼──────┐
│DBEdit│  │DBLabel│ │DBMemo │ │DBGrid   │
└──────┘  └───────┘ └───────┘ └─────────┘
    ↑        ↑        ↑         ↑
Composants data-aware (affichent/modifient)
```

**Pourquoi un intermédiaire ?**

Au lieu de connecter chaque composant directement à `TSQLQuery`, on les connecte tous à un seul `TDataSource`, qui lui-même est connecté à `TSQLQuery`. Cela simplifie l'architecture et permet de changer facilement la source de données.

### Ajouter TDataSource

1. Palette **Data Access**, sélectionnez `TDataSource`
2. Placez-le sur votre formulaire (composant non visuel)
3. Dans l'Inspecteur d'objets, configurez :
   - **DataSet** : sélectionnez votre `SQLQuery1`

```pascal
// Ou en code :
DataSource1.DataSet := SQLQuery1;
```

C'est tout ! Votre `DataSource` est maintenant connecté à votre requête.

## Les composants data-aware principaux

Voyons les composants les plus utilisés. Tous se trouvent dans la palette **Data Controls**.

### 1. TDBEdit : Champ de saisie

**Usage :** Afficher et modifier un champ texte de la base de données.

**Propriétés importantes :**

**DataSource**
- Le `TDataSource` auquel se connecter
```pascal
DBEdit1.DataSource := DataSource1;
```

**DataField**
- Le nom du champ de la base de données à afficher
- Sélectionnez dans la liste déroulante (si requête ouverte au design)
```pascal
DBEdit1.DataField := 'nom';
```

**ReadOnly**
- `True` : lecture seule
- `False` : modifiable (par défaut)

**Exemple :**
```pascal
// Configuration au design ou en code
DBEditNom.DataSource := DataSource1;  
DBEditNom.DataField := 'nom';  

DBEditPrenom.DataSource := DataSource1;  
DBEditPrenom.DataField := 'prenom';  

DBEditEmail.DataSource := DataSource1;  
DBEditEmail.DataField := 'email';  
```

Une fois configurés, ces champs afficheront **automatiquement** les données de l'enregistrement courant !

### 2. TDBLabel : Étiquette

**Usage :** Afficher une valeur en lecture seule.

**Propriétés :**
- `DataSource`
- `DataField`

Exactement comme `TDBEdit`, mais non modifiable. Utile pour afficher des IDs, des dates calculées, etc.

```pascal
DBLabelID.DataSource := DataSource1;  
DBLabelID.DataField := 'id';  
```

### 3. TDBMemo : Zone de texte multi-lignes

**Usage :** Afficher et modifier des textes longs (descriptions, commentaires, notes).

**Propriétés :**
- `DataSource`
- `DataField`

```pascal
DBMemo1.DataSource := DataSource1;  
DBMemo1.DataField := 'description';  
```

Parfait pour les champs TEXT de grande taille dans SQLite.

### 4. TDBCheckBox : Case à cocher

**Usage :** Afficher et modifier des valeurs booléennes (vrai/faux, oui/non).

**Propriétés :**
- `DataSource`
- `DataField`
- `ValueChecked` : valeur quand coché (défaut : `1`)
- `ValueUnchecked` : valeur quand décoché (défaut : `0`)

```pascal
DBCheckBox1.DataSource := DataSource1;  
DBCheckBox1.DataField := 'actif';  
DBCheckBox1.ValueChecked := '1';  
DBCheckBox1.ValueUnchecked := '0';  
```

### 5. TDBComboBox : Liste déroulante

**Usage :** Sélectionner une valeur parmi une liste prédéfinie.

**Propriétés :**
- `DataSource`
- `DataField`
- `Items` : liste des valeurs possibles

```pascal
DBComboBox1.DataSource := DataSource1;  
DBComboBox1.DataField := 'statut';  
DBComboBox1.Items.Clear;  
DBComboBox1.Items.Add('Actif');  
DBComboBox1.Items.Add('Inactif');  
DBComboBox1.Items.Add('En attente');  
```

### 6. TDBImage : Affichage d'image

**Usage :** Afficher et modifier des images stockées dans la base (champs BLOB).

**Propriétés :**
- `DataSource`
- `DataField`

```pascal
DBImage1.DataSource := DataSource1;  
DBImage1.DataField := 'photo';  
```

### 7. TDBGrid : Grille de données

**Le plus puissant !** Affiche tous les enregistrements sous forme de tableau.

**Propriétés importantes :**

**DataSource**
```pascal
DBGrid1.DataSource := DataSource1;
```

**Options** (TDBGridOptions)
- `dgTitles` : afficher les en-têtes de colonnes
- `dgIndicator` : afficher l'indicateur de ligne courante
- `dgColumnResize` : permettre le redimensionnement des colonnes
- `dgColLines` : afficher les lignes verticales
- `dgRowLines` : afficher les lignes horizontales
- `dgTabs` : navigation avec Tab
- `dgRowSelect` : sélectionner toute la ligne (recommandé)
- `dgAlwaysShowSelection` : garder la sélection visible
- `dgConfirmDelete` : demander confirmation avant suppression
- `dgCancelOnExit` : annuler les modifications en sortant

**Configuration recommandée :**
```pascal
DBGrid1.DataSource := DataSource1;  
DBGrid1.Options := [dgTitles, dgIndicator, dgColumnResize,  
                    dgColLines, dgRowLines, dgRowSelect,
                    dgAlwaysShowSelection, dgConfirmDelete];
```

**Personnaliser les colonnes :**

Clic droit sur le `DBGrid` → "Modifier les colonnes..."

Pour chaque colonne, vous pouvez définir :
- `Title.Caption` : Titre affiché
- `FieldName` : Champ de la base
- `Width` : Largeur en pixels
- `ReadOnly` : Lecture seule
- `Visible` : Visibilité

### 8. TDBNavigator : Barre de navigation

**Le composant magique !** Fournit des boutons pour naviguer et modifier les données.

**Boutons disponibles :**
- ⏮ `nbFirst` : Premier enregistrement
- ◀ `nbPrior` : Enregistrement précédent
- ▶ `nbNext` : Enregistrement suivant
- ⏭ `nbLast` : Dernier enregistrement
- ➕ `nbInsert` : Insérer un nouvel enregistrement
- 🗑 `nbDelete` : Supprimer l'enregistrement courant
- ✏️ `nbEdit` : Passer en mode édition
- ✔️ `nbPost` : Valider les modifications
- ✖️ `nbCancel` : Annuler les modifications
- 🔄 `nbRefresh` : Rafraîchir les données

**Configuration :**
```pascal
DBNavigator1.DataSource := DataSource1;
```

**Personnaliser les boutons visibles :**
```pascal
// Afficher uniquement certains boutons
DBNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbRefresh];
```

**Masquer un bouton spécifique :**
```pascal
// Masquer le bouton Supprimer (trop dangereux !)
DBNavigator1.VisibleButtons := DBNavigator1.VisibleButtons - [nbDelete];
```

## Exemple complet : Formulaire de gestion de contacts

Créons une application complète avec interface data-aware.

### Étape 1 : Préparer les composants non visuels

Sur votre formulaire, ajoutez :
- `TSQLite3Connection` → `SQLite3Connection1`
- `TSQLTransaction` → `SQLTransaction1`
- `TSQLQuery` → `SQLQuery1`
- `TDataSource` → `DataSource1`

**Configuration :**
```pascal
// Connexion
SQLite3Connection1.DatabaseName := 'contacts.db';  
SQLite3Connection1.Transaction := SQLTransaction1;  

// Transaction
SQLTransaction1.Database := SQLite3Connection1;

// Requête
SQLQuery1.Database := SQLite3Connection1;  
SQLQuery1.SQL.Text := 'SELECT * FROM Contacts ORDER BY nom';  

// DataSource (IMPORTANT !)
DataSource1.DataSet := SQLQuery1;
```

### Étape 2 : Ajouter les composants visuels

**Palette Data Controls :**
- `TDBNavigator` → `DBNavigator1`
- `TDBEdit` (×4) → `DBEditID`, `DBEditNom`, `DBEditPrenom`, `DBEditEmail`
- `TDBGrid` → `DBGrid1`

**Palette Standard :**
- `TLabel` (×4) → Pour les libellés des champs

**Layout du formulaire :**
```
┌───────────────────────────────────┐
│ [DBNavigator1]  ⏮ ◀ ▶ ⏭ ➕ 🗑 ✔️  │
├───────────────────────────────────┤
│ ID:      [DBEditID      ]         │
│ Nom:     [DBEditNom     ]         │
│ Prénom:  [DBEditPrenom  ]         │
│ Email:   [DBEditEmail   ]         │
├───────────────────────────────────┤
│ ┌────────────────────────────┐    │
│ │      DBGrid1               │    │
│ │ ID  Nom    Prénom   Email  │    │
│ │ 1   Dupont Pierre   p@...  │    │
│ │ 2   Martin Marie    m@...  │    │
│ │ ...                        │    │
│ └────────────────────────────┘    │
└───────────────────────────────────┘
```

### Étape 3 : Configurer les composants data-aware

**DBNavigator1 :**
```pascal
DBNavigator1.DataSource := DataSource1;
```

**DBEditID :**
```pascal
DBEditID.DataSource := DataSource1;  
DBEditID.DataField := 'id';  
DBEditID.ReadOnly := True;  // L'ID ne se modifie pas  
```

**DBEditNom :**
```pascal
DBEditNom.DataSource := DataSource1;  
DBEditNom.DataField := 'nom';  
```

**DBEditPrenom :**
```pascal
DBEditPrenom.DataSource := DataSource1;  
DBEditPrenom.DataField := 'prenom';  
```

**DBEditEmail :**
```pascal
DBEditEmail.DataSource := DataSource1;  
DBEditEmail.DataField := 'email';  
```

**DBGrid1 :**
```pascal
DBGrid1.DataSource := DataSource1;  
DBGrid1.Options := [dgTitles, dgIndicator, dgColumnResize,  
                    dgColLines, dgRowLines, dgRowSelect,
                    dgAlwaysShowSelection];
```

### Étape 4 : Code d'initialisation

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  try
    // Configurer la connexion
    SQLite3Connection1.DatabaseName := 'contacts.db';
    SQLite3Connection1.CharSet := 'UTF8';
    SQLite3Connection1.Transaction := SQLTransaction1;
    SQLite3Connection1.Params.Add('foreign_keys=ON');

    SQLTransaction1.Database := SQLite3Connection1;
    SQLQuery1.Database := SQLite3Connection1;

    // Ouvrir la connexion
    SQLite3Connection1.Open;

    // Créer la table si nécessaire
    SQLQuery1.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS Contacts (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  nom TEXT NOT NULL,' +
      '  prenom TEXT,' +
      '  email TEXT' +
      ')';
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    // Charger les données
    SQLQuery1.SQL.Text := 'SELECT * FROM Contacts ORDER BY nom';
    SQLQuery1.Open;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur d''initialisation : ' + E.Message);
      Application.Terminate;
    end;
  end;
end;
```

### Étape 5 : C'est tout !

**Oui, vraiment !** Vous avez maintenant une application complète qui permet de :
- ✅ Naviguer entre les contacts (avec les boutons du `DBNavigator`)
- ✅ Ajouter un contact (bouton ➕)
- ✅ Modifier un contact (modifier directement dans les champs)
- ✅ Supprimer un contact (bouton 🗑)
- ✅ Voir tous les contacts dans la grille
- ✅ Sélectionner un contact en cliquant dans la grille

**Tout cela SANS écrire de code de navigation ou de modification !**

## Le cycle de vie des modifications

Comprendre comment les modifications sont gérées est important.

### Mode navigation vs mode édition

Le dataset (`TSQLQuery`) peut être dans deux états :

**Mode navigation (dsBrowse)**
- Vous naviguez entre les enregistrements
- Pas de modifications en cours

**Mode édition (dsEdit)**
- Vous modifiez l'enregistrement courant
- Les modifications ne sont pas encore enregistrées

### États du dataset

```pascal
// Vérifier l'état
case SQLQuery1.State of
  dsBrowse: ShowMessage('Mode navigation');
  dsEdit:   ShowMessage('Mode édition');
  dsInsert: ShowMessage('Mode insertion');
end;
```

### Le processus de modification

1. **L'utilisateur clique dans un `TDBEdit`**
   - Le dataset passe automatiquement en mode `dsEdit`

2. **L'utilisateur modifie le texte**
   - Les modifications sont stockées temporairement

3. **L'utilisateur quitte le champ ou clique sur ✔️ (Post)**
   - Le dataset valide les modifications
   - État : retour à `dsBrowse`
   - Les données sont envoyées à la base

4. **L'utilisateur clique sur ✖️ (Cancel)**
   - Les modifications sont annulées
   - Retour aux valeurs originales

### Valider manuellement avec Post

```pascal
procedure TForm1.ButtonEnregistrerClick(Sender: TObject);  
begin  
  if SQLQuery1.State in [dsEdit, dsInsert] then
  begin
    try
      SQLQuery1.Post;  // Valider les modifications
      SQLTransaction1.Commit;  // Enregistrer dans la base
      ShowMessage('Modifications enregistrées');
    except
      on E: Exception do
      begin
        SQLTransaction1.Rollback;
        ShowMessage('Erreur : ' + E.Message);
      end;
    end;
  end;
end;
```

### Annuler manuellement avec Cancel

```pascal
procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  if SQLQuery1.State in [dsEdit, dsInsert] then
  begin
    SQLQuery1.Cancel;  // Annuler les modifications
    ShowMessage('Modifications annulées');
  end;
end;
```

## Insertion de nouveaux enregistrements

### Avec le DBNavigator

Le bouton ➕ du `DBNavigator` :
1. Ajoute une ligne vide
2. Passe en mode `dsInsert`
3. Vous pouvez remplir les champs
4. Cliquez ✔️ pour valider

### Manuellement en code

```pascal
procedure TForm1.ButtonNouveauClick(Sender: TObject);  
begin  
  SQLQuery1.Insert;  // Passer en mode insertion
  // Les champs data-aware sont maintenant prêts à recevoir des données
end;
```

### Avec valeurs par défaut

```pascal
procedure TForm1.ButtonNouveauClientClick(Sender: TObject);  
begin  
  SQLQuery1.Insert;

  // Définir des valeurs par défaut
  SQLQuery1.FieldByName('statut').AsString := 'Actif';
  SQLQuery1.FieldByName('date_creation').AsDateTime := Now;

  // L'utilisateur remplit le reste dans les TDBEdit
end;
```

## Suppression d'enregistrements

### Avec le DBNavigator

Le bouton 🗑 :
- Demande confirmation (si `dgConfirmDelete` activé)
- Supprime l'enregistrement courant

### Manuellement en code

```pascal
procedure TForm1.ButtonSupprimerClick(Sender: TObject);  
begin  
  if MessageDlg('Confirmer la suppression ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      SQLQuery1.Delete;  // Supprime l'enregistrement courant
      SQLTransaction1.Commit;
    except
      on E: Exception do
      begin
        SQLTransaction1.Rollback;
        ShowMessage('Erreur : ' + E.Message);
      end;
    end;
  end;
end;
```

## Rafraîchir les données

### Avec le DBNavigator

Le bouton 🔄 rafraîchit automatiquement.

### Manuellement

```pascal
procedure TForm1.ButtonRafraichirClick(Sender: TObject);  
begin  
  SQLQuery1.Refresh;  // Recharge depuis la base
end;
```

### Après une modification externe

Si vous modifiez les données avec une autre requête SQL, rafraîchissez :

```pascal
// Modification avec une autre requête
SQLQuery2.SQL.Text := 'UPDATE Clients SET statut = ''VIP'' WHERE id = 5';  
SQLQuery2.ExecSQL;  
SQLTransaction1.Commit;  

// Rafraîchir l'affichage
SQLQuery1.Refresh;
```

## Événements utiles

Les composants data-aware déclenchent des événements que vous pouvez intercepter.

### Événements de TSQLQuery (Dataset)

**BeforePost** : Avant de valider les modifications
```pascal
procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
begin  
  // Valider les données avant enregistrement
  if Trim(DataSet.FieldByName('nom').AsString) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    Abort;  // Annule le Post
  end;

  // Définir une valeur automatiquement
  if DataSet.FieldByName('date_creation').IsNull then
    DataSet.FieldByName('date_creation').AsDateTime := Now;
end;
```

**AfterPost** : Après validation réussie
```pascal
procedure TForm1.SQLQuery1AfterPost(DataSet: TDataSet);  
begin  
  SQLTransaction1.Commit;  // Enregistrer dans la base
  ShowMessage('Enregistrement réussi');
end;
```

**BeforeDelete** : Avant suppression
```pascal
procedure TForm1.SQLQuery1BeforeDelete(DataSet: TDataSet);  
begin  
  if DataSet.FieldByName('statut').AsString = 'VIP' then
  begin
    if MessageDlg('Ce client est VIP. Confirmer ?',
                  mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Abort;  // Annule la suppression
  end;
end;
```

**AfterDelete** : Après suppression
```pascal
procedure TForm1.SQLQuery1AfterDelete(DataSet: TDataSet);  
begin  
  SQLTransaction1.Commit;
end;
```

**OnNewRecord** : Nouvel enregistrement créé
```pascal
procedure TForm1.SQLQuery1NewRecord(DataSet: TDataSet);  
begin  
  // Valeurs par défaut pour un nouveau client
  DataSet.FieldByName('statut').AsString := 'Nouveau';
  DataSet.FieldByName('date_creation').AsDateTime := Now;
  DataSet.FieldByName('actif').AsInteger := 1;
end;
```

### Événements de TDBGrid

**OnCellClick** : Cellule cliquée
```pascal
procedure TForm1.DBGrid1CellClick(Column: TColumn);  
begin  
  ShowMessage('Colonne cliquée : ' + Column.Title.Caption);
end;
```

**OnDblClick** : Double-clic sur la grille
```pascal
procedure TForm1.DBGrid1DblClick(Sender: TObject);  
begin  
  // Ouvrir un formulaire de détails
  FormDetails.ChargerClient(SQLQuery1.FieldByName('id').AsInteger);
  FormDetails.ShowModal;
end;
```

## Personnaliser le DBGrid

### Formater l'affichage

Utilisez l'événement `OnDrawColumnCell` pour personnaliser :

```pascal
procedure TForm1.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  // Colorer les clients VIP en or
  if Column.FieldName = 'statut' then
  begin
    if Column.Field.AsString = 'VIP' then
    begin
      DBGrid1.Canvas.Brush.Color := clYellow;
      DBGrid1.Canvas.FillRect(Rect);
      DBGrid1.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, Column.Field.AsString);
    end;
  end;
end;
```

### Colonnes calculées

Ajoutez une colonne qui n'existe pas dans la base :

```pascal
procedure TForm1.SQLQuery1CalcFields(DataSet: TDataSet);  
var  
  NomComplet: TStringField;
begin
  // Créer un champ calculé "nom_complet"
  if DataSet.FindField('nom_complet') = nil then
  begin
    NomComplet := TStringField.Create(DataSet);
    NomComplet.FieldName := 'nom_complet';
    NomComplet.Size := 100;
    NomComplet.FieldKind := fkCalculated;
    NomComplet.DataSet := DataSet;
  end;

  // Calculer la valeur
  DataSet.FieldByName('nom_complet').AsString :=
    DataSet.FieldByName('prenom').AsString + ' ' +
    DataSet.FieldByName('nom').AsString;
end;
```

## Filtrer les données affichées

### Avec une clause WHERE dans le SQL

```pascal
SQLQuery1.Close;  
SQLQuery1.SQL.Text :=  
  'SELECT * FROM Clients WHERE ville = :ville ORDER BY nom';
SQLQuery1.ParamByName('ville').AsString := 'Paris';  
SQLQuery1.Open;  
```

### Avec un filtre sur le dataset

```pascal
// Activer le filtrage
SQLQuery1.Filtered := True;  
SQLQuery1.Filter := 'ville = ''Paris''';  

// Désactiver
SQLQuery1.Filtered := False;
```

### Exemple avec une zone de recherche

```pascal
procedure TForm1.EditRechercheChange(Sender: TObject);  
var  
  Texte: string;
begin
  Texte := Trim(EditRecherche.Text);

  if Texte = '' then
  begin
    // Pas de filtre
    SQLQuery1.Filtered := False;
  end
  else
  begin
    // Appliquer le filtre
    SQLQuery1.Filter := 'nom LIKE ''%' + Texte + '%''';
    SQLQuery1.Filtered := True;
  end;
end;
```

## Tri des données

### Dans le SQL

```pascal
SQLQuery1.Close;  
SQLQuery1.SQL.Text := 'SELECT * FROM Clients ORDER BY nom ASC';  
SQLQuery1.Open;  
```

### En cliquant sur les colonnes du DBGrid

```pascal
procedure TForm1.DBGrid1TitleClick(Column: TColumn);  
begin  
  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'SELECT * FROM Clients ORDER BY ' + Column.FieldName;
  SQLQuery1.Open;
end;
```

### Avec inversion du tri

```pascal
var
  FTriCroissant: Boolean = True;

procedure TForm1.DBGrid1TitleClick(Column: TColumn);  
var  
  Ordre: string;
begin
  // Inverser le sens
  FTriCroissant := not FTriCroissant;

  if FTriCroissant then
    Ordre := 'ASC'
  else
    Ordre := 'DESC';

  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'SELECT * FROM Clients ORDER BY ' + Column.FieldName + ' ' + Ordre;
  SQLQuery1.Open;
end;
```

## Bonnes pratiques

### 1. Toujours utiliser TDataSource

Ne connectez jamais directement les composants à `TSQLQuery`. Passez toujours par `TDataSource`.

```pascal
// MAL (impossible de toute façon)
// DBEdit1.DataSet := SQLQuery1;  // N'existe pas !

// BIEN
DataSource1.DataSet := SQLQuery1;  
DBEdit1.DataSource := DataSource1;  
```

### 2. Gérer les transactions dans les événements

```pascal
procedure TForm1.SQLQuery1AfterPost(DataSet: TDataSet);  
begin  
  try
    SQLTransaction1.Commit;
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      raise;
    end;
  end;
end;
```

### 3. Valider les données dans BeforePost

```pascal
procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
begin  
  // Vérifier les champs obligatoires
  if Trim(DataSet.FieldByName('email').AsString) = '' then
  begin
    ShowMessage('L''email est obligatoire');
    Abort;
  end;

  // Vérifier le format
  if Pos('@', DataSet.FieldByName('email').AsString) = 0 then
  begin
    ShowMessage('Email invalide');
    Abort;
  end;
end;
```

### 4. Désactiver certains boutons du Navigator

```pascal
// Masquer Supprimer et Édition manuelle
DBNavigator1.VisibleButtons :=
  [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbRefresh];
```

### 5. Protéger les champs sensibles

```pascal
DBEditID.ReadOnly := True;  // L'ID ne se modifie jamais  
DBEditDateCreation.ReadOnly := True;  // Date de création fixe  
```

### 6. Rafraîchir après modifications externes

```pascal
// Après une modification hors data-aware
SQLQuery2.ExecSQL;  
SQLTransaction1.Commit;  
SQLQuery1.Refresh;  // Important !  
```

## Limitations et pièges

### Limitation 1 : Une seule requête à la fois

Un `TDataSource` ne peut être connecté qu'à **un seul** dataset à la fois.

Si vous avez plusieurs tables à afficher, utilisez plusieurs `TDataSource` et plusieurs `TSQLQuery`.

### Limitation 2 : Requêtes complexes

Les modifications automatiques (INSERT/UPDATE/DELETE via data-aware) ne fonctionnent bien qu'avec des requêtes simples :

```sql
-- Fonctionne bien
SELECT * FROM Clients;

-- Peut poser problème
SELECT C.*, V.nom AS ville_nom  
FROM Clients C  
LEFT JOIN Villes V ON C.id_ville = V.id;  
```

Pour les requêtes complexes, désactivez les modifications :
```pascal
SQLQuery1.ReadOnly := True;
```

### Piège 3 : Oublier le Commit

Les modifications ne sont **pas automatiquement enregistrées** dans la base. Utilisez les événements `AfterPost` et `AfterDelete` pour faire `Commit`.

## Résumé

**Composants data-aware = Interface automatique**
- Affichage automatique
- Navigation automatique
- Modification automatique

**Le pont : TDataSource**
- Connecte TSQLQuery aux composants visuels
- Un DataSource par requête

**Composants principaux :**
- `TDBEdit`, `TDBLabel`, `TDBMemo` : champs simples
- `TDBGrid` : tableau de tous les enregistrements
- `TDBNavigator` : boutons de navigation/modification

**Cycle de vie :**
- `dsBrowse` → `dsEdit`/`dsInsert` → `Post` ou `Cancel`
- Toujours faire `Commit` après modifications

**Événements utiles :**
- `BeforePost` : validation
- `AfterPost` : commit
- `BeforeDelete` : confirmation
- `OnNewRecord` : valeurs par défaut

Les composants data-aware transforment radicalement le développement d'applications base de données en éliminant le code répétitif !

---

*Avec les composants data-aware, vos interfaces deviennent vivantes et réactives automatiquement !*

⏭️ [Navigation dans les données](/16-bases-donnees-maitrise-approfondie/07-navigation-donnees.md)
