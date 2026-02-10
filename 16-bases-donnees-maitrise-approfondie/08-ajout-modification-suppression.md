🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.8 Ajout, modification, suppression

## Introduction : les opérations CRUD

Dans toute application de gestion de données, quatre opérations fondamentales reviennent constamment. On les appelle **CRUD** :

- **C**reate : **Créer** (ajouter) de nouveaux enregistrements
- **R**ead : **Lire** (consulter) les données existantes
- **U**pdate : **Mettre à jour** (modifier) les données
- **D**elete : **Supprimer** des enregistrements

Vous avez déjà vu le **R** (Read) dans les sections précédentes. Maintenant, nous allons nous concentrer sur les trois autres : **Create**, **Update** et **Delete**.

### L'analogie du cahier

Imaginez votre base de données comme un **cahier** :

- **Ajouter** = écrire une nouvelle page
- **Modifier** = corriger/compléter une page existante
- **Supprimer** = arracher une page

Dans les trois cas, vous devez :
1. **Dire** que vous voulez faire quelque chose
2. **Faire** les modifications
3. **Valider** pour que ce soit permanent

C'est exactement le même processus avec les datasets !

## Les états du dataset

Votre dataset (`TSQLQuery`) peut être dans différents **états** qui déterminent ce que vous pouvez faire.

### Les trois états principaux

**1. dsBrowse (Navigation)**
- État par défaut
- Vous naviguez entre les enregistrements
- Aucune modification en cours
- Vous pouvez lire les données

**2. dsEdit (Modification)**
- Vous modifiez un enregistrement existant
- Les changements ne sont pas encore enregistrés
- Vous pouvez annuler (Cancel) ou valider (Post)

**3. dsInsert (Insertion)**
- Vous ajoutez un nouvel enregistrement
- L'enregistrement n'existe pas encore dans la base
- Vous pouvez annuler (Cancel) ou valider (Post)

### Vérifier l'état actuel

```pascal
case SQLQuery1.State of
  dsBrowse:  ShowMessage('Mode navigation');
  dsEdit:    ShowMessage('Mode modification');
  dsInsert:  ShowMessage('Mode insertion');
  dsInactive: ShowMessage('Dataset fermé');
end;
```

### Le cycle de vie d'une modification

```
┌─────────────────────────────────────────────────────┐
│                    dsBrowse                         │
│                  (Navigation)                       │
└────────┬──────────────────────────┬─────────────────┘
         │                          │
         │ Insert/Append            │ Edit
         ↓                          ↓
    ┌─────────┐               ┌──────────┐
    │dsInsert │               │ dsEdit   │
    │(Ajout)  │               │(Modif)   │
    └────┬────┘               └─────┬────┘
         │                          │
         │ Post/Cancel              │ Post/Cancel
         ↓                          ↓
    ┌──────────────────────────────────┐
    │         dsBrowse                 │
    │   (Retour à la navigation)       │
    └──────────────────────────────────┘
```

## Ajouter un nouvel enregistrement

Il existe deux méthodes principales pour ajouter un enregistrement : `Insert` et `Append`.

### Insert : Insérer avant l'enregistrement courant

```pascal
SQLQuery1.Insert;
```

**Effet :**
- Crée un nouvel enregistrement vide
- Le positionne **avant** l'enregistrement courant
- Passe en mode `dsInsert`
- Les composants data-aware se vident (prêts à recevoir les données)

**Exemple :**
```pascal
procedure TForm1.ButtonNouveauClick(Sender: TObject);  
begin  
  SQLQuery1.Insert;
  // Les TDBEdit sont maintenant vides et prêts à être remplis
  DBEditNom.SetFocus;  // Mettre le focus sur le premier champ
end;
```

### Append : Ajouter après le dernier enregistrement

```pascal
SQLQuery1.Append;
```

**Effet :**
- Crée un nouvel enregistrement vide
- Le positionne **après** le dernier enregistrement
- Passe en mode `dsInsert`
- Les composants data-aware se vident

**Différence avec Insert :**
- `Insert` : ajoute à la position courante
- `Append` : ajoute toujours à la fin

**En pratique :** Avec une base de données SQL ordonnée par ID, `Insert` et `Append` ont le même effet visuel. Utilisez celui qui vous semble le plus logique.

### Remplir les champs

Une fois en mode `dsInsert`, deux façons de remplir les données :

**Méthode 1 : L'utilisateur saisit dans les TDBEdit**
```pascal
SQLQuery1.Insert;
// L'utilisateur tape dans les champs
// Puis clique sur un bouton "Enregistrer"
```

**Méthode 2 : Vous définissez les valeurs en code**
```pascal
SQLQuery1.Insert;  
SQLQuery1.FieldByName('nom').AsString := 'Dupont';  
SQLQuery1.FieldByName('prenom').AsString := 'Pierre';  
SQLQuery1.FieldByName('email').AsString := 'pierre@email.fr';  
SQLQuery1.Post;  // Valider immédiatement  
```

### Post : Valider l'ajout

```pascal
SQLQuery1.Post;
```

**Effet :**
- Valide les données saisies
- Envoie l'INSERT à la base de données
- Retourne en mode `dsBrowse`
- L'enregistrement est maintenant dans la base

**Exemple complet :**
```pascal
procedure TForm1.ButtonEnregistrerClick(Sender: TObject);  
begin  
  if SQLQuery1.State in [dsInsert, dsEdit] then
  begin
    try
      SQLQuery1.Post;  // Valider
      SQLTransaction1.Commit;  // Enregistrer dans la base
      ShowMessage('Enregistrement réussi');
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

### Cancel : Annuler l'ajout

```pascal
SQLQuery1.Cancel;
```

**Effet :**
- Annule toutes les modifications
- Retourne en mode `dsBrowse`
- Aucune donnée n'est envoyée à la base
- L'enregistrement en cours de création est abandonné

**Exemple :**
```pascal
procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  if SQLQuery1.State in [dsInsert, dsEdit] then
  begin
    if MessageDlg('Annuler les modifications ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      SQLQuery1.Cancel;
      ShowMessage('Modifications annulées');
    end;
  end;
end;
```

### Exemple complet : Formulaire d'ajout

```pascal
procedure TForm1.ButtonNouveauClientClick(Sender: TObject);  
begin  
  try
    // Passer en mode insertion
    SQLQuery1.Insert;

    // Définir des valeurs par défaut (optionnel)
    SQLQuery1.FieldByName('date_creation').AsDateTime := Now;
    SQLQuery1.FieldByName('statut').AsString := 'Nouveau';
    SQLQuery1.FieldByName('actif').AsInteger := 1;

    // Activer les champs de saisie
    PanelSaisie.Enabled := True;

    // Mettre le focus sur le premier champ
    DBEditNom.SetFocus;

  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

procedure TForm1.ButtonValiderClick(Sender: TObject);  
begin  
  if SQLQuery1.State in [dsInsert, dsEdit] then
  begin
    try
      // Valider
      SQLQuery1.Post;
      SQLTransaction1.Commit;

      ShowMessage('Client enregistré avec succès');

      // Désactiver les champs de saisie
      PanelSaisie.Enabled := False;

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

## Modifier un enregistrement existant

### Edit : Passer en mode modification

```pascal
SQLQuery1.Edit;
```

**Effet :**
- Passe en mode `dsEdit`
- L'enregistrement courant devient modifiable
- Les composants data-aware permettent la saisie
- Aucune modification n'est encore envoyée à la base

**Exemple :**
```pascal
procedure TForm1.ButtonModifierClick(Sender: TObject);  
begin  
  if SQLQuery1.IsEmpty then
  begin
    ShowMessage('Aucun enregistrement à modifier');
    Exit;
  end;

  SQLQuery1.Edit;
  // L'utilisateur peut maintenant modifier dans les TDBEdit
  DBEditNom.SetFocus;
end;
```

### Modification automatique

**Important :** Avec les composants data-aware, vous n'avez **pas toujours besoin** d'appeler `Edit` !

Quand l'utilisateur **clique dans un TDBEdit** et commence à taper, le dataset passe **automatiquement** en mode `dsEdit`.

```pascal
// L'utilisateur clique dans DBEditNom
// → SQLQuery1.State devient automatiquement dsEdit

// L'utilisateur modifie le texte
// → Les modifications sont temporaires

// L'utilisateur quitte le champ ou clique sur Post
// → SQLQuery1.Post est appelé automatiquement
```

### Post : Valider la modification

Identique à l'ajout :

```pascal
SQLQuery1.Post;  
SQLTransaction1.Commit;  
```

### Cancel : Annuler la modification

Identique à l'ajout :

```pascal
SQLQuery1.Cancel;
```

**Effet :** Les valeurs d'origine sont restaurées.

### Exemple complet : Modification avec validation

```pascal
procedure TForm1.ButtonModifierClientClick(Sender: TObject);  
begin  
  if SQLQuery1.IsEmpty then
  begin
    ShowMessage('Sélectionnez un client à modifier');
    Exit;
  end;

  // Mémoriser les valeurs originales
  FNomOriginal := SQLQuery1.FieldByName('nom').AsString;

  // Passer en mode édition
  SQLQuery1.Edit;

  // Activer les champs
  PanelSaisie.Enabled := True;
  DBEditNom.SetFocus;
end;

procedure TForm1.ButtonValiderModificationClick(Sender: TObject);  
begin  
  if SQLQuery1.State = dsEdit then
  begin
    try
      // Vérifier si quelque chose a changé
      if SQLQuery1.FieldByName('nom').AsString = FNomOriginal then
      begin
        ShowMessage('Aucune modification détectée');
        SQLQuery1.Cancel;
        Exit;
      end;

      // Valider
      SQLQuery1.Post;
      SQLTransaction1.Commit;

      ShowMessage('Modifications enregistrées');
      PanelSaisie.Enabled := False;

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

## Supprimer un enregistrement

### Delete : Supprimer l'enregistrement courant

```pascal
SQLQuery1.Delete;
```

**Effet :**
- Supprime **immédiatement** l'enregistrement courant
- Pas besoin de `Post` après `Delete`
- Le curseur se déplace sur l'enregistrement suivant
- **Attention :** L'opération est définitive après `Commit` !

### Suppression sécurisée avec confirmation

**TOUJOURS** demander confirmation avant de supprimer :

```pascal
procedure TForm1.ButtonSupprimerClick(Sender: TObject);  
var  
  NomClient: string;
begin
  if SQLQuery1.IsEmpty then
  begin
    ShowMessage('Aucun enregistrement à supprimer');
    Exit;
  end;

  // Mémoriser le nom pour le message
  NomClient := SQLQuery1.FieldByName('nom').AsString;

  // Demander confirmation
  if MessageDlg(Format('Supprimer le client "%s" ?', [NomClient]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      SQLQuery1.Delete;
      SQLTransaction1.Commit;
      ShowMessage('Client supprimé');
    except
      on E: Exception do
      begin
        SQLTransaction1.Rollback;
        ShowMessage('Erreur lors de la suppression : ' + E.Message);
      end;
    end;
  end;
end;
```

### Supprimer plusieurs enregistrements

```pascal
procedure TForm1.ButtonSupprimerInactifsClick(Sender: TObject);  
var  
  Compteur: Integer;
begin
  if MessageDlg('Supprimer tous les clients inactifs ?',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    Compteur := 0;

    try
      SQLQuery1.DisableControls;
      try
        SQLQuery1.First;
        while not SQLQuery1.EOF do
        begin
          if SQLQuery1.FieldByName('actif').AsInteger = 0 then
          begin
            SQLQuery1.Delete;  // Delete avance automatiquement
            Inc(Compteur);
          end
          else
            SQLQuery1.Next;  // Avancer seulement si pas de suppression
        end;
      finally
        SQLQuery1.EnableControls;
      end;

      SQLTransaction1.Commit;
      ShowMessage(Format('%d client(s) supprimé(s)', [Compteur]));

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

**Note importante :** `Delete` avance automatiquement au suivant, donc **ne faites pas `Next`** après un `Delete` !

### Alternative : suppression SQL directe

Pour supprimer beaucoup d'enregistrements, utilisez une requête SQL directe :

```pascal
procedure TForm1.SupprimerInactifsSQL;  
var  
  NbSupprimes: Integer;
begin
  try
    // Utiliser une requête auxiliaire
    SQLQuery2.SQL.Text := 'DELETE FROM Clients WHERE actif = 0';
    SQLQuery2.ExecSQL;

    // Récupérer le nombre de lignes affectées
    NbSupprimes := SQLQuery2.RowsAffected;

    SQLTransaction1.Commit;

    // Rafraîchir l'affichage
    SQLQuery1.Refresh;

    ShowMessage(Format('%d client(s) supprimé(s)', [NbSupprimes]));

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

**Avantages :**
- ✅ Beaucoup plus rapide
- ✅ Plus efficace pour les suppressions de masse
- ✅ Une seule opération SQL

## Validation des données

### Validation avant enregistrement (BeforePost)

L'événement `BeforePost` est le meilleur endroit pour valider vos données :

```pascal
procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
var  
  Nom, Email: string;
begin
  // Récupérer les valeurs
  Nom := Trim(DataSet.FieldByName('nom').AsString);
  Email := Trim(DataSet.FieldByName('email').AsString);

  // Validation : nom obligatoire
  if Nom = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    DBEditNom.SetFocus;
    Abort;  // Annule le Post
  end;

  // Validation : longueur minimale
  if Length(Nom) < 2 then
  begin
    ShowMessage('Le nom doit contenir au moins 2 caractères');
    DBEditNom.SetFocus;
    Abort;
  end;

  // Validation : format email
  if (Email <> '') and (Pos('@', Email) = 0) then
  begin
    ShowMessage('Format d''email invalide');
    DBEditEmail.SetFocus;
    Abort;
  end;

  // Validation : email unique
  if VerifierEmailExiste(Email, DataSet.FieldByName('id').AsInteger) then
  begin
    ShowMessage('Cet email est déjà utilisé');
    DBEditEmail.SetFocus;
    Abort;
  end;
end;

function TForm1.VerifierEmailExiste(const Email: string; IDActuel: Integer): Boolean;  
begin  
  Result := False;

  if Email = '' then Exit;

  try
    SQLQuery2.Close;
    SQLQuery2.SQL.Text :=
      'SELECT COUNT(*) AS nb FROM Clients ' +
      'WHERE email = :email AND id <> :id';
    SQLQuery2.ParamByName('email').AsString := Email;
    SQLQuery2.ParamByName('id').AsInteger := IDActuel;
    SQLQuery2.Open;

    Result := (SQLQuery2.FieldByName('nb').AsInteger > 0);

    SQLQuery2.Close;
  except
    Result := False;
  end;
end;
```

### Valeurs par défaut (OnNewRecord)

L'événement `OnNewRecord` permet de définir des valeurs par défaut lors d'un ajout :

```pascal
procedure TForm1.SQLQuery1NewRecord(DataSet: TDataSet);  
begin  
  // Valeurs par défaut pour un nouveau client
  DataSet.FieldByName('date_creation').AsDateTime := Now;
  DataSet.FieldByName('statut').AsString := 'Nouveau';
  DataSet.FieldByName('actif').AsInteger := 1;
  DataSet.FieldByName('points_fidelite').AsInteger := 0;

  // Générer un code client unique
  DataSet.FieldByName('code').AsString := GenererCodeClient;
end;

function TForm1.GenererCodeClient: string;  
var  
  Annee: string;
  Numero: Integer;
begin
  Annee := FormatDateTime('yy', Now);

  // Trouver le prochain numéro
  SQLQuery2.Close;
  SQLQuery2.SQL.Text :=
    'SELECT MAX(CAST(SUBSTR(code, 3) AS INTEGER)) AS max_num ' +
    'FROM Clients WHERE code LIKE :prefix';
  SQLQuery2.ParamByName('prefix').AsString := Annee + '%';
  SQLQuery2.Open;

  if SQLQuery2.FieldByName('max_num').IsNull then
    Numero := 1
  else
    Numero := SQLQuery2.FieldByName('max_num').AsInteger + 1;

  Result := Format('%s%4.4d', [Annee, Numero]);  // Ex: 25001, 25002...

  SQLQuery2.Close;
end;
```

### Normalisation des données (BeforePost)

Vous pouvez aussi normaliser les données avant l'enregistrement :

```pascal
procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
begin  
  // Mettre en majuscules
  DataSet.FieldByName('nom').AsString :=
    UpperCase(Trim(DataSet.FieldByName('nom').AsString));

  // Nettoyer les espaces multiples
  DataSet.FieldByName('prenom').AsString :=
    NettoyerEspaces(DataSet.FieldByName('prenom').AsString);

  // Formater le téléphone
  DataSet.FieldByName('telephone').AsString :=
    FormaterTelephone(DataSet.FieldByName('telephone').AsString);
end;

function TForm1.NettoyerEspaces(const Texte: string): string;  
var  
  i: Integer;
begin
  Result := Trim(Texte);

  // Remplacer les espaces multiples par un seul
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

function TForm1.FormaterTelephone(const Tel: string): string;  
var  
  i: Integer;
begin
  // Retirer tous les caractères non numériques
  Result := '';
  for i := 1 to Length(Tel) do
    if Tel[i] in ['0'..'9'] then
      Result := Result + Tel[i];

  // Formater en 06 12 34 56 78 si 10 chiffres
  if Length(Result) = 10 then
    Result := Copy(Result, 1, 2) + ' ' + Copy(Result, 3, 2) + ' ' +
              Copy(Result, 5, 2) + ' ' + Copy(Result, 7, 2) + ' ' +
              Copy(Result, 9, 2);
end;
```

## Événements liés aux modifications

### BeforeInsert et AfterInsert

```pascal
procedure TForm1.SQLQuery1BeforeInsert(DataSet: TDataSet);  
begin  
  // Avant de passer en mode insertion
  ShowMessage('Préparation de l''ajout...');
end;

procedure TForm1.SQLQuery1AfterInsert(DataSet: TDataSet);  
begin  
  // Après être passé en mode insertion
  LabelMode.Caption := 'Mode : AJOUT';
  LabelMode.Font.Color := clGreen;
end;
```

### BeforeEdit et AfterEdit

```pascal
procedure TForm1.SQLQuery1BeforeEdit(DataSet: TDataSet);  
begin  
  // Avant de passer en mode édition
  // Vérifier les permissions
  if not UtilisateurPeutModifier then
  begin
    ShowMessage('Vous n''avez pas les droits de modification');
    Abort;
  end;
end;

procedure TForm1.SQLQuery1AfterEdit(DataSet: TDataSet);  
begin  
  // Après être passé en mode édition
  LabelMode.Caption := 'Mode : MODIFICATION';
  LabelMode.Font.Color := clBlue;
end;
```

### BeforePost et AfterPost

```pascal
procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
begin  
  // Validation des données (voir exemples précédents)
  // C'est ici qu'on vérifie que tout est correct
end;

procedure TForm1.SQLQuery1AfterPost(DataSet: TDataSet);  
begin  
  // Après validation réussie
  try
    SQLTransaction1.Commit;  // Enregistrer dans la base
    ShowMessage('Enregistrement réussi');
    LabelMode.Caption := 'Mode : NAVIGATION';
    LabelMode.Font.Color := clBlack;
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      raise;
    end;
  end;
end;
```

### BeforeDelete et AfterDelete

```pascal
procedure TForm1.SQLQuery1BeforeDelete(DataSet: TDataSet);  
var  
  NbCommandes: Integer;
begin
  // Vérifier les dépendances avant suppression
  SQLQuery2.Close;
  SQLQuery2.SQL.Text :=
    'SELECT COUNT(*) AS nb FROM Commandes WHERE id_client = :id';
  SQLQuery2.ParamByName('id').AsInteger :=
    DataSet.FieldByName('id').AsInteger;
  SQLQuery2.Open;

  NbCommandes := SQLQuery2.FieldByName('nb').AsInteger;
  SQLQuery2.Close;

  if NbCommandes > 0 then
  begin
    if MessageDlg(
      Format('Ce client a %d commande(s). Supprimer quand même ?',
             [NbCommandes]),
      mtWarning, [mbYes, mbNo], 0) <> mrYes then
    begin
      Abort;  // Annuler la suppression
    end;
  end;
end;

procedure TForm1.SQLQuery1AfterDelete(DataSet: TDataSet);  
begin  
  // Après suppression réussie
  try
    SQLTransaction1.Commit;
    ShowMessage('Suppression effectuée');
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      raise;
    end;
  end;
end;
```

### BeforeCancel et AfterCancel

```pascal
procedure TForm1.SQLQuery1BeforeCancel(DataSet: TDataSet);  
begin  
  // Avant d'annuler
  // Dernier avertissement ?
end;

procedure TForm1.SQLQuery1AfterCancel(DataSet: TDataSet);  
begin  
  // Après annulation
  ShowMessage('Modifications annulées');
  LabelMode.Caption := 'Mode : NAVIGATION';
end;
```

## Gestion des erreurs

### Erreurs courantes et solutions

#### Erreur : Violation de contrainte NOT NULL

```pascal
// Erreur si le champ est vide
SQLQuery1.FieldByName('nom').AsString := '';  
SQLQuery1.Post;  // → Erreur : nom is not null  

// Solution : validation dans BeforePost
if Trim(DataSet.FieldByName('nom').AsString) = '' then  
begin  
  ShowMessage('Le nom est obligatoire');
  Abort;
end;
```

#### Erreur : Violation de contrainte UNIQUE

```pascal
// Erreur si l'email existe déjà
SQLQuery1.FieldByName('email').AsString := 'existant@email.fr';  
SQLQuery1.Post;  // → Erreur : UNIQUE constraint failed  

// Solution : vérifier avant d'enregistrer
if EmailExiste(Email) then  
begin  
  ShowMessage('Cet email est déjà utilisé');
  Abort;
end;
```

#### Erreur : Violation de clé étrangère

```pascal
// Erreur si l'ID référencé n'existe pas
SQLQuery1.FieldByName('id_ville').AsInteger := 999;  
SQLQuery1.Post;  // → Erreur : FOREIGN KEY constraint failed  

// Solution : vérifier ou utiliser un TDBLookupComboBox
```

### Try-Except robuste

```pascal
procedure TForm1.EnregistrerModifications;  
begin  
  if not (SQLQuery1.State in [dsEdit, dsInsert]) then Exit;

  try
    // Tenter d'enregistrer
    SQLQuery1.Post;

    try
      SQLTransaction1.Commit;
      ShowMessage('Enregistrement réussi');
    except
      on E: EDatabaseError do
      begin
        SQLTransaction1.Rollback;

        // Analyser le type d'erreur
        if Pos('UNIQUE', E.Message) > 0 then
          ShowMessage('Cette valeur existe déjà')
        else if Pos('NOT NULL', E.Message) > 0 then
          ShowMessage('Certains champs obligatoires sont vides')
        else if Pos('FOREIGN KEY', E.Message) > 0 then
          ShowMessage('Référence invalide')
        else
          ShowMessage('Erreur de base de données : ' + E.Message);
      end;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur : ' + E.Message);
      // Rester en mode édition pour permettre la correction
    end;
  end;
end;
```

## Interface utilisateur adaptée

### Activer/désactiver les boutons selon le mode

```pascal
procedure TForm1.SQLQuery1AfterScroll(DataSet: TDataSet);  
begin  
  MettreAJourBoutons;
end;

procedure TForm1.MettreAJourBoutons;  
var  
  EnEdition: Boolean;
  ADesEnregistrements: Boolean;
begin
  EnEdition := SQLQuery1.State in [dsEdit, dsInsert];
  ADesEnregistrements := not SQLQuery1.IsEmpty;

  // Navigation
  DBNavigator1.Enabled := not EnEdition;

  // Boutons d'action
  ButtonNouveau.Enabled := not EnEdition;
  ButtonModifier.Enabled := (not EnEdition) and ADesEnregistrements;
  ButtonSupprimer.Enabled := (not EnEdition) and ADesEnregistrements;

  // Boutons de validation
  ButtonValider.Enabled := EnEdition;
  ButtonAnnuler.Enabled := EnEdition;

  // Champs de saisie
  PanelSaisie.Enabled := EnEdition or ADesEnregistrements;
end;
```

### Indicateur visuel du mode

```pascal
procedure TForm1.ActualiserIndicateurMode;  
begin  
  case SQLQuery1.State of
    dsBrowse:
    begin
      LabelMode.Caption := 'NAVIGATION';
      LabelMode.Font.Color := clBlack;
      PanelMode.Color := clWhite;
    end;
    dsEdit:
    begin
      LabelMode.Caption := 'MODIFICATION EN COURS';
      LabelMode.Font.Color := clBlue;
      PanelMode.Color := $00FFE4C4;  // Bleu clair
    end;
    dsInsert:
    begin
      LabelMode.Caption := 'AJOUT EN COURS';
      LabelMode.Font.Color := clGreen;
      PanelMode.Color := $00E4FFE4;  // Vert clair
    end;
  end;
end;
```

## Bonnes pratiques

### 1. Toujours valider dans BeforePost

```pascal
procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
begin  
  // Validation obligatoire ici
  if not DonneesValides(DataSet) then
    Abort;
end;
```

### 2. Toujours faire Commit après Post

```pascal
procedure TForm1.SQLQuery1AfterPost(DataSet: TDataSet);  
begin  
  try
    SQLTransaction1.Commit;
  except
    SQLTransaction1.Rollback;
    raise;
  end;
end;
```

### 3. Demander confirmation pour les suppressions

```pascal
if MessageDlg('Confirmer la suppression ?',
              mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  SQLQuery1.Delete;
```

### 4. Gérer les erreurs de contraintes

```pascal
try
  SQLQuery1.Post;
except
  on E: EDatabaseError do
  begin
    // Analyser et expliquer l'erreur
    ExplquerErreurBDD(E.Message);
    // Rester en mode édition
  end;
end;
```

### 5. Vérifier l'état avant d'agir

```pascal
if SQLQuery1.State in [dsEdit, dsInsert] then
  SQLQuery1.Post;
```

### 6. Utiliser OnNewRecord pour les valeurs par défaut

```pascal
procedure TForm1.SQLQuery1NewRecord(DataSet: TDataSet);  
begin  
  DataSet.FieldByName('date_creation').AsDateTime := Now;
  DataSet.FieldByName('actif').AsInteger := 1;
end;
```

### 7. Normaliser les données

```pascal
procedure TForm1.SQLQuery1BeforePost(DataSet: TDataSet);  
begin  
  // Nettoyage automatique
  DataSet.FieldByName('nom').AsString :=
    UpperCase(Trim(DataSet.FieldByName('nom').AsString));
end;
```

### 8. Désactiver les contrôles pendant les traitements de masse

```pascal
SQLQuery1.DisableControls;  
try  
  // Modifications multiples
finally
  SQLQuery1.EnableControls;
end;
```

## Résumé

**Ajouter :**
- `Insert` ou `Append` : passer en mode `dsInsert`
- Remplir les champs (automatiquement avec TDBEdit ou manuellement)
- `Post` : valider et enregistrer
- `Cancel` : annuler

**Modifier :**
- `Edit` : passer en mode `dsEdit` (ou automatique avec TDBEdit)
- Modifier les valeurs
- `Post` : valider et enregistrer
- `Cancel` : annuler

**Supprimer :**
- `Delete` : suppression immédiate
- Toujours demander confirmation
- Vérifier les dépendances dans `BeforeDelete`

**Événements clés :**
- `OnNewRecord` : valeurs par défaut
- `BeforePost` : validation
- `AfterPost` : commit
- `BeforeDelete` : vérifications
- `AfterDelete` : commit

**Toujours :**
- ✅ Valider les données
- ✅ Faire `Commit` après `Post` et `Delete`
- ✅ Gérer les erreurs avec try-except
- ✅ Demander confirmation pour les suppressions
- ✅ Donner un feedback utilisateur clair

Maîtriser ces opérations CRUD vous permet de créer des applications de gestion complètes et professionnelles !

---

*Créer, Lire, Modifier, Supprimer : les quatre piliers de toute application de gestion !*

⏭️ [Transactions basics](/16-bases-donnees-maitrise-approfondie/09-transactions-basics.md)
