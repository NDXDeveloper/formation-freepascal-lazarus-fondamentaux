🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.5 TSQLQuery et requêtes

## Introduction : parler à la base de données

Dans la section précédente, vous avez appris à **connecter** votre application à une base de données SQLite. Maintenant, nous allons voir comment **communiquer** avec elle : poser des questions, ajouter des données, les modifier ou les supprimer.

### L'analogie du messager

Si `TSQLite3Connection` est le téléphone qui vous relie à la base de données, alors `TSQLQuery` est le **messager** qui :

1. Prend votre requête SQL
2. L'envoie à la base de données
3. Attend la réponse
4. Vous rapporte les résultats

C'est votre principal outil de travail pour manipuler les données depuis Pascal !

## Qu'est-ce que TSQLQuery ?

`TSQLQuery` est un composant qui permet d'**exécuter des requêtes SQL** et de **récupérer les résultats** sous forme de dataset (jeu de données).

**Caractéristiques principales :**
- Exécute des requêtes SELECT, INSERT, UPDATE, DELETE
- Retourne les résultats comme un tableau de données
- Permet de naviguer entre les enregistrements
- Supporte les paramètres (requêtes préparées)
- Peut être lié à des composants visuels

## Ajouter TSQLQuery à votre projet

### Étape 1 : Ajouter le composant

1. Dans la palette **SQLdb**, cliquez sur `TSQLQuery`
2. Cliquez sur votre formulaire pour le placer
3. Il apparaîtra comme une icône non visuelle

### Étape 2 : Configuration de base

Dans l'Inspecteur d'objets, configurez ces propriétés :

**Database**
- Sélectionnez votre `SQLite3Connection1`
- Lie la requête à la connexion

**Transaction** (optionnel mais recommandé)
- Sélectionnez votre `SQLTransaction1`
- Permet une meilleure gestion des transactions

**SQL** (TStringList)
- C'est ici que vous écrivez votre requête SQL
- Double-cliquez pour ouvrir l'éditeur de texte

## Les deux types d'opérations SQL

TSQLQuery gère deux types d'opérations SQL différents :

### 1. Requêtes de sélection (SELECT)

Retournent des données que vous pouvez parcourir.

```sql
SELECT * FROM Clients;  
SELECT nom, prenom FROM Clients WHERE ville = 'Paris';  
```

**Méthode à utiliser :** `Open`

### 2. Requêtes d'action (INSERT, UPDATE, DELETE)

Modifient les données mais ne retournent pas de résultats.

```sql
INSERT INTO Clients (nom, prenom) VALUES ('Dupont', 'Pierre');  
UPDATE Clients SET email = 'nouveau@email.fr' WHERE id = 1;  
DELETE FROM Clients WHERE id = 5;  
```

**Méthode à utiliser :** `ExecSQL`

## Exécuter une requête SELECT

### Exemple complet avec un bouton

Ajoutez un `TMemo` (Memo1) et un `TButton` (Button1) sur votre formulaire.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  // Effacer le mémo
  Memo1.Clear;

  try
    // Définir la requête SQL
    SQLQuery1.SQL.Text := 'SELECT * FROM Clients';

    // Exécuter la requête (ouvre le dataset)
    SQLQuery1.Open;

    // Parcourir les résultats
    while not SQLQuery1.EOF do
    begin
      Memo1.Lines.Add(
        'ID: ' + SQLQuery1.FieldByName('id').AsString +
        ' - Nom: ' + SQLQuery1.FieldByName('nom').AsString +
        ' - Email: ' + SQLQuery1.FieldByName('email').AsString
      );

      SQLQuery1.Next;  // Passer à l'enregistrement suivant
    end;

    // Fermer la requête
    SQLQuery1.Close;

  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### Décomposition du code

#### 1. Définir la requête

```pascal
SQLQuery1.SQL.Text := 'SELECT * FROM Clients';
```

`SQL` est une propriété `TStringList`. Vous pouvez aussi faire :

```pascal
SQLQuery1.SQL.Clear;  
SQLQuery1.SQL.Add('SELECT nom, prenom, email');  
SQLQuery1.SQL.Add('FROM Clients');  
SQLQuery1.SQL.Add('WHERE ville = ''Paris''');  
// Note : '' = apostrophe échappée en Pascal
```

#### 2. Exécuter avec Open

```pascal
SQLQuery1.Open;
```

Cette méthode :
- Envoie la requête à la base de données
- Récupère les résultats
- Positionne le curseur sur le **premier** enregistrement

#### 3. Parcourir les résultats

```pascal
while not SQLQuery1.EOF do  
begin  
  // Lire les données
  // ...

  SQLQuery1.Next;  // Important : passer au suivant !
end;
```

**EOF** (End Of File) : `True` quand on a atteint la fin des résultats.

#### 4. Accéder aux champs

Plusieurs méthodes :

**Par nom (recommandé) :**
```pascal
SQLQuery1.FieldByName('nom').AsString  
SQLQuery1.FieldByName('age').AsInteger  
SQLQuery1.FieldByName('prix').AsFloat  
SQLQuery1.FieldByName('actif').AsBoolean  
```

**Par index (plus rapide mais moins lisible) :**
```pascal
SQLQuery1.Fields[0].AsString  // Premier champ  
SQLQuery1.Fields[1].AsString  // Deuxième champ  
```

**Conversion automatique :**
- `AsString` : convertit tout en texte
- `AsInteger` : convertit en entier
- `AsFloat` : convertit en nombre décimal
- `AsBoolean` : convertit en booléen
- `AsDateTime` : convertit en date/heure

#### 5. Fermer la requête

```pascal
SQLQuery1.Close;
```

Toujours fermer quand vous avez fini ! Cela libère les ressources.

## Navigation dans les résultats

### Méthodes de navigation

**Next** : Aller à l'enregistrement suivant
```pascal
SQLQuery1.Next;
```

**Prior** : Aller à l'enregistrement précédent
```pascal
SQLQuery1.Prior;
```

**First** : Aller au premier enregistrement
```pascal
SQLQuery1.First;
```

**Last** : Aller au dernier enregistrement
```pascal
SQLQuery1.Last;
```

### Propriétés de position

**EOF** : Fin du dataset atteinte ?
```pascal
if SQLQuery1.EOF then
  ShowMessage('Fin des données');
```

**BOF** : Début du dataset atteint ?
```pascal
if SQLQuery1.BOF then
  ShowMessage('Début des données');
```

**RecNo** : Numéro de l'enregistrement courant
```pascal
ShowMessage('Enregistrement n° ' + IntToStr(SQLQuery1.RecNo));
```

**RecordCount** : Nombre total d'enregistrements
```pascal
ShowMessage('Total : ' + IntToStr(SQLQuery1.RecordCount) + ' enregistrements');
```

### Exemple de navigation

```pascal
procedure TForm1.Button2Click(Sender: TObject);  
begin  
  SQLQuery1.SQL.Text := 'SELECT * FROM Clients';
  SQLQuery1.Open;

  // Aller au premier
  SQLQuery1.First;
  ShowMessage('Premier : ' + SQLQuery1.FieldByName('nom').AsString);

  // Aller au dernier
  SQLQuery1.Last;
  ShowMessage('Dernier : ' + SQLQuery1.FieldByName('nom').AsString);

  // Afficher le nombre total
  ShowMessage('Total : ' + IntToStr(SQLQuery1.RecordCount));

  SQLQuery1.Close;
end;
```

## Exécuter des requêtes d'action (INSERT, UPDATE, DELETE)

Pour les requêtes qui **modifient** les données, utilisez `ExecSQL` au lieu de `Open`.

### INSERT : Ajouter des données

```pascal
procedure TForm1.AjouterClient;  
begin  
  try
    SQLQuery1.SQL.Text :=
      'INSERT INTO Clients (nom, prenom, email) ' +
      'VALUES (''Dupont'', ''Pierre'', ''pierre@email.fr'')';

    SQLQuery1.ExecSQL;  // Pas Open, mais ExecSQL !

    // Valider la transaction
    SQLTransaction1.Commit;

    ShowMessage('Client ajouté avec succès');
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;  // Annuler en cas d'erreur
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

**Important :**
- `ExecSQL` ne retourne pas de données
- Toujours faire `Commit` pour valider
- Utiliser `Rollback` si erreur

### UPDATE : Modifier des données

```pascal
procedure TForm1.ModifierClient;  
begin  
  try
    SQLQuery1.SQL.Text :=
      'UPDATE Clients ' +
      'SET email = ''nouveau@email.fr'' ' +
      'WHERE id = 1';

    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    ShowMessage('Client modifié');
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

### DELETE : Supprimer des données

```pascal
procedure TForm1.SupprimerClient;  
begin  
  if MessageDlg('Confirmer la suppression ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      SQLQuery1.SQL.Text := 'DELETE FROM Clients WHERE id = 5';

      SQLQuery1.ExecSQL;
      SQLTransaction1.Commit;

      ShowMessage('Client supprimé');
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

**Toujours demander confirmation avant de supprimer !**

## Les paramètres : requêtes sécurisées

### Le problème des valeurs codées en dur

Imaginez que vous voulez rechercher un client par son nom saisi dans un `TEdit` :

```pascal
// MAUVAISE pratique (dangereuse !)
SQLQuery1.SQL.Text :=
  'SELECT * FROM Clients WHERE nom = ''' + Edit1.Text + '''';
```

**Problèmes :**
1. **Injection SQL** : un utilisateur malveillant peut insérer du SQL
2. **Erreurs de syntaxe** : si le nom contient une apostrophe (O'Brien)
3. **Performance** : la requête n'est pas précompilée

### La solution : les paramètres

Les **paramètres** sont des espaces réservés (placeholders) dans votre SQL, marqués par `:` :

```pascal
// BONNE pratique (sécurisée)
SQLQuery1.SQL.Text := 'SELECT * FROM Clients WHERE nom = :nom';  
SQLQuery1.ParamByName('nom').AsString := Edit1.Text;  
SQLQuery1.Open;  
```

### Syntaxe des paramètres

```pascal
// Définir la requête avec paramètres
SQLQuery1.SQL.Text :=
  'SELECT * FROM Clients ' +
  'WHERE nom = :nom AND ville = :ville';

// Affecter les valeurs
SQLQuery1.ParamByName('nom').AsString := 'Dupont';  
SQLQuery1.ParamByName('ville').AsString := 'Paris';  

// Exécuter
SQLQuery1.Open;
```

### Types de paramètres

```pascal
// String
SQLQuery1.ParamByName('nom').AsString := 'Dupont';

// Integer
SQLQuery1.ParamByName('age').AsInteger := 25;

// Float
SQLQuery1.ParamByName('prix').AsFloat := 19.99;

// Date/Time
SQLQuery1.ParamByName('date').AsDateTime := Now;

// Boolean
SQLQuery1.ParamByName('actif').AsBoolean := True;

// NULL
SQLQuery1.ParamByName('telephone').Clear;  // Valeur NULL
```

### Exemple complet avec recherche

```pascal
procedure TForm1.ButtonRechercherClick(Sender: TObject);  
var  
  NomRecherche: string;
begin
  NomRecherche := EditNom.Text;

  // Vérifier que le champ n'est pas vide
  if Trim(NomRecherche) = '' then
  begin
    ShowMessage('Veuillez saisir un nom');
    Exit;
  end;

  try
    // Requête avec paramètre
    SQLQuery1.SQL.Text :=
      'SELECT id, nom, prenom, email ' +
      'FROM Clients ' +
      'WHERE nom LIKE :nom ' +
      'ORDER BY nom';

    // Affecter le paramètre (avec joker %)
    SQLQuery1.ParamByName('nom').AsString := '%' + NomRecherche + '%';

    // Exécuter
    SQLQuery1.Open;

    // Afficher le nombre de résultats
    LabelResultats.Caption :=
      IntToStr(SQLQuery1.RecordCount) + ' résultat(s) trouvé(s)';

  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### Exemple INSERT avec paramètres

```pascal
procedure TForm1.ButtonAjouterClick(Sender: TObject);  
begin  
  try
    SQLQuery1.SQL.Text :=
      'INSERT INTO Clients (nom, prenom, email, telephone) ' +
      'VALUES (:nom, :prenom, :email, :telephone)';

    // Affecter les paramètres depuis les champs du formulaire
    SQLQuery1.ParamByName('nom').AsString := EditNom.Text;
    SQLQuery1.ParamByName('prenom').AsString := EditPrenom.Text;
    SQLQuery1.ParamByName('email').AsString := EditEmail.Text;

    // Téléphone optionnel
    if Trim(EditTelephone.Text) <> '' then
      SQLQuery1.ParamByName('telephone').AsString := EditTelephone.Text
    else
      SQLQuery1.ParamByName('telephone').Clear;  // NULL

    // Exécuter
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    ShowMessage('Client ajouté avec succès');

    // Vider les champs
    EditNom.Clear;
    EditPrenom.Clear;
    EditEmail.Clear;
    EditTelephone.Clear;

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

### Exemple UPDATE avec paramètres

```pascal
procedure TForm1.ButtonModifierClick(Sender: TObject);  
var  
  ClientID: Integer;
begin
  // Supposons que vous avez l'ID du client à modifier
  ClientID := StrToInt(EditID.Text);

  try
    SQLQuery1.SQL.Text :=
      'UPDATE Clients ' +
      'SET nom = :nom, prenom = :prenom, email = :email ' +
      'WHERE id = :id';

    SQLQuery1.ParamByName('nom').AsString := EditNom.Text;
    SQLQuery1.ParamByName('prenom').AsString := EditPrenom.Text;
    SQLQuery1.ParamByName('email').AsString := EditEmail.Text;
    SQLQuery1.ParamByName('id').AsInteger := ClientID;

    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    ShowMessage('Client modifié avec succès');
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

## Propriétés importantes de TSQLQuery

### Active (Boolean)

Indique si la requête est ouverte (dataset actif).

```pascal
if SQLQuery1.Active then
  ShowMessage('Requête ouverte')
else
  ShowMessage('Requête fermée');
```

### SQL (TStringList)

Contient la requête SQL.

```pascal
// Méthode 1 : tout en une ligne
SQLQuery1.SQL.Text := 'SELECT * FROM Clients';

// Méthode 2 : ligne par ligne (plus lisible)
SQLQuery1.SQL.Clear;  
SQLQuery1.SQL.Add('SELECT nom, prenom, email');  
SQLQuery1.SQL.Add('FROM Clients');  
SQLQuery1.SQL.Add('WHERE ville = :ville');  
```

### Database et Transaction

Lient la requête à la connexion et à la transaction.

```pascal
SQLQuery1.Database := SQLite3Connection1;  
SQLQuery1.Transaction := SQLTransaction1;  
```

### FieldCount

Nombre de colonnes dans le résultat.

```pascal
ShowMessage('Nombre de colonnes : ' + IntToStr(SQLQuery1.FieldCount));
```

### RecordCount

Nombre d'enregistrements dans le résultat.

```pascal
ShowMessage('Nombre d''enregistrements : ' + IntToStr(SQLQuery1.RecordCount));
```

**Note :** Pour obtenir le compte exact, vous devez parfois parcourir tout le dataset ou utiliser `Last` puis `First`.

### IsEmpty

Vérifie si le résultat est vide.

```pascal
if SQLQuery1.IsEmpty then
  ShowMessage('Aucun résultat trouvé')
else
  ShowMessage('Résultats trouvés');
```

## Méthodes importantes

### Open

Exécute une requête SELECT et ouvre le dataset.

```pascal
SQLQuery1.Open;
```

### Close

Ferme le dataset.

```pascal
SQLQuery1.Close;
```

### ExecSQL

Exécute une requête d'action (INSERT, UPDATE, DELETE).

```pascal
SQLQuery1.ExecSQL;
```

### Refresh

Rafraîchit les données (ré-exécute la requête).

```pascal
SQLQuery1.Refresh;
```

### Locate

Recherche un enregistrement par critère.

```pascal
// Chercher un client par ID
if SQLQuery1.Locate('id', 5, []) then
  ShowMessage('Client trouvé : ' + SQLQuery1.FieldByName('nom').AsString)
else
  ShowMessage('Client non trouvé');
```

Options :
- `[]` : recherche exacte
- `[loCaseInsensitive]` : ignorer la casse
- `[loPartialKey]` : recherche partielle

## Gestion des valeurs NULL

### Vérifier si un champ est NULL

```pascal
if SQLQuery1.FieldByName('telephone').IsNull then
  ShowMessage('Pas de téléphone')
else
  ShowMessage('Téléphone : ' + SQLQuery1.FieldByName('telephone').AsString);
```

### Insérer une valeur NULL

```pascal
SQLQuery1.ParamByName('telephone').Clear;  // Définit NULL
```

### Gérer NULL dans l'affichage

```pascal
var
  Telephone: string;
begin
  if SQLQuery1.FieldByName('telephone').IsNull then
    Telephone := 'Non renseigné'
  else
    Telephone := SQLQuery1.FieldByName('telephone').AsString;

  Memo1.Lines.Add('Téléphone : ' + Telephone);
end;
```

## Transactions : quand Commit et Rollback ?

### Règle générale

**Pour les SELECT :** Pas besoin de Commit/Rollback
```pascal
SQLQuery1.Open;
// Parcourir les données
SQLQuery1.Close;
```

**Pour les INSERT/UPDATE/DELETE :** Toujours faire Commit
```pascal
SQLQuery1.ExecSQL;  
SQLTransaction1.Commit;  // IMPORTANT !  
```

### Exemple de transaction complète

```pascal
procedure TForm1.TransfererArgent;  
begin  
  // Commencer une transaction explicite
  SQLTransaction1.StartTransaction;

  try
    // Débiter le compte source
    SQLQuery1.SQL.Text :=
      'UPDATE Comptes SET solde = solde - :montant WHERE id = :id';
    SQLQuery1.ParamByName('montant').AsFloat := 100.0;
    SQLQuery1.ParamByName('id').AsInteger := 1;
    SQLQuery1.ExecSQL;

    // Créditer le compte destination
    SQLQuery1.SQL.Text :=
      'UPDATE Comptes SET solde = solde + :montant WHERE id = :id';
    SQLQuery1.ParamByName('montant').AsFloat := 100.0;
    SQLQuery1.ParamByName('id').AsInteger := 2;
    SQLQuery1.ExecSQL;

    // Tout s'est bien passé : valider
    SQLTransaction1.Commit;
    ShowMessage('Transfert effectué');

  except
    on E: Exception do
    begin
      // Erreur : annuler tout
      SQLTransaction1.Rollback;
      ShowMessage('Erreur, transaction annulée : ' + E.Message);
    end;
  end;
end;
```

## Bonnes pratiques

### 1. Toujours fermer les requêtes

```pascal
SQLQuery1.Open;  
try  
  // Utiliser les données
finally
  SQLQuery1.Close;  // Fermer même si erreur
end;
```

### 2. Utiliser les paramètres

```pascal
// MAL
SQLQuery1.SQL.Text :=
  'SELECT * FROM Clients WHERE nom = ''' + Edit1.Text + '''';

// BIEN
SQLQuery1.SQL.Text := 'SELECT * FROM Clients WHERE nom = :nom';  
SQLQuery1.ParamByName('nom').AsString := Edit1.Text;  
```

### 3. Vérifier si le dataset est vide

```pascal
SQLQuery1.Open;  
if SQLQuery1.IsEmpty then  
begin  
  ShowMessage('Aucun résultat');
  SQLQuery1.Close;
  Exit;
end;
// Continuer...
```

### 4. Gérer les erreurs

```pascal
try
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;
except
  on E: Exception do
  begin
    SQLTransaction1.Rollback;
    ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### 5. Valider les entrées utilisateur

```pascal
// Vérifier que les champs ne sont pas vides
if (Trim(EditNom.Text) = '') or (Trim(EditEmail.Text) = '') then  
begin  
  ShowMessage('Veuillez remplir tous les champs obligatoires');
  Exit;
end;

// Vérifier le format email (simple)
if Pos('@', EditEmail.Text) = 0 then  
begin  
  ShowMessage('Email invalide');
  Exit;
end;
```

### 6. Réutiliser les requêtes

Si vous utilisez souvent la même requête, gardez-la préparée :

```pascal
// Au démarrage (FormCreate)
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Préparer la requête
  SQLQuery1.SQL.Text := 'SELECT * FROM Clients WHERE id = :id';
end;

// Lors de l'utilisation
procedure TForm1.ChargerClient(ClientID: Integer);  
begin  
  SQLQuery1.ParamByName('id').AsInteger := ClientID;
  SQLQuery1.Open;
  // Utiliser les données
  SQLQuery1.Close;
end;
```

## Exemple complet : gestionnaire de contacts

Voici un exemple complet avec formulaire de gestion :

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  sqldb, sqlite3conn;

type
  TForm1 = class(TForm)
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    SQLQuery1: TSQLQuery;
    EditNom: TEdit;
    EditPrenom: TEdit;
    EditEmail: TEdit;
    ButtonAjouter: TButton;
    ButtonLister: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonAjouterClick(Sender: TObject);
    procedure ButtonListerClick(Sender: TObject);
  private
    procedure InitialiserBase;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  InitialiserBase;
end;

procedure TForm1.InitialiserBase;  
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

    // Créer la table si elle n'existe pas
    SQLQuery1.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS Contacts (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  nom TEXT NOT NULL,' +
      '  prenom TEXT,' +
      '  email TEXT' +
      ')';
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur d''initialisation : ' + E.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TForm1.ButtonAjouterClick(Sender: TObject);  
begin  
  // Valider les entrées
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    EditNom.SetFocus;
    Exit;
  end;

  try
    // Préparer la requête avec paramètres
    SQLQuery1.SQL.Text :=
      'INSERT INTO Contacts (nom, prenom, email) ' +
      'VALUES (:nom, :prenom, :email)';

    SQLQuery1.ParamByName('nom').AsString := EditNom.Text;
    SQLQuery1.ParamByName('prenom').AsString := EditPrenom.Text;
    SQLQuery1.ParamByName('email').AsString := EditEmail.Text;

    // Exécuter
    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

    ShowMessage('Contact ajouté avec succès');

    // Vider les champs
    EditNom.Clear;
    EditPrenom.Clear;
    EditEmail.Clear;
    EditNom.SetFocus;

  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;

procedure TForm1.ButtonListerClick(Sender: TObject);  
begin  
  Memo1.Clear;

  try
    SQLQuery1.SQL.Text := 'SELECT * FROM Contacts ORDER BY nom';
    SQLQuery1.Open;

    if SQLQuery1.IsEmpty then
    begin
      Memo1.Lines.Add('Aucun contact');
    end
    else
    begin
      Memo1.Lines.Add('Liste des contacts :');
      Memo1.Lines.Add('');

      while not SQLQuery1.EOF do
      begin
        Memo1.Lines.Add(Format('%d. %s %s - %s',
          [SQLQuery1.FieldByName('id').AsInteger,
           SQLQuery1.FieldByName('nom').AsString,
           SQLQuery1.FieldByName('prenom').AsString,
           SQLQuery1.FieldByName('email').AsString]));

        SQLQuery1.Next;
      end;

      Memo1.Lines.Add('');
      Memo1.Lines.Add('Total : ' + IntToStr(SQLQuery1.RecordCount) + ' contact(s)');
    end;

    SQLQuery1.Close;

  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

end.
```

## Résumé

**TSQLQuery est l'outil principal pour :**
- Exécuter des requêtes SQL
- Récupérer et parcourir les résultats
- Modifier les données

**Deux méthodes principales :**
- `Open` : pour SELECT (retourne des données)
- `ExecSQL` : pour INSERT/UPDATE/DELETE (modifie les données)

**Toujours utiliser des paramètres :**
- Sécurité (contre l'injection SQL)
- Lisibilité
- Performance

**Gestion des transactions :**
- `Commit` après modifications réussies
- `Rollback` en cas d'erreur

**Navigation :**
- `Next`, `Prior`, `First`, `Last`
- `EOF`, `BOF`, `RecordCount`
- `FieldByName('nom').AsString`

Dans la section suivante, nous verrons comment lier automatiquement ces données à des composants visuels avec les **composants data-aware** !

---

*Vous savez maintenant dialoguer avec votre base de données depuis Pascal. La magie opère !*

⏭️ [Composants data-aware](/16-bases-donnees-maitrise-approfondie/06-composants-data-aware.md)
