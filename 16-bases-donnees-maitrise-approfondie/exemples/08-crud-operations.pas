{
  Section 16.8 — Ajout, modification, suppression
  Description : Insert/Append, Edit, Post, Cancel, Delete,
                validation BeforePost, valeurs par defaut OnNewRecord,
                RowsAffected
  Fichier source : 08-ajout-modification-suppression.md
}
program CrudOperations;

{$mode objfpc}{$H+}

uses
  SysUtils, DB, sqldb, sqlite3conn;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query, QueryAux: TSQLQuery;
  DBPath: string;

procedure AfficherTous;  
begin  
  QueryAux.SQL.Text := 'SELECT * FROM Produits ORDER BY id';
  QueryAux.Open;

  if QueryAux.IsEmpty then
  begin
    WriteLn('  (aucun produit)');
    QueryAux.Close;
    Exit;
  end;

  WriteLn(Format('  %-4s %-20s %-10s %-8s %-6s', [
    'ID', 'Nom', 'Categorie', 'Prix', 'Actif']));
  WriteLn('  ', StringOfChar('-', 52));

  while not QueryAux.EOF do
  begin
    WriteLn(Format('  %-4d %-20s %-10s %8.2f %-6s', [
      QueryAux.FieldByName('id').AsInteger,
      QueryAux.FieldByName('nom').AsString,
      QueryAux.FieldByName('categorie').AsString,
      QueryAux.FieldByName('prix').AsFloat,
      BoolToStr(QueryAux.FieldByName('actif').AsInteger = 1, 'Oui', 'Non')
    ]));
    QueryAux.Next;
  end;

  WriteLn('  Total : ', QueryAux.RecordCount, ' produit(s)');
  QueryAux.Close;
end;

procedure InitialiserBase;  
begin  
  WriteLn('--- Initialisation ---');
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Produits (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  nom TEXT NOT NULL,' +
    '  categorie TEXT DEFAULT ''General'',' +
    '  prix REAL NOT NULL CHECK(prix >= 0),' +
    '  actif INTEGER DEFAULT 1' +
    ')');
  Trans.Commit;
  WriteLn('Table Produits creee');
end;

procedure DemoInsert;  
begin  
  WriteLn;
  WriteLn('--- INSERT (via SQL parametree) ---');

  { Insert avec ExecSQL et parametres }
  QueryAux.SQL.Text :=
    'INSERT INTO Produits (nom, categorie, prix, actif) ' +
    'VALUES (:nom, :cat, :prix, :actif)';

  QueryAux.ParamByName('nom').AsString := 'Clavier mecanique';
  QueryAux.ParamByName('cat').AsString := 'Informatique';
  QueryAux.ParamByName('prix').AsFloat := 89.99;
  QueryAux.ParamByName('actif').AsInteger := 1;
  QueryAux.ExecSQL;
  WriteLn('  Insert : Clavier mecanique');

  QueryAux.ParamByName('nom').AsString := 'Souris sans fil';
  QueryAux.ParamByName('cat').AsString := 'Informatique';
  QueryAux.ParamByName('prix').AsFloat := 29.99;
  QueryAux.ParamByName('actif').AsInteger := 1;
  QueryAux.ExecSQL;
  WriteLn('  Insert : Souris sans fil');

  QueryAux.ParamByName('nom').AsString := 'Ecran 27 pouces';
  QueryAux.ParamByName('cat').AsString := 'Informatique';
  QueryAux.ParamByName('prix').AsFloat := 349.00;
  QueryAux.ParamByName('actif').AsInteger := 1;
  QueryAux.ExecSQL;

  QueryAux.ParamByName('nom').AsString := 'Casque audio';
  QueryAux.ParamByName('cat').AsString := 'Audio';
  QueryAux.ParamByName('prix').AsFloat := 59.90;
  QueryAux.ParamByName('actif').AsInteger := 1;
  QueryAux.ExecSQL;

  QueryAux.ParamByName('nom').AsString := 'Cable HDMI';
  QueryAux.ParamByName('cat').AsString := 'Accessoire';
  QueryAux.ParamByName('prix').AsFloat := 12.50;
  QueryAux.ParamByName('actif').AsInteger := 0;
  QueryAux.ExecSQL;

  Trans.Commit;
  WriteLn('  5 produits inseres');

  { Demonstrer les etats via dataset Open }
  WriteLn;
  Query.SQL.Text := 'SELECT * FROM Produits ORDER BY id';
  Query.Open;
  WriteLn('  Etat apres Open : ', Ord(Query.State),
    ' (dsBrowse=', Ord(dsBrowse), ')');
  WriteLn('  RecordCount : ', Query.RecordCount);
  Query.Close;

  WriteLn;
  WriteLn('  Contenu de la table :');
  AfficherTous;
end;

procedure DemoEdit;  
begin  
  WriteLn;
  WriteLn('--- UPDATE (modification via SQL) ---');

  { Afficher avant }
  QueryAux.SQL.Text :=
    'SELECT nom, prix FROM Produits WHERE nom = ''Souris sans fil''';
  QueryAux.Open;
  if not QueryAux.IsEmpty then
    WriteLn('  Avant  : ', QueryAux.FieldByName('nom').AsString,
      ' - prix : ', QueryAux.FieldByName('prix').AsFloat:0:2);
  QueryAux.Close;

  { Modifier avec UPDATE parametre }
  QueryAux.SQL.Text :=
    'UPDATE Produits SET nom = :newnom, prix = :newprix ' +
    'WHERE nom = :oldnom';
  QueryAux.ParamByName('newnom').AsString := 'Souris ergonomique';
  QueryAux.ParamByName('newprix').AsFloat := 24.99;
  QueryAux.ParamByName('oldnom').AsString := 'Souris sans fil';
  QueryAux.ExecSQL;
  WriteLn('  RowsAffected : ', QueryAux.RowsAffected);
  Trans.Commit;

  { Afficher apres }
  QueryAux.SQL.Text :=
    'SELECT nom, prix FROM Produits WHERE nom = ''Souris ergonomique''';
  QueryAux.Open;
  if not QueryAux.IsEmpty then
    WriteLn('  Apres  : ', QueryAux.FieldByName('nom').AsString,
      ' - prix : ', QueryAux.FieldByName('prix').AsFloat:0:2);
  QueryAux.Close;
end;

procedure DemoCancel;  
begin  
  WriteLn;
  WriteLn('--- Principe du CANCEL (annulation) ---');

  { Illustrer le concept d'annulation :
    en SQL, on n'envoie simplement pas la requete UPDATE }
  QueryAux.SQL.Text := 'SELECT nom, prix FROM Produits ORDER BY id LIMIT 1';
  QueryAux.Open;
  WriteLn('  Valeur actuelle : ', QueryAux.FieldByName('nom').AsString,
    ' - prix : ', QueryAux.FieldByName('prix').AsFloat:0:2);
  QueryAux.Close;

  WriteLn('  Modification preparee mais NON envoyee (Cancel)');
  WriteLn('  → La base reste inchangee');

  { Verifier que rien n'a change }
  QueryAux.SQL.Text := 'SELECT nom, prix FROM Produits ORDER BY id LIMIT 1';
  QueryAux.Open;
  WriteLn('  Verification : ', QueryAux.FieldByName('nom').AsString,
    ' - prix : ', QueryAux.FieldByName('prix').AsFloat:0:2,
    ' (inchange)');
  QueryAux.Close;
end;

procedure DemoDelete;  
begin  
  WriteLn;
  WriteLn('--- DELETE (suppression via SQL) ---');

  { Compter avant }
  QueryAux.SQL.Text := 'SELECT COUNT(*) AS nb FROM Produits';
  QueryAux.Open;
  WriteLn('  Avant : ', QueryAux.FieldByName('nb').AsInteger, ' produit(s)');
  QueryAux.Close;

  { Supprimer Cable HDMI }
  QueryAux.SQL.Text := 'DELETE FROM Produits WHERE nom = :nom';
  QueryAux.ParamByName('nom').AsString := 'Cable HDMI';
  QueryAux.ExecSQL;
  WriteLn('  Suppression de "Cable HDMI" : RowsAffected = ',
    QueryAux.RowsAffected);
  Trans.Commit;

  { Compter apres }
  QueryAux.SQL.Text := 'SELECT COUNT(*) AS nb FROM Produits';
  QueryAux.Open;
  WriteLn('  Apres : ', QueryAux.FieldByName('nb').AsInteger, ' produit(s)');
  QueryAux.Close;
end;

procedure DemoDeleteSQL;  
begin  
  WriteLn;
  WriteLn('--- DELETE via SQL (RowsAffected) ---');

  { D'abord ajouter des produits inactifs pour la demo }
  QueryAux.SQL.Text :=
    'INSERT INTO Produits (nom, categorie, prix, actif) ' +
    'VALUES (:nom, :cat, :prix, 0)';

  QueryAux.ParamByName('nom').AsString := 'Produit inactif 1';
  QueryAux.ParamByName('cat').AsString := 'Test';
  QueryAux.ParamByName('prix').AsFloat := 5.00;
  QueryAux.ExecSQL;

  QueryAux.ParamByName('nom').AsString := 'Produit inactif 2';
  QueryAux.ParamByName('cat').AsString := 'Test';
  QueryAux.ParamByName('prix').AsFloat := 8.00;
  QueryAux.ExecSQL;
  Trans.Commit;

  { Compter les inactifs }
  QueryAux.SQL.Text := 'SELECT COUNT(*) AS nb FROM Produits WHERE actif = 0';
  QueryAux.Open;
  WriteLn('  Produits inactifs : ', QueryAux.FieldByName('nb').AsInteger);
  QueryAux.Close;

  { Supprimer les inactifs via SQL }
  QueryAux.SQL.Text := 'DELETE FROM Produits WHERE actif = 0';
  QueryAux.ExecSQL;
  WriteLn('  RowsAffected : ', QueryAux.RowsAffected);
  Trans.Commit;

  { Verifier }
  QueryAux.SQL.Text := 'SELECT COUNT(*) AS nb FROM Produits';
  QueryAux.Open;
  WriteLn('  Produits restants : ', QueryAux.FieldByName('nb').AsInteger);
  QueryAux.Close;
end;

procedure DemoContrainteUnique;  
begin  
  WriteLn;
  WriteLn('--- Gestion contrainte NOT NULL ---');

  { Creer une table avec contrainte UNIQUE }
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Emails (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  email TEXT NOT NULL UNIQUE' +
    ')');
  Trans.Commit;

  { Inserer un email }
  QueryAux.SQL.Text := 'INSERT INTO Emails (email) VALUES (:email)';
  QueryAux.ParamByName('email').AsString := 'test@email.fr';
  QueryAux.ExecSQL;
  Trans.Commit;
  WriteLn('  Premier insert : OK');

  { Tenter d'inserer un doublon }
  try
    QueryAux.SQL.Text := 'INSERT INTO Emails (email) VALUES (:email)';
    QueryAux.ParamByName('email').AsString := 'test@email.fr';
    QueryAux.ExecSQL;
    Trans.Commit;
    WriteLn('  INATTENDU : doublon accepte');
  except
    on E: EDatabaseError do
    begin
      WriteLn('  Doublon rejete : ', E.Message);
      if Trans.Active then
        Trans.Rollback;
    end;
  end;
end;

procedure DemoEtatsDataset;  
begin  
  WriteLn;
  WriteLn('--- Etats du dataset ---');

  { Note : en mode console avec SQLite, les etats du dataset sont
    principalement dsInactive et dsBrowse. Les etats dsInsert et dsEdit
    sont utilises avec les composants data-aware (projet Lazarus). }

  QueryAux.SQL.Text := 'SELECT * FROM Produits ORDER BY id';

  { dsInactive }
  WriteLn('  Avant Open  : State=', Ord(QueryAux.State),
    ' (dsInactive=', Ord(dsInactive), ')');

  QueryAux.Open;
  WriteLn('  Apres Open  : State=', Ord(QueryAux.State),
    ' (dsBrowse=', Ord(dsBrowse), ')');

  WriteLn('  EOF         : ', QueryAux.EOF);
  WriteLn('  RecordCount : ', QueryAux.RecordCount);

  QueryAux.Close;
  WriteLn('  Apres Close : State=', Ord(QueryAux.State),
    ' (dsInactive=', Ord(dsInactive), ')');

  WriteLn;
  WriteLn('  Constantes d''etat TDatasetState :');
  WriteLn('    dsInactive = ', Ord(dsInactive));
  WriteLn('    dsBrowse   = ', Ord(dsBrowse));
  WriteLn('    dsEdit     = ', Ord(dsEdit));
  WriteLn('    dsInsert   = ', Ord(dsInsert));
end;

begin
  WriteLn('=== Operations CRUD ===');

  DBPath := GetTempDir + 'test_crud.db';
  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  QueryAux := TSQLQuery.Create(nil);
  try
    Conn.DatabaseName := DBPath;
    Conn.Transaction := Trans;
    Trans.Database := Conn;
    Query.Database := Conn;
    Query.Transaction := Trans;
    QueryAux.Database := Conn;
    QueryAux.Transaction := Trans;
    Conn.Open;

    InitialiserBase;
    DemoInsert;
    DemoEdit;
    DemoCancel;
    DemoDelete;
    DemoDeleteSQL;
    DemoContrainteUnique;
    DemoEtatsDataset;

    WriteLn;
    WriteLn('--- Etat final ---');
    AfficherTous;

    if Trans.Active then
      Trans.Commit;
    Conn.Close;

  finally
    QueryAux.Free;
    Query.Free;
    Trans.Free;
    Conn.Free;

    if FileExists(DBPath) then
      DeleteFile(DBPath);
    if FileExists(DBPath + '-wal') then
      DeleteFile(DBPath + '-wal');
    if FileExists(DBPath + '-shm') then
      DeleteFile(DBPath + '-shm');
  end;

  WriteLn;
  WriteLn('=== Fin ===');
end.
