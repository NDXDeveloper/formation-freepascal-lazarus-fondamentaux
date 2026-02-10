{
  Section 16.7 — Navigation dans les donnees
  Description : First/Last/Next/Prior, MoveBy, Locate, Lookup,
                Bookmarks, DisableControls, export CSV
  Fichier source : 07-navigation-donnees.md
}
program NavigationDonnees;

{$mode objfpc}{$H+}

uses
  SysUtils, DB, sqldb, sqlite3conn;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query: TSQLQuery;
  DBPath: string;

procedure InitialiserBase;  
var  
  i: Integer;
const
  Noms: array[0..9] of string = (
    'Dupont', 'Martin', 'Bernard', 'Dubois', 'Thomas',
    'Robert', 'Richard', 'Petit', 'Durand', 'Leroy');
  Prenoms: array[0..9] of string = (
    'Pierre', 'Marie', 'Jean', 'Sophie', 'Paul',
    'Claire', 'Antoine', 'Julie', 'Nicolas', 'Emma');
  Montants: array[0..9] of Double = (
    150.50, 200.00, 75.25, 310.00, 45.00,
    180.75, 95.50, 420.00, 60.00, 250.25);
begin
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Clients (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  nom TEXT NOT NULL,' +
    '  prenom TEXT,' +
    '  email TEXT,' +
    '  montant REAL DEFAULT 0' +
    ')');
  Trans.Commit;

  Query.SQL.Text :=
    'INSERT INTO Clients (nom, prenom, email, montant) ' +
    'VALUES (:nom, :prenom, :email, :montant)';

  for i := 0 to 9 do
  begin
    Query.ParamByName('nom').AsString := Noms[i];
    Query.ParamByName('prenom').AsString := Prenoms[i];
    Query.ParamByName('email').AsString :=
      LowerCase(Prenoms[i]) + '@email.fr';
    Query.ParamByName('montant').AsFloat := Montants[i];
    Query.ExecSQL;
  end;
  Trans.Commit;
  WriteLn('10 clients inseres');
end;

procedure DemoNavigationBase;  
begin  
  WriteLn;
  WriteLn('--- Navigation de base (First/Last/Next/Prior) ---');

  Query.SQL.Text := 'SELECT * FROM Clients ORDER BY nom';
  Query.Open;

  { First }
  Query.First;
  WriteLn('  First  : ', Query.FieldByName('prenom').AsString, ' ',
    Query.FieldByName('nom').AsString,
    ' (RecNo=', Query.RecNo, ')');

  { Last }
  Query.Last;
  WriteLn('  Last   : ', Query.FieldByName('prenom').AsString, ' ',
    Query.FieldByName('nom').AsString,
    ' (RecNo=', Query.RecNo, ')');

  { Prior }
  Query.Prior;
  WriteLn('  Prior  : ', Query.FieldByName('prenom').AsString, ' ',
    Query.FieldByName('nom').AsString,
    ' (RecNo=', Query.RecNo, ')');

  { First puis Next }
  Query.First;
  Query.Next;
  WriteLn('  Next   : ', Query.FieldByName('prenom').AsString, ' ',
    Query.FieldByName('nom').AsString,
    ' (RecNo=', Query.RecNo, ')');

  { RecordCount }
  WriteLn('  RecordCount : ', Query.RecordCount);

  { BOF / EOF }
  Query.First;
  WriteLn('  BOF apres First : ', Query.BOF);
  Query.Last;
  Query.Next;
  WriteLn('  EOF apres Last+Next : ', Query.EOF);

  Query.Close;
end;

procedure DemoMoveBy;  
begin  
  WriteLn;
  WriteLn('--- MoveBy ---');

  Query.SQL.Text := 'SELECT * FROM Clients ORDER BY nom';
  Query.Open;

  Query.First;
  WriteLn('  Position initiale : RecNo=', Query.RecNo, ' (',
    Query.FieldByName('nom').AsString, ')');

  Query.MoveBy(3);
  WriteLn('  Apres MoveBy(3)  : RecNo=', Query.RecNo, ' (',
    Query.FieldByName('nom').AsString, ')');

  Query.MoveBy(-2);
  WriteLn('  Apres MoveBy(-2) : RecNo=', Query.RecNo, ' (',
    Query.FieldByName('nom').AsString, ')');

  Query.Close;
end;

procedure DemoRecherche;
{ Note : Locate/Lookup de TDataSet ne fonctionnent pas avec SQLite
  car les colonnes TEXT sont rapportees comme ftMemo (type non indexable).
  On utilise donc des requetes SQL WHERE, plus fiable avec SQLite. }
var
  QSearch: TSQLQuery;
begin
  WriteLn;
  WriteLn('--- Recherche SQL (equivalent Locate) ---');

  QSearch := TSQLQuery.Create(nil);
  try
    QSearch.Database := Conn;
    QSearch.Transaction := Trans;

    { Recherche exacte par nom }
    QSearch.SQL.Text := 'SELECT * FROM Clients WHERE nom = :nom';
    QSearch.ParamByName('nom').AsString := 'Dupont';
    QSearch.Open;
    if not QSearch.IsEmpty then
      WriteLn('  Recherche "Dupont"  : Trouve - ',
        QSearch.FieldByName('prenom').AsString, ' ',
        QSearch.FieldByName('nom').AsString)
    else
      WriteLn('  Recherche "Dupont"  : Non trouve');
    QSearch.Close;

    { Recherche insensible a la casse }
    QSearch.SQL.Text :=
      'SELECT * FROM Clients WHERE LOWER(nom) = LOWER(:nom)';
    QSearch.ParamByName('nom').AsString := 'martin';
    QSearch.Open;
    if not QSearch.IsEmpty then
      WriteLn('  Recherche "martin" (CI) : Trouve - ',
        QSearch.FieldByName('prenom').AsString, ' ',
        QSearch.FieldByName('nom').AsString)
    else
      WriteLn('  Recherche "martin" (CI) : Non trouve');
    QSearch.Close;

    { Recherche partielle (LIKE) }
    QSearch.SQL.Text :=
      'SELECT * FROM Clients WHERE nom LIKE :pattern';
    QSearch.ParamByName('pattern').AsString := 'Ber%';
    QSearch.Open;
    if not QSearch.IsEmpty then
      WriteLn('  Recherche "Ber%" (partiel) : Trouve - ',
        QSearch.FieldByName('prenom').AsString, ' ',
        QSearch.FieldByName('nom').AsString)
    else
      WriteLn('  Recherche "Ber%" (partiel) : Non trouve');
    QSearch.Close;

    { Recherche inexistante }
    QSearch.SQL.Text := 'SELECT * FROM Clients WHERE nom = :nom';
    QSearch.ParamByName('nom').AsString := 'Inexistant';
    QSearch.Open;
    if not QSearch.IsEmpty then
      WriteLn('  Recherche "Inexistant" : Trouve')
    else
      WriteLn('  Recherche "Inexistant" : Non trouve (attendu)');
    QSearch.Close;

  finally
    QSearch.Free;
  end;
end;

procedure DemoLookup;
{ Lookup equivalent via requete SQL separee,
  sans changer la position du dataset principal }
var
  QSearch: TSQLQuery;
begin
  WriteLn;
  WriteLn('--- Lookup SQL (sans changer de position) ---');

  Query.SQL.Text := 'SELECT * FROM Clients ORDER BY nom';
  Query.Open;

  { Se positionner au premier }
  Query.First;
  WriteLn('  Position actuelle : ', Query.FieldByName('nom').AsString);

  { Lookup via une requete separee }
  QSearch := TSQLQuery.Create(nil);
  try
    QSearch.Database := Conn;
    QSearch.Transaction := Trans;
    QSearch.SQL.Text :=
      'SELECT email FROM Clients WHERE nom = :nom';
    QSearch.ParamByName('nom').AsString := 'Dupont';
    QSearch.Open;

    if not QSearch.IsEmpty then
      WriteLn('  Lookup email Dupont : ',
        QSearch.FieldByName('email').AsString)
    else
      WriteLn('  Lookup email Dupont : non trouve');
    QSearch.Close;
  finally
    QSearch.Free;
  end;

  { Verifier que la position n'a pas change }
  WriteLn('  Position apres Lookup : ', Query.FieldByName('nom').AsString,
    ' (inchangee)');

  Query.Close;
end;

procedure DemoBookmarks;  
var  
  Signet: TBookmark;
begin
  WriteLn;
  WriteLn('--- Bookmarks (signets) ---');

  Query.SQL.Text := 'SELECT * FROM Clients ORDER BY nom';
  Query.Open;

  { Se placer au 3eme enregistrement }
  Query.First;
  Query.MoveBy(2);
  WriteLn('  Position memorisee : RecNo=', Query.RecNo, ' (',
    Query.FieldByName('nom').AsString, ')');

  { Memoriser la position }
  Signet := Query.GetBookmark;
  try
    { Naviguer ailleurs }
    Query.Last;
    WriteLn('  Apres Last : RecNo=', Query.RecNo, ' (',
      Query.FieldByName('nom').AsString, ')');

    Query.First;
    WriteLn('  Apres First : RecNo=', Query.RecNo, ' (',
      Query.FieldByName('nom').AsString, ')');

    { Revenir au signet }
    if Query.BookmarkValid(Signet) then
    begin
      Query.GotoBookmark(Signet);
      WriteLn('  Retour au signet : RecNo=', Query.RecNo, ' (',
        Query.FieldByName('nom').AsString, ')');
    end
    else
      WriteLn('  Signet invalide !');

  finally
    Query.FreeBookmark(Signet);
  end;

  Query.Close;
end;

procedure DemoParcoursComplet;  
var  
  Total: Double;
  Count: Integer;
begin
  WriteLn;
  WriteLn('--- Parcours complet (calcul du total) ---');

  Query.SQL.Text := 'SELECT * FROM Clients ORDER BY nom';
  Query.Open;

  Total := 0;
  Count := 0;

  { DisableControls n'a pas d'effet visible en console,
    mais le pattern est illustre ici pour reference }
  Query.DisableControls;
  try
    Query.First;
    while not Query.EOF do
    begin
      Total := Total + Query.FieldByName('montant').AsFloat;
      Inc(Count);
      Query.Next;
    end;
  finally
    Query.EnableControls;
  end;

  WriteLn(Format('  %d clients, total montants : %.2f EUR', [Count, Total]));
  WriteLn(Format('  Moyenne : %.2f EUR', [Total / Count]));

  Query.Close;
end;

procedure DemoExportCSV;  
var  
  CSVPath: string;
  F: TextFile;
  Ligne: string;
begin
  WriteLn;
  WriteLn('--- Export CSV ---');

  CSVPath := GetTempDir + 'export_clients.csv';
  AssignFile(F, CSVPath);
  Rewrite(F);
  try
    { En-tete }
    WriteLn(F, 'ID;Nom;Prenom;Email;Montant');

    Query.SQL.Text := 'SELECT * FROM Clients ORDER BY nom';
    Query.Open;

    Query.DisableControls;
    try
      Query.First;
      while not Query.EOF do
      begin
        WriteLn(F, Format('%d;%s;%s;%s;%.2f', [
          Query.FieldByName('id').AsInteger,
          Query.FieldByName('nom').AsString,
          Query.FieldByName('prenom').AsString,
          Query.FieldByName('email').AsString,
          Query.FieldByName('montant').AsFloat
        ]));
        Query.Next;
      end;
    finally
      Query.EnableControls;
    end;

    Query.Close;
    CloseFile(F);
    WriteLn('  Fichier CSV cree : ', CSVPath);

    { Lire et afficher le contenu }
    AssignFile(F, CSVPath);
    Reset(F);
    while not System.EOF(F) do
    begin
      System.ReadLn(F, Ligne);
      WriteLn('  ', Ligne);
    end;
    CloseFile(F);

    { Nettoyage }
    DeleteFile(CSVPath);
  except
    on E: Exception do
    begin
      WriteLn('  Erreur export CSV : ', E.Message);
      CloseFile(F);
    end;
  end;
end;

begin
  WriteLn('=== Navigation dans les donnees ===');

  DBPath := GetTempDir + 'test_navigation.db';
  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Conn.DatabaseName := DBPath;
    Conn.Transaction := Trans;
    Trans.Database := Conn;
    Query.Database := Conn;
    Query.Transaction := Trans;
    Conn.Open;

    InitialiserBase;
    DemoNavigationBase;
    DemoMoveBy;
    DemoRecherche;
    DemoLookup;
    DemoBookmarks;
    DemoParcoursComplet;
    DemoExportCSV;

    if Trans.Active then
      Trans.Commit;
    Conn.Close;

  finally
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
