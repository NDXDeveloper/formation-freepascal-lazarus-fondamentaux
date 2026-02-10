{
  Section 16.5 — TSQLQuery et requetes
  Description : CREATE TABLE, INSERT avec parametres, SELECT avec iteration,
                UPDATE, DELETE, requetes parametrees anti-injection
  Fichier source : 05-tsqlquery-requetes.md
}
program RequetesParametrees;

{$mode objfpc}{$H+}

uses
  SysUtils, sqldb, sqlite3conn;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query: TSQLQuery;
  DBPath: string;

procedure InitialiserBase;  
begin  
  WriteLn('--- Initialisation de la base ---');
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Contacts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  nom TEXT NOT NULL,' +
    '  prenom TEXT,' +
    '  email TEXT,' +
    '  age INTEGER' +
    ')');
  Trans.Commit;
  WriteLn('Table Contacts creee');
end;

procedure InsererAvecParametres;  
var  
  i: Integer;
const
  Noms: array[0..4] of string = ('Dupont', 'Martin', 'Bernard', 'Dubois', 'Thomas');
  Prenoms: array[0..4] of string = ('Pierre', 'Marie', 'Jean', 'Sophie', 'Paul');
  Ages: array[0..4] of Integer = (25, 30, 45, 28, 35);
begin
  WriteLn;
  WriteLn('--- INSERT avec parametres ---');

  Query.SQL.Text :=
    'INSERT INTO Contacts (nom, prenom, email, age) ' +
    'VALUES (:nom, :prenom, :email, :age)';

  for i := 0 to 4 do
  begin
    Query.ParamByName('nom').AsString := Noms[i];
    Query.ParamByName('prenom').AsString := Prenoms[i];
    Query.ParamByName('email').AsString :=
      LowerCase(Prenoms[i]) + '.' + LowerCase(Noms[i]) + '@email.fr';
    Query.ParamByName('age').AsInteger := Ages[i];
    Query.ExecSQL;
    WriteLn('  Insere : ', Prenoms[i], ' ', Noms[i]);
  end;

  Trans.Commit;
  WriteLn('5 contacts inseres');
end;

procedure SelectTous;  
begin  
  WriteLn;
  WriteLn('--- SELECT * (iteration avec EOF/Next) ---');

  Query.SQL.Text := 'SELECT * FROM Contacts ORDER BY nom';
  Query.Open;

  WriteLn(Format('  %-4s %-12s %-10s %-30s %4s',
    ['ID', 'Nom', 'Prenom', 'Email', 'Age']));
  WriteLn('  ', StringOfChar('-', 64));

  while not Query.EOF do
  begin
    WriteLn(Format('  %-4d %-12s %-10s %-30s %4d', [
      Query.FieldByName('id').AsInteger,
      Query.FieldByName('nom').AsString,
      Query.FieldByName('prenom').AsString,
      Query.FieldByName('email').AsString,
      Query.FieldByName('age').AsInteger
    ]));
    Query.Next;
  end;

  WriteLn('  RecordCount : ', Query.RecordCount);
  Query.Close;
end;

procedure SelectAvecFiltre;  
begin  
  WriteLn;
  WriteLn('--- SELECT avec parametre (age > 30) ---');

  Query.SQL.Text :=
    'SELECT nom, prenom, age FROM Contacts WHERE age > :age_min ORDER BY age';
  Query.ParamByName('age_min').AsInteger := 30;
  Query.Open;

  while not Query.EOF do
  begin
    WriteLn(Format('  %s %s (age: %d)', [
      Query.FieldByName('prenom').AsString,
      Query.FieldByName('nom').AsString,
      Query.FieldByName('age').AsInteger
    ]));
    Query.Next;
  end;

  WriteLn('  Trouves : ', Query.RecordCount);
  Query.Close;
end;

procedure RechercheAvecLike;  
begin  
  WriteLn;
  WriteLn('--- SELECT avec LIKE parametre (recherche "Dup") ---');

  Query.SQL.Text :=
    'SELECT nom, prenom FROM Contacts WHERE nom LIKE :recherche';
  Query.ParamByName('recherche').AsString := '%Dup%';
  Query.Open;

  if Query.IsEmpty then
    WriteLn('  Aucun resultat')
  else
    while not Query.EOF do
    begin
      WriteLn('  Trouve : ', Query.FieldByName('prenom').AsString, ' ',
        Query.FieldByName('nom').AsString);
      Query.Next;
    end;

  Query.Close;
end;

procedure UpdateAvecParametres;  
begin  
  WriteLn;
  WriteLn('--- UPDATE avec parametres ---');

  Query.SQL.Text :=
    'UPDATE Contacts SET email = :email WHERE nom = :nom';
  Query.ParamByName('email').AsString := 'nouveau.email@exemple.fr';
  Query.ParamByName('nom').AsString := 'Dupont';
  Query.ExecSQL;
  Trans.Commit;

  WriteLn('  Lignes modifiees : ', Query.RowsAffected);

  { Verifier la modification }
  Query.SQL.Text := 'SELECT email FROM Contacts WHERE nom = :nom';
  Query.ParamByName('nom').AsString := 'Dupont';
  Query.Open;
  WriteLn('  Nouvel email de Dupont : ', Query.FieldByName('email').AsString);
  Query.Close;
end;

procedure DeleteAvecParametres;  
begin  
  WriteLn;
  WriteLn('--- DELETE avec parametres ---');

  { Compter avant }
  Query.SQL.Text := 'SELECT COUNT(*) AS nb FROM Contacts';
  Query.Open;
  WriteLn('  Avant suppression : ', Query.FieldByName('nb').AsInteger, ' contacts');
  Query.Close;

  { Supprimer }
  Query.SQL.Text := 'DELETE FROM Contacts WHERE nom = :nom';
  Query.ParamByName('nom').AsString := 'Thomas';
  Query.ExecSQL;
  Trans.Commit;
  WriteLn('  Lignes supprimees : ', Query.RowsAffected);

  { Compter apres }
  Query.SQL.Text := 'SELECT COUNT(*) AS nb FROM Contacts';
  Query.Open;
  WriteLn('  Apres suppression : ', Query.FieldByName('nb').AsInteger, ' contacts');
  Query.Close;
end;

procedure DemoAntiInjection;  
begin  
  WriteLn;
  WriteLn('--- Protection contre l''injection SQL ---');

  { Tentative d'injection via parametre : le parametre est echappe }
  Query.SQL.Text := 'SELECT * FROM Contacts WHERE nom = :nom';
  Query.ParamByName('nom').AsString := 'Dupont'' OR ''1''=''1';
  Query.Open;

  WriteLn('  Recherche avec injection tentee :');
  WriteLn('  Resultats : ', Query.RecordCount,
    ' (0 = injection bloquee par les parametres)');
  Query.Close;
end;

procedure DemoAccesParIndex;  
begin  
  WriteLn;
  WriteLn('--- Acces aux champs par index ---');

  Query.SQL.Text := 'SELECT id, nom, prenom FROM Contacts ORDER BY id LIMIT 2';
  Query.Open;

  WriteLn('  FieldCount : ', Query.FieldCount);
  WriteLn('  Champs : ', Query.Fields[0].FieldName, ', ',
    Query.Fields[1].FieldName, ', ', Query.Fields[2].FieldName);

  while not Query.EOF do
  begin
    WriteLn(Format('  Fields[0]=%s, Fields[1]=%s, Fields[2]=%s', [
      Query.Fields[0].AsString,
      Query.Fields[1].AsString,
      Query.Fields[2].AsString
    ]));
    Query.Next;
  end;

  Query.Close;
end;

procedure DemoValeurNull;  
begin  
  WriteLn;
  WriteLn('--- Gestion des valeurs NULL ---');

  { Inserer un contact sans email }
  Query.SQL.Text :=
    'INSERT INTO Contacts (nom, prenom, email, age) ' +
    'VALUES (:nom, :prenom, :email, :age)';
  Query.ParamByName('nom').AsString := 'SansEmail';
  Query.ParamByName('prenom').AsString := 'Test';
  Query.ParamByName('email').Clear; { NULL }
  Query.ParamByName('age').AsInteger := 20;
  Query.ExecSQL;
  Trans.Commit;

  { Verifier le NULL }
  Query.SQL.Text := 'SELECT nom, email FROM Contacts WHERE nom = :nom';
  Query.ParamByName('nom').AsString := 'SansEmail';
  Query.Open;

  if Query.FieldByName('email').IsNull then
    WriteLn('  Email de SansEmail : NULL (IsNull = True)')
  else
    WriteLn('  Email de SansEmail : ', Query.FieldByName('email').AsString);

  Query.Close;
end;

begin
  WriteLn('=== Requetes parametrees SQLite ===');

  DBPath := GetTempDir + 'test_requetes.db';
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
    InsererAvecParametres;
    SelectTous;
    SelectAvecFiltre;
    RechercheAvecLike;
    UpdateAvecParametres;
    DeleteAvecParametres;
    DemoAntiInjection;
    DemoAccesParIndex;
    DemoValeurNull;

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
