{
  Section 16.12 — Gestion avancee des transactions
  Description : SAVEPOINT, ROLLBACK TO, import en lot tolerant aux erreurs,
                batch par blocs de N
  Fichier source : 12-gestion-avancee-transactions.md
}
program SavepointsBatch;

{$mode objfpc}{$H+}

uses
  SysUtils, sqldb, sqlite3conn, DateUtils;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query, QueryExec: TSQLQuery;
  DBPath: string;

procedure AfficherContacts;  
begin  
  Query.SQL.Text := 'SELECT * FROM Contacts ORDER BY id';
  Query.Open;

  WriteLn(Format('  %-4s %-15s %-25s', ['ID', 'Nom', 'Email']));
  WriteLn('  ', StringOfChar('-', 46));

  while not Query.EOF do
  begin
    WriteLn(Format('  %-4d %-15s %-25s', [
      Query.FieldByName('id').AsInteger,
      Query.FieldByName('nom').AsString,
      Query.FieldByName('email').AsString
    ]));
    Query.Next;
  end;

  WriteLn('  Total : ', Query.RecordCount);
  Query.Close;
end;

procedure InitialiserBase;  
begin  
  WriteLn('--- Initialisation ---');
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Contacts (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  nom TEXT NOT NULL,' +
    '  email TEXT NOT NULL UNIQUE' +
    ')');
  Trans.Commit;
  WriteLn('Table Contacts creee (email UNIQUE)');
end;

procedure DemoSavepoint;  
begin  
  WriteLn;
  WriteLn('--- SAVEPOINT de base ---');

  { Inserer un premier contact }
  QueryExec.SQL.Text :=
    'INSERT INTO Contacts (nom, email) VALUES (:nom, :email)';
  QueryExec.ParamByName('nom').AsString := 'Dupont';
  QueryExec.ParamByName('email').AsString := 'dupont@email.fr';
  QueryExec.ExecSQL;
  WriteLn('  Insert Dupont : OK');

  { Creer un savepoint }
  Conn.ExecuteDirect('SAVEPOINT etape1');
  WriteLn('  SAVEPOINT etape1 cree');

  { Inserer un second contact }
  QueryExec.ParamByName('nom').AsString := 'Martin';
  QueryExec.ParamByName('email').AsString := 'martin@email.fr';
  QueryExec.ExecSQL;
  WriteLn('  Insert Martin : OK');

  { Creer un second savepoint }
  Conn.ExecuteDirect('SAVEPOINT etape2');
  WriteLn('  SAVEPOINT etape2 cree');

  { Inserer un troisieme contact }
  QueryExec.ParamByName('nom').AsString := 'Bernard';
  QueryExec.ParamByName('email').AsString := 'bernard@email.fr';
  QueryExec.ExecSQL;
  WriteLn('  Insert Bernard : OK');

  { Rollback jusqu'a etape2 (annule Bernard seulement) }
  Conn.ExecuteDirect('ROLLBACK TO etape2');
  WriteLn('  ROLLBACK TO etape2 : Bernard annule');

  { Liberer le savepoint }
  Conn.ExecuteDirect('RELEASE SAVEPOINT etape2');
  WriteLn('  RELEASE SAVEPOINT etape2');

  { Inserer un remplacement }
  QueryExec.ParamByName('nom').AsString := 'Durand';
  QueryExec.ParamByName('email').AsString := 'durand@email.fr';
  QueryExec.ExecSQL;
  WriteLn('  Insert Durand (remplacement) : OK');

  { Commit de tout }
  Trans.Commit;
  WriteLn('  COMMIT : Dupont + Martin + Durand valides');

  WriteLn;
  WriteLn('  Contenu :');
  AfficherContacts;
end;

procedure DemoImportTolerant;  
type  
  TLigneCSV = record
    Nom: string;
    Email: string;
    Valide: Boolean;
  end;
const
  NB_LIGNES = 8;
var
  Lignes: array[0..NB_LIGNES-1] of TLigneCSV;
  i, NbOK, NbErreurs: Integer;
begin
  WriteLn;
  WriteLn('--- Import tolerant aux erreurs ---');

  { Simuler des donnees CSV (certaines invalides) }
  Lignes[0].Nom := 'Petit';      Lignes[0].Email := 'petit@email.fr';
  Lignes[1].Nom := 'Leroy';      Lignes[1].Email := 'leroy@email.fr';
  Lignes[2].Nom := '';            Lignes[2].Email := 'vide@email.fr';     { Nom vide -> erreur NOT NULL }
  Lignes[3].Nom := 'Moreau';     Lignes[3].Email := 'moreau@email.fr';
  Lignes[4].Nom := 'Dupont2';    Lignes[4].Email := 'dupont@email.fr';   { Email doublon }
  Lignes[5].Nom := 'Simon';      Lignes[5].Email := 'simon@email.fr';
  Lignes[6].Nom := 'Laurent';    Lignes[6].Email := 'laurent@email.fr';
  Lignes[7].Nom := 'Roux';       Lignes[7].Email := 'roux@email.fr';

  NbOK := 0;
  NbErreurs := 0;

  for i := 0 to NB_LIGNES - 1 do
  begin
    { Savepoint pour chaque ligne }
    Conn.ExecuteDirect(Format('SAVEPOINT ligne_%d', [i]));

    try
      QueryExec.SQL.Text :=
        'INSERT INTO Contacts (nom, email) VALUES (:nom, :email)';
      QueryExec.ParamByName('nom').AsString := Lignes[i].Nom;
      QueryExec.ParamByName('email').AsString := Lignes[i].Email;
      QueryExec.ExecSQL;

      { Succes : liberer le savepoint }
      Conn.ExecuteDirect(Format('RELEASE SAVEPOINT ligne_%d', [i]));
      Inc(NbOK);
      WriteLn(Format('  Ligne %d OK    : %s (%s)', [i, Lignes[i].Nom, Lignes[i].Email]));
    except
      on E: Exception do
      begin
        { Erreur : rollback vers le savepoint }
        Conn.ExecuteDirect(Format('ROLLBACK TO ligne_%d', [i]));
        Conn.ExecuteDirect(Format('RELEASE SAVEPOINT ligne_%d', [i]));
        Inc(NbErreurs);
        WriteLn(Format('  Ligne %d ECHEC : %s -> %s',
          [i, Lignes[i].Email, E.Message]));
      end;
    end;
  end;

  Trans.Commit;

  WriteLn;
  WriteLn(Format('  Import termine : %d OK, %d erreurs sur %d lignes',
    [NbOK, NbErreurs, NB_LIGNES]));

  WriteLn;
  WriteLn('  Contenu final :');
  AfficherContacts;
end;

procedure DemoBatchParBlocs;  
const  
  NB_TOTAL = 5000;
  TAILLE_BLOC = 500;
var
  i, Bloc, NbInseres: Integer;
  Debut: TDateTime;
  Duree: Int64;
begin
  WriteLn;
  WriteLn('--- Batch par blocs avec savepoints ---');
  WriteLn(Format('  %d inserts en blocs de %d', [NB_TOTAL, TAILLE_BLOC]));

  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS BatchData (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  valeur TEXT' +
    ')');
  Trans.Commit;

  Debut := Now;
  NbInseres := 0;
  Bloc := 0;

  for i := 1 to NB_TOTAL do
  begin
    { Debut d'un nouveau bloc }
    if ((i - 1) mod TAILLE_BLOC) = 0 then
    begin
      Inc(Bloc);
      Conn.ExecuteDirect(Format('SAVEPOINT bloc_%d', [Bloc]));
    end;

    try
      QueryExec.SQL.Text := 'INSERT INTO BatchData (valeur) VALUES (:val)';
      QueryExec.ParamByName('val').AsString := Format('Donnee_%d', [i]);
      QueryExec.ExecSQL;
      Inc(NbInseres);
    except
      on E: Exception do
      begin
        WriteLn(Format('  Erreur ligne %d : %s', [i, E.Message]));
        { En cas d'erreur dans un bloc, rollback du bloc entier }
        Conn.ExecuteDirect(Format('ROLLBACK TO bloc_%d', [Bloc]));
        Conn.ExecuteDirect(Format('RELEASE SAVEPOINT bloc_%d', [Bloc]));
        { Sauter au prochain bloc }
        Continue;
      end;
    end;

    { Fin d'un bloc }
    if (i mod TAILLE_BLOC) = 0 then
    begin
      Conn.ExecuteDirect(Format('RELEASE SAVEPOINT bloc_%d', [Bloc]));
      Write(Format('  Bloc %d/%d commite' + #13,
        [Bloc, NB_TOTAL div TAILLE_BLOC]));
    end;
  end;

  Trans.Commit;
  Duree := MilliSecondsBetween(Now, Debut);

  WriteLn;
  WriteLn(Format('  Duree : %d ms', [Duree]));
  WriteLn(Format('  Inseres : %d / %d', [NbInseres, NB_TOTAL]));

  { Verifier }
  Query.SQL.Text := 'SELECT COUNT(*) AS nb FROM BatchData';
  Query.Open;
  WriteLn('  Lignes en base : ', Query.FieldByName('nb').AsInteger);
  Query.Close;

  { Nettoyage }
  Conn.ExecuteDirect('DROP TABLE IF EXISTS BatchData');
  Trans.Commit;
end;

procedure DemoRetryAvecBackoff;  
var  
  Tentatives, MaxTentatives: Integer;
  Delai: Integer;
  Success: Boolean;
begin
  WriteLn;
  WriteLn('--- Retry avec backoff exponentiel (simulation) ---');

  MaxTentatives := 4;
  Tentatives := 0;
  Delai := 100; { ms }
  Success := False;

  while (Tentatives < MaxTentatives) and (not Success) do
  begin
    Inc(Tentatives);

    try
      { Simuler une operation qui echoue les 2 premieres fois }
      if Tentatives <= 2 then
        raise Exception.Create('Erreur simulee (tentative ' +
          IntToStr(Tentatives) + ')');

      { Reussir la 3eme fois }
      QueryExec.SQL.Text :=
        'INSERT INTO Contacts (nom, email) VALUES (:nom, :email)';
      QueryExec.ParamByName('nom').AsString := 'RetryTest';
      QueryExec.ParamByName('email').AsString := 'retry@test.fr';
      QueryExec.ExecSQL;
      Trans.Commit;

      Success := True;
      WriteLn(Format('  Tentative %d/%d : SUCCES',
        [Tentatives, MaxTentatives]));
    except
      on E: Exception do
      begin
        if Trans.Active then
          Trans.Rollback;

        WriteLn(Format('  Tentative %d/%d : %s (attente %d ms)',
          [Tentatives, MaxTentatives, E.Message, Delai]));

        if Tentatives < MaxTentatives then
        begin
          Sleep(Delai);
          Delai := Delai * 2; { Backoff exponentiel }
          if Delai > 5000 then
            Delai := 5000;
        end;
      end;
    end;
  end;

  if Success then
    WriteLn('  Operation reussie apres ', Tentatives, ' tentative(s)')
  else
    WriteLn('  Operation echouee apres ', MaxTentatives, ' tentatives');
end;

begin
  WriteLn('=== Savepoints et batch ===');

  DBPath := GetTempDir + 'test_savepoints.db';
  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  QueryExec := TSQLQuery.Create(nil);
  try
    Conn.DatabaseName := DBPath;
    Conn.Transaction := Trans;
    Trans.Database := Conn;
    Query.Database := Conn;
    Query.Transaction := Trans;
    QueryExec.Database := Conn;
    QueryExec.Transaction := Trans;
    Conn.Open;

    InitialiserBase;
    DemoSavepoint;
    DemoImportTolerant;
    DemoBatchParBlocs;
    DemoRetryAvecBackoff;

    if Trans.Active then
      Trans.Commit;
    Conn.Close;

  finally
    QueryExec.Free;
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
