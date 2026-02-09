{
  Section 16.9 — Transactions basics
  Description : ACID demo, commit/rollback, transfert bancaire atomique,
                batch inserts avec performance comparee
  Fichier source : 09-transactions-basics.md
}
program Transactions;

{$mode objfpc}{$H+}

uses
  SysUtils, sqldb, sqlite3conn, DateUtils;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query: TSQLQuery;
  DBPath: string;

procedure AfficherComptes;
begin
  Query.SQL.Text := 'SELECT * FROM Comptes ORDER BY id';
  Query.Open;

  WriteLn(Format('  %-4s %-15s %10s', ['ID', 'Titulaire', 'Solde']));
  WriteLn('  ', StringOfChar('-', 32));

  while not Query.EOF do
  begin
    WriteLn(Format('  %-4d %-15s %10.2f', [
      Query.FieldByName('id').AsInteger,
      Query.FieldByName('titulaire').AsString,
      Query.FieldByName('solde').AsFloat
    ]));
    Query.Next;
  end;

  Query.Close;
end;

function SommeComptes: Double;
begin
  Query.SQL.Text := 'SELECT SUM(solde) AS total FROM Comptes';
  Query.Open;
  Result := Query.FieldByName('total').AsFloat;
  Query.Close;
end;

procedure InitialiserBase;
begin
  WriteLn('--- Initialisation ---');
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Comptes (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  titulaire TEXT NOT NULL,' +
    '  solde REAL NOT NULL DEFAULT 0 CHECK(solde >= 0)' +
    ')');
  Trans.Commit;

  { Inserer des comptes initiaux }
  Query.SQL.Text :=
    'INSERT INTO Comptes (titulaire, solde) VALUES (:nom, :solde)';

  Query.ParamByName('nom').AsString := 'Alice';
  Query.ParamByName('solde').AsFloat := 1000.00;
  Query.ExecSQL;

  Query.ParamByName('nom').AsString := 'Bob';
  Query.ParamByName('solde').AsFloat := 500.00;
  Query.ExecSQL;

  Query.ParamByName('nom').AsString := 'Charlie';
  Query.ParamByName('solde').AsFloat := 750.00;
  Query.ExecSQL;

  Trans.Commit;
  WriteLn('Comptes crees');
  AfficherComptes;
  WriteLn(Format('  Total des soldes : %.2f EUR', [SommeComptes]));
end;

procedure DemoTransfertReussi;
var
  TotalAvant, TotalApres: Double;
begin
  WriteLn;
  WriteLn('--- Transfert reussi (COMMIT) ---');
  WriteLn('  Transfert de 200 EUR de Alice vers Bob');

  TotalAvant := SommeComptes;

  try
    { Debiter Alice }
    Query.SQL.Text :=
      'UPDATE Comptes SET solde = solde - :montant WHERE titulaire = :nom';
    Query.ParamByName('montant').AsFloat := 200.00;
    Query.ParamByName('nom').AsString := 'Alice';
    Query.ExecSQL;

    { Crediter Bob }
    Query.SQL.Text :=
      'UPDATE Comptes SET solde = solde + :montant WHERE titulaire = :nom';
    Query.ParamByName('montant').AsFloat := 200.00;
    Query.ParamByName('nom').AsString := 'Bob';
    Query.ExecSQL;

    { Tout valider en une seule fois }
    Trans.Commit;
    WriteLn('  COMMIT effectue');
  except
    on E: Exception do
    begin
      Trans.Rollback;
      WriteLn('  ROLLBACK : ', E.Message);
    end;
  end;

  AfficherComptes;
  TotalApres := SommeComptes;
  WriteLn(Format('  Total avant : %.2f, apres : %.2f (coherence : %s)',
    [TotalAvant, TotalApres, BoolToStr(Abs(TotalAvant - TotalApres) < 0.01,
    'OK', 'ERREUR')]));
end;

procedure DemoRollback;
var
  TotalAvant, TotalApres: Double;
begin
  WriteLn;
  WriteLn('--- Rollback (annulation) ---');
  WriteLn('  Tentative de transfert de 2000 EUR de Bob vers Charlie');
  WriteLn('  (Bob n''a que 700 EUR -> erreur CHECK)');

  TotalAvant := SommeComptes;

  try
    { Crediter Charlie d'abord (ca marche) }
    Query.SQL.Text :=
      'UPDATE Comptes SET solde = solde + :montant WHERE titulaire = :nom';
    Query.ParamByName('montant').AsFloat := 2000.00;
    Query.ParamByName('nom').AsString := 'Charlie';
    Query.ExecSQL;

    { Debiter Bob (devrait echouer : CHECK solde >= 0) }
    Query.SQL.Text :=
      'UPDATE Comptes SET solde = solde - :montant WHERE titulaire = :nom';
    Query.ParamByName('montant').AsFloat := 2000.00;
    Query.ParamByName('nom').AsString := 'Bob';
    Query.ExecSQL;

    Trans.Commit;
    WriteLn('  COMMIT effectue');
  except
    on E: Exception do
    begin
      WriteLn('  Erreur detectee : ', E.Message);
      Trans.Rollback;
      WriteLn('  ROLLBACK effectue');
    end;
  end;

  AfficherComptes;
  TotalApres := SommeComptes;
  WriteLn(Format('  Total avant : %.2f, apres : %.2f (coherence : %s)',
    [TotalAvant, TotalApres, BoolToStr(Abs(TotalAvant - TotalApres) < 0.01,
    'OK', 'ERREUR')]));
end;

procedure DemoTransactionActive;
begin
  WriteLn;
  WriteLn('--- Propriete Active ---');

  WriteLn('  Active avant operation : ', Trans.Active);

  Query.SQL.Text := 'SELECT COUNT(*) FROM Comptes';
  Query.Open;
  Query.Close;

  WriteLn('  Active apres SELECT   : ', Trans.Active);

  if Trans.Active then
    Trans.Commit;

  WriteLn('  Active apres Commit   : ', Trans.Active);
end;

procedure DemoPerformanceBatch;
const
  NB_INSERTS = 5000;
var
  Debut: TDateTime;
  DureeSansTransaction, DureeAvecTransaction: Int64;
  i: Integer;
begin
  WriteLn;
  WriteLn('--- Performance : 1 transaction vs N commits ---');
  WriteLn(Format('  Insertion de %d enregistrements', [NB_INSERTS]));

  { Creer la table de logs }
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Logs (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  message TEXT,' +
    '  timestamp TEXT' +
    ')');
  Trans.Commit;

  { Test 1 : un commit par insert (lent) }
  WriteLn;
  WriteLn('  Test 1 : un commit par insert...');
  Debut := Now;

  for i := 1 to NB_INSERTS do
  begin
    Query.SQL.Text := 'INSERT INTO Logs (message, timestamp) VALUES (:msg, :ts)';
    Query.ParamByName('msg').AsString := 'Message ' + IntToStr(i);
    Query.ParamByName('ts').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    Query.ExecSQL;
    Trans.Commit;
  end;

  DureeSansTransaction := MilliSecondsBetween(Now, Debut);
  WriteLn(Format('  Duree : %d ms', [DureeSansTransaction]));

  { Vider la table }
  Conn.ExecuteDirect('DELETE FROM Logs');
  Trans.Commit;

  { Test 2 : un seul commit pour tous (rapide) }
  WriteLn;
  WriteLn('  Test 2 : un seul commit pour tous...');
  Debut := Now;

  for i := 1 to NB_INSERTS do
  begin
    Query.SQL.Text := 'INSERT INTO Logs (message, timestamp) VALUES (:msg, :ts)';
    Query.ParamByName('msg').AsString := 'Message ' + IntToStr(i);
    Query.ParamByName('ts').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    Query.ExecSQL;
  end;
  Trans.Commit;

  DureeAvecTransaction := MilliSecondsBetween(Now, Debut);
  WriteLn(Format('  Duree : %d ms', [DureeAvecTransaction]));

  { Comparaison }
  WriteLn;
  if DureeAvecTransaction > 0 then
    WriteLn(Format('  Gain : x%.0f plus rapide avec une seule transaction',
      [DureeSansTransaction / DureeAvecTransaction]))
  else
    WriteLn('  Gain : transaction unique quasi-instantanee');

  { Verifier }
  Query.SQL.Text := 'SELECT COUNT(*) AS nb FROM Logs';
  Query.Open;
  WriteLn('  Lignes inserees : ', Query.FieldByName('nb').AsInteger);
  Query.Close;

  { Nettoyage }
  Conn.ExecuteDirect('DROP TABLE IF EXISTS Logs');
  Trans.Commit;
end;

procedure DemoBatchParLots;
const
  NB_TOTAL = 10000;
  TAILLE_LOT = 1000;
var
  i: Integer;
  Debut: TDateTime;
  Duree: Int64;
begin
  WriteLn;
  WriteLn('--- Transactions par lots ---');
  WriteLn(Format('  %d inserts en lots de %d', [NB_TOTAL, TAILLE_LOT]));

  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS BatchTest (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  valeur TEXT' +
    ')');
  Trans.Commit;

  Debut := Now;

  for i := 1 to NB_TOTAL do
  begin
    Query.SQL.Text := 'INSERT INTO BatchTest (valeur) VALUES (:val)';
    Query.ParamByName('val').AsString := 'Valeur ' + IntToStr(i);
    Query.ExecSQL;

    { Commit tous les TAILLE_LOT }
    if (i mod TAILLE_LOT) = 0 then
    begin
      Trans.Commit;
      Write(Format('  Lot %d/%d commite' + #13,
        [i div TAILLE_LOT, NB_TOTAL div TAILLE_LOT]));
    end;
  end;

  { Commit du reste }
  if Trans.Active then
    Trans.Commit;

  Duree := MilliSecondsBetween(Now, Debut);
  WriteLn;
  WriteLn(Format('  Duree : %d ms pour %d inserts', [Duree, NB_TOTAL]));

  { Verifier }
  Query.SQL.Text := 'SELECT COUNT(*) AS nb FROM BatchTest';
  Query.Open;
  WriteLn('  Lignes inserees : ', Query.FieldByName('nb').AsInteger);
  Query.Close;

  { Nettoyage }
  Conn.ExecuteDirect('DROP TABLE IF EXISTS BatchTest');
  Trans.Commit;
end;

begin
  WriteLn('=== Transactions SQLite ===');

  DBPath := GetTempDir + 'test_transactions.db';
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
    DemoTransfertReussi;
    DemoRollback;
    DemoTransactionActive;
    DemoPerformanceBatch;
    DemoBatchParLots;

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
