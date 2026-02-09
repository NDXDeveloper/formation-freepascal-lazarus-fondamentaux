{
  Section 16.13 — Gestion des erreurs de connexion et resilience
  Description : Classification erreurs, retry avec backoff exponentiel,
                test connexion, monitoring requetes lentes, logging structure
  Fichier source : 13-gestion-erreurs-connexion-resilience.md
}
program ResilienceErreurs;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, sqldb, sqlite3conn, DateUtils;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query: TSQLQuery;
  DBPath: string;
  LogFile: string;

{ --- Logging structure --- }

procedure LogMessage(const Msg: string; const Niveau: string = 'INFO');
var
  F: TextFile;
  Ligne: string;
begin
  Ligne := Format('[%s] [%-7s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), Niveau, Msg]);

  WriteLn('  ', Ligne);

  try
    AssignFile(F, LogFile);
    if FileExists(LogFile) then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, Ligne);
    CloseFile(F);
  except
    { Ignorer les erreurs de log }
  end;
end;

procedure LogErreur(const Contexte: string; E: Exception);
begin
  LogMessage(Format('%s - %s: %s', [Contexte, E.ClassName, E.Message]), 'ERROR');
end;

{ --- Classification des erreurs --- }

function ClassifierErreur(const MessageErreur: string): string;
var
  Msg: string;
begin
  Msg := LowerCase(MessageErreur);

  { Note : les messages SQLite via FPC commencent souvent par
    "tsqlite3connection : ..." — il faut tester les mots-cles specifiques
    AVANT le test generique "connection" pour eviter les faux positifs }

  if Pos('no such table', Msg) > 0 then
    Result := 'TABLE_INEXISTANTE'
  else if Pos('locked', Msg) > 0 then
    Result := 'VERROUILLAGE'
  else if (Pos('unique', Msg) > 0) or
          (Pos('duplicate', Msg) > 0) then
    Result := 'DOUBLON'
  else if Pos('not null', Msg) > 0 then
    Result := 'CONTRAINTE'
  else if Pos('constraint', Msg) > 0 then
    Result := 'CONTRAINTE'
  else if Pos('syntax', Msg) > 0 then
    Result := 'SYNTAXE_SQL'
  else if (Pos('unable to open', Msg) > 0) or
          (Pos('connection refused', Msg) > 0) or
          (Pos('could not connect', Msg) > 0) then
    Result := 'CONNEXION'
  else
    Result := 'INCONNUE';
end;

function EstErreurTemporaire(const MessageErreur: string): Boolean;
var
  Msg: string;
begin
  Msg := LowerCase(MessageErreur);
  Result := (Pos('locked', Msg) > 0) or
            (Pos('busy', Msg) > 0) or
            (Pos('timeout', Msg) > 0);
end;

{ --- Test de connexion --- }

function TesterConnexion: Boolean;
begin
  Result := False;

  if not Conn.Connected then
  begin
    LogMessage('Connexion non active', 'WARNING');
    Exit;
  end;

  try
    Query.SQL.Text := 'SELECT 1 AS ping';
    Query.Open;
    Result := (Query.FieldByName('ping').AsInteger = 1);
    Query.Close;
    LogMessage('Test connexion : OK');
  except
    on E: Exception do
    begin
      LogErreur('Test connexion', E);
      Result := False;
    end;
  end;
end;

{ --- Reconnexion avec backoff exponentiel --- }

function ReconnecterAvecBackoff(MaxTentatives: Integer;
  DelaiInitial: Integer = 200): Boolean;
var
  Tentative, Delai, Jitter: Integer;
begin
  Result := False;
  Tentative := 0;
  Delai := DelaiInitial;

  while (Tentative < MaxTentatives) and (not Result) do
  begin
    Inc(Tentative);

    try
      if Conn.Connected then
        Conn.Close;

      LogMessage(Format('Tentative de connexion %d/%d (delai=%dms)',
        [Tentative, MaxTentatives, Delai]));

      Conn.Open;
      Result := True;
      LogMessage('Reconnexion reussie');

    except
      on E: Exception do
      begin
        LogErreur(Format('Tentative %d', [Tentative]), E);

        if Tentative < MaxTentatives then
        begin
          Jitter := Random(Delai div 4);
          Sleep(Delai + Jitter);
          Delai := Delai * 2;
          if Delai > 5000 then
            Delai := 5000;
        end;
      end;
    end;
  end;
end;

{ --- Execution avec monitoring --- }

procedure ExecuterAvecMonitoring(const SQL: string; SeuilMs: Integer = 100);
var
  Debut: TDateTime;
  Duree: Int64;
begin
  Debut := Now;

  try
    Query.SQL.Text := SQL;
    Query.ExecSQL;

    Duree := MilliSecondsBetween(Now, Debut);

    if Duree > SeuilMs then
      LogMessage(Format('Requete lente (%dms) : %s',
        [Duree, Copy(SQL, 1, 80)]), 'WARNING')
    else
      LogMessage(Format('Requete OK (%dms)', [Duree]), 'DEBUG');
  except
    on E: Exception do
    begin
      LogErreur('Execution requete', E);
      raise;
    end;
  end;
end;

{ --- Execution avec retry --- }

function ExecuterAvecRetry(const SQL: string;
  MaxTentatives: Integer = 3): Boolean;
var
  Tentative, Delai: Integer;
begin
  Result := False;
  Tentative := 0;
  Delai := 100;

  while (Tentative < MaxTentatives) and (not Result) do
  begin
    Inc(Tentative);

    try
      if not Conn.Connected then
      begin
        LogMessage('Connexion perdue, reconnexion...');
        if not ReconnecterAvecBackoff(3) then
        begin
          LogMessage('Reconnexion impossible', 'ERROR');
          Exit;
        end;
      end;

      Query.SQL.Text := SQL;
      Query.ExecSQL;
      Trans.Commit;
      Result := True;

    except
      on E: Exception do
      begin
        if Trans.Active then
          Trans.Rollback;

        if EstErreurTemporaire(E.Message) then
        begin
          LogMessage(Format('Erreur temporaire, retry %d/%d',
            [Tentative, MaxTentatives]), 'WARNING');
          Sleep(Delai);
          Delai := Delai * 2;
        end
        else
        begin
          LogErreur('Erreur non temporaire', E);
          raise;
        end;
      end;
    end;
  end;
end;

{ --- Procedures de demo --- }

procedure DemoClassification;
begin
  WriteLn;
  WriteLn('--- Classification des erreurs ---');

  { Erreur table inexistante }
  try
    Query.SQL.Text := 'SELECT * FROM TableInexistante';
    Query.Open;
  except
    on E: Exception do
    begin
      WriteLn(Format('  Erreur    : %s', [E.Message]));
      WriteLn(Format('  Categorie : %s', [ClassifierErreur(E.Message)]));
      WriteLn(Format('  Temporaire: %s', [BoolToStr(
        EstErreurTemporaire(E.Message), 'Oui', 'Non')]));
    end;
  end;

  WriteLn;

  { Erreur de contrainte }
  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS TestUnique (' +
    '  id INTEGER PRIMARY KEY,' +
    '  valeur TEXT UNIQUE' +
    ')');
  Trans.Commit;

  try
    Query.SQL.Text := 'INSERT INTO TestUnique (valeur) VALUES (''doublon'')';
    Query.ExecSQL;
    Trans.Commit;

    Query.SQL.Text := 'INSERT INTO TestUnique (valeur) VALUES (''doublon'')';
    Query.ExecSQL;
    Trans.Commit;
  except
    on E: Exception do
    begin
      WriteLn(Format('  Erreur    : %s', [E.Message]));
      WriteLn(Format('  Categorie : %s', [ClassifierErreur(E.Message)]));
      WriteLn(Format('  Temporaire: %s', [BoolToStr(
        EstErreurTemporaire(E.Message), 'Oui', 'Non')]));
      if Trans.Active then
        Trans.Rollback;
    end;
  end;

  { Erreur de syntaxe SQL }
  WriteLn;
  try
    Query.SQL.Text := 'SELEKT * FRUM Blah';
    Query.Open;
  except
    on E: Exception do
    begin
      WriteLn(Format('  Erreur    : %s', [E.Message]));
      WriteLn(Format('  Categorie : %s', [ClassifierErreur(E.Message)]));
    end;
  end;

  Conn.ExecuteDirect('DROP TABLE IF EXISTS TestUnique');
  Trans.Commit;
end;

procedure DemoTestConnexion;
begin
  WriteLn;
  WriteLn('--- Test de connexion (ping) ---');

  WriteLn('  Connected : ', Conn.Connected);
  WriteLn('  Ping      : ', BoolToStr(TesterConnexion, 'OK', 'ECHEC'));

  { Test version SQLite }
  Query.SQL.Text := 'SELECT sqlite_version() AS ver';
  Query.Open;
  WriteLn('  Version   : ', Query.FieldByName('ver').AsString);
  Query.Close;
end;

procedure DemoMonitoring;
var
  i: Integer;
begin
  WriteLn;
  WriteLn('--- Monitoring des requetes ---');

  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS MonitorTest (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  donnee TEXT' +
    ')');
  Trans.Commit;

  { Requetes rapides }
  for i := 1 to 100 do
  begin
    Query.SQL.Text := 'INSERT INTO MonitorTest (donnee) VALUES (:val)';
    Query.ParamByName('val').AsString := 'Test ' + IntToStr(i);
    Query.ExecSQL;
  end;
  Trans.Commit;
  LogMessage('100 inserts rapides termines');

  { Requete avec monitoring (INSERT pour illustrer ExecSQL) }
  ExecuterAvecMonitoring(
    'INSERT INTO MonitorTest (donnee) VALUES (''monitored'')', 50);

  { Nettoyage }
  Conn.ExecuteDirect('DROP TABLE IF EXISTS MonitorTest');
  Trans.Commit;
end;

procedure DemoRetry;
begin
  WriteLn;
  WriteLn('--- Execution avec retry ---');

  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS RetryTest (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  valeur TEXT' +
    ')');
  Trans.Commit;

  { Insert normal }
  if ExecuterAvecRetry(
    'INSERT INTO RetryTest (valeur) VALUES (''test retry'')') then
    LogMessage('Insert avec retry : OK')
  else
    LogMessage('Insert avec retry : ECHEC', 'ERROR');

  { Verifier }
  Query.SQL.Text := 'SELECT COUNT(*) AS nb FROM RetryTest';
  Query.Open;
  LogMessage(Format('Lignes dans RetryTest : %d',
    [Query.FieldByName('nb').AsInteger]));
  Query.Close;

  Conn.ExecuteDirect('DROP TABLE IF EXISTS RetryTest');
  Trans.Commit;
end;

procedure DemoReconnexion;
begin
  WriteLn;
  WriteLn('--- Reconnexion avec backoff ---');

  { Fermer et rouvrir la connexion }
  LogMessage('Fermeture de la connexion...');
  Conn.Close;
  WriteLn('  Connected : ', Conn.Connected);

  { Reconnecter }
  if ReconnecterAvecBackoff(3, 100) then
    WriteLn('  Reconnecte : ', Conn.Connected)
  else
    WriteLn('  Echec de reconnexion');

  { Verifier que la connexion fonctionne }
  if Conn.Connected then
  begin
    Query.SQL.Text := 'SELECT 1 AS ok';
    Query.Open;
    WriteLn('  Verification post-reconnexion : ',
      Query.FieldByName('ok').AsString);
    Query.Close;
  end;
end;

procedure DemoLogging;
var
  F: TextFile;
  Ligne: string;
  Count: Integer;
begin
  WriteLn;
  WriteLn('--- Logging structure ---');

  LogMessage('Demarrage de l''application');
  LogMessage('Connexion a la base de donnees');
  LogMessage('Requete lente detectee (simulation)', 'WARNING');
  LogMessage('Erreur de contrainte (simulation)', 'ERROR');
  LogMessage('Fin du traitement');

  WriteLn;
  WriteLn('  Fichier log : ', LogFile);

  { Lire et afficher le contenu du log }
  if FileExists(LogFile) then
  begin
    WriteLn('  Contenu du fichier log :');
    Count := 0;
    AssignFile(F, LogFile);
    Reset(F);
    while not System.EOF(F) do
    begin
      System.ReadLn(F, Ligne);
      Inc(Count);
    end;
    CloseFile(F);
    WriteLn(Format('  %d lignes de log ecrites', [Count]));
  end;
end;

procedure DemoStatistiques;
var
  NbRequetes, NbErreurs, NbRetries: Integer;
  i: Integer;
begin
  WriteLn;
  WriteLn('--- Statistiques de resilience ---');

  NbRequetes := 0;
  NbErreurs := 0;
  NbRetries := 0;

  Conn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS Stats (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  valeur TEXT NOT NULL' +
    ')');
  Trans.Commit;

  { Simuler une serie d'operations }
  for i := 1 to 20 do
  begin
    Inc(NbRequetes);
    try
      Query.SQL.Text := 'INSERT INTO Stats (valeur) VALUES (:val)';
      Query.ParamByName('val').AsString := Format('Stat_%d', [i]);
      Query.ExecSQL;
    except
      on E: Exception do
      begin
        Inc(NbErreurs);
        if Trans.Active then
          Trans.Rollback;
      end;
    end;
  end;
  Trans.Commit;

  WriteLn(Format('  Requetes executees  : %d', [NbRequetes]));
  WriteLn(Format('  Erreurs             : %d', [NbErreurs]));
  WriteLn(Format('  Retries             : %d', [NbRetries]));
  WriteLn(Format('  Taux de succes      : %.1f%%',
    [(NbRequetes - NbErreurs) / NbRequetes * 100]));

  Conn.ExecuteDirect('DROP TABLE IF EXISTS Stats');
  Trans.Commit;
end;

begin
  WriteLn('=== Resilience et gestion des erreurs ===');

  Randomize;
  DBPath := GetTempDir + 'test_resilience.db';
  LogFile := GetTempDir + 'test_resilience.log';

  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Conn.DatabaseName := DBPath;
    Conn.Transaction := Trans;
    Trans.Database := Conn;
    Query.Database := Conn;
    Query.Transaction := Trans;
    Conn.Params.Add('busy_timeout=5000');
    Conn.Open;

    LogMessage('Application demarree');

    DemoClassification;
    DemoTestConnexion;
    DemoMonitoring;
    DemoRetry;
    DemoReconnexion;
    DemoLogging;
    DemoStatistiques;

    LogMessage('Application terminee');

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
    if FileExists(LogFile) then
      DeleteFile(LogFile);
  end;

  WriteLn;
  WriteLn('=== Fin ===');
end.
