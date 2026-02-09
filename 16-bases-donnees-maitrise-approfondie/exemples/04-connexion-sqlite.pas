{
  Section 16.4 — Composants de connexion
  Description : Connexion SQLite, configuration (foreign_keys, journal_mode),
                verification Connected, chemins cross-platform, gestion erreurs
  Fichier source : 04-composants-connexion.md
}
program ConnexionSQLite;

{$mode objfpc}{$H+}

uses
  SysUtils, sqldb, sqlite3conn;

var
  Conn, Conn2: TSQLite3Connection;
  Trans: TSQLTransaction;
  Query: TSQLQuery;
  DBPath: string;

begin
  WriteLn('=== Connexion SQLite ===');
  WriteLn;

  { Chemin de la base de donnees temporaire }
  DBPath := GetTempDir + 'test_connexion.db';
  WriteLn('Chemin de la base : ', DBPath);

  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    { --- Configuration de la connexion --- }
    WriteLn;
    WriteLn('--- Configuration ---');
    Conn.DatabaseName := DBPath;
    Conn.CharSet := 'UTF8';
    Conn.Transaction := Trans;
    Trans.Database := Conn;
    Query.Database := Conn;
    Query.Transaction := Trans;
    WriteLn('DatabaseName   : ', Conn.DatabaseName);
    WriteLn('CharSet        : ', Conn.CharSet);

    { --- Parametres SQLite --- }
    Conn.Params.Clear;
    Conn.Params.Add('foreign_keys=ON');
    Conn.Params.Add('journal_mode=WAL');
    Conn.Params.Add('busy_timeout=5000');
    WriteLn('Params         : ', Conn.Params.Text);

    { --- Ouverture --- }
    WriteLn('--- Ouverture de la connexion ---');
    try
      Conn.Open;
      WriteLn('Connected      : ', Conn.Connected);
    except
      on E: Exception do
      begin
        WriteLn('ERREUR a l''ouverture : ', E.Message);
        Halt(1);
      end;
    end;

    { --- Verification via une requete simple --- }
    WriteLn;
    WriteLn('--- Verification de la connexion ---');
    Query.SQL.Text := 'SELECT sqlite_version() AS version';
    Query.Open;
    WriteLn('Version SQLite : ', Query.FieldByName('version').AsString);
    Query.Close;

    { --- Verification des parametres PRAGMA --- }
    Query.SQL.Text := 'PRAGMA foreign_keys';
    Query.Open;
    WriteLn('foreign_keys   : ', Query.Fields[0].AsString);
    Query.Close;

    Query.SQL.Text := 'PRAGMA journal_mode';
    Query.Open;
    WriteLn('journal_mode   : ', Query.Fields[0].AsString);
    Query.Close;

    { --- Creation d'une table de test --- }
    WriteLn;
    WriteLn('--- Creation de table ---');
    Conn.ExecuteDirect(
      'CREATE TABLE IF NOT EXISTS TestConnexion (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  nom TEXT NOT NULL' +
      ')');
    Trans.Commit;
    WriteLn('Table TestConnexion creee');

    { --- Verification de l''existence de la table --- }
    Query.SQL.Text :=
      'SELECT name FROM sqlite_master WHERE type=''table'' AND name=''TestConnexion''';
    Query.Open;
    if not Query.IsEmpty then
      WriteLn('Table trouvee  : ', Query.FieldByName('name').AsString)
    else
      WriteLn('Table NON trouvee !');
    Query.Close;

    { --- Chemins cross-platform --- }
    WriteLn;
    WriteLn('--- Chemins cross-platform ---');
    WriteLn('PathDelim      : ', PathDelim);
    WriteLn('GetTempDir     : ', GetTempDir);
    WriteLn('GetCurrentDir  : ', GetCurrentDir);
    WriteLn('Chemin construit : ',
      GetCurrentDir + PathDelim + 'data' + PathDelim + 'ma_base.db');

    { --- Test de gestion d'erreurs : chemin invalide --- }
    WriteLn;
    WriteLn('--- Test gestion d''erreurs ---');
    begin
      Conn2 := TSQLite3Connection.Create(nil);
      try
        Conn2.DatabaseName := '/chemin/invalide/inexistant/base.db';
        Conn2.Transaction := TSQLTransaction.Create(Conn2);
        TSQLTransaction(Conn2.Transaction).Database := Conn2;
        try
          Conn2.Open;
          WriteLn('INATTENDU : connexion reussie a un chemin invalide');
        except
          on E: Exception do
            WriteLn('Erreur attendue : ', E.ClassName, ' - ', E.Message);
        end;
      finally
        Conn2.Free;
      end;
    end;

    { --- Fermeture propre --- }
    WriteLn;
    WriteLn('--- Fermeture ---');
    if Trans.Active then
      Trans.Commit;
    Conn.Close;
    WriteLn('Connected      : ', Conn.Connected);
    WriteLn('Connexion fermee proprement');

  finally
    Query.Free;
    Trans.Free;
    Conn.Free;

    { Nettoyage du fichier temporaire }
    if FileExists(DBPath) then
      DeleteFile(DBPath);
    { Supprimer aussi les fichiers WAL et SHM }
    if FileExists(DBPath + '-wal') then
      DeleteFile(DBPath + '-wal');
    if FileExists(DBPath + '-shm') then
      DeleteFile(DBPath + '-shm');
  end;

  WriteLn;
  WriteLn('=== Fin ===');
end.
