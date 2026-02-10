🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.13 Gestion des erreurs de connexion et résilience

## Introduction : créer des applications robustes

Dans le monde réel, les choses ne se passent pas toujours comme prévu :
- 📡 Le réseau peut tomber
- 🔌 Le serveur de base de données peut redémarrer
- ⏱️ Les requêtes peuvent être trop longues
- 🔒 La base de données peut être saturée
- 💻 Le pare-feu peut bloquer la connexion

Une application **robuste** doit gérer ces situations gracieusement au lieu de planter. Cette section vous apprend à créer des applications **résilientes** qui continuent de fonctionner même quand les choses tournent mal.

### L'analogie du téléphone

Imaginez que vous appelez un ami :

**Application fragile :**
- L'appel ne passe pas → vous abandonnez et fermez le téléphone
- Votre ami ne répond pas dans les 2 secondes → vous raccrochez
- La ligne coupe pendant la conversation → vous jetez le téléphone par la fenêtre 😅

**Application résiliente :**
- L'appel ne passe pas → vous réessayez 3 fois
- Votre ami ne répond pas → vous attendez raisonnablement, puis laissez un message
- La ligne coupe → vous rappelez automatiquement
- Si vraiment impossible → vous informez gentiment l'utilisateur

C'est cette deuxième approche que nous allons apprendre !

## Les types d'erreurs de connexion

### Erreurs à la connexion initiale

**Causes courantes :**
- ❌ Serveur de base de données arrêté
- ❌ Mauvais nom d'hôte ou port
- ❌ Identifiants incorrects (utilisateur/mot de passe)
- ❌ Base de données inexistante
- ❌ Pare-feu qui bloque
- ❌ Réseau indisponible

**Message typique :**
```
Could not connect to server  
Connection refused  
Access denied for user  
Unknown database  
```

### Erreurs pendant l'exécution

**Causes courantes :**
- ❌ Perte de connexion réseau
- ❌ Timeout de requête
- ❌ Serveur qui redémarre
- ❌ Trop de connexions simultanées
- ❌ Erreur SQL dans une requête

**Message typique :**
```
Lost connection to MySQL server during query  
Server has gone away  
Lock wait timeout exceeded  
Too many connections  
```

### Erreurs de transaction

**Causes courantes :**
- ❌ Deadlock
- ❌ Violation de contrainte
- ❌ Transaction trop longue

**Message typique :**
```
Deadlock detected  
Duplicate entry  
Foreign key constraint fails  
```

## Détecter et identifier les erreurs

### La hiérarchie des exceptions

FreePascal/Lazarus utilise une hiérarchie d'exceptions pour les erreurs de base de données :

```
Exception (base)
  └─ EDatabaseError (erreurs BDD génériques)
      ├─ EIBDatabaseError (InterBase/Firebird)
      ├─ EPQDatabaseError (PostgreSQL)
      ├─ EMySQLDatabaseError (MySQL/MariaDB)
      └─ ESQLite3Exception (SQLite)
```

### Capturer une erreur spécifique

```pascal
uses
  sqldb, pqconnection;  // ou mysql80conn

procedure TForm1.ExecuterRequete;  
begin  
  try
    SQLQuery1.SQL.Text := 'SELECT * FROM clients';
    SQLQuery1.Open;
  except
    // Erreur PostgreSQL spécifique
    on E: EPQDatabaseError do
    begin
      ShowMessage('Erreur PostgreSQL : ' + E.Message);
    end;

    // Erreur générique de base de données
    on E: EDatabaseError do
    begin
      ShowMessage('Erreur de base de données : ' + E.Message);
    end;

    // Toute autre exception
    on E: Exception do
    begin
      ShowMessage('Erreur inattendue : ' + E.Message);
    end;
  end;
end;
```

### Analyser le message d'erreur

```pascal
function TForm1.AnalyserErreur(const MessageErreur: string): string;  
begin  
  Result := 'Erreur inconnue';

  // Problème de connexion
  if (Pos('connection', LowerCase(MessageErreur)) > 0) or
     (Pos('connect', LowerCase(MessageErreur)) > 0) then
    Result := 'Impossible de se connecter au serveur'

  // Timeout
  else if Pos('timeout', LowerCase(MessageErreur)) > 0 then
    Result := 'Délai d''attente dépassé'

  // Authentification
  else if (Pos('access denied', LowerCase(MessageErreur)) > 0) or
          (Pos('authentication', LowerCase(MessageErreur)) > 0) then
    Result := 'Identifiants incorrects'

  // Base inexistante
  else if Pos('unknown database', LowerCase(MessageErreur)) > 0 then
    Result := 'Base de données introuvable'

  // Deadlock
  else if Pos('deadlock', LowerCase(MessageErreur)) > 0 then
    Result := 'Conflit entre transactions'

  // Contrainte violée
  else if (Pos('constraint', LowerCase(MessageErreur)) > 0) or
          (Pos('duplicate', LowerCase(MessageErreur)) > 0) then
    Result := 'Données en conflit avec les règles'

  // Trop de connexions
  else if Pos('too many connections', LowerCase(MessageErreur)) > 0 then
    Result := 'Serveur saturé';
end;
```

## Stratégies de reconnexion

### Reconnexion simple

```pascal
function TForm1.SeConnecter: Boolean;  
begin  
  Result := False;

  try
    if PQConnection1.Connected then
      PQConnection1.Close;

    PQConnection1.Open;
    Result := True;
    ShowMessage('Connecté avec succès');
  except
    on E: Exception do
    begin
      ShowMessage('Échec de la connexion : ' + E.Message);
    end;
  end;
end;
```

### Reconnexion avec plusieurs tentatives

```pascal
function TForm1.SeConnecterAvecRetry(MaxTentatives: Integer): Boolean;  
var  
  Tentative: Integer;
  Delai: Integer;
begin
  Result := False;
  Tentative := 0;

  while (Tentative < MaxTentatives) and (not Result) do
  begin
    Inc(Tentative);

    try
      if PQConnection1.Connected then
        PQConnection1.Close;

      LabelStatut.Caption := Format('Tentative %d/%d...',
        [Tentative, MaxTentatives]);
      Application.ProcessMessages;

      PQConnection1.Open;
      Result := True;
      LabelStatut.Caption := 'Connecté ✓';

    except
      on E: Exception do
      begin
        if Tentative >= MaxTentatives then
        begin
          LabelStatut.Caption := 'Échec de connexion ✗';
          ShowMessage('Impossible de se connecter après ' +
                      IntToStr(MaxTentatives) + ' tentatives');
        end
        else
        begin
          // Délai progressif : 1s, 2s, 4s, 8s...
          Delai := 1000 * Round(Power(2, Tentative - 1));
          if Delai > 10000 then Delai := 10000;  // Maximum 10 secondes

          LabelStatut.Caption := Format('Échec. Nouvelle tentative dans %ds...',
            [Delai div 1000]);
          Application.ProcessMessages;
          Sleep(Delai);
        end;
      end;
    end;
  end;
end;
```

### Reconnexion avec backoff exponentiel

Le **backoff exponentiel** augmente progressivement le délai entre les tentatives :

```pascal
function TForm1.ReconnecterAvecBackoff(
  MaxTentatives: Integer;
  DelaiInitial: Integer = 1000): Boolean;
var
  Tentative: Integer;
  Delai: Integer;
  Jitter: Integer;
begin
  Result := False;
  Tentative := 0;
  Delai := DelaiInitial;

  while (Tentative < MaxTentatives) and (not Result) do
  begin
    Inc(Tentative);

    try
      if PQConnection1.Connected then
        PQConnection1.Close;

      PQConnection1.Open;
      Result := True;
      LogMessage('Reconnexion réussie après ' + IntToStr(Tentative) + ' tentative(s)');

    except
      on E: Exception do
      begin
        LogMessage(Format('Tentative %d échouée : %s', [Tentative, E.Message]));

        if Tentative < MaxTentatives then
        begin
          // Ajouter un jitter (variation aléatoire) pour éviter les thundering herd
          Jitter := Random(Delai div 4);
          Sleep(Delai + Jitter);

          // Doubler le délai pour la prochaine fois (backoff exponentiel)
          Delai := Delai * 2;

          // Plafonner à 30 secondes
          if Delai > 30000 then
            Delai := 30000;
        end;
      end;
    end;
  end;
end;
```

**Avantages du backoff exponentiel :**
- ✅ Ne surcharge pas un serveur déjà en difficulté
- ✅ Donne du temps pour que le problème se résolve
- ✅ Jitter évite que tous les clients réessayent en même temps

## Vérifier l'état de la connexion

### Propriété Connected

```pascal
if PQConnection1.Connected then
  ShowMessage('Connecté')
else
  ShowMessage('Déconnecté');
```

### Fonction de test active

```pascal
function TForm1.TesterConnexion: Boolean;  
begin  
  Result := False;

  if not PQConnection1.Connected then
    Exit;

  try
    // Exécuter une requête simple
    SQLQuery1.SQL.Text := 'SELECT 1';
    SQLQuery1.Open;
    SQLQuery1.Close;
    Result := True;
  except
    Result := False;
  end;
end;
```

### Ping de base de données

**PostgreSQL :**
```pascal
function TForm1.PingPostgreSQL: Boolean;  
begin  
  Result := False;

  try
    SQLQuery1.SQL.Text := 'SELECT 1 AS ping';
    SQLQuery1.Open;

    Result := (SQLQuery1.FieldByName('ping').AsInteger = 1);

    SQLQuery1.Close;
  except
    Result := False;
  end;
end;
```

**MySQL/MariaDB :**
```pascal
function TForm1.PingMySQL: Boolean;  
begin  
  Result := False;

  try
    SQLQuery1.SQL.Text := 'SELECT 1 AS ping';
    SQLQuery1.Open;

    Result := (SQLQuery1.RecordCount > 0);

    SQLQuery1.Close;
  except
    Result := False;
  end;
end;
```

## Gestion des timeouts

### Configurer les timeouts de connexion

**PostgreSQL :**
```pascal
procedure TForm1.ConfigurerTimeoutsPostgreSQL;  
begin  
  PQConnection1.Params.Clear;

  // Timeout de connexion (en secondes)
  PQConnection1.Params.Add('connect_timeout=10');

  // Après la connexion, définir les timeouts d'exécution
  try
    PQConnection1.Open;

    SQLQuery1.SQL.Text := 'SET statement_timeout = ''30s''';  // 30 secondes max par requête
    SQLQuery1.ExecSQL;

    SQLQuery1.SQL.Text := 'SET lock_timeout = ''10s''';  // 10 secondes d'attente pour un verrou
    SQLQuery1.ExecSQL;
  except
    on E: Exception do
      ShowMessage('Erreur de configuration : ' + E.Message);
  end;
end;
```

**MySQL/MariaDB :**
```pascal
procedure TForm1.ConfigurerTimeoutsMySQL;  
begin  
  MySQL80Connection1.Params.Clear;

  // Timeout de connexion
  MySQL80Connection1.Params.Add('connect_timeout=10');

  // Après la connexion
  try
    MySQL80Connection1.Open;

    // Timeout de requête (en secondes)
    SQLQuery1.SQL.Text := 'SET SESSION max_execution_time = 30000';  // 30 secondes (en millisecondes)
    SQLQuery1.ExecSQL;

    // Timeout d'attente de verrou
    SQLQuery1.SQL.Text := 'SET SESSION innodb_lock_wait_timeout = 10';
    SQLQuery1.ExecSQL;
  except
    on E: Exception do
      ShowMessage('Erreur de configuration : ' + E.Message);
  end;
end;
```

### Gérer les timeouts dans le code

```pascal
procedure TForm1.ExecuterAvecTimeout(const SQL: string; TimeoutSec: Integer);  
var  
  Thread: TThread;
  Termine: Boolean;
  ErreurMsg: string;
  Debut: QWord;
begin
  Termine := False;
  ErreurMsg := '';

  // Créer un thread pour l'exécution
  Thread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        SQLQuery1.SQL.Text := SQL;
        SQLQuery1.ExecSQL;
        Termine := True;
      except
        on E: Exception do
        begin
          ErreurMsg := E.Message;
          Termine := True;
        end;
      end;
    end);

  Thread.FreeOnTerminate := False;
  Thread.Start;

  // Attendre avec timeout (WaitFor n'accepte pas de paramètre en FreePascal)
  Debut := GetTickCount64;
  while (not Termine) and
        ((GetTickCount64 - Debut) < QWord(TimeoutSec) * 1000) do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;

  if not Termine then
  begin
    // Timeout atteint
    Thread.Terminate;
    ShowMessage('Requête annulée : timeout dépassé');
  end
  else if ErreurMsg <> '' then
  begin
    ShowMessage('Erreur : ' + ErreurMsg);
  end
  else
  begin
    ShowMessage('Requête exécutée avec succès');
  end;

  Thread.WaitFor;
  Thread.Free;
end;
```

## Reconnexion automatique pendant l'exécution

### Détecter une déconnexion

```pascal
function TForm1.EstDeconnecte(E: Exception): Boolean;  
var  
  Msg: string;
begin
  Msg := LowerCase(E.Message);

  Result := (Pos('connection', Msg) > 0) or
            (Pos('server has gone away', Msg) > 0) or
            (Pos('lost connection', Msg) > 0) or
            (Pos('broken pipe', Msg) > 0) or
            (Pos('connection refused', Msg) > 0);
end;
```

### Wrapper avec reconnexion automatique

```pascal
function TForm1.ExecuterAvecReconnexion(
  const SQL: string;
  MaxTentatives: Integer = 3): Boolean;
var
  Tentative: Integer;
  Success: Boolean;
begin
  Result := False;
  Tentative := 0;
  Success := False;

  while (Tentative < MaxTentatives) and (not Success) do
  begin
    Inc(Tentative);

    try
      // Vérifier la connexion
      if not PQConnection1.Connected then
      begin
        LogMessage('Connexion perdue. Reconnexion...');
        if not SeConnecterAvecRetry(3) then
        begin
          LogMessage('Impossible de se reconnecter');
          Exit;
        end;
      end;

      // Exécuter la requête
      SQLQuery1.SQL.Text := SQL;
      SQLQuery1.ExecSQL;
      SQLTransaction1.Commit;

      Success := True;
      Result := True;

    except
      on E: Exception do
      begin
        SQLTransaction1.Rollback;

        // Si c'est une déconnexion, réessayer
        if EstDeconnecte(E) then
        begin
          LogMessage(Format('Déconnexion détectée (tentative %d)', [Tentative]));
          PQConnection1.Connected := False;
          Sleep(1000);
        end
        else
        begin
          // Autre erreur, ne pas réessayer
          LogMessage('Erreur non liée à la connexion : ' + E.Message);
          raise;
        end;
      end;
    end;
  end;
end;
```

## Pool de connexions (concept simplifié)

### Pourquoi un pool ?

**Problème :** Créer/détruire des connexions est **coûteux** en temps.

**Solution :** Créer plusieurs connexions à l'avance et les **réutiliser**.

### Implémentation simple

```pascal
type
  TConnectionPool = class
  private
    FConnections: TList;
    FMaxConnections: Integer;
  public
    constructor Create(MaxConnections: Integer);
    destructor Destroy; override;
    function ObtenirConnexion: TPQConnection;
    procedure LibererConnexion(Conn: TPQConnection);
  end;

constructor TConnectionPool.Create(MaxConnections: Integer);  
var  
  i: Integer;
  Conn: TPQConnection;
begin
  inherited Create;
  FConnections := TList.Create;
  FMaxConnections := MaxConnections;

  // Créer les connexions
  for i := 1 to MaxConnections do
  begin
    Conn := TPQConnection.Create(nil);
    Conn.HostName := 'localhost';
    Conn.DatabaseName := 'ma_base';
    Conn.UserName := 'utilisateur';
    Conn.Password := 'motdepasse';

    try
      Conn.Open;
      FConnections.Add(Conn);
    except
      Conn.Free;
    end;
  end;
end;

destructor TConnectionPool.Destroy;  
var  
  i: Integer;
begin
  for i := 0 to FConnections.Count - 1 do
    TPQConnection(FConnections[i]).Free;

  FConnections.Free;
  inherited;
end;

function TConnectionPool.ObtenirConnexion: TPQConnection;  
begin  
  if FConnections.Count > 0 then
  begin
    Result := TPQConnection(FConnections[0]);
    FConnections.Delete(0);
  end
  else
    Result := nil;
end;

procedure TConnectionPool.LibererConnexion(Conn: TPQConnection);  
begin  
  if Assigned(Conn) then
    FConnections.Add(Conn);
end;
```

**Utilisation :**
```pascal
var
  Pool: TConnectionPool;
  Conn: TPQConnection;
begin
  Pool := TConnectionPool.Create(5);  // 5 connexions

  try
    // Obtenir une connexion
    Conn := Pool.ObtenirConnexion;

    if Assigned(Conn) then
    begin
      try
        // Utiliser la connexion
        // ...
      finally
        // Remettre dans le pool
        Pool.LibererConnexion(Conn);
      end;
    end;
  finally
    Pool.Free;
  end;
end;
```

**Note :** Pour une vraie application, utilisez une bibliothèque de pool existante.

## Interface utilisateur réactive

### Afficher l'état de la connexion

```pascal
procedure TForm1.MettreAJourStatut;  
begin  
  if PQConnection1.Connected then
  begin
    ShapeStatut.Brush.Color := clLime;
    LabelStatut.Caption := 'Connecté';
    LabelStatut.Font.Color := clGreen;
  end
  else
  begin
    ShapeStatut.Brush.Color := clRed;
    LabelStatut.Caption := 'Déconnecté';
    LabelStatut.Font.Color := clRed;
  end;
end;
```

### Barre de progression pendant reconnexion

```pascal
procedure TForm1.ReconnecterAvecProgress;  
var  
  Tentative: Integer;
  MaxTentatives: Integer;
begin
  MaxTentatives := 5;
  ProgressBar1.Max := MaxTentatives;
  ProgressBar1.Position := 0;

  for Tentative := 1 to MaxTentatives do
  begin
    ProgressBar1.Position := Tentative;
    LabelStatut.Caption := Format('Reconnexion... %d/%d',
      [Tentative, MaxTentatives]);
    Application.ProcessMessages;

    try
      if not PQConnection1.Connected then
        PQConnection1.Open;

      if PQConnection1.Connected then
      begin
        LabelStatut.Caption := 'Reconnecté avec succès !';
        Exit;
      end;
    except
      if Tentative < MaxTentatives then
        Sleep(2000);
    end;
  end;

  LabelStatut.Caption := 'Échec de reconnexion';
end;
```

### Mode hors ligne

```pascal
type
  TForm1 = class(TForm)
  private
    FModeHorsLigne: Boolean;
    FOperationsEnAttente: TStringList;
  public
    procedure BasculerModeHorsLigne;
    procedure SynchroniserOperations;
  end;

procedure TForm1.BasculerModeHorsLigne;  
begin  
  FModeHorsLigne := True;
  FOperationsEnAttente := TStringList.Create;

  ShowMessage('Mode hors ligne activé. ' +
              'Les opérations seront synchronisées plus tard.');

  // Désactiver les fonctions nécessitant la connexion
  ButtonAjouter.Enabled := False;
  ButtonModifier.Enabled := False;
  ButtonSupprimer.Enabled := False;
end;

procedure TForm1.SynchroniserOperations;  
var  
  i: Integer;
begin
  if not PQConnection1.Connected then
  begin
    ShowMessage('Impossible de synchroniser : pas de connexion');
    Exit;
  end;

  try
    for i := 0 to FOperationsEnAttente.Count - 1 do
    begin
      SQLQuery1.SQL.Text := FOperationsEnAttente[i];
      SQLQuery1.ExecSQL;
    end;

    SQLTransaction1.Commit;
    FOperationsEnAttente.Clear;
    FModeHorsLigne := False;

    // Réactiver les fonctions
    ButtonAjouter.Enabled := True;
    ButtonModifier.Enabled := True;
    ButtonSupprimer.Enabled := True;

    ShowMessage('Synchronisation réussie !');
  except
    on E: Exception do
    begin
      SQLTransaction1.Rollback;
      ShowMessage('Erreur de synchronisation : ' + E.Message);
    end;
  end;
end;
```

## Logging et monitoring

### Système de log simple

```pascal
procedure TForm1.LogMessage(const Message: string; Niveau: string = 'INFO');  
var  
  F: TextFile;
  Timestamp: string;
begin
  Timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Afficher dans l'interface
  Memo1.Lines.Add(Format('[%s] [%s] %s', [Timestamp, Niveau, Message]));

  // Écrire dans un fichier
  try
    AssignFile(F, 'app.log');
    if FileExists('app.log') then
      Append(F)
    else
      Rewrite(F);

    WriteLn(F, Format('[%s] [%s] %s', [Timestamp, Niveau, Message]));
    CloseFile(F);
  except
    // Ignorer les erreurs de log
  end;
end;

procedure TForm1.LogErreur(const Message: string; E: Exception);  
begin  
  LogMessage(Format('%s - %s: %s',
    [Message, E.ClassName, E.Message]), 'ERROR');
end;
```

### Monitoring des requêtes lentes

```pascal
procedure TForm1.ExecuterAvecMonitoring(const SQL: string; SeuilMs: Integer = 1000);  
var  
  Debut: TDateTime;
  Duree: Integer;
begin
  Debut := Now;

  try
    SQLQuery1.SQL.Text := SQL;
    SQLQuery1.ExecSQL;

    Duree := MilliSecondsBetween(Now, Debut);

    if Duree > SeuilMs then
    begin
      LogMessage(Format('⚠ Requête lente (%dms) : %s',
        [Duree, Copy(SQL, 1, 100)]), 'WARNING');
    end
    else
    begin
      LogMessage(Format('Requête exécutée en %dms', [Duree]), 'DEBUG');
    end;
  except
    on E: Exception do
    begin
      LogErreur('Erreur d''exécution de requête', E);
      raise;
    end;
  end;
end;
```

## Checklist de résilience

### À la connexion initiale

✅ **Tentatives multiples** avec backoff exponentiel  
✅ **Messages clairs** pour l'utilisateur  
✅ **Configuration des timeouts**  
✅ **Validation des paramètres** (host, port, base, etc.)

### Pendant l'exécution

✅ **Détection des déconnexions**  
✅ **Reconnexion automatique** quand possible  
✅ **Gestion des timeouts** de requête  
✅ **Logging des erreurs** pour diagnostic  
✅ **Interface réactive** (feedback utilisateur)

### Gestion des erreurs

✅ **Try-except** systématique  
✅ **Messages utilisateur** compréhensibles  
✅ **Rollback** en cas d'erreur de transaction  
✅ **Retry** pour les erreurs temporaires  
✅ **Abandon** pour les erreurs permanentes

### Monitoring

✅ **État de connexion** visible  
✅ **Logs structurés** (timestamp, niveau, message)  
✅ **Alertes** sur requêtes lentes  
✅ **Statistiques** (nombre d'erreurs, reconnexions, etc.)

## Exemple complet : Application résiliente

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  sqldb, pqconnection;

type
  TForm1 = class(TForm)
    PQConnection1: TPQConnection;
    SQLTransaction1: TSQLTransaction;
    SQLQuery1: TSQLQuery;
    ButtonConnecter: TButton;
    ButtonExecuter: TButton;
    Memo1: TMemo;
    ShapeStatut: TShape;
    LabelStatut: TLabel;
    Timer1: TTimer;
    procedure ButtonConnecterClick(Sender: TObject);
    procedure ButtonExecuterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function SeConnecterAvecRetry(MaxTentatives: Integer): Boolean;
    function ExecuterAvecResilience(const SQL: string): Boolean;
    function EstDeconnecte(E: Exception): Boolean;
    procedure LogMessage(const Msg: string);
    procedure MettreAJourStatut;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration
  PQConnection1.HostName := 'localhost';
  PQConnection1.DatabaseName := 'ma_base_test';
  PQConnection1.UserName := 'mon_app';
  PQConnection1.Password := 'motdepasse_app';
  PQConnection1.Params.Add('connect_timeout=10');

  PQConnection1.Transaction := SQLTransaction1;
  SQLTransaction1.Database := PQConnection1;
  SQLQuery1.Database := PQConnection1;

  // Timer pour vérifier la connexion
  Timer1.Interval := 10000;  // Toutes les 10 secondes
  Timer1.Enabled := True;
end;

function TForm1.SeConnecterAvecRetry(MaxTentatives: Integer): Boolean;  
var  
  Tentative: Integer;
  Delai: Integer;
begin
  Result := False;
  Tentative := 0;

  while (Tentative < MaxTentatives) and (not Result) do
  begin
    Inc(Tentative);

    try
      if PQConnection1.Connected then
        PQConnection1.Close;

      LogMessage(Format('Tentative de connexion %d/%d',
        [Tentative, MaxTentatives]));

      PQConnection1.Open;
      Result := True;
      LogMessage('✓ Connexion réussie');
      MettreAJourStatut;

    except
      on E: Exception do
      begin
        LogMessage(Format('✗ Échec : %s', [E.Message]));

        if Tentative < MaxTentatives then
        begin
          Delai := 1000 * Tentative;
          LogMessage(Format('Nouvelle tentative dans %ds...', [Delai div 1000]));
          Sleep(Delai);
        end;
      end;
    end;
  end;

  MettreAJourStatut;
end;

function TForm1.EstDeconnecte(E: Exception): Boolean;  
var  
  Msg: string;
begin
  Msg := LowerCase(E.Message);
  Result := (Pos('connection', Msg) > 0) or
            (Pos('server has gone away', Msg) > 0) or
            (Pos('lost connection', Msg) > 0);
end;

function TForm1.ExecuterAvecResilience(const SQL: string): Boolean;  
var  
  Tentative: Integer;
begin
  Result := False;
  Tentative := 0;

  while (Tentative < 3) and (not Result) do
  begin
    Inc(Tentative);

    try
      if not PQConnection1.Connected then
      begin
        LogMessage('Connexion perdue, reconnexion...');
        if not SeConnecterAvecRetry(3) then
        begin
          LogMessage('Impossible de se reconnecter');
          Exit;
        end;
      end;

      SQLQuery1.SQL.Text := SQL;
      SQLQuery1.ExecSQL;
      SQLTransaction1.Commit;

      Result := True;
      LogMessage('✓ Requête exécutée avec succès');

    except
      on E: Exception do
      begin
        SQLTransaction1.Rollback;

        if EstDeconnecte(E) then
        begin
          LogMessage('Déconnexion détectée');
          PQConnection1.Connected := False;
        end
        else
        begin
          LogMessage('Erreur : ' + E.Message);
          raise;
        end;
      end;
    end;
  end;
end;

procedure TForm1.LogMessage(const Msg: string);  
begin  
  Memo1.Lines.Add(Format('[%s] %s',
    [FormatDateTime('hh:nn:ss', Now), Msg]));
end;

procedure TForm1.MettreAJourStatut;  
begin  
  if PQConnection1.Connected then
  begin
    ShapeStatut.Brush.Color := clLime;
    LabelStatut.Caption := 'Connecté';
    ButtonExecuter.Enabled := True;
  end
  else
  begin
    ShapeStatut.Brush.Color := clRed;
    LabelStatut.Caption := 'Déconnecté';
    ButtonExecuter.Enabled := False;
  end;
end;

procedure TForm1.ButtonConnecterClick(Sender: TObject);  
begin  
  if SeConnecterAvecRetry(5) then
    ShowMessage('Connexion établie')
  else
    ShowMessage('Échec de connexion');
end;

procedure TForm1.ButtonExecuterClick(Sender: TObject);  
begin  
  if ExecuterAvecResilience('INSERT INTO test (nom) VALUES (''Test'')') then
    ShowMessage('Opération réussie')
  else
    ShowMessage('Opération échouée');
end;

procedure TForm1.Timer1Timer(Sender: TObject);  
begin  
  // Vérifier périodiquement la connexion
  if PQConnection1.Connected then
  begin
    try
      SQLQuery1.SQL.Text := 'SELECT 1';
      SQLQuery1.Open;
      SQLQuery1.Close;
    except
      LogMessage('Connexion perdue (détecté par le timer)');
      PQConnection1.Connected := False;
      MettreAJourStatut;
    end;
  end;
end;

end.
```

## Bonnes pratiques récapitulatives

### 1. Toujours prévoir les erreurs

```pascal
// MAL
PQConnection1.Open;

// BIEN
try
  PQConnection1.Open;
except
  on E: Exception do
    GererErreurConnexion(E);
end;
```

### 2. Implémenter le retry intelligemment

✅ Backoff exponentiel  
✅ Nombre limité de tentatives  
✅ Jitter pour éviter les pics  
✅ Messages clairs

### 3. Configurer les timeouts

✅ Timeout de connexion  
✅ Timeout de requête  
✅ Timeout de verrou

### 4. Logger pour diagnostiquer

✅ Timestamp  
✅ Niveau (INFO, WARNING, ERROR)  
✅ Message clair  
✅ Contexte (quelle opération)

### 5. Informer l'utilisateur

✅ État de connexion visible  
✅ Messages compréhensibles  
✅ Options claires (réessayer, annuler, continuer)

### 6. Tester les scénarios d'erreur

✅ Serveur arrêté  
✅ Réseau coupé  
✅ Mauvais identifiants  
✅ Requête trop longue  
✅ Deadlock

## Résumé

**Types d'erreurs :**
- Connexion initiale
- Déconnexion pendant exécution
- Timeouts
- Erreurs de transaction

**Stratégies de résilience :**
- ✅ Retry avec backoff exponentiel
- ✅ Détection automatique des déconnexions
- ✅ Reconnexion automatique
- ✅ Timeouts configurés
- ✅ Logging structuré

**Interface utilisateur :**
- ✅ État de connexion visible
- ✅ Messages clairs et utiles
- ✅ Feedback pendant opérations longues
- ✅ Options pour l'utilisateur

**Code robuste :**
- ✅ Try-except systématique
- ✅ Vérification de connexion
- ✅ Rollback en cas d'erreur
- ✅ Gestion des cas limites

**Monitoring :**
- ✅ Logs avec timestamp et niveau
- ✅ Alertes sur anomalies
- ✅ Statistiques d'utilisation

Une application résiliente n'est pas une application qui ne rencontre jamais d'erreurs, mais une application qui les **gère gracieusement** et **récupère automatiquement** quand c'est possible.

Félicitations ! Vous avez terminé le chapitre 16 sur les bases de données. Vous maîtrisez maintenant :
- SQLite pour les applications simples
- PostgreSQL/MariaDB pour les applications professionnelles
- Les transactions et leur gestion avancée
- La création d'applications robustes et résilientes

Vous êtes prêt à créer des applications de qualité professionnelle ! 🎉

---

*Des bases aux concepts avancés : vous êtes maintenant un développeur de bases de données accompli !*

⏭️ [Communications Réseau et API REST](/17-communications-reseau-api-rest/README.md)
