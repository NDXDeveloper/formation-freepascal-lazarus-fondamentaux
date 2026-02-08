🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.9 Logging des erreurs

## Introduction

Imaginez que votre application soit en production chez des clients. Un jour, un utilisateur vous signale : "Ça ne marche pas". Sans plus de détails. Comment savoir ce qui s'est passé ? C'est là qu'intervient le **logging** (journalisation en français).

Le logging est l'art d'enregistrer ce qui se passe dans votre application : les actions réussies, les erreurs, les avertissements, les informations de débogage. C'est comme la boîte noire d'un avion : quand quelque chose tourne mal, vous pouvez analyser les logs pour comprendre ce qui s'est passé.

## Pourquoi logger les erreurs ?

### Sans logging

```pascal
try
  TraiterDonnees(fichier);
except
  on E: Exception do
    ShowMessage('Une erreur s''est produite');
end;
```

**Problème :** L'utilisateur voit un message vague, et vous ne savez absolument rien de l'erreur.

### Avec logging

```pascal
try
  TraiterDonnees(fichier);
except
  on E: Exception do
  begin
    LogErreur('TraiterDonnees', fichier, E);
    ShowMessage('Une erreur s''est produite (détails enregistrés)');
  end;
end;
```

**Avantage :** L'erreur est enregistrée avec tous les détails, vous pouvez l'analyser plus tard.

### Les bénéfices du logging

1. **Diagnostic à distance** : Comprendre les erreurs sans être devant la machine
2. **Historique** : Voir quand et à quelle fréquence les erreurs se produisent
3. **Patterns** : Identifier des tendances ou des problèmes récurrents
4. **Support client** : Demander le fichier log pour analyse
5. **Amélioration continue** : Corriger les bugs basés sur les logs réels
6. **Audit** : Tracer qui a fait quoi et quand

## Les niveaux de logging

Un système de logging efficace utilise différents **niveaux** pour classifier les messages :

### Les niveaux standards

```pascal
type
  TNiveauLog = (
    nlTrace,      // Niveau le plus détaillé
    nlDebug,      // Informations de débogage
    nlInfo,       // Informations générales
    nlWarning,    // Avertissements
    nlError,      // Erreurs
    nlFatal       // Erreurs critiques/fatales
  );
```

### Quand utiliser chaque niveau ?

#### TRACE - Tout, absolument tout

Utilisé pour un débogage très détaillé, généralement désactivé.

```pascal
Log(nlTrace, 'Entrée dans la fonction CalculerTotal');
Log(nlTrace, 'Valeur de a: ' + IntToStr(a));
Log(nlTrace, 'Valeur de b: ' + IntToStr(b));
Log(nlTrace, 'Sortie de la fonction, résultat: ' + IntToStr(resultat));
```

#### DEBUG - Informations de débogage

Pour le développement, désactivé en production.

```pascal
Log(nlDebug, 'Configuration chargée: ' + Config.ToString);
Log(nlDebug, 'Connexion établie à la base de données');
Log(nlDebug, 'Requête SQL: ' + query);
```

#### INFO - Événements importants

Informations générales sur le fonctionnement normal.

```pascal
Log(nlInfo, 'Application démarrée');
Log(nlInfo, 'Utilisateur ' + username + ' connecté');
Log(nlInfo, 'Fichier ' + filename + ' traité avec succès');
Log(nlInfo, 'Application fermée normalement');
```

#### WARNING - Avertissements

Situations anormales mais non critiques.

```pascal
Log(nlWarning, 'Fichier de cache introuvable, utilisation des valeurs par défaut');
Log(nlWarning, 'Connexion lente détectée (' + IntToStr(delai) + ' ms)');
Log(nlWarning, 'Espace disque faible: ' + IntToStr(espaceMo) + ' Mo restants');
```

#### ERROR - Erreurs

Erreurs qui empêchent une opération mais pas l'application entière.

```pascal
Log(nlError, 'Impossible de sauvegarder le fichier: ' + E.Message);
Log(nlError, 'Échec de connexion à la base de données: ' + E.Message);
Log(nlError, 'Format de données invalide dans la ligne ' + IntToStr(ligne));
```

#### FATAL - Erreurs fatales

Erreurs critiques qui empêchent l'application de continuer.

```pascal
Log(nlFatal, 'Fichier de configuration critique manquant');
Log(nlFatal, 'Mémoire insuffisante, arrêt de l''application');
Log(nlFatal, 'Base de données inaccessible après 5 tentatives');
```

## Logger vers la console

La méthode la plus simple : écrire dans la console.

### Version basique

```pascal
procedure LogConsole(const niveau: String; const message: String);
begin
  WriteLn(Format('[%s] %s - %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), niveau, message]));
end;

// Utilisation
LogConsole('ERROR', 'Fichier introuvable: donnees.txt');
```

**Sortie :**
```
[2025-10-14 14:35:42] ERROR - Fichier introuvable: donnees.txt
```

### Version avec couleurs (Windows)

```pascal
uses
  Windows;

procedure LogConsoleAvecCouleur(niveau: TNiveauLog; const message: String);
var
  couleur: Word;
  handle: THandle;
begin
  handle := GetStdHandle(STD_OUTPUT_HANDLE);

  case niveau of
    nlTrace:   couleur := FOREGROUND_INTENSITY;
    nlDebug:   couleur := FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    nlInfo:    couleur := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    nlWarning: couleur := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    nlError:   couleur := FOREGROUND_RED or FOREGROUND_INTENSITY;
    nlFatal:   couleur := BACKGROUND_RED or FOREGROUND_RED or FOREGROUND_GREEN or
                         FOREGROUND_BLUE or FOREGROUND_INTENSITY;
  end;

  SetConsoleTextAttribute(handle, couleur);
  WriteLn(Format('[%s] [%s] %s',
    [FormatDateTime('hh:nn:ss', Now), NiveauToString(niveau), message]));
  SetConsoleTextAttribute(handle, FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
end;
```

## Logger vers un fichier

Le logging dans un fichier est la méthode la plus courante en production.

### Version simple

```pascal
var
  FichierLog: TextFile;

procedure InitialiserLog(const nomFichier: String);
begin
  AssignFile(FichierLog, nomFichier);
  if FileExists(nomFichier) then
    Append(FichierLog)  // Ajouter à la fin
  else
    Rewrite(FichierLog); // Créer nouveau
end;

procedure LogFichier(niveau: TNiveauLog; const message: String);
begin
  WriteLn(FichierLog, Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
     NiveauToString(niveau),
     message]));
  Flush(FichierLog);  // Écrire immédiatement sur disque
end;

procedure FermerLog;
begin
  CloseFile(FichierLog);
end;

// Utilisation
begin
  InitialiserLog('application.log');
  try
    // Votre code
    LogFichier(nlInfo, 'Application démarrée');
    TraiterDonnees;
    LogFichier(nlInfo, 'Traitement terminé');
  finally
    FermerLog;
  end;
end;
```

**Contenu du fichier application.log :**
```
[2025-10-14 14:35:42.123] [INFO] Application démarrée
[2025-10-14 14:35:42.456] [INFO] Traitement terminé
```

### Version thread-safe

Si votre application utilise plusieurs threads, protégez l'accès au fichier :

```pascal
uses
  SyncObjs;

var
  FichierLog: TextFile;
  SectionCritique: TCriticalSection;

procedure InitialiserLogThreadSafe;
begin
  SectionCritique := TCriticalSection.Create;
  AssignFile(FichierLog, 'application.log');
  if FileExists('application.log') then
    Append(FichierLog)
  else
    Rewrite(FichierLog);
end;

procedure LogFichierThreadSafe(niveau: TNiveauLog; const message: String);
begin
  SectionCritique.Enter;
  try
    WriteLn(FichierLog, Format('[%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
       NiveauToString(niveau),
       message]));
    Flush(FichierLog);
  finally
    SectionCritique.Leave;
  end;
end;

procedure FermerLogThreadSafe;
begin
  CloseFile(FichierLog);
  SectionCritique.Free;
end;
```

## Créer une classe de logging complète

Voici une classe réutilisable pour gérer le logging :

```pascal
type
  TNiveauLog = (nlTrace, nlDebug, nlInfo, nlWarning, nlError, nlFatal);

  TLogger = class
  private
    FFichier: TextFile;
    FFichierOuvert: Boolean;
    FNiveauMinimum: TNiveauLog;
    FNomFichier: String;
    FSectionCritique: TCriticalSection;

    function NiveauToString(niveau: TNiveauLog): String;
    function DoitLogger(niveau: TNiveauLog): Boolean;
  public
    constructor Create(const nomFichier: String; niveauMin: TNiveauLog = nlInfo);
    destructor Destroy; override;

    procedure Log(niveau: TNiveauLog; const message: String); overload;
    procedure Log(niveau: TNiveauLog; const format: String; const args: array of const); overload;

    procedure Trace(const message: String);
    procedure Debug(const message: String);
    procedure Info(const message: String);
    procedure Warning(const message: String);
    procedure Error(const message: String);
    procedure Fatal(const message: String);

    procedure LogException(const contexte: String; E: Exception);

    property NiveauMinimum: TNiveauLog read FNiveauMinimum write FNiveauMinimum;
  end;

implementation

constructor TLogger.Create(const nomFichier: String; niveauMin: TNiveauLog);
begin
  inherited Create;
  FNomFichier := nomFichier;
  FNiveauMinimum := niveauMin;
  FSectionCritique := TCriticalSection.Create;

  AssignFile(FFichier, nomFichier);
  try
    if FileExists(nomFichier) then
      Append(FFichier)
    else
      Rewrite(FFichier);
    FFichierOuvert := True;
  except
    on E: Exception do
    begin
      FFichierOuvert := False;
      WriteLn('Erreur ouverture log: ', E.Message);
    end;
  end;
end;

destructor TLogger.Destroy;
begin
  if FFichierOuvert then
    CloseFile(FFichier);
  FSectionCritique.Free;
  inherited;
end;

function TLogger.NiveauToString(niveau: TNiveauLog): String;
begin
  case niveau of
    nlTrace:   Result := 'TRACE';
    nlDebug:   Result := 'DEBUG';
    nlInfo:    Result := 'INFO ';
    nlWarning: Result := 'WARN ';
    nlError:   Result := 'ERROR';
    nlFatal:   Result := 'FATAL';
  end;
end;

function TLogger.DoitLogger(niveau: TNiveauLog): Boolean;
begin
  Result := FFichierOuvert and (niveau >= FNiveauMinimum);
end;

procedure TLogger.Log(niveau: TNiveauLog; const message: String);
begin
  if not DoitLogger(niveau) then Exit;

  FSectionCritique.Enter;
  try
    WriteLn(FFichier, Format('[%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
       NiveauToString(niveau),
       message]));
    Flush(FFichier);
  finally
    FSectionCritique.Leave;
  end;
end;

procedure TLogger.Log(niveau: TNiveauLog; const format: String; const args: array of const);
begin
  Log(niveau, Format(format, args));
end;

procedure TLogger.Trace(const message: String);
begin
  Log(nlTrace, message);
end;

procedure TLogger.Debug(const message: String);
begin
  Log(nlDebug, message);
end;

procedure TLogger.Info(const message: String);
begin
  Log(nlInfo, message);
end;

procedure TLogger.Warning(const message: String);
begin
  Log(nlWarning, message);
end;

procedure TLogger.Error(const message: String);
begin
  Log(nlError, message);
end;

procedure TLogger.Fatal(const message: String);
begin
  Log(nlFatal, message);
end;

procedure TLogger.LogException(const contexte: String; E: Exception);
begin
  Error(Format('%s - Exception: [%s] %s', [contexte, E.ClassName, E.Message]));
end;
```

### Utilisation de la classe TLogger

```pascal
var
  Logger: TLogger;

begin
  Logger := TLogger.Create('application.log', nlDebug);
  try
    Logger.Info('Application démarrée');

    try
      TraiterDonnees;
      Logger.Info('Données traitées avec succès');
    except
      on E: Exception do
      begin
        Logger.LogException('TraiterDonnees', E);
        Logger.Error('Arrêt du traitement');
      end;
    end;

    Logger.Info('Application terminée');
  finally
    Logger.Free;
  end;
end;
```

## Rotation des fichiers logs

Les fichiers logs peuvent devenir très gros. La rotation permet de créer un nouveau fichier périodiquement.

### Rotation par taille

```pascal
procedure VerifierRotationParTaille(const nomFichier: String; tailleLimite: Int64);
var
  fichier: TFileStream;
  nouveauNom: String;
begin
  if not FileExists(nomFichier) then Exit;

  fichier := TFileStream.Create(nomFichier, fmOpenRead or fmShareDenyNone);
  try
    if fichier.Size > tailleLimite then
    begin
      // Renommer l'ancien fichier
      nouveauNom := ChangeFileExt(nomFichier,
        FormatDateTime('_yyyy-mm-dd_hhnnss', Now) + '.log');
      RenameFile(nomFichier, nouveauNom);
    end;
  finally
    fichier.Free;
  end;
end;

// Utilisation : vérifier avant d'ouvrir le log
VerifierRotationParTaille('application.log', 10 * 1024 * 1024); // 10 Mo
Logger := TLogger.Create('application.log');
```

### Rotation par date

```pascal
function ObtenirNomFichierLogJour: String;
begin
  Result := Format('application_%s.log', [FormatDateTime('yyyy-mm-dd', Now)]);
end;

// Utilisation : un nouveau fichier chaque jour
Logger := TLogger.Create(ObtenirNomFichierLogJour);
```

### Nettoyage des anciens logs

```pascal
procedure SupprimerVieuxLogs(const repertoire: String; joursAConserver: Integer);
var
  sr: TSearchRec;
  dateMin: TDateTime;
  fichier: String;
begin
  dateMin := Now - joursAConserver;

  if FindFirst(repertoire + '*.log', faAnyFile, sr) = 0 then
  try
    repeat
      fichier := repertoire + sr.Name;
      if FileDateToDateTime(sr.Time) < dateMin then
      begin
        DeleteFile(fichier);
        WriteLn('Log supprimé: ', sr.Name);
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

// Utilisation : garder 30 jours de logs
SupprimerVieuxLogs('logs/', 30);
```

## Informations utiles à logger

### Lors d'une exception

```pascal
procedure LoggerExceptionComplete(E: Exception);
begin
  Logger.Error('=== EXCEPTION DÉTECTÉE ===');
  Logger.Error('Type: ' + E.ClassName);
  Logger.Error('Message: ' + E.Message);
  Logger.Error('Heure: ' + DateTimeToStr(Now));

  // Informations système
  Logger.Error('Système: ' + {$I %FPCTARGETOS%});
  Logger.Error('Architecture: ' + {$I %FPCTARGETCPU%});

  // Si disponible, la pile d'appels
  // (nécessite des unités spécifiques)

  Logger.Error('=========================');
end;
```

### Contexte applicatif

```pascal
Logger.Info('=== SESSION DÉMARRÉE ===');
Logger.Info('Version application: ' + GetVersionApp);
Logger.Info('Utilisateur: ' + GetCurrentUser);
Logger.Info('Machine: ' + GetComputerName);
Logger.Info('Système: ' + GetOSInfo);
Logger.Info('Mémoire disponible: ' + IntToStr(GetMemoryAvailable) + ' Mo');
Logger.Info('========================');
```

### Opérations importantes

```pascal
procedure EnregistrerClient(const client: TClient);
begin
  Logger.Info('Début enregistrement client: ' + client.Nom);

  try
    BaseDonnees.Insert(client);
    Logger.Info('Client enregistré avec succès - ID: ' + IntToStr(client.ID));
  except
    on E: Exception do
    begin
      Logger.Error('Échec enregistrement client: ' + client.Nom);
      Logger.LogException('EnregistrerClient', E);
      raise;
    end;
  end;
end;
```

### Performances

```pascal
procedure LoggerPerformance(const operation: String; debut: TDateTime);
var
  duree: Integer;
begin
  duree := MilliSecondsBetween(Now, debut);

  if duree > 1000 then
    Logger.Warning(Format('%s: temps élevé %d ms', [operation, duree]))
  else
    Logger.Debug(Format('%s: %d ms', [operation, duree]));
end;

// Utilisation
var
  debut: TDateTime;
begin
  debut := Now;
  TraiterGrosVolume;
  LoggerPerformance('TraiterGrosVolume', debut);
end;
```

## Format de log structuré

Pour faciliter l'analyse automatique, utilisez un format structuré.

### Format JSON

```pascal
procedure LogJSON(niveau: TNiveauLog; const message: String;
  const donnees: String = '');
var
  logEntry: String;
begin
  logEntry := Format(
    '{"timestamp":"%s","level":"%s","message":"%s"',
    [FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Now),
     NiveauToString(niveau),
     StringReplace(message, '"', '\"', [rfReplaceAll])]);

  if donnees <> '' then
    logEntry := logEntry + ',"data":' + donnees;

  logEntry := logEntry + '}';

  WriteLn(FichierLog, logEntry);
end;

// Utilisation
LogJSON(nlError, 'Connexion échouée', '{"tentative":3,"delai":"5000ms"}');
```

**Résultat :**
```json
{"timestamp":"2025-10-14T14:35:42.123","level":"ERROR","message":"Connexion échouée","data":{"tentative":3,"delai":"5000ms"}}
```

### Format CSV

Pour import dans Excel :

```pascal
procedure LogCSV(niveau: TNiveauLog; const message: String);
begin
  WriteLn(FichierLog, Format('%s;%s;%s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
     NiveauToString(niveau),
     StringReplace(message, ';', ',', [rfReplaceAll])]));
end;
```

## Logger dans une base de données

Pour des applications d'entreprise, logger dans une base peut être utile.

```pascal
type
  TLoggerDB = class
  private
    FConnexion: TSQLConnection;
    FQuery: TSQLQuery;
  public
    constructor Create(connexion: TSQLConnection);
    destructor Destroy; override;
    procedure Log(niveau: TNiveauLog; const message: String);
  end;

constructor TLoggerDB.Create(connexion: TSQLConnection);
begin
  inherited Create;
  FConnexion := connexion;
  FQuery := TSQLQuery.Create(nil);
  FQuery.Database := FConnexion;
end;

destructor TLoggerDB.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TLoggerDB.Log(niveau: TNiveauLog; const message: String);
begin
  FQuery.SQL.Text :=
    'INSERT INTO logs (date_log, niveau, message) VALUES (:date, :niveau, :message)';
  FQuery.ParamByName('date').AsDateTime := Now;
  FQuery.ParamByName('niveau').AsString := NiveauToString(niveau);
  FQuery.ParamByName('message').AsString := message;

  try
    FQuery.ExecSQL;
  except
    // Si le logging DB échoue, ne pas crasher l'application
    // On peut logger dans un fichier de secours
  end;
end;
```

**Table SQL pour les logs :**

```sql
CREATE TABLE logs (
    id INTEGER PRIMARY KEY AUTO_INCREMENT,
    date_log DATETIME NOT NULL,
    niveau VARCHAR(10) NOT NULL,
    message TEXT NOT NULL,
    INDEX idx_date (date_log),
    INDEX idx_niveau (niveau)
);
```

## Bonnes pratiques de logging

### 1. Logger au bon niveau

```pascal
// ✓ BON
Logger.Info('Utilisateur connecté: ' + username);
Logger.Error('Échec connexion DB: ' + E.Message);

// ✗ MAUVAIS
Logger.Fatal('Utilisateur connecté');  // Ce n'est pas fatal !
Logger.Debug('Fichier introuvable');   // C'est une erreur, pas du debug !
```

### 2. Messages clairs et contextuels

```pascal
// ✗ MAUVAIS
Logger.Error('Erreur');

// ✓ BON
Logger.Error('Impossible d''ouvrir le fichier "config.xml" : fichier introuvable');
```

### 3. Ne pas logger de données sensibles

```pascal
// ✗ TRÈS MAUVAIS
Logger.Info('Connexion réussie - Mot de passe: ' + password);
Logger.Info('Carte bancaire: ' + numeroCarteBancaire);

// ✓ BON
Logger.Info('Connexion réussie - Utilisateur: ' + username);
Logger.Info('Paiement validé - Transaction: ' + transactionID);
```

### 4. Utiliser différents fichiers par catégorie

```pascal
LoggerApp := TLogger.Create('application.log');
LoggerErreur := TLogger.Create('erreurs.log', nlError);  // Seulement erreurs
LoggerDebug := TLogger.Create('debug.log', nlTrace);     // Tout pour dev
```

### 5. Flush régulièrement

```pascal
// Garantir que les logs sont écrits immédiatement
WriteLn(FichierLog, message);
Flush(FichierLog);  // Important !
```

**Pourquoi ?** Si l'application crash juste après, le message sera dans le fichier.

### 6. Gérer les erreurs de logging

```pascal
procedure LogSecurise(const message: String);
begin
  try
    WriteLn(FichierLog, message);
    Flush(FichierLog);
  except
    // Si le logging échoue, ne pas crasher l'application
    // Peut-être écrire dans un fichier de secours
    on E: Exception do
      WriteLn(StdErr, 'Erreur logging: ', E.Message);
  end;
end;
```

### 7. Logger les démarrages et arrêts

```pascal
initialization
  Logger := TLogger.Create('application.log');
  Logger.Info('=== APPLICATION DÉMARRÉE ===');
  Logger.Info('Version: ' + GetAppVersion);

finalization
  Logger.Info('=== APPLICATION ARRÊTÉE ===');
  Logger.Free;
```

## Exemple complet d'application avec logging

```pascal
program ApplicationAvecLogging;

uses
  SysUtils, DateUtils;

var
  Logger: TLogger;

procedure TraiterFichier(const nomFichier: String);
var
  debut: TDateTime;
begin
  debut := Now;
  Logger.Info('Début traitement fichier: ' + nomFichier);

  try
    if not FileExists(nomFichier) then
    begin
      Logger.Error('Fichier introuvable: ' + nomFichier);
      raise Exception.Create('Fichier introuvable');
    end;

    Logger.Debug('Fichier trouvé, ouverture en cours...');

    // Traitement...
    Sleep(1000);  // Simulation

    Logger.Info(Format('Traitement réussi en %d ms',
      [MilliSecondsBetween(Now, debut)]));

  except
    on E: Exception do
    begin
      Logger.LogException('TraiterFichier', E);
      Logger.Error(Format('Traitement échoué après %d ms',
        [MilliSecondsBetween(Now, debut)]));
      raise;
    end;
  end;
end;

begin
  Logger := TLogger.Create('application.log', nlDebug);
  try
    Logger.Info('=== SESSION DÉMARRÉE ===');
    Logger.Info('Date: ' + DateTimeToStr(Now));

    try
      TraiterFichier('donnees.txt');
      Logger.Info('Opération terminée avec succès');
    except
      on E: Exception do
      begin
        Logger.Fatal('Erreur critique, arrêt de l''application');
        WriteLn('Une erreur critique s''est produite. Consultez le fichier log.');
      end;
    end;

    Logger.Info('=== SESSION TERMINÉE ===');
  finally
    Logger.Free;
  end;
end.
```

**Contenu de application.log :**
```
[2025-10-14 14:35:42.123] [INFO ] === SESSION DÉMARRÉE ===
[2025-10-14 14:35:42.125] [INFO ] Date: 14/10/2025 14:35:42
[2025-10-14 14:35:42.127] [INFO ] Début traitement fichier: donnees.txt
[2025-10-14 14:35:42.130] [ERROR] Fichier introuvable: donnees.txt
[2025-10-14 14:35:42.132] [ERROR] TraiterFichier - Exception: [Exception] Fichier introuvable
[2025-10-14 14:35:42.134] [ERROR] Traitement échoué après 7 ms
[2025-10-14 14:35:42.136] [FATAL] Erreur critique, arrêt de l'application
[2025-10-14 14:35:42.138] [INFO ] === SESSION TERMINÉE ===
```

## Outils d'analyse de logs

### Grep (ligne de commande)

```bash
# Chercher toutes les erreurs
grep ERROR application.log

# Compter les erreurs
grep -c ERROR application.log

# Erreurs d'aujourd'hui
grep "2025-10-14" application.log | grep ERROR

# Dernières 50 lignes
tail -50 application.log
```

### Analyseur simple en Pascal

```pascal
procedure AnalyserLogs(const nomFichier: String);
var
  f: TextFile;
  ligne: String;
  nbInfo, nbWarn, nbError, nbFatal: Integer;
begin
  nbInfo := 0; nbWarn := 0; nbError := 0; nbFatal := 0;

  AssignFile(f, nomFichier);
  Reset(f);
  try
    while not EOF(f) do
    begin
      ReadLn(f, ligne);
      if Pos('[INFO]', ligne) > 0 then Inc(nbInfo);
      if Pos('[WARN]', ligne) > 0 then Inc(nbWarn);
      if Pos('[ERROR]', ligne) > 0 then Inc(nbError);
      if Pos('[FATAL]', ligne) > 0 then Inc(nbFatal);
    end;
  finally
    CloseFile(f);
  end;

  WriteLn('=== ANALYSE DES LOGS ===');
  WriteLn('Informations: ', nbInfo);
  WriteLn('Avertissements: ', nbWarn);
  WriteLn('Erreurs: ', nbError);
  WriteLn('Fatales: ', nbFatal);
end;
```

## Conclusion

Le logging est un investissement qui rapporte énormément :

- En développement : aide au débogage
- En test : trace l'exécution
- En production : diagnostic des problèmes
- En support : comprendre les incidents clients

**Les clés d'un bon système de logging :**

1. Utiliser les niveaux appropriés (TRACE à FATAL)
2. Messages clairs et contextuels
3. Logger dans des fichiers avec rotation
4. Thread-safe si multithreading
5. Ne jamais logger de données sensibles
6. Gérer les erreurs de logging
7. Analyser régulièrement les logs

Avec un bon système de logging, vous transformez les "ça ne marche pas" en diagnostics précis que vous pouvez corriger rapidement !

---

**Points clés à retenir :**

- Le logging enregistre ce qui se passe dans votre application
- 6 niveaux standard : TRACE, DEBUG, INFO, WARNING, ERROR, FATAL
- Logger dans des fichiers pour conservation à long terme
- Utiliser `Flush` pour écrire immédiatement
- Rotation des logs pour éviter les fichiers géants
- Messages clairs avec contexte (qui, quoi, quand, pourquoi)
- Thread-safe pour applications multithread
- Ne JAMAIS logger de données sensibles (mots de passe, cartes bancaires)
- Logger les exceptions avec leur contexte complet
- Analyser régulièrement les logs pour détecter les patterns
- Le logging ne doit jamais faire crasher l'application

⏭️ [Introduction aux Applications Graphiques](/14-introduction-applications-graphiques/README.md)
