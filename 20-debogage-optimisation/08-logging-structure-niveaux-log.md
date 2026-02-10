🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.8 Logging Structuré et Niveaux de Log

## Introduction

Le logging (journalisation en français) est l'art d'enregistrer ce qui se passe dans votre programme. C'est comme tenir un journal de bord pour un navire : chaque événement important est consigné pour pouvoir retracer ce qui s'est passé en cas de problème.

**Analogie simple :** Imaginez que votre application est une voiture. Le logging, c'est la boîte noire : elle enregistre tout ce qui se passe (vitesse, freinages, virages) pour comprendre ce qui s'est passé en cas d'accident.

**Pourquoi le logging est essentiel :**
- 🔍 **Débogage** : Comprendre ce qui s'est passé quand un bug survient
- 📊 **Monitoring** : Surveiller le comportement en production
- 🔒 **Sécurité** : Tracer les accès et actions suspectes
- 📈 **Analyse** : Comprendre comment l'application est utilisée
- 🚨 **Alertes** : Détecter les problèmes avant qu'ils deviennent critiques

**Dans cette section, vous apprendrez à :**
- Comprendre les différents niveaux de log
- Implémenter un système de logging structuré
- Gérer les fichiers de log efficacement
- Optimiser les performances du logging
- Utiliser le logging en production

---

## 1. Concepts Fondamentaux

### 1.1 Logging vs WriteLn

**❌ Approche naïve avec WriteLn :**

```pascal
begin
  WriteLn('Démarrage du programme');

  if not ConnecterBDD then
    WriteLn('ERREUR: Impossible de se connecter à la base de données');

  WriteLn('Traitement de ', NombreFichiers, ' fichiers');

  WriteLn('Programme terminé');
end.
```

**Problèmes :**
- Tout s'affiche dans la console (pollution visuelle)
- Pas de distinction entre info et erreur
- Pas d'historique persistant
- Impossible à désactiver facilement
- Pas de timestamp
- Pas de filtrage

**✅ Approche professionnelle avec logging :**

```pascal
begin
  Logger.Info('Démarrage du programme');

  if not ConnecterBDD then
    Logger.Error('Impossible de se connecter à la base de données');

  Logger.Info('Traitement de %d fichiers', [NombreFichiers]);

  Logger.Info('Programme terminé');
end.
```

**Avantages :**
- ✅ Logs dans un fichier (historique permanent)
- ✅ Niveaux de sévérité clairs
- ✅ Timestamps automatiques
- ✅ Filtrage par niveau
- ✅ Formatage cohérent
- ✅ Rotation automatique des fichiers

### 1.2 Qu'est-ce que le Logging Structuré ?

**Logging non structuré (à éviter) :**

```
Programme démarré  
Connexion à la BDD réussie  
Utilisateur Jean connecté  
Erreur inconnue  
Programme terminé  
```

**Problèmes :**
- Pas de timestamp
- Pas de niveau de sévérité
- Format incohérent
- Difficile à parser automatiquement

**Logging structuré (recommandé) :**

```
2025-10-15 14:30:12.456 [INFO ] Programme démarré
2025-10-15 14:30:12.789 [INFO ] Connexion à la BDD réussie
2025-10-15 14:30:15.123 [INFO ] Utilisateur 'Jean' connecté (ID: 1234)
2025-10-15 14:30:18.456 [ERROR] Fichier introuvable: data.xml (Code: 404)
2025-10-15 14:30:20.789 [INFO ] Programme terminé
```

**Avantages :**
- ✅ Timestamp précis
- ✅ Niveau de sévérité visible
- ✅ Format cohérent et parsable
- ✅ Informations contextuelles
- ✅ Facile à analyser avec des outils

---

## 2. Niveaux de Log

### 2.1 Les 5 Niveaux Standards

| Niveau | Usage | Exemple | En Production |
|--------|-------|---------|---------------|
| **DEBUG** | Détails techniques | Variables, flux d'exécution | ❌ Désactivé |
| **INFO** | Événements normaux | Démarrage, connexions | ✅ Activé |
| **WARNING** | Situations anormales non critiques | Paramètre manquant (valeur par défaut utilisée) | ✅ Activé |
| **ERROR** | Erreurs gérées | Fichier introuvable, échec connexion | ✅ Activé |
| **FATAL** | Erreurs critiques non récupérables | Corruption mémoire, crash imminent | ✅ Activé |

### 2.2 DEBUG : Pour le Développeur

**Usage :** Informations très détaillées pour comprendre le flux d'exécution.

**Exemples :**

```pascal
Logger.Debug('Entrée dans la fonction CalculerTotal');  
Logger.Debug('Valeur de x = %d, y = %d', [x, y]);  
Logger.Debug('Boucle itération %d/%d', [i, total]);  
Logger.Debug('Requête SQL: %s', [query]);  
Logger.Debug('Sortie de la fonction, résultat = %f', [resultat]);  
```

**Caractéristiques :**
- 🟢 Très verbeux
- 🟢 Aide au débogage
- 🔴 Désactivé en production (trop volumineux)
- 🔴 Impact performance si activé

**Quand utiliser :**
- Tracer le flux d'exécution détaillé
- Afficher les valeurs de variables importantes
- Comprendre un bug complexe

### 2.3 INFO : Événements Normaux

**Usage :** Événements significatifs mais normaux.

**Exemples :**

```pascal
Logger.Info('Application démarrée (version %s)', [Version]);  
Logger.Info('Utilisateur %s connecté', [Username]);  
Logger.Info('Traitement de %d fichiers démarré', [Count]);  
Logger.Info('Sauvegarde effectuée avec succès');  
Logger.Info('Application arrêtée proprement');  
```

**Caractéristiques :**
- 🟢 Informatif sans être verbeux
- 🟢 Activé en production
- 🟢 Impact performance minimal

**Quand utiliser :**
- Démarrage/arrêt de l'application
- Connexions utilisateurs
- Début/fin d'opérations importantes
- Confirmations d'actions réussies

### 2.4 WARNING : Attention Requise

**Usage :** Situations inhabituelles mais gérables.

**Exemples :**

```pascal
Logger.Warning('Paramètre manquant, utilisation de la valeur par défaut: %d', [ValeurDefaut]);  
Logger.Warning('Fichier de configuration introuvable, création d''un nouveau');  
Logger.Warning('Temps de réponse élevé: %d ms (seuil: %d ms)', [Temps, Seuil]);  
Logger.Warning('Cache plein à %d%%, nettoyage recommandé', [Pourcentage]);  
Logger.Warning('Tentative de reconnexion %d/%d', [Essai, MaxEssais]);  
```

**Caractéristiques :**
- 🟡 Indique un problème potentiel
- 🟢 L'application continue normalement
- 🟢 Nécessite attention mais pas urgence

**Quand utiliser :**
- Valeurs par défaut utilisées
- Performances dégradées mais acceptables
- Ressources faibles mais suffisantes
- Tentatives de retry

### 2.5 ERROR : Erreurs Gérées

**Usage :** Erreurs qui empêchent une opération mais pas l'application entière.

**Exemples :**

```pascal
Logger.Error('Impossible d''ouvrir le fichier: %s (Code: %d)', [NomFichier, CodeErreur]);  
Logger.Error('Échec de connexion à la base de données après %d tentatives', [Tentatives]);  
Logger.Error('Format de données invalide: %s', [Donnees]);  
Logger.Error('Exception capturée: %s', [E.Message]);  
Logger.Error('Timeout lors de l''appel API (%d ms)', [Timeout]);  
```

**Caractéristiques :**
- 🔴 Erreur confirmée
- 🟢 Erreur gérée (try-except)
- 🟢 Application continue
- 🔴 Nécessite investigation

**Quand utiliser :**
- Exceptions capturées
- Opérations échouées
- Données invalides
- Ressources indisponibles

### 2.6 FATAL : Erreurs Critiques

**Usage :** Erreurs catastrophiques qui vont arrêter l'application.

**Exemples :**

```pascal
Logger.Fatal('Mémoire insuffisante, arrêt du programme');  
Logger.Fatal('Corruption de données détectée dans la table principale');  
Logger.Fatal('Composant critique indisponible: %s', [Composant]);  
Logger.Fatal('Exception non gérée: %s', [E.Message]);  
```

**Caractéristiques :**
- 💀 Erreur critique
- 🔴 Application va s'arrêter
- 🚨 Alerte immédiate nécessaire

**Quand utiliser :**
- Avant un crash imminent
- Corruption de données critique
- Ressource vitale indisponible
- État incohérent irréparable

### 2.7 Hiérarchie des Niveaux

**Principe :** Activer un niveau active aussi tous les niveaux supérieurs.

```
DEBUG ──→ INFO ──→ WARNING ──→ ERROR ──→ FATAL
 (5)       (4)       (3)         (2)       (1)

Si niveau = WARNING:
✅ WARNING activé
✅ ERROR activé
✅ FATAL activé
❌ INFO désactivé
❌ DEBUG désactivé
```

**Configuration typique par environnement :**

| Environnement | Niveau | Logs Visibles |
|---------------|--------|---------------|
| **Développement** | DEBUG | Tout |
| **Test** | INFO | INFO, WARN, ERROR, FATAL |
| **Staging** | INFO | INFO, WARN, ERROR, FATAL |
| **Production** | WARNING | WARN, ERROR, FATAL |

---

## 3. Implémentation d'un Logger Simple

### 3.1 Structure de Base

```pascal
unit SimpleLogger;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  TSimpleLogger = class
  private
    FLogFile: TextFile;
    FLogFileName: String;
    FMinLevel: TLogLevel;
    FFileOpen: Boolean;
    function LevelToString(Level: TLogLevel): String;
    function GetTimestamp: String;
  public
    constructor Create(const AFileName: String; AMinLevel: TLogLevel = llInfo);
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; const Msg: String); overload;
    procedure Log(Level: TLogLevel; const Msg: String; const Args: array of const); overload;

    procedure Debug(const Msg: String); overload;
    procedure Debug(const Msg: String; const Args: array of const); overload;

    procedure Info(const Msg: String); overload;
    procedure Info(const Msg: String; const Args: array of const); overload;

    procedure Warning(const Msg: String); overload;
    procedure Warning(const Msg: String; const Args: array of const); overload;

    procedure Error(const Msg: String); overload;
    procedure Error(const Msg: String; const Args: array of const); overload;

    procedure Fatal(const Msg: String); overload;
    procedure Fatal(const Msg: String; const Args: array of const); overload;

    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
  end;

var
  Logger: TSimpleLogger;

implementation

constructor TSimpleLogger.Create(const AFileName: String; AMinLevel: TLogLevel);  
begin  
  inherited Create;
  FLogFileName := AFileName;
  FMinLevel := AMinLevel;
  FFileOpen := False;

  try
    AssignFile(FLogFile, FLogFileName);
    if FileExists(FLogFileName) then
      Append(FLogFile)
    else
      Rewrite(FLogFile);
    FFileOpen := True;
  except
    on E: Exception do
      WriteLn('Erreur ouverture fichier log: ', E.Message);
  end;
end;

destructor TSimpleLogger.Destroy;  
begin  
  if FFileOpen then
    CloseFile(FLogFile);
  inherited Destroy;
end;

function TSimpleLogger.LevelToString(Level: TLogLevel): String;  
begin  
  case Level of
    llDebug:   Result := 'DEBUG';
    llInfo:    Result := 'INFO ';
    llWarning: Result := 'WARN ';
    llError:   Result := 'ERROR';
    llFatal:   Result := 'FATAL';
  end;
end;

function TSimpleLogger.GetTimestamp: String;  
begin  
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

procedure TSimpleLogger.Log(Level: TLogLevel; const Msg: String);  
var  
  LogLine: String;
begin
  if Level < FMinLevel then Exit;

  LogLine := Format('%s [%s] %s', [GetTimestamp, LevelToString(Level), Msg]);

  if FFileOpen then
  begin
    WriteLn(FLogFile, LogLine);
    Flush(FLogFile);  // Écriture immédiate
  end;

  // Affichage console aussi (optionnel)
  WriteLn(LogLine);
end;

procedure TSimpleLogger.Log(Level: TLogLevel; const Msg: String; const Args: array of const);  
begin  
  Log(Level, Format(Msg, Args));
end;

procedure TSimpleLogger.Debug(const Msg: String);  
begin  
  Log(llDebug, Msg);
end;

procedure TSimpleLogger.Debug(const Msg: String; const Args: array of const);  
begin  
  Log(llDebug, Msg, Args);
end;

procedure TSimpleLogger.Info(const Msg: String);  
begin  
  Log(llInfo, Msg);
end;

procedure TSimpleLogger.Info(const Msg: String; const Args: array of const);  
begin  
  Log(llInfo, Msg, Args);
end;

procedure TSimpleLogger.Warning(const Msg: String);  
begin  
  Log(llWarning, Msg);
end;

procedure TSimpleLogger.Warning(const Msg: String; const Args: array of const);  
begin  
  Log(llWarning, Msg, Args);
end;

procedure TSimpleLogger.Error(const Msg: String);  
begin  
  Log(llError, Msg);
end;

procedure TSimpleLogger.Error(const Msg: String; const Args: array of const);  
begin  
  Log(llError, Msg, Args);
end;

procedure TSimpleLogger.Fatal(const Msg: String);  
begin  
  Log(llFatal, Msg);
end;

procedure TSimpleLogger.Fatal(const Msg: String; const Args: array of const);  
begin  
  Log(llFatal, Msg, Args);
end;

initialization
  Logger := TSimpleLogger.Create('application.log', llInfo);

finalization
  Flush(Output);
  FreeAndNil(Logger);

end.
```

### 3.2 Utilisation du Logger

```pascal
program TestLogger;

uses
  SimpleLogger, SysUtils;

procedure ConnecterBDD;  
begin  
  Logger.Info('Tentative de connexion à la base de données');

  if Random(2) = 0 then
  begin
    Logger.Error('Connexion échouée: Timeout');
    Exit;
  end;

  Logger.Info('Connexion réussie');
end;

procedure TraiterFichier(const NomFichier: String);  
var  
  i: Integer;
begin
  Logger.Debug('Début traitement: %s', [NomFichier]);

  if not FileExists(NomFichier) then
  begin
    Logger.Error('Fichier introuvable: %s', [NomFichier]);
    Exit;
  end;

  Logger.Info('Traitement de %s', [NomFichier]);

  for i := 1 to 100 do
  begin
    Logger.Debug('Ligne %d traitée', [i]);
    Sleep(10);
  end;

  Logger.Info('Traitement terminé avec succès');
end;

begin
  Randomize;

  Logger.Info('=== Démarrage de l''application ===');
  Logger.Info('Version: 1.0.0');

  ConnecterBDD;

  TraiterFichier('data.txt');
  TraiterFichier('inexistant.txt');

  Logger.Info('=== Application terminée ===');
end.
```

**Sortie dans application.log :**

```
2025-10-15 14:30:12.456 [INFO ] === Démarrage de l'application ===
2025-10-15 14:30:12.457 [INFO ] Version: 1.0.0
2025-10-15 14:30:12.458 [INFO ] Tentative de connexion à la base de données
2025-10-15 14:30:12.789 [ERROR] Connexion échouée: Timeout
2025-10-15 14:30:12.790 [INFO ] Traitement de data.txt
2025-10-15 14:30:13.890 [INFO ] Traitement terminé avec succès
2025-10-15 14:30:13.891 [ERROR] Fichier introuvable: inexistant.txt
2025-10-15 14:30:13.892 [INFO ] === Application terminée ===
```

---

## 4. Fonctionnalités Avancées

### 4.1 Rotation des Logs

**Problème :** Les fichiers de log peuvent devenir énormes.

**Solution :** Rotation automatique par taille ou par date.

**Rotation par taille :**

```pascal
type
  TLogRotation = class
  private
    FMaxSize: Int64;  // Taille max en octets
    FMaxFiles: Integer;  // Nombre de fichiers à conserver
    procedure RotateFile(const FileName: String);
  public
    constructor Create(AMaxSize: Int64 = 10485760; AMaxFiles: Integer = 5);
    function ShouldRotate(const FileName: String): Boolean;
  end;

constructor TLogRotation.Create(AMaxSize: Int64; AMaxFiles: Integer);  
begin  
  FMaxSize := AMaxSize;  // Défaut: 10 MB
  FMaxFiles := AMaxFiles;  // Défaut: 5 fichiers
end;

function TLogRotation.ShouldRotate(const FileName: String): Boolean;  
var  
  FileInfo: TSearchRec;
begin
  Result := False;
  if FindFirst(FileName, faAnyFile, FileInfo) = 0 then
  begin
    Result := FileInfo.Size >= FMaxSize;
    FindClose(FileInfo);
  end;
end;

procedure TLogRotation.RotateFile(const FileName: String);  
var  
  i: Integer;
  OldName, NewName: String;
begin
  // Supprimer le plus ancien
  if FileExists(FileName + '.' + IntToStr(FMaxFiles)) then
    DeleteFile(FileName + '.' + IntToStr(FMaxFiles));

  // Renommer les fichiers existants
  for i := FMaxFiles - 1 downto 1 do
  begin
    OldName := FileName + '.' + IntToStr(i);
    NewName := FileName + '.' + IntToStr(i + 1);
    if FileExists(OldName) then
      RenameFile(OldName, NewName);
  end;

  // Renommer le fichier courant
  if FileExists(FileName) then
    RenameFile(FileName, FileName + '.1');
end;
```

**Résultat :**
```
application.log         ← Fichier actif  
application.log.1       ← Rotation précédente  
application.log.2  
application.log.3  
application.log.4  
application.log.5       ← Plus ancien (sera supprimé au prochain rotate)  
```

**Rotation par date :**

```pascal
function GetLogFileName: String;  
begin  
  Result := Format('app_%s.log', [FormatDateTime('yyyymmdd', Date)]);
end;

// Utilisation
Logger := TSimpleLogger.Create(GetLogFileName);
```

**Résultat :**
```
app_20251015.log  
app_20251016.log  
app_20251017.log  
```

### 4.2 Logging Asynchrone

**Problème :** L'écriture sur disque ralentit le programme.

**Solution :** Buffer en mémoire et écriture en arrière-plan.

```pascal
// Nécessite FPC 3.2+ avec les mode switches suivants :
// {$modeswitch anonymousfunctions}
// {$modeswitch functionreferences}

type
  TAsyncLogger = class
  private
    FBuffer: TStringList;
    FThread: TThread;
    FLock: TCriticalSection;
    procedure FlushBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const Msg: String);
  end;

constructor TAsyncLogger.Create;  
begin  
  inherited Create;
  FBuffer := TStringList.Create;
  FLock := TCriticalSection.Create;

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      while not TThread.CurrentThread.Terminated do
      begin
        Sleep(1000);  // Flush toutes les secondes
        FlushBuffer;
      end;
    end);
  FThread.FreeOnTerminate := False;  // Important : on gère la libération nous-mêmes
  FThread.Start;
end;

destructor TAsyncLogger.Destroy;  
begin  
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;

  FlushBuffer;  // Dernier flush

  FBuffer.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TAsyncLogger.Log(const Msg: String);  
begin  
  FLock.Enter;
  try
    FBuffer.Add(Msg);
  finally
    FLock.Leave;
  end;
end;

procedure TAsyncLogger.FlushBuffer;  
var  
  F: TextFile;
  i: Integer;
  LocalBuffer: TStringList;
begin
  // Copier le buffer rapidement
  FLock.Enter;
  try
    if FBuffer.Count = 0 then Exit;

    LocalBuffer := TStringList.Create;
    LocalBuffer.Assign(FBuffer);
    FBuffer.Clear;
  finally
    FLock.Leave;
  end;

  // Écrire sans bloquer
  try
    AssignFile(F, 'async.log');
    if FileExists('async.log') then
      Append(F)
    else
      Rewrite(F);

    for i := 0 to LocalBuffer.Count - 1 do
      WriteLn(F, LocalBuffer[i]);

    CloseFile(F);
  finally
    LocalBuffer.Free;
  end;
end;
```

**Avantages :**
- ⚡ Pas de ralentissement du programme
- 📦 Écriture par lots (plus efficace)
- 🔒 Thread-safe

**Inconvénients :**
- ⚠️ Logs peuvent être perdus en cas de crash
- 🔧 Plus complexe à implémenter

### 4.3 Formatage Avancé

**Ajouter des informations contextuelles :**

```pascal
procedure TAdvancedLogger.Log(Level: TLogLevel; const Msg: String);  
var  
  LogLine: String;
  ThreadID: String;
  MemUsage: String;
begin
  ThreadID := IntToStr(GetCurrentThreadId);
  MemUsage := FormatFloat('#,##0', GetMemoryUsage div 1024) + ' KB';

  LogLine := Format('%s [%s] [Thread:%s] [Mem:%s] %s',
    [GetTimestamp, LevelToString(Level), ThreadID, MemUsage, Msg]);

  WriteLn(FLogFile, LogLine);
end;
```

**Sortie :**
```
2025-10-15 14:30:12.456 [INFO ] [Thread:1234] [Mem:15,234 KB] Démarrage
2025-10-15 14:30:15.789 [ERROR] [Thread:5678] [Mem:18,456 KB] Erreur traitement
```

### 4.4 Logging avec Stack Trace

**Capturer la pile d'appels lors d'erreurs :**

```pascal
uses SysUtils;

procedure LogWithStackTrace(const Msg: String);  
var  
  i: Integer;
begin
  Logger.Error(Msg);
  Logger.Error('Stack trace:');

  // FreePascal fournit des fonctions de stack trace
  {$IFDEF DEBUG}
  for i := 0 to 9 do
  begin
    // Fonction simplifiée - à adapter selon version FPC
    Logger.Error('  %d: %s', [i, GetStackFrame(i)]);
  end;
  {$ENDIF}
end;
```

---

## 5. Logging par Catégorie

### 5.1 Loggers Multiples

**Séparer les logs par domaine :**

```pascal
var
  LoggerApp: TSimpleLogger;
  LoggerBDD: TSimpleLogger;
  LoggerReseau: TSimpleLogger;

initialization
  LoggerApp := TSimpleLogger.Create('application.log');
  LoggerBDD := TSimpleLogger.Create('database.log');
  LoggerReseau := TSimpleLogger.Create('network.log');

finalization
  FreeAndNil(LoggerApp);
  FreeAndNil(LoggerBDD);
  FreeAndNil(LoggerReseau);
```

**Utilisation :**

```pascal
LoggerApp.Info('Application démarrée');  
LoggerBDD.Info('Connexion établie');  
LoggerReseau.Debug('Requête HTTP GET /api/users');  
```

**Résultat :**

**application.log :**
```
2025-10-15 14:30:12.456 [INFO ] Application démarrée
```

**database.log :**
```
2025-10-15 14:30:13.123 [INFO ] Connexion établie
```

**network.log :**
```
2025-10-15 14:30:14.789 [DEBUG] Requête HTTP GET /api/users
```

### 5.2 Préfixes et Tags

**Ajouter des tags pour filtrer :**

```pascal
procedure TTaggedLogger.Log(const Tag, Msg: String);  
begin  
  inherited Log(Format('[%s] %s', [Tag, Msg]));
end;

// Utilisation
Logger.Log('AUTH', 'Utilisateur connecté: %s', [Username]);  
Logger.Log('FILE', 'Fichier sauvegardé: %s', [FileName]);  
Logger.Log('CACHE', 'Cache vidé: %d entrées', [Count]);  
```

**Sortie :**
```
2025-10-15 14:30:12.456 [INFO ] [AUTH] Utilisateur connecté: Jean
2025-10-15 14:30:15.789 [INFO ] [FILE] Fichier sauvegardé: data.xml
2025-10-15 14:30:18.123 [INFO ] [CACHE] Cache vidé: 1234 entrées
```

---

## 6. Performance et Optimisation

### 6.1 Impact du Logging

**Mesure de l'impact :**

```pascal
var
  debut: QWord;
  i: Integer;
begin
  // Sans logging
  debut := GetTickCount64;
  for i := 1 to 100000 do
    Traiter();
  WriteLn('Sans log: ', GetTickCount64 - debut, ' ms');

  // Avec logging DEBUG (verbeux)
  Logger.MinLevel := llDebug;
  debut := GetTickCount64;
  for i := 1 to 100000 do
  begin
    Logger.Debug('Itération %d', [i]);
    Traiter();
  end;
  WriteLn('Avec log DEBUG: ', GetTickCount64 - debut, ' ms');

  // Avec logging INFO (modéré)
  Logger.MinLevel := llInfo;
  debut := GetTickCount64;
  for i := 1 to 100000 do
  begin
    if i mod 10000 = 0 then
      Logger.Info('Progression: %d%%', [(i * 100) div 100000]);
    Traiter();
  end;
  WriteLn('Avec log INFO: ', GetTickCount64 - debut, ' ms');
end.
```

**Résultats typiques :**
```
Sans log: 125 ms  
Avec log DEBUG: 8500 ms (68x plus lent !)  
Avec log INFO: 140 ms (12% plus lent)  
```

**Conclusion :** DEBUG peut ralentir énormément. Utilisez avec parcimonie.

### 6.2 Optimisation : Évaluation Paresseuse

**❌ Inefficace : Formatage systématique**

```pascal
// Le formatage se fait même si DEBUG est désactivé !
Logger.Debug(Format('Valeur complexe: %s', [CalculComplexe()]));
```

**✅ Optimisé : Vérification avant formatage**

```pascal
if Logger.MinLevel <= llDebug then
  Logger.Debug('Valeur complexe: %s', [CalculComplexe()]);
```

**Ou avec méthode inline :**

```pascal
function TSimpleLogger.IsDebugEnabled: Boolean;  
begin  
  Result := FMinLevel <= llDebug;
end;

// Utilisation
if Logger.IsDebugEnabled then
  Logger.Debug('Détail: %s', [FonctionCouteuse()]);
```

### 6.3 Buffer et Flush

**Écriture immédiate vs buffered :**

```pascal
// Immédiat (lent mais sûr)
procedure WriteImmediate(const Msg: String);  
begin  
  WriteLn(FLogFile, Msg);
  Flush(FLogFile);  // Force l'écriture sur disque
end;

// Buffered (rapide mais risque perte)
procedure WriteBuffered(const Msg: String);  
begin  
  WriteLn(FLogFile, Msg);
  // Flush automatique par l'OS
end;
```

**Compromis : Flush périodique**

```pascal
var
  FBufferCount: Integer;

procedure TLogger.Log(const Msg: String);  
begin  
  WriteLn(FLogFile, Msg);
  Inc(FBufferCount);

  if FBufferCount >= 100 then  // Flush tous les 100 logs
  begin
    Flush(FLogFile);
    FBufferCount := 0;
  end;
end;
```

---

## 7. Logging en Production

### 7.1 Configuration par Environnement

**Fichier de configuration : `config.ini`**

```ini
[Logging]
Level=INFO  
FileName=application.log  
MaxSize=10485760  
MaxFiles=5  
Console=false  
```

**Chargement de la configuration :**

```pascal
uses IniFiles;

procedure ConfigureLogger;  
var  
  ini: TIniFile;
  level: String;
begin
  ini := TIniFile.Create('config.ini');
  try
    level := ini.ReadString('Logging', 'Level', 'INFO');

    if level = 'DEBUG' then
      Logger.MinLevel := llDebug
    else if level = 'INFO' then
      Logger.MinLevel := llInfo
    else if level = 'WARNING' then
      Logger.MinLevel := llWarning
    else if level = 'ERROR' then
      Logger.MinLevel := llError;

    LogToConsole := ini.ReadBool('Logging', 'Console', False);
  finally
    ini.Free;
  end;
end;
```

### 7.2 Sécurité et Données Sensibles

**❌ DANGER : Logger des données sensibles**

```pascal
// NE JAMAIS FAIRE ÇA !
Logger.Debug('Mot de passe: %s', [Password]);  
Logger.Info('Numéro de carte: %s', [CardNumber]);  
Logger.Debug('Token API: %s', [ApiToken]);  
```

**✅ Correct : Masquer ou omettre**

```pascal
Logger.Info('Authentification réussie pour: %s', [Username]);  
Logger.Debug('Carte se terminant par: ****%s', [Copy(CardNumber, Length(CardNumber) - 3, 4)]);  

function MaskPassword(const pwd: String): String;  
begin  
  if Length(pwd) > 0 then
    Result := '****'
  else
    Result := '<vide>';
end;

Logger.Debug('Authentification avec mot de passe: %s', [MaskPassword(Password)]);
```

### 7.3 Monitoring et Alertes

**Détecter les erreurs critiques :**

```pascal
type
  TLogMonitor = class
  private
    FErrorCount: Integer;
    FLastAlertTime: TDateTime;
    procedure SendAlert(const Msg: String);
  public
    procedure OnError(const Msg: String);
  end;

procedure TLogMonitor.OnError(const Msg: String);  
begin  
  Inc(FErrorCount);

  // Alerte si 10 erreurs en 1 minute
  // Nécessite uses DateUtils pour MinutesBetween
  if (FErrorCount >= 10) and
     (MinutesBetween(Now, FLastAlertTime) >= 1) then
  begin
    SendAlert(Format('ALERTE: %d erreurs en 1 minute', [FErrorCount]));
    FErrorCount := 0;
    FLastAlertTime := Now;
  end;
end;

procedure TLogMonitor.SendAlert(const Msg: String);  
begin  
  // Envoyer email, SMS, webhook...
  Logger.Fatal('ALERTE ENVOYÉE: %s', [Msg]);
end;
```

### 7.4 Nettoyage Automatique

**Supprimer les vieux logs :**

```pascal
procedure CleanOldLogs(const LogDir: String; DaysToKeep: Integer);  
var  
  SR: TSearchRec;
  FilePath: String;
  CutoffDate: TDateTime;
begin
  CutoffDate := Now - DaysToKeep;

  if FindFirst(LogDir + '*.log', faAnyFile, SR) = 0 then
  begin
    repeat
      FilePath := LogDir + SR.Name;

      if FileDateToDateTime(SR.Time) < CutoffDate then
      begin
        Logger.Info('Suppression ancien log: %s', [SR.Name]);
        DeleteFile(FilePath);
      end;
    until FindNext(SR) <> 0;

    FindClose(SR);
  end;
end;

// Exécuter au démarrage
CleanOldLogs('logs/', 30);  // Garde 30 jours
```

---

## 8. Outils et Bibliothèques

### 8.1 LazLogger (Lazarus)

**Logger intégré à Lazarus :**

```pascal
uses LazLogger;

begin
  DebugLn('Message de debug');
  DebugLnEnter('Entrée dans fonction');
  DebugLnExit('Sortie de fonction');
end.
```

**Avantages :**
- ✅ Intégré à Lazarus
- ✅ Simple à utiliser
- ✅ Pas de dépendance externe

**Limitations :**
- ⚠️ Pas de niveaux de log structurés
- ⚠️ Pas de rotation automatique
- ⚠️ Basique pour production

### 8.2 EventLog (Windows)

**Intégration avec le journal Windows :**

```pascal
uses Windows, JwaTlHelp32;

procedure WriteToEventLog(const Msg: String; EventType: Word);  
var  
  EventSource: THandle;
  Strings: array[0..0] of PChar;
begin
  EventSource := RegisterEventSource(nil, 'MonApplication');
  if EventSource <> 0 then
  begin
    Strings[0] := PChar(Msg);
    ReportEvent(EventSource, EventType, 0, 0, nil, 1, 0, @Strings, nil);
    DeregisterEventSource(EventSource);
  end;
end;

// Utilisation
WriteToEventLog('Application démarrée', EVENTLOG_INFORMATION_TYPE);  
WriteToEventLog('Erreur critique', EVENTLOG_ERROR_TYPE);  
```

**Visible dans :** Observateur d'événements Windows

### 8.3 Syslog (Linux)

**Intégration avec syslog système :**

```pascal
uses BaseUnix;

procedure WriteToSyslog(Priority: Integer; const Msg: String);  
begin  
  openlog('MonApplication', LOG_PID, LOG_USER);
  syslog(Priority, PChar(Msg));
  closelog();
end;

// Utilisation
WriteToSyslog(LOG_INFO, 'Application démarrée');  
WriteToSyslog(LOG_ERR, 'Erreur critique');  
```

**Logs visibles dans :** `/var/log/syslog` ou `/var/log/messages`

### 8.4 Analyse de Logs

**Outils recommandés :**

| Outil | Usage | Plateforme |
|-------|-------|------------|
| **grep** | Recherche rapide | Linux |
| **tail -f** | Suivi en temps réel | Linux |
| **LogParser** | Analyse puissante | Windows |
| **ELK Stack** | Analyse centralisée | Win/Linux |
| **Splunk** | Enterprise monitoring | Win/Linux |

**Exemples grep :**

```bash
# Toutes les erreurs
grep "ERROR" application.log

# Erreurs aujourd'hui
grep "2025-10-15" application.log | grep "ERROR"

# Compter les erreurs par type
grep "ERROR" application.log | cut -d']' -f3 | sort | uniq -c

# Suivre en temps réel
tail -f application.log | grep "ERROR"
```

---

## 9. Multi-plateforme : Windows vs Linux

### 9.1 Chemins de Fichiers

**Code portable :**

```pascal
uses SysUtils;

function GetLogPath: String;  
begin  
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + '\MonApp\logs\';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.monapp/logs/';
  {$ENDIF}

  // Créer le répertoire si nécessaire
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

// Utilisation
Logger := TSimpleLogger.Create(GetLogPath + 'application.log');
```

**Résultats :**
- **Windows :** `C:\Users\Jean\AppData\Roaming\MonApp\logs\application.log`
- **Linux :** `/home/jean/.monapp/logs/application.log`

### 9.2 Fins de Ligne

**Gérer les différences :**

```pascal
// FreePascal fournit déjà la constante LineEnding (unit System)
// qui vaut #13#10 sous Windows et #10 sous Linux.
// Il suffit de l'utiliser directement :
WriteLn(FLogFile, Msg);  // WriteLn ajoute automatiquement le saut de ligne
```

### 9.3 Permissions (Linux)

**Vérifier les droits d'écriture :**

```pascal
function CanWriteToLogDir(const Dir: String): Boolean;  
begin  
  Result := DirectoryExists(Dir);

  {$IFDEF LINUX}
  // Vérifier permissions d'écriture
  Result := Result and (FpAccess(PChar(Dir), W_OK) = 0);
  {$ENDIF}
end;

if not CanWriteToLogDir(LogPath) then
  raise Exception.Create('Impossible d''écrire dans: ' + LogPath);
```

---

## 10. Bonnes Pratiques

### 10.1 Checklist du Logging

**Configuration :**
- [ ] Niveau approprié par environnement (DEBUG en dev, INFO/WARN en prod)
- [ ] Rotation des fichiers configurée
- [ ] Chemin de logs accessible en écriture
- [ ] Taille maximale définie

**Sécurité :**
- [ ] Pas de mots de passe dans les logs
- [ ] Pas de tokens/clés API
- [ ] Données sensibles masquées
- [ ] Permissions fichiers restreintes (Linux)

**Performance :**
- [ ] Logging DEBUG désactivé en production
- [ ] Évaluation paresseuse pour calculs coûteux
- [ ] Buffer ou logging asynchrone si nécessaire
- [ ] Flush périodique configuré

**Maintenance :**
- [ ] Nettoyage automatique des vieux logs
- [ ] Monitoring des erreurs actif
- [ ] Documentation des codes d'erreur
- [ ] Procédure d'analyse en cas d'incident

### 10.2 Messages de Log Efficaces

**❌ Mauvais messages :**

```pascal
Logger.Error('Erreur');  // Trop vague  
Logger.Info('OK');  // Pas assez d'info  
Logger.Debug('x=5');  // Pas de contexte  
```

**✅ Bons messages :**

```pascal
Logger.Error('Impossible de se connecter à la BDD (host: %s, port: %d, erreur: %s)',
             [DBHost, DBPort, E.Message]);

Logger.Info('Traitement de %d fichiers terminé en %d secondes (taux: %.2f fichiers/s)',
            [Count, Seconds, Count/Seconds]);

Logger.Debug('Fonction CalculerTotal - x=%d, y=%d, total=%d', [x, y, total]);
```

**Règles :**
- ✅ Inclure le contexte (quoi, où, pourquoi)
- ✅ Ajouter des valeurs numériques
- ✅ Inclure les codes d'erreur
- ✅ Être précis et actionnable
- ❌ Éviter les messages génériques

### 10.3 Structure de Message Recommandée

**Template :**
```
[TIMESTAMP] [LEVEL] [COMPONENT] Action: Details (Values) [Code]
```

**Exemples :**
```
2025-10-15 14:30:12.456 [INFO ] [AUTH] Connexion utilisateur: jean@example.com (ID: 1234)
2025-10-15 14:30:15.789 [ERROR] [BDD] Échec requête SELECT (timeout: 5000ms) [DB-001]
2025-10-15 14:30:18.123 [WARN ] [CACHE] Mémoire cache: 85% (seuil: 80%) [CACHE-002]
```

### 10.4 Logs et Exceptions

**Pattern try-except avec logging :**

```pascal
procedure TraiterDonnees;  
begin  
  try
    Logger.Info('Début traitement données');

    // Opérations
    ChargerDonnees;
    ValiderDonnees;
    EnregistrerDonnees;

    Logger.Info('Traitement terminé avec succès');
  except
    on E: EFileNotFound do
      Logger.Error('Fichier introuvable: %s', [E.Message]);

    on E: EValidationError do
      Logger.Warning('Données invalides: %s (ignorées)', [E.Message]);

    on E: Exception do
    begin
      Logger.Fatal('Exception non gérée: %s - %s', [E.ClassName, E.Message]);
      raise;  // Re-lever l'exception
    end;
  end;
end;
```

---

## 11. Cas Pratiques

### 11.1 Application Console

```pascal
program ConsoleApp;

uses
  SimpleLogger, SysUtils;

procedure Executer;  
var  
  i: Integer;
begin
  Logger.Info('Démarrage du traitement');

  for i := 1 to 10 do
  begin
    Logger.Debug('Itération %d/10', [i]);

    try
      // Simulation travail
      Sleep(100);

      if i = 7 then
        raise Exception.Create('Erreur simulée');

      Logger.Info('Étape %d terminée', [i]);
    except
      on E: Exception do
        Logger.Error('Erreur à l''étape %d: %s', [i, E.Message]);
    end;
  end;

  Logger.Info('Traitement terminé');
end;

begin
  try
    Logger.Info('=== Application démarrée ===');
    Executer;
    Logger.Info('=== Application terminée normalement ===');
  except
    on E: Exception do
    begin
      Logger.Fatal('Crash: %s', [E.Message]);
      ExitCode := 1;
    end;
  end;
end.
```

### 11.2 Service / Daemon

```pascal
program ServiceApp;

{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

procedure ServiceLoop;  
begin  
  Logger.Info('Service démarré');

  while not Terminated do
  begin
    try
      // Travail périodique
      Logger.Debug('Cycle de surveillance');

      VerifierSysteme;
      TraiterFiles;

      Sleep(60000);  // 1 minute
    except
      on E: Exception do
        Logger.Error('Erreur dans boucle service: %s', [E.Message]);
    end;
  end;

  Logger.Info('Service arrêté');
end;

begin
  Logger.Info('=== Service en cours de démarrage ===');

  try
    ServiceLoop;
  except
    on E: Exception do
      Logger.Fatal('Service crashé: %s', [E.Message]);
  end;
end.
```

### 11.3 Application Web/API

```pascal
procedure TAPIController.HandleRequest(Request: TRequest; Response: TResponse);  
var  
  debut: QWord;
  duree: Integer;
begin
  debut := GetTickCount64;

  Logger.Info('API Request: %s %s (Client: %s)',
              [Request.Method, Request.Path, Request.RemoteAddr]);

  try
    // Traitement de la requête
    ProcessRequest(Request, Response);

    duree := GetTickCount64 - debut;
    Logger.Info('API Response: %d (%d ms)', [Response.StatusCode, duree]);

    if duree > 1000 then
      Logger.Warning('Requête lente: %s (%d ms)', [Request.Path, duree]);

  except
    on E: Exception do
    begin
      Logger.Error('API Error: %s - %s', [Request.Path, E.Message]);
      Response.StatusCode := 500;
      Response.Content := '{"error": "Internal Server Error"}';
    end;
  end;
end;
```

---

## 12. Récapitulatif

### 12.1 Les 5 Commandements du Logging

1. **Tu loggeras avec des niveaux** - DEBUG, INFO, WARN, ERROR, FATAL
2. **Tu structureras tes logs** - Timestamp, niveau, message cohérent
3. **Tu protégeras les données sensibles** - Jamais de mots de passe
4. **Tu optimiseras en production** - Niveau WARNING, rotation active
5. **Tu monitoreras les erreurs** - Alertes sur erreurs critiques

### 12.2 Configuration Recommandée

**Développement :**
```ini
[Logging]
Level=DEBUG  
Console=true  
File=dev.log  
Rotation=false  
```

**Production :**
```ini
[Logging]
Level=WARNING  
Console=false  
File=production.log  
Rotation=true  
MaxSize=10MB  
MaxFiles=10  
```

### 12.3 Tableau de Décision Rapide

| Situation | Niveau | Exemple |
|-----------|--------|---------|
| Traçage détaillé du code | DEBUG | Variables, flux d'exécution |
| Événement normal | INFO | Démarrage, connexions |
| Situation anormale mais OK | WARNING | Valeur par défaut utilisée |
| Erreur gérée | ERROR | Fichier introuvable |
| Crash imminent | FATAL | Mémoire saturée |

---

## Conclusion

Le logging structuré est un pilier de toute application professionnelle. Un bon système de logging vous fait gagner des heures de débogage et vous permet de comprendre ce qui se passe en production.

**Points clés à retenir :**

1. **Utiliser les niveaux correctement** - Chaque niveau a sa signification
2. **Structurer les messages** - Format cohérent et parsable
3. **Configurer par environnement** - DEBUG en dev, WARNING en prod
4. **Gérer la rotation** - Éviter les fichiers géants
5. **Protéger les données sensibles** - Jamais de mots de passe ou tokens
6. **Monitorer activement** - Alertes sur erreurs critiques

**Citation finale :**
> "Good logging is not about quantity, it's about quality and relevance."

Avec un système de logging bien conçu, vous transformez votre application en une boîte noire qui vous révèle tous ses secrets quand vous en avez besoin.

---

**Multi-plateforme Windows/Ubuntu** ✓  
**Formation FreePascal/Lazarus - Niveau Débutant à Intermédiaire**  

⏭️ [Tests unitaires avec FPCUnit (introduction)](/20-debogage-optimisation/09-tests-unitaires-fpcunit-introduction.md)
