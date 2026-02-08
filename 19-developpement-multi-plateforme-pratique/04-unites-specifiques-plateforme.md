🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.4 Unités spécifiques par plateforme

## Introduction

FreePascal fournit de nombreuses unités (bibliothèques de code) pour interagir avec le système d'exploitation. Certaines sont **portables** et fonctionnent partout (comme `SysUtils`), tandis que d'autres sont **spécifiques** à une plateforme donnée.

**Analogie :** Imaginez un traducteur universel (unités portables) versus un dictionnaire spécialisé français-allemand (unités spécifiques). Le traducteur universel fonctionne pour toutes les langues, mais le dictionnaire spécialisé offre plus de détails pour un usage particulier.

Dans ce chapitre, nous allons découvrir ces unités spécifiques et apprendre quand les utiliser.

---

## 1. Unités Portables vs Spécifiques

### Unités Portables (Utilisez-les en Priorité !)

Ces unités fonctionnent sur toutes les plateformes :

| Unité | Description |
|-------|-------------|
| `SysUtils` | Fonctions système de base (fichiers, dates, chaînes) |
| `Classes` | Classes fondamentales (TList, TStringList, TStream) |
| `Process` | Exécution de processus externes |
| `FileUtil` | Manipulation de fichiers et répertoires |
| `Math` | Fonctions mathématiques |
| `StrUtils` | Manipulation avancée de chaînes |

**Règle d'or :** Si une fonction portable existe, utilisez-la plutôt que la version spécifique à une plateforme !

### Unités Spécifiques à Windows

Ces unités ne fonctionnent **QUE** sous Windows :

| Unité | Description |
|-------|-------------|
| `Windows` | API Windows de base |
| `ShellAPI` | Interaction avec le shell Windows |
| `Registry` | Accès à la base de registre Windows |
| `WinSock` | Programmation réseau Windows (ancien) |
| `MMSystem` | Multimédia Windows |
| `ActiveX` | Support ActiveX/COM |

### Unités Spécifiques à Unix/Linux

Ces unités ne fonctionnent **QUE** sous Unix/Linux/macOS :

| Unité | Description |
|-------|-------------|
| `BaseUnix` | Appels système Unix de base |
| `Unix` | Fonctions Unix étendues |
| `UnixUtil` | Utilitaires Unix |
| `Termio` | Contrôle du terminal |
| `CThreads` | Support des threads POSIX |

---

## 2. Comment Utiliser les Unités Spécifiques

### Méthode 1 : Clause Uses Conditionnelle

La manière la plus propre est d'inclure les unités de façon conditionnelle dans la clause `uses` :

```pascal
program UtilisationConditionnelle;

uses
  SysUtils,  // Portable - toujours disponible
  {$IFDEF WINDOWS}
  Windows,   // Seulement sous Windows
  Registry,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,  // Seulement sous Unix/Linux
  Unix,
  {$ENDIF}
  Classes;

begin
  WriteLn('Programme multi-plateforme');
end.
```

**Avantage :** Le code est clair et le compilateur ne charge que les unités nécessaires.

### Méthode 2 : Uses Séparées Interface/Implementation

Pour les gros programmes, vous pouvez mettre les unités portables dans `interface` et les unités spécifiques dans `implementation` (une seule clause `uses` par section) :

```pascal
unit MonUnite;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;  // Unités portables communes

// ... déclarations ...

implementation

{$IFDEF WINDOWS}
uses
  Windows, Registry;
{$ENDIF}

{$IFDEF UNIX}
uses
  BaseUnix, Unix;
{$ENDIF}

// ... code ...

end.
```

**Note :** On ne peut avoir qu'une seule clause `uses` par section (`interface` ou `implementation`). Les blocs `{$IFDEF}` dans `implementation` fonctionnent car un seul sera actif à la compilation.

---

## 3. Unité Windows : API Windows

### Présentation

L'unité `Windows` donne accès à l'API (Application Programming Interface) Windows complète. C'est la bibliothèque officielle de Microsoft pour interagir avec le système.

### Cas d'Usage Courants

#### Obtenir le Nom de l'Ordinateur

```pascal
program NomOrdinateur;

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

function ObtenirNomPC: string;
{$IFDEF WINDOWS}
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  if GetComputerName(@Buffer, Size) then
    Result := Buffer
  else
    Result := 'Inconnu';
  {$ELSE}
  Result := GetEnvironmentVariable('HOSTNAME');  // Unix/Linux
  {$ENDIF}
end;

begin
  WriteLn('Nom de l''ordinateur : ', ObtenirNomPC);
end.
```

#### Obtenir des Informations sur le Système

```pascal
program InfoSysteme;

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

procedure AfficherInfoSysteme;
{$IFDEF WINDOWS}
var
  VersionInfo: TOSVersionInfo;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(VersionInfo) then
  begin
    WriteLn('Windows Version : ', VersionInfo.dwMajorVersion, '.',
            VersionInfo.dwMinorVersion);
    WriteLn('Build : ', VersionInfo.dwBuildNumber);
  end;
  {$ELSE}
  WriteLn('Système Unix/Linux');
  // Utiliser d'autres méthodes pour Unix
  {$ENDIF}
end;

begin
  AfficherInfoSysteme;
end.
```

#### Accéder aux Variables d'Environnement

```pascal
program VariablesEnv;

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

function ObtenirTempDir: string;
{$IFDEF WINDOWS}
var
  Buffer: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Méthode Windows native
  GetTempPath(MAX_PATH, @Buffer);
  Result := Buffer;
  {$ELSE}
  // Méthode portable (fonctionne aussi sous Windows !)
  Result := GetTempDir;  // SysUtils
  {$ENDIF}
end;

begin
  WriteLn('Répertoire temporaire : ', ObtenirTempDir);
end.
```

**Note :** Dans ce dernier exemple, la fonction portable `GetTempDir` de `SysUtils` fait le même travail. Privilégiez toujours la version portable quand elle existe !

---

## 4. Unité Registry : Registre Windows

### Présentation

Le registre Windows est une base de données système qui stocke les configurations. **Attention :** Cette fonctionnalité est 100% spécifique à Windows.

### Lecture du Registre

```pascal
program LectureRegistre;

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Registry
  {$ENDIF};

procedure LireParametre;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  Valeur: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Reg := TRegistry.Create;
  try
    // Ouvrir une clé en lecture seule
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKeyReadOnly('Software\MonApplication') then
    begin
      if Reg.ValueExists('Parametres') then
      begin
        Valeur := Reg.ReadString('Parametres');
        WriteLn('Paramètre trouvé : ', Valeur);
      end
      else
        WriteLn('Paramètre non trouvé');

      Reg.CloseKey;
    end
    else
      WriteLn('Clé non trouvée');
  finally
    Reg.Free;
  end;
  {$ELSE}
  WriteLn('Le registre Windows n''existe pas sous Unix/Linux');
  WriteLn('Utiliser des fichiers de configuration à la place');
  {$ENDIF}
end;

begin
  LireParametre;
end.
```

### Écriture dans le Registre

```pascal
program EcritureRegistre;

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Registry
  {$ENDIF};

procedure SauvegarderParametre(const Valeur: string);
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Créer la clé si elle n'existe pas
    if Reg.OpenKey('Software\MonApplication', True) then
    begin
      Reg.WriteString('Parametres', Valeur);
      WriteLn('Paramètre sauvegardé dans le registre');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  {$ELSE}
  WriteLn('Registre non disponible - sauvegarde dans un fichier INI');
  // Utiliser TIniFile à la place
  {$ENDIF}
end;

begin
  SauvegarderParametre('Ma valeur de test');
end.
```

### Alternative Portable : Fichiers INI

Pour une application multi-plateforme, préférez les fichiers INI :

```pascal
program ConfigPortable;

uses
  SysUtils, IniFiles;

procedure SauvegarderConfig(const Valeur: string);
var
  IniFile: TIniFile;
  CheminIni: string;
begin
  // Fonctionne sous Windows ET Unix/Linux !
  CheminIni := GetAppConfigDir(False) + 'config.ini';

  // Créer le répertoire s'il n'existe pas
  ForceDirectories(GetAppConfigDir(False));

  IniFile := TIniFile.Create(CheminIni);
  try
    IniFile.WriteString('Parametres', 'Valeur', Valeur);
    WriteLn('Configuration sauvegardée dans : ', CheminIni);
  finally
    IniFile.Free;
  end;
end;

begin
  SauvegarderConfig('Configuration portable');
end.
```

---

## 5. Unités Unix/Linux

### Unité BaseUnix

Fournit les appels système Unix de base :

```pascal
program ExempleBaseUnix;

uses
  SysUtils
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

procedure AfficherProcessID;
{$IFDEF UNIX}
var
  PID: TPid;
{$ENDIF}
begin
  {$IFDEF UNIX}
  PID := FpGetPid;
  WriteLn('ID du processus : ', PID);

  // ID de l'utilisateur
  WriteLn('User ID : ', FpGetUid);
  WriteLn('Group ID : ', FpGetGid);
  {$ELSE}
  WriteLn('Fonctionnalité Unix uniquement');
  {$ENDIF}
end;

begin
  AfficherProcessID;
end.
```

### Unité Unix

Fonctions Unix étendues :

```pascal
program ExempleUnix;

uses
  SysUtils
  {$IFDEF UNIX}
  , Unix
  {$ENDIF};

procedure ListerEnvironnement;
{$IFDEF UNIX}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF UNIX}
  WriteLn('Variables d''environnement :');
  for i := 0 to GetEnvironmentVariableCount - 1 do
    WriteLn('  ', GetEnvironmentString(i));
  {$ELSE}
  WriteLn('Utilisez GetEnvironmentVariable() pour une approche portable');
  {$ENDIF}
end;

begin
  ListerEnvironnement;
end.
```

### Vérification des Permissions de Fichiers

```pascal
program PermissionsFichier;

uses
  SysUtils
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

procedure VerifierPermissions(const Fichier: string);
{$IFDEF UNIX}
var
  StatInfo: TStat;
{$ENDIF}
begin
  {$IFDEF UNIX}
  if FpStat(Fichier, StatInfo) = 0 then
  begin
    WriteLn('Permissions du fichier : ', Fichier);

    // Lecture
    if (StatInfo.st_mode and S_IRUSR) <> 0 then
      WriteLn('  Propriétaire : Lecture OK');

    // Écriture
    if (StatInfo.st_mode and S_IWUSR) <> 0 then
      WriteLn('  Propriétaire : Écriture OK');

    // Exécution
    if (StatInfo.st_mode and S_IXUSR) <> 0 then
      WriteLn('  Propriétaire : Exécution OK');
  end
  else
    WriteLn('Impossible de lire les informations du fichier');
  {$ELSE}
  WriteLn('Vérification des permissions Unix uniquement');
  WriteLn('Sous Windows, utilisez FileGetAttr()');
  {$ENDIF}
end;

begin
  VerifierPermissions('/etc/hosts');
end.
```

---

## 6. Unité CThreads (Important pour Linux/Unix)

### Pourquoi C'est Important

Sous Linux/Unix, si vous utilisez des threads (TThread), vous **DEVEZ** inclure l'unité `CThreads` **avant** toutes les autres unités :

```pascal
program AppliAvecThreads;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  CThreads,  // DOIT être en PREMIER sous Unix/Linux !
  {$ENDIF}
  Classes, SysUtils;

type
  TMonThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TMonThread.Execute;
begin
  WriteLn('Thread en cours d''exécution...');
  Sleep(1000);
end;

var
  MonThread: TMonThread;
begin
  MonThread := TMonThread.Create(False);
  MonThread.WaitFor;
  MonThread.Free;
  WriteLn('Thread terminé');
end.
```

**Important :** Si vous oubliez `CThreads` sous Linux, votre programme plantera ou se comportera de manière incorrecte !

**Sous Windows :** L'unité `CThreads` n'est pas nécessaire (et n'existe même pas).

---

## 7. Stratégies pour le Code Multi-Plateforme

### Stratégie 1 : Wrapper Functions (Fonctions Enveloppes)

Créez des fonctions qui masquent les différences :

```pascal
unit PlatformUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

// Fonction portable pour obtenir le nom d'utilisateur
function ObtenirNomUtilisateur: string;

// Fonction portable pour obtenir l'ID du processus
function ObtenirProcessID: Integer;

implementation

function ObtenirNomUtilisateur: string;
{$IFDEF WINDOWS}
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Size := 256;
  if GetUserName(@Buffer, Size) then
    Result := Buffer
  else
    Result := 'Inconnu';
  {$ELSE}
  // Unix/Linux
  Result := GetEnvironmentVariable('USER');
  if Result = '' then
    Result := GetEnvironmentVariable('USERNAME');
  {$ENDIF}
end;

function ObtenirProcessID: Integer;
begin
  {$IFDEF WINDOWS}
  Result := GetCurrentProcessId;
  {$ELSE}
  {$IFDEF UNIX}
  Result := FpGetPid;
  {$ELSE}
  Result := -1;  // Plateforme non supportée
  {$ENDIF}
  {$ENDIF}
end;

end.
```

**Utilisation :**

```pascal
program UtilisationWrapper;

uses
  SysUtils, PlatformUtils;

begin
  WriteLn('Nom d''utilisateur : ', ObtenirNomUtilisateur);
  WriteLn('ID du processus : ', ObtenirProcessID);
  // Ce code fonctionne partout !
end.
```

### Stratégie 2 : Unités Séparées par Plateforme

Pour du code complexe, créez des unités séparées :

**Fichier : SystemInfo.pas (interface commune)**
```pascal
unit SystemInfo;

{$mode objfpc}{$H+}

interface

function ObtenirVersionOS: string;
function ObtenirNomMachine: string;

implementation

{$IFDEF WINDOWS}
  {$I SystemInfo.Windows.inc}
{$ENDIF}

{$IFDEF UNIX}
  {$I SystemInfo.Unix.inc}
{$ENDIF}

end.
```

**Fichier : SystemInfo.Windows.inc**
```pascal
uses
  SysUtils, Windows;

function ObtenirVersionOS: string;
var
  Info: TOSVersionInfo;
begin
  Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(Info) then
    Result := Format('Windows %d.%d', [Info.dwMajorVersion, Info.dwMinorVersion])
  else
    Result := 'Windows version inconnue';
end;

function ObtenirNomMachine: string;
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  if GetComputerName(@Buffer, Size) then
    Result := Buffer
  else
    Result := 'Inconnu';
end;
```

**Fichier : SystemInfo.Unix.inc**
```pascal
uses
  SysUtils, BaseUnix, Unix;

function ObtenirVersionOS: string;
var
  Info: UtsName;
begin
  if FpUname(Info) = 0 then
    Result := Format('%s %s', [Info.sysname, Info.release])
  else
    Result := 'Unix version inconnue';
end;

function ObtenirNomMachine: string;
var
  Info: UtsName;
begin
  if FpUname(Info) = 0 then
    Result := Info.nodename
  else
    Result := 'Inconnu';
end;
```

---

## 8. Cas Pratiques Complets

### Cas 1 : Ouvrir un Fichier avec l'Application par Défaut

```pascal
unit FileOpener;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows, ShellAPI
  {$ENDIF}
  {$IFDEF UNIX}
  , Process
  {$ENDIF};

procedure OuvrirFichier(const NomFichier: string);

implementation

procedure OuvrirFichier(const NomFichier: string);
{$IFDEF UNIX}
var
  Proc: TProcess;
{$ENDIF}
begin
  if not FileExists(NomFichier) then
  begin
    WriteLn('Fichier introuvable : ', NomFichier);
    Exit;
  end;

  {$IFDEF WINDOWS}
  // Utiliser ShellExecute sous Windows
  ShellExecute(0, 'open', PChar(NomFichier), nil, nil, SW_SHOWNORMAL);
  {$ENDIF}

  {$IFDEF UNIX}
  // Utiliser xdg-open sous Linux
  Proc := TProcess.Create(nil);
  try
    {$IFDEF LINUX}
    Proc.Executable := 'xdg-open';
    {$ENDIF}
    {$IFDEF DARWIN}
    Proc.Executable := 'open';
    {$ENDIF}
    Proc.Parameters.Add(NomFichier);
    Proc.Execute;
  finally
    Proc.Free;
  end;
  {$ENDIF}
end;

end.
```

### Cas 2 : Obtenir la Liste des Disques

```pascal
program ListeDisques;

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF};

procedure ListerDisques;
{$IFDEF WINDOWS}
var
  Drives: DWORD;
  i: Char;
  TypeDisque: UINT;
  NomType: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  WriteLn('Disques disponibles sous Windows :');
  Drives := GetLogicalDrives;

  for i := 'A' to 'Z' do
  begin
    if (Drives and (1 shl (Ord(i) - Ord('A')))) <> 0 then
    begin
      TypeDisque := GetDriveType(PChar(i + ':\'));

      case TypeDisque of
        DRIVE_REMOVABLE: NomType := 'Amovible';
        DRIVE_FIXED: NomType := 'Disque fixe';
        DRIVE_REMOTE: NomType := 'Réseau';
        DRIVE_CDROM: NomType := 'CD-ROM';
        DRIVE_RAMDISK: NomType := 'RAM Disk';
        else NomType := 'Inconnu';
      end;

      WriteLn('  ', i, ':\ - ', NomType);
    end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('Sous Unix/Linux, les disques sont montés dans l''arborescence');
  WriteLn('Points de montage courants :');
  WriteLn('  / (racine)');
  WriteLn('  /home (répertoires utilisateurs)');
  WriteLn('  /mnt ou /media (disques amovibles)');
  WriteLn('Consultez /etc/fstab ou utilisez la commande "mount"');
  {$ENDIF}
end;

begin
  ListerDisques;
end.
```

### Cas 3 : Notification Système

```pascal
unit SystemNotifications;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Process
  {$ENDIF};

procedure AfficherNotification(const Titre, Message: string);

implementation

procedure AfficherNotification(const Titre, Message: string);
{$IFDEF UNIX}
var
  Proc: TProcess;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Sous Windows, utiliser MessageBox
  MessageBox(0, PChar(Message), PChar(Titre), MB_ICONINFORMATION or MB_OK);
  {$ENDIF}

  {$IFDEF UNIX}
  // Sous Linux, utiliser notify-send si disponible
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'notify-send';
    Proc.Parameters.Add(Titre);
    Proc.Parameters.Add(Message);
    Proc.Options := Proc.Options + [poWaitOnExit, poNoConsole];
    try
      Proc.Execute;
    except
      // Si notify-send n'est pas disponible, afficher dans la console
      WriteLn(Titre, ' : ', Message);
    end;
  finally
    Proc.Free;
  end;
  {$ENDIF}
end;

end.
```

---

## 9. Bonnes Pratiques

### ✅ 1. Privilégier les Unités Portables

```pascal
// MAUVAIS : Utiliser une API spécifique quand une version portable existe
{$IFDEF WINDOWS}
uses Windows;
// Utiliser GetTempPath de Windows
{$ENDIF}

// BON : Utiliser la fonction portable
uses SysUtils;
// Utiliser GetTempDir qui fonctionne partout
```

### ✅ 2. Créer des Abstractions

Ne laissez pas le code spécifique à une plateforme se répandre partout. Créez une couche d'abstraction :

```pascal
// Créer une unité PlatformAPI qui expose des fonctions portables
// Le reste du code utilise uniquement cette unité
```

### ✅ 3. Documenter les Dépendances

```pascal
// Cette fonction nécessite Windows XP ou supérieur
// Sous Linux, nécessite notify-send installé
procedure MaFonction;
```

### ✅ 4. Fournir des Alternatives

```pascal
{$IFDEF WINDOWS}
// Version optimisée Windows
{$ELSE}
// Version portable de secours
{$ENDIF}
```

### ✅ 5. Tester sur Toutes les Plateformes

Ne supposez jamais qu'une fonctionnalité fonctionne sans l'avoir testée !

---

## 10. Pièges à Éviter

### ❌ Piège 1 : Oublier l'Alternative

```pascal
// MAUVAIS
{$IFDEF WINDOWS}
procedure ImportanteFonction;
begin
  // Code Windows
end;
{$ENDIF}

// Appel quelque part :
ImportanteFonction;  // Erreur de compilation sous Linux !

// BON
procedure ImportanteFonction;
begin
  {$IFDEF WINDOWS}
  // Code Windows
  {$ELSE}
  // Code pour les autres plateformes
  {$ENDIF}
end;
```

### ❌ Piège 2 : Dépendance Cachée

```pascal
// MAUVAIS : Utiliser un type spécifique sans condition
var
  Handle: THandle;  // THandle existe sous Windows ET Unix, MAIS...
  // Certains types n'existent que sur une plateforme !
```

### ❌ Piège 3 : Ordre des Unités

```pascal
// MAUVAIS (sous Linux avec threads)
uses
  Classes, SysUtils, CThreads;  // ERREUR !

// BON
uses
  {$IFDEF UNIX}
  CThreads,  // TOUJOURS en premier !
  {$ENDIF}
  Classes, SysUtils;
```

### ❌ Piège 4 : Supposer la Disponibilité d'Outils

```pascal
// MAUVAIS
{$IFDEF UNIX}
// Utiliser notify-send sans vérifier s'il est installé
{$ENDIF}

// BON
{$IFDEF UNIX}
if FileExists('/usr/bin/notify-send') then
  // Utiliser notify-send
else
  // Alternative (affichage console, fichier log, etc.)
{$ENDIF}
```

---

## 11. Tableau Récapitulatif des Unités

### Unités Portables (Priorité Maximale)

| Unité | Principales Fonctionnalités |
|-------|----------------------------|
| `SysUtils` | Gestion fichiers, dates, conversions, exceptions |
| `Classes` | TList, TStringList, TStream, TThread |
| `Process` | Exécution de processus externes |
| `FileUtil` | Opérations avancées sur fichiers |
| `IniFiles` | Fichiers de configuration INI |

### Unités Windows

| Unité | Quand l'Utiliser |
|-------|-----------------|
| `Windows` | API Windows bas niveau nécessaire |
| `Registry` | Besoin absolu du registre (préférer INI sinon) |
| `ShellAPI` | Interaction avec le shell Windows |

### Unités Unix/Linux

| Unité | Quand l'Utiliser |
|-------|-----------------|
| `BaseUnix` | Appels système Unix nécessaires |
| `Unix` | Fonctions Unix étendues |
| `CThreads` | **OBLIGATOIRE** si utilisation de threads |

---

## Conclusion

Les unités spécifiques par plateforme sont puissantes mais doivent être utilisées avec précaution :

1. **Privilégiez TOUJOURS les unités portables** quand elles existent
2. **Isolez le code spécifique** dans des fonctions wrapper
3. **Documentez les dépendances** de plateforme
4. **Testez sur toutes les plateformes** cibles
5. **Prévoyez des alternatives** fonctionnelles

**Règle d'or :** Si vous pouvez éviter d'utiliser une unité spécifique à une plateforme, évitez-la ! Votre code n'en sera que plus portable et maintenable.

Dans le prochain chapitre, nous verrons comment configurer Lazarus pour compiler vers différentes plateformes (cross-compilation).

⏭️ [Configuration de projets multi-cibles dans Lazarus](/19-developpement-multi-plateforme-pratique/05-configuration-projets-multi-cibles.md)
