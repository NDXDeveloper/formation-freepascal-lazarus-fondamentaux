{ ============================================================================
  Section 8.8 : Fichiers INI pour configuration
  Description : Configuration de connexion avec TMemIniFile
  Fichier source : 08-fichiers-ini-configuration.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ConfigConnexion;

uses
  IniFiles, SysUtils;

type
  TConfigConnexion = record
    Serveur: string;
    Port: Integer;
    BaseDeDonnees: string;
    Utilisateur: string;
    Timeout: Integer;
    SSL: Boolean;
  end;

function ChargerConfigConnexion(Fichier: string): TConfigConnexion;
var
  Ini: TMemIniFile;
begin
  if not FileExists(Fichier) then
  begin
    WriteLn('ATTENTION : Fichier de configuration introuvable !');
    WriteLn('Utilisation des valeurs par défaut.');

    // Valeurs par defaut
    Result.Serveur := 'localhost';
    Result.Port := 5432;
    Result.BaseDeDonnees := 'mabase';
    Result.Utilisateur := 'admin';
    Result.Timeout := 30;
    Result.SSL := False;
    Exit;
  end;

  Ini := TMemIniFile.Create(Fichier);
  try
    Result.Serveur := Ini.ReadString('Connexion', 'Serveur', 'localhost');
    Result.Port := Ini.ReadInteger('Connexion', 'Port', 5432);
    Result.BaseDeDonnees := Ini.ReadString('Connexion', 'BaseDeDonnees', 'mabase');
    Result.Utilisateur := Ini.ReadString('Connexion', 'Utilisateur', 'admin');
    Result.Timeout := Ini.ReadInteger('Connexion', 'Timeout', 30);
    Result.SSL := Ini.ReadBool('Connexion', 'SSL', False);
  finally
    Ini.Free;
  end;
end;

procedure SauvegarderConfigConnexion(Fichier: string; const Config: TConfigConnexion);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(Fichier);
  try
    Ini.WriteString('_INFO', 'Description', 'Configuration de connexion base de données');
    Ini.WriteString('_INFO', 'Version', '1.0');
    Ini.WriteString('_INFO', 'DateModif', DateTimeToStr(Now));

    Ini.WriteString('Connexion', 'Serveur', Config.Serveur);
    Ini.WriteInteger('Connexion', 'Port', Config.Port);
    Ini.WriteString('Connexion', 'BaseDeDonnees', Config.BaseDeDonnees);
    Ini.WriteString('Connexion', 'Utilisateur', Config.Utilisateur);
    Ini.WriteInteger('Connexion', 'Timeout', Config.Timeout);
    Ini.WriteBool('Connexion', 'SSL', Config.SSL);

    Ini.UpdateFile;
    WriteLn('Configuration sauvegardée dans : ', Fichier);
  finally
    Ini.Free;
  end;
end;

procedure AfficherConfig(const Config: TConfigConnexion);
begin
  WriteLn;
  WriteLn('=== CONFIGURATION DE CONNEXION ===');
  WriteLn('Serveur        : ', Config.Serveur);
  WriteLn('Port           : ', Config.Port);
  WriteLn('Base de données: ', Config.BaseDeDonnees);
  WriteLn('Utilisateur    : ', Config.Utilisateur);
  WriteLn('Timeout        : ', Config.Timeout, ' secondes');
  WriteLn('SSL            : ', Config.SSL);
  WriteLn('==================================');
end;

function TesterConnexion(const Config: TConfigConnexion): Boolean;
begin
  WriteLn;
  WriteLn('Test de connexion à ', Config.Serveur, ':', Config.Port, '...');
  WriteLn('Connexion simulée réussie !');
  Result := True;
end;

var
  Config: TConfigConnexion;
  FichierConfig: string;

begin
  FichierConfig := 'database.ini';

  WriteLn('===================================');
  WriteLn('  CONFIGURATION BASE DE DONNÉES   ');
  WriteLn('===================================');
  WriteLn;

  // Charger la configuration (valeurs par defaut car fichier absent)
  Config := ChargerConfigConnexion(FichierConfig);
  AfficherConfig(Config);

  // Sauvegarder pour creer le fichier
  SauvegarderConfigConnexion(FichierConfig, Config);

  // Tester la connexion
  if TesterConnexion(Config) then
    WriteLn('V Connexion OK')
  else
    WriteLn('X Échec de connexion');

  // Nettoyage du fichier de demo
  if FileExists(FichierConfig) then
    DeleteFile(FichierConfig);
end.
