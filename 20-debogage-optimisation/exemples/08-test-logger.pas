{ ============================================================================
  Section 20.8 : Logging structure et niveaux de log
  Description : Demonstration du logger avec niveaux de log
  Fichier source : 08-logging-structure-niveaux-log.md
  ============================================================================ }
program TestLogger;

{$mode objfpc}{$H+}

uses
  SimpleLogger, SysUtils;

procedure ConnecterBDD;
begin
  Logger.Info('Tentative de connexion a la base de donnees');

  if Random(2) = 0 then
  begin
    Logger.Error('Connexion echouee: Timeout');
    Exit;
  end;

  Logger.Info('Connexion reussie');
end;

procedure TraiterFichier(const NomFichier: String);
var
  i: Integer;
begin
  Logger.Debug('Debut traitement: %s', [NomFichier]);

  if not FileExists(NomFichier) then
  begin
    Logger.Error('Fichier introuvable: %s', [NomFichier]);
    Exit;
  end;

  Logger.Info('Traitement de %s', [NomFichier]);

  for i := 1 to 100 do
  begin
    Logger.Debug('Ligne %d traitee', [i]);
    Sleep(10);
  end;

  Logger.Info('Traitement termine avec succes');
end;

begin
  Randomize;

  Logger.Info('=== Demarrage de l''application ===');
  Logger.Info('Version: 1.0.0');

  ConnecterBDD;

  TraiterFichier('data.txt');
  TraiterFichier('inexistant.txt');

  Logger.Info('=== Application terminee ===');
end.
