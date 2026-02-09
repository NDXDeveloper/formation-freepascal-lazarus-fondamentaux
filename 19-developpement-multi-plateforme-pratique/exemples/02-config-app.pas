{ ============================================================================
  Section 19.2 : Gestion portable des chemins
  Description : Configuration d'application avec PathDelim
  Fichier source : 02-gestion-portable-chemins.md
  ============================================================================ }
program ConfigApp;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  RepApp, RepConfig, FichierConfig: string;
begin
  // Repertoire de l'application
  RepApp := ExtractFilePath(ParamStr(0));

  // Sous-repertoire config
  RepConfig := RepApp + 'config';

  // Fichier dans ce repertoire
  FichierConfig := RepConfig + PathDelim + 'app.ini';

  WriteLn('Fichier de config : ', FichierConfig);

  // Creer le repertoire s'il n'existe pas
  if not DirectoryExists(RepConfig) then
  begin
    CreateDir(RepConfig);
    WriteLn('Repertoire config cree.');
  end;
end.
