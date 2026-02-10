{ ============================================================================
  Section 19.2 : Gestion portable des chemins
  Description : Chargement d'un fichier de configuration portable
  Fichier source : 02-gestion-portable-chemins.md
  ============================================================================ }
program ChargementConfig;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

function ObtenirCheminConfig: string;  
var  
  RepApp: string;
begin
  // Repertoire de l'executable
  RepApp := ExtractFilePath(ParamStr(0));

  // Construire le chemin vers le fichier config
  Result := IncludeTrailingPathDelimiter(RepApp) +
            'config' + PathDelim + 'settings.ini';
end;

procedure ChargerConfiguration;  
var  
  CheminConfig: string;
  Config: TStringList;
begin
  CheminConfig := ObtenirCheminConfig;

  WriteLn('Chargement de : ', CheminConfig);

  if FileExists(CheminConfig) then
  begin
    Config := TStringList.Create;
    try
      Config.LoadFromFile(CheminConfig);
      WriteLn('Configuration chargee : ', Config.Count, ' lignes');
    finally
      Config.Free;
    end;
  end
  else
    WriteLn('Fichier de configuration introuvable !');
end;

begin
  ChargerConfiguration;
end.
