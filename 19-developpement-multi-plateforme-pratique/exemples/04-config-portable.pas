{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Alternative portable au registre avec fichiers INI
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program ConfigPortable;

{$mode objfpc}{$H+}

uses
  SysUtils, IniFiles;

procedure SauvegarderConfig(const Valeur: string);  
var  
  IniFile: TIniFile;
  CheminIni: string;
begin
  // Fonctionne sous Windows ET Unix/Linux !
  CheminIni := GetAppConfigDir(False) + 'config.ini';

  // Creer le repertoire s'il n'existe pas
  ForceDirectories(GetAppConfigDir(False));

  IniFile := TIniFile.Create(CheminIni);
  try
    IniFile.WriteString('Parametres', 'Valeur', Valeur);
    WriteLn('Configuration sauvegardee dans : ', CheminIni);
  finally
    IniFile.Free;
  end;
end;

begin
  SauvegarderConfig('Configuration portable');
end.
