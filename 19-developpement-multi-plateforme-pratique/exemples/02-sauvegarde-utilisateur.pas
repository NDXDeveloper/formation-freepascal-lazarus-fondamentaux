{ ============================================================================
  Section 19.2 : Gestion portable des chemins
  Description : Sauvegarde de donnees utilisateur dans un repertoire portable
  Fichier source : 02-gestion-portable-chemins.md
  ============================================================================ }
program SauvegardeUtilisateur;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

function ObtenirRepertoireDonnees: string;  
begin  
  {$IFDEF WINDOWS}
  // Sous Windows : AppData\Roaming
  Result := GetEnvironmentVariable('APPDATA');
  {$ENDIF}

  {$IFDEF LINUX}
  // Sous Linux : ~/.local/share
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.local' + PathDelim + 'share';
  {$ENDIF}

  {$IFDEF DARWIN}
  // Sous macOS : ~/Library/Application Support
  Result := GetEnvironmentVariable('HOME') + PathDelim + 'Library' + PathDelim + 'Application Support';
  {$ENDIF}

  // Ajouter le nom de notre application
  Result := IncludeTrailingPathDelimiter(Result) + 'MonApp';

  // Creer le repertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);

  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure SauvegarderDonnees(const Donnees: string);  
var  
  CheminFichier: string;
  Fichier: TStringList;
begin
  CheminFichier := ObtenirRepertoireDonnees + 'donnees.txt';

  WriteLn('Sauvegarde dans : ', CheminFichier);

  Fichier := TStringList.Create;
  try
    Fichier.Text := Donnees;
    Fichier.SaveToFile(CheminFichier);
    WriteLn('Donnees sauvegardees avec succes.');
  finally
    Fichier.Free;
  end;
end;

begin
  SauvegarderDonnees('Mes donnees importantes...');
end.
