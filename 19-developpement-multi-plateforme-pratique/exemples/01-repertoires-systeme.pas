{
  Section 19.1 - Différences fondamentales Windows/Linux
  Description : Affiche les répertoires système de manière portable
                (home, temp, config, répertoire courant) avec IFDEF
  Fichier source : 01-differences-windows-linux.md
}
program RepertoiresSysteme;

{$mode objfpc}{$H+}

uses
  SysUtils;

{ Fonction portable pour obtenir le répertoire de configuration }
function GetConfigDir: string;  
begin  
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + 'MonApp' + PathDelim;
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + 'MonApp' + PathDelim;
  {$ENDIF}
end;

var
  RepUtilisateur: string;
begin
  WriteLn('=== Répertoires système (portable) ===');
  WriteLn;

  { Section 5 - Répertoire utilisateur }
  WriteLn('--- Répertoire utilisateur ---');
  {$IFDEF WINDOWS}
  RepUtilisateur := GetEnvironmentVariable('USERPROFILE');
  WriteLn('Source : %USERPROFILE%');
  {$ENDIF}

  {$IFDEF LINUX}
  RepUtilisateur := GetEnvironmentVariable('HOME');
  WriteLn('Source : $HOME');
  {$ENDIF}

  WriteLn('Valeur : ', RepUtilisateur);
  WriteLn;

  { Section 7 - Répertoire temporaire }
  WriteLn('--- Répertoire temporaire ---');
  WriteLn('GetTempDir : ', GetTempDir);
  WriteLn;

  { Section 8 - Répertoire de configuration }
  WriteLn('--- Répertoire de configuration ---');
  WriteLn('GetConfigDir (MonApp) : ', GetConfigDir);
  WriteLn;

  { Répertoire courant }
  WriteLn('--- Répertoire courant ---');
  WriteLn('GetCurrentDir : ', GetCurrentDir);
end.
