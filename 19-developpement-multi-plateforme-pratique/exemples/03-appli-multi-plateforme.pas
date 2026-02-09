{ ============================================================================
  Section 19.3 : Directives de compilation conditionnelle
  Description : Application complete multi-plateforme avec directives
  Fichier source : 03-directives-compilation-conditionnelle.md
  ============================================================================ }
program AppliMultiPlateforme;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes;

// Definir la version
{$DEFINE VERSION_1_5}

// Configuration selon la plateforme
{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
const
  NomPlateforme = 'Windows';
  ExtensionExe = '.exe';
{$ENDIF}

{$IFDEF LINUX}
const
  NomPlateforme = 'Linux';
  ExtensionExe = '';
{$ENDIF}

{$IFDEF DARWIN}
const
  NomPlateforme = 'macOS';
  ExtensionExe = '';
{$ENDIF}

// Fonction pour obtenir le repertoire de configuration
function ObtenirDirConfig: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + 'MonApp';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.config' + PathDelim + 'MonApp';
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME') + PathDelim +
            'Library' + PathDelim + 'Application Support' + PathDelim + 'MonApp';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

// Fonction de log conditionnelle
procedure Log(const Message: string);
begin
  {$IFDEF DEBUG}
  WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', Message);
  {$ENDIF}
end;

// Programme principal
var
  DirConfig: string;
begin
  WriteLn('=================================');
  WriteLn('Application Multi-Plateforme');

  {$IFDEF VERSION_1_5}
  WriteLn('Version 1.5');
  {$ELSE}
  WriteLn('Version 1.0');
  {$ENDIF}

  WriteLn('=================================');
  WriteLn;

  WriteLn('Plateforme : ', NomPlateforme);

  {$IFDEF CPU64}
  WriteLn('Architecture : 64 bits');
  {$ELSE}
  WriteLn('Architecture : 32 bits');
  {$ENDIF}

  WriteLn;

  DirConfig := ObtenirDirConfig;
  WriteLn('Repertoire de configuration :');
  WriteLn('  ', DirConfig);

  Log('Programme initialise');

  Log('Programme termine');
end.
