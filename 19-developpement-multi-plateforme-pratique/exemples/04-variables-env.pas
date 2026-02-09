{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Acces aux variables d'environnement et repertoire temporaire
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program VariablesEnv;

{$mode objfpc}{$H+}

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
  // Methode Windows native
  GetTempPath(MAX_PATH, @Buffer);
  Result := Buffer;
  {$ELSE}
  // Methode portable (fonctionne aussi sous Windows !)
  Result := GetTempDir;  // SysUtils
  {$ENDIF}
end;

begin
  WriteLn('Repertoire temporaire : ', ObtenirTempDir);
end.
