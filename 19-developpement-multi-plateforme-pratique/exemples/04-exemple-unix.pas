{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Fonctions Unix etendues - lister les variables d'environnement
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program ExempleUnix;

{$mode objfpc}{$H+}

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
