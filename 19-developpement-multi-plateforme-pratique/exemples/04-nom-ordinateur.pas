{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Obtenir le nom de l'ordinateur de maniere portable
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program NomOrdinateur;

{$mode objfpc}{$H+}

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

function ObtenirNomPC: string;
{$IFDEF WINDOWS}
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  if GetComputerName(@Buffer, Size) then
    Result := Buffer
  else
    Result := 'Inconnu';
  {$ELSE}
  Result := GetEnvironmentVariable('HOSTNAME');
  {$ENDIF}
end;

begin
  WriteLn('Nom de l''ordinateur : ', ObtenirNomPC);
end.
