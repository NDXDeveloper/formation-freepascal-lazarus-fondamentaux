{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Unite wrapper pour fonctions portables (nom utilisateur, PID)
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
unit PlatformUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

// Fonction portable pour obtenir le nom d'utilisateur
function ObtenirNomUtilisateur: string;

// Fonction portable pour obtenir l'ID du processus
function ObtenirProcessID: Integer;

implementation

function ObtenirNomUtilisateur: string;
{$IFDEF WINDOWS}
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Size := 256;
  if GetUserName(@Buffer, Size) then
    Result := Buffer
  else
    Result := 'Inconnu';
  {$ELSE}
  // Unix/Linux
  Result := GetEnvironmentVariable('USER');
  if Result = '' then
    Result := GetEnvironmentVariable('USERNAME');
  {$ENDIF}
end;

function ObtenirProcessID: Integer;
begin
  {$IFDEF WINDOWS}
  Result := GetCurrentProcessId;
  {$ELSE}
  {$IFDEF UNIX}
  Result := FpGetPid;
  {$ELSE}
  Result := -1;  // Plateforme non supportee
  {$ENDIF}
  {$ENDIF}
end;

end.
