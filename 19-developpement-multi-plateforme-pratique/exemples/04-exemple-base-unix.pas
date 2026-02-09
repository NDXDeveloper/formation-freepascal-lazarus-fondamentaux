{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Appels systeme Unix de base avec BaseUnix
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program ExempleBaseUnix;

{$mode objfpc}{$H+}

uses
  SysUtils
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

procedure AfficherProcessID;
{$IFDEF UNIX}
var
  PID: TPid;
{$ENDIF}
begin
  {$IFDEF UNIX}
  PID := FpGetPid;
  WriteLn('ID du processus : ', PID);

  // ID de l'utilisateur
  WriteLn('User ID : ', FpGetUid);
  WriteLn('Group ID : ', FpGetGid);
  {$ELSE}
  WriteLn('Fonctionnalite Unix uniquement');
  {$ENDIF}
end;

begin
  AfficherProcessID;
end.
