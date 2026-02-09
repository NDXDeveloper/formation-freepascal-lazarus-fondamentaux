{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Verification des permissions de fichiers sous Unix
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program PermissionsFichier;

{$mode objfpc}{$H+}

uses
  SysUtils
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

procedure VerifierPermissions(const Fichier: string);
{$IFDEF UNIX}
var
  StatInfo: TStat;
{$ENDIF}
begin
  {$IFDEF UNIX}
  if FpStat(Fichier, StatInfo) = 0 then
  begin
    WriteLn('Permissions du fichier : ', Fichier);

    // Lecture
    if (StatInfo.st_mode and S_IRUSR) <> 0 then
      WriteLn('  Proprietaire : Lecture OK');

    // Ecriture
    if (StatInfo.st_mode and S_IWUSR) <> 0 then
      WriteLn('  Proprietaire : Ecriture OK');

    // Execution
    if (StatInfo.st_mode and S_IXUSR) <> 0 then
      WriteLn('  Proprietaire : Execution OK');
  end
  else
    WriteLn('Impossible de lire les informations du fichier');
  {$ELSE}
  WriteLn('Verification des permissions Unix uniquement');
  WriteLn('Sous Windows, utilisez FileGetAttr()');
  {$ENDIF}
end;

begin
  VerifierPermissions('/etc/hosts');
end.
