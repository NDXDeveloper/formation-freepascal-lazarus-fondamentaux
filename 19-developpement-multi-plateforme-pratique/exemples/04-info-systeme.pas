{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Obtenir des informations sur le systeme d'exploitation
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program InfoSysteme;

{$mode objfpc}{$H+}

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

procedure AfficherInfoSysteme;
{$IFDEF WINDOWS}
var
  VersionInfo: TOSVersionInfo;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(VersionInfo) then
  begin
    WriteLn('Windows Version : ', VersionInfo.dwMajorVersion, '.',
            VersionInfo.dwMinorVersion);
    WriteLn('Build : ', VersionInfo.dwBuildNumber);
  end;
  {$ELSE}
  WriteLn('Systeme Unix/Linux');
  {$ENDIF}
end;

begin
  AfficherInfoSysteme;
end.
