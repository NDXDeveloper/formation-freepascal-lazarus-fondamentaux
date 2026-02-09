{ ============================================================================
  Section 19.4 : Unites specifiques a chaque plateforme
  Description : Liste des disques / points de montage selon la plateforme
  Fichier source : 04-unites-specifiques-plateforme.md
  ============================================================================ }
program ListeDisques;

{$mode objfpc}{$H+}

uses
  SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF};

procedure ListerDisques;
{$IFDEF WINDOWS}
var
  Drives: DWORD;
  i: Char;
  TypeDisque: UINT;
  NomType: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  WriteLn('Disques disponibles sous Windows :');
  Drives := GetLogicalDrives;

  for i := 'A' to 'Z' do
  begin
    if (Drives and (1 shl (Ord(i) - Ord('A')))) <> 0 then
    begin
      TypeDisque := GetDriveType(PChar(i + ':\'));

      case TypeDisque of
        DRIVE_REMOVABLE: NomType := 'Amovible';
        DRIVE_FIXED: NomType := 'Disque fixe';
        DRIVE_REMOTE: NomType := 'Reseau';
        DRIVE_CDROM: NomType := 'CD-ROM';
        DRIVE_RAMDISK: NomType := 'RAM Disk';
        else NomType := 'Inconnu';
      end;

      WriteLn('  ', i, ':\ - ', NomType);
    end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('Sous Unix/Linux, les disques sont montes dans l''arborescence');
  WriteLn('Points de montage courants :');
  WriteLn('  / (racine)');
  WriteLn('  /home (repertoires utilisateurs)');
  WriteLn('  /mnt ou /media (disques amovibles)');
  WriteLn('Consultez /etc/fstab ou utilisez la commande "mount"');
  {$ENDIF}
end;

begin
  ListerDisques;
end.
