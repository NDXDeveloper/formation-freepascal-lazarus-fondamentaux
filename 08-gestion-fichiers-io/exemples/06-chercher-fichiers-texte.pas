{ ============================================================================
  Section 8.6 : Manipulation de repertoires
  Description : Rechercher des fichiers par type (*.pas dans cet exemple)
  Fichier source : 06-manipulation-repertoires.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ChercherFichiersTexte;

uses
  SysUtils;

var
  Info: TSearchRec;
  Compteur: Integer;

begin
  Compteur := 0;

  WriteLn('Recherche de fichiers .pas...');

  if FindFirst('*.pas', faAnyFile, Info) = 0 then
  begin
    try
      repeat
        if (Info.Attr and faDirectory) = 0 then
        begin
          WriteLn('  ', Info.Name, ' (', Info.Size, ' octets)');
          Inc(Compteur);
        end;
      until FindNext(Info) <> 0;
    finally
      FindClose(Info);
    end;
  end;

  WriteLn;
  WriteLn('Total : ', Compteur, ' fichier(s) .pas trouvé(s)');
end.
