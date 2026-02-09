{ ============================================================================
  Section 8.6 : Manipulation de repertoires
  Description : Lister le contenu d'un repertoire avec FindFirst/FindNext
  Fichier source : 06-manipulation-repertoires.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ListerFichiers;

uses
  SysUtils;

var
  Info: TSearchRec;
  Resultat: Integer;

begin
  WriteLn('Contenu du répertoire courant :');
  WriteLn('--------------------------------');

  // Rechercher tous les fichiers et repertoires
  Resultat := FindFirst('*', faAnyFile, Info);

  try
    while Resultat = 0 do
    begin
      // Ignorer . et ..
      if (Info.Name <> '.') and (Info.Name <> '..') then
      begin
        if (Info.Attr and faDirectory) = faDirectory then  // and bitwise : teste si le bit répertoire est présent dans Attr
          WriteLn('[DIR]  ', Info.Name)
        else
          WriteLn('[FILE] ', Info.Name, ' (', Info.Size, ' octets)');
      end;

      Resultat := FindNext(Info);
    end;
  finally
    FindClose(Info);  // Toujours fermer !
  end;

  WriteLn('--------------------------------');
end.
