{ ============================================================================
  Section 19.2 : Gestion portable des chemins
  Description : Parcours recursif d'un arbre de repertoires
  Fichier source : 02-gestion-portable-chemins.md
  ============================================================================ }
program ParcoursRepertoire;

{$mode objfpc}{$H+}

uses
  SysUtils;

procedure ParcoururRepertoire(const Repertoire: string; Niveau: Integer = 0);  
var  
  Info: TSearchRec;
  Chemin: string;
  Indentation: string;
begin
  Indentation := StringOfChar(' ', Niveau * 2);
  Chemin := IncludeTrailingPathDelimiter(Repertoire);

  // Chercher tous les fichiers et repertoires
  if FindFirst(Chemin + '*', faAnyFile, Info) = 0 then
  begin
    repeat
      // Ignorer . et ..
      if (Info.Name <> '.') and (Info.Name <> '..') then
      begin
        if (Info.Attr and faDirectory) = faDirectory then
        begin
          // C'est un repertoire
          WriteLn(Indentation, '[DIR] ', Info.Name);

          // Appel recursif
          ParcoururRepertoire(Chemin + Info.Name, Niveau + 1);
        end
        else
        begin
          // C'est un fichier
          WriteLn(Indentation, Info.Name);
        end;
      end;
    until FindNext(Info) <> 0;

    FindClose(Info);
  end;
end;

begin
  WriteLn('Parcours du repertoire courant :');
  WriteLn('================================');
  ParcoururRepertoire(GetCurrentDir);
end.
