{ ============================================================================
  Section 19.6 : Cross-compilation : theorie et pratique
  Description : Gestionnaire de fichiers multi-plateforme
  Fichier source : 06-cross-compilation-theorie-pratique.md
  ============================================================================ }
program FileManager;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

procedure ListFiles(const Directory: string);
var
  SearchRec: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(Directory);

  if FindFirst(Path + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        WriteLn(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

begin
  WriteLn('=== File Manager ===');
  WriteLn('Platform: ', {$I %FPCTARGETOS%});
  WriteLn;

  WriteLn('Files in current directory:');
  ListFiles(GetCurrentDir);
end.
