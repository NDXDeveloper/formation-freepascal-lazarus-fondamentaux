{ ============================================================================
  Section 8.7 : Chemins et noms de fichiers
  Description : Construction portable de chemins avec une fonction personnalisee
  Fichier source : 07-chemins-noms-fichiers.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ConstruireCheminPortable;

uses
  SysUtils;

function ConstruireChemin(const Segments: array of string): string;  
var  
  i: Integer;
begin
  Result := '';

  for i := Low(Segments) to High(Segments) do
  begin
    if i = Low(Segments) then
      Result := Segments[i]
    else
      Result := IncludeTrailingPathDelimiter(Result) + Segments[i];
  end;
end;

var
  Chemin: string;

begin
  Chemin := ConstruireChemin(['Projet', 'Source', 'Units', 'Database.pas']);
  WriteLn(Chemin);

  Chemin := 'Data' + PathDelim + 'Config' + PathDelim + 'settings.ini';
  WriteLn(Chemin);
end.
