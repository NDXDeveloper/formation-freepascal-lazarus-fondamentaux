{ ============================================================================
  Section 8.7 : Chemins et noms de fichiers
  Description : Validation d'un nom de fichier (caracteres interdits, noms reserves)
  Fichier source : 07-chemins-noms-fichiers.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ValiderNomFichier;

uses
  SysUtils;

function NomFichierValide(Nom: string): Boolean;  
const  
  CaracteresInterdits = '\/:*?"<>|';
var
  i: Integer;
begin
  Result := False;

  if Nom = '' then
    Exit;

  // Verifier les caracteres interdits
  for i := 1 to Length(CaracteresInterdits) do
  begin
    if Pos(CaracteresInterdits[i], Nom) > 0 then
      Exit;
  end;

  // Verifier les noms reserves Windows
  if (UpperCase(Nom) = 'CON') or
     (UpperCase(Nom) = 'PRN') or
     (UpperCase(Nom) = 'AUX') or
     (UpperCase(Nom) = 'NUL') then
    Exit;

  Result := True;
end;

begin
  WriteLn('test.txt valide ? ', NomFichierValide('test.txt'));
  WriteLn('test*.txt valide ? ', NomFichierValide('test*.txt'));
  WriteLn('CON valide ? ', NomFichierValide('CON'));
end.
