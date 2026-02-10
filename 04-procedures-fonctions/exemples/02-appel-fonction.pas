{ ============================================================================
  Section 4.2 : Declaration et appel
  Description : Appel de fonction dans une affectation
  Fichier source : 02-declaration-appel.md
  ============================================================================ }
{$mode objfpc}{$H+}
program AppelFonction;

function ObtenirNombre: Integer;  
begin  
  Result := 100;
end;

var
  x: Integer;
begin
  x := ObtenirNombre;  // La valeur 100 est affectée à x
  WriteLn('x vaut : ', x);
end.
