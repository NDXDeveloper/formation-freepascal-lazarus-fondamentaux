{ ============================================================================
  Section 4.2 : Declaration et appel
  Description : Fonction simple avec Result et affectation
  Fichier source : 02-declaration-appel.md
  ============================================================================ }
{$mode objfpc}{$H+}
program ExempleFonction;

function ObtenirAge: Integer;  
begin  
  Result := 25;  // Result nécessite {$mode objfpc} ; sinon utiliser NomFonction := valeur
end;

var
  age: Integer;
begin
  age := ObtenirAge;  // Appel de la fonction
  WriteLn('L''âge est : ', age);
end.
