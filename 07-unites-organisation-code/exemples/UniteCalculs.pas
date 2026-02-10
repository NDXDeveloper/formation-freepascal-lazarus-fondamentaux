{ ============================================================================
  Section 7.3 : Clauses Uses et dependances
  Description : Unite utilisant UniteMaths (chaine de dependances)
  Fichier source : 03-clauses-uses-dependances.md
  ============================================================================ }
{$mode objfpc}{$H+}
unit UniteCalculs;  
interface  
uses  
  UniteMaths;

function SommesCarres(a, b: Integer): Integer;

implementation  
function SommesCarres(a, b: Integer): Integer;  
begin  
  Result := Carre(a) + Carre(b);
end;  
end.  
