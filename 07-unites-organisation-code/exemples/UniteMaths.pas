{ ============================================================================
  Section 7.3 : Clauses Uses et dependances
  Description : Unite de base avec fonction Carre (chaine de dependances)
  Fichier source : 03-clauses-uses-dependances.md
  ============================================================================ }
{$mode objfpc}{$H+}
unit UniteMaths;
interface
  function Carre(x: Integer): Integer;
implementation
  function Carre(x: Integer): Integer;
  begin
    Result := x * x;
  end;
end.
