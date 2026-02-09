{ ============================================================================
  Section 4.8 : Variables locales vs globales
  Description : Exemple simple de variable globale avec procedure et fonction
  Fichier source : 08-variables-locales-vs-globales.md
  ============================================================================ }
{$mode objfpc}{$H+}
program Exemple;

var
  variableGlobale: Integer;  // Variable globale

procedure MaProcedure;
begin
  variableGlobale := 10;  // Accessible ici
end;

function MaFonction: Integer;
begin
  Result := variableGlobale + 5;  // Accessible ici aussi
end;

begin
  variableGlobale := 100;  // Accessible dans le programme principal
  MaProcedure;
  WriteLn(MaFonction);
end.
