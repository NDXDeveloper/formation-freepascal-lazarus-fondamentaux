{ ============================================================================
  Section 20.4 : Profiling basique - identifier les goulots
  Description : Comparaison de deux methodes de calcul (boucle vs formule)
  Fichier source : 04-profiling-basique-identifier-goulots.md
  ============================================================================ }
program ComparaisonAlgorithmes;

{$mode objfpc}{$H+}

uses SysUtils;

function MethodeA: Int64;  
var  
  i: Integer;
  total: Int64;
begin
  total := 0;
  for i := 1 to 1000000 do
    total := total + i;
  Result := total;
end;

function MethodeB: Int64;  
var  
  n: Int64;
begin
  n := 1000000;
  Result := (n * (n + 1)) div 2;  // Formule mathematique
end;

var
  debut, fin: QWord;
  resultat: Int64;
begin
  // Test Methode A
  debut := GetTickCount64;
  resultat := MethodeA;
  fin := GetTickCount64;
  WriteLn('Methode A : ', fin - debut, ' ms (Resultat: ', resultat, ')');

  // Test Methode B
  debut := GetTickCount64;
  resultat := MethodeB;
  fin := GetTickCount64;
  WriteLn('Methode B : ', fin - debut, ' ms (Resultat: ', resultat, ')');
end.
