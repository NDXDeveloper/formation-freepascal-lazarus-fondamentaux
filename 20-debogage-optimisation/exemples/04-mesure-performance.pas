{ ============================================================================
  Section 20.4 : Profiling basique - identifier les goulots
  Description : Mesure du temps d'execution avec GetTickCount64
  Fichier source : 04-profiling-basique-identifier-goulots.md
  ============================================================================ }
program MesurePerformance;

{$mode objfpc}{$H+}

uses SysUtils;

procedure TraiterDonnees;
var
  i: Integer;
  total: Int64;
begin
  total := 0;
  for i := 1 to 10000000 do  // 10 millions d'iterations
    total := total + i;
end;

var
  debut, fin: QWord;
begin
  WriteLn('Demarrage du traitement...');

  debut := GetTickCount64;
  TraiterDonnees;
  fin := GetTickCount64;

  WriteLn('Traitement termine en ', fin - debut, ' ms');
end.
