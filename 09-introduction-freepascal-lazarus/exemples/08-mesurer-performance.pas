{ ============================================================================
  Section 9.8 : Compilation et exécution
  Description : Mesure du temps d'exécution avec MilliSecondsBetween
  Fichier source : 08-compilation-execution.md
  ============================================================================ }
program MesurerPerformance;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils;

var
  Debut, Fin: TDateTime;
  i: Integer;
begin
  Debut := Now;

  // Code à mesurer
  for i := 1 to 1000000 do
    ; // Opération vide

  Fin := Now;
  WriteLn('Temps écoulé : ', MilliSecondsBetween(Fin, Debut), ' ms');
end.
