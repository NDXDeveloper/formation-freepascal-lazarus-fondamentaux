{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Placement d'un point d'arret (breakpoint) sur une ligne
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program ExempleBreakpoint;  
var  
  a, b, resultat: Integer;
begin
  a := 10;              // <- Placer un breakpoint ici
  b := 5;
  resultat := a + b;
  WriteLn('Résultat : ', resultat);
end.
