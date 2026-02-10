{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Bug - indice de tableau incorrect (commence a 0 au lieu de 1)
  Fichier source : 09-debogage-pas-a-pas.md
  NOTE : Erreur intentionnelle - acces hors limites du tableau.
  ============================================================================ }
program BugIndice;  
var  
  tableau: array[1..5] of Integer;
  i: Integer;
begin
  for i := 0 to 5 do  // BUG : commence à 0 au lieu de 1
    tableau[i] := i * 10;
end.
