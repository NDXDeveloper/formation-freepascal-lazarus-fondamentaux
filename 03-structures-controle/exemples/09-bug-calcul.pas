{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Bug - division entiere perd la partie decimale (div au lieu
                de /)
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program BugCalcul;  
var  
  a, b, resultat: Integer;
begin
  a := 5;
  b := 2;
  resultat := a div b;  // BUG : division entière perd la partie décimale
  WriteLn('Résultat : ', resultat);
end.
