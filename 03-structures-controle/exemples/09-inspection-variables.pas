{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Inspection des variables locales pendant le debogage
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program InspectionVariables;
var
  a, b, somme: Integer;
  moyenne: Real;
begin
  a := 10;              // Point d'arrêt ici
  b := 20;
  somme := a + b;
  moyenne := somme / 2;
  WriteLn('Moyenne : ', moyenne:0:2);
end.
