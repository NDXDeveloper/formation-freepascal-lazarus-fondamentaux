{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Bug - variable non initialisee (somme a une valeur aleatoire)
  Fichier source : 09-debogage-pas-a-pas.md
  NOTE : Comportement indefini - la variable somme n'est pas initialisee.
  ============================================================================ }
program BugNonInitialisee;
var
  somme, i: Integer;
begin
  // BUG : somme n'est pas initialisée
  for i := 1 to 10 do
    somme := somme + i;  // somme a une valeur aléatoire au départ

  WriteLn('Somme : ', somme);
end.
