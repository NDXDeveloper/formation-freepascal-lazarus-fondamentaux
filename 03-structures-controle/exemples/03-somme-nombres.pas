{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Calcul de la somme des nombres de 1 a 100
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program SommeNombres;
var
  i, somme: Integer;
begin
  somme := 0;
  for i := 1 to 100 do
    somme := somme + i;
  WriteLn('La somme des nombres de 1 à 100 est : ', somme);
end.
