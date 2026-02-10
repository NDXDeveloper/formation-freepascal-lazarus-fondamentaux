{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Comptage de 1 a 10 avec une boucle for
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program AffichageNumeros;  
var  
  i: Integer;
begin
  WriteLn('Comptage de 1 à 10 :');
  for i := 1 to 10 do
    WriteLn(i);
end.
