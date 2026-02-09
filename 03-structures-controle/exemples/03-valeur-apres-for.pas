{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Demonstration de la valeur indefinie du compteur apres la boucle
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program ValeurApresFor;
var
  i: Integer;
begin
  for i := 1 to 5 do
    WriteLn(i);
  { Attention : la valeur de i apres la boucle est indefinie selon la norme }
  WriteLn('Après la boucle, i vaut : ', i);
end.
