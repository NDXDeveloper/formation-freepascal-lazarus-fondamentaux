{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Premier exemple de boucle for, affiche "Bonjour !" 5 fois
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program PremierFor;  
var  
  i: Integer;
begin
  for i := 1 to 5 do
    WriteLn('Bonjour !');
end.
