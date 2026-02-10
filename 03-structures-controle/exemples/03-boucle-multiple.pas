{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Instructions multiples dans une boucle for avec begin-end
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program BoucleMultiple;  
var  
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    WriteLn('--- Itération numéro ', i, ' ---');
    WriteLn('Le double de ', i, ' est ', i * 2);
    WriteLn('Le triple de ', i, ' est ', i * 3);
    WriteLn;
  end;
end.
