{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Table de multiplication 5x5 avec boucles imbriquees
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program TableMultiplicationComplete;  
var  
  i, j, resultat: Integer;
begin
  WriteLn('TABLE DE MULTIPLICATION (1 à 5)');
  WriteLn;
  for i := 1 to 5 do
  begin
    for j := 1 to 5 do
    begin
      resultat := i * j;
      Write(resultat:4);   { :4 = affiche sur 4 caracteres, aligne a droite }
    end;
    WriteLn;
  end;
end.
