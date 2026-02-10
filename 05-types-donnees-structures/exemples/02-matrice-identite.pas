{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Creation et affichage d'une matrice identite 3x3
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program MatriceIdentite;  
var  
  matrice: array[1..3, 1..3] of Integer;
  i, j: Integer;
begin
  // Créer une matrice identité (1 sur la diagonale, 0 ailleurs)
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    begin
      if i = j then
        matrice[i, j] := 1
      else
        matrice[i, j] := 0;
    end;
  end;

  // Affichage
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
      Write(matrice[i, j]:3);
    WriteLn;
  end;
end.
