{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Utilisation de Low() et High() avec tableaux multidimensionnels
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program LowHighMultiDim;  
var  
  tableau: array[1..3, 5..8] of Integer;
  i, j: Integer;
begin
  WriteLn('Lignes : de ', Low(tableau), ' à ', High(tableau));
  WriteLn('Colonnes : de ', Low(tableau[1]), ' à ', High(tableau[1]));

  // Parcours flexible
  for i := Low(tableau) to High(tableau) do
    for j := Low(tableau[i]) to High(tableau[i]) do
      tableau[i, j] := i * j;
end.
