{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Calcul de la somme par ligne d'un tableau 2D
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program SommeLignes;
var
  tableau: array[1..3, 1..4] of Integer;
  ligne, colonne, somme: Integer;
begin
  // Remplissage (exemple)
  for ligne := 1 to 3 do
    for colonne := 1 to 4 do
      tableau[ligne, colonne] := ligne + colonne;

  // Calcul de la somme de chaque ligne
  for ligne := 1 to 3 do
  begin
    somme := 0;
    for colonne := 1 to 4 do
      somme := somme + tableau[ligne, colonne];
    WriteLn('Somme de la ligne ', ligne, ' : ', somme);
  end;
end.
