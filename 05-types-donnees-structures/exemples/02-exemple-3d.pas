{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Tableau 3D - remplissage et affichage par couche
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program Exemple3D;
var
  cube: array[1..2, 1..2, 1..2] of Integer;
  x, y, z, compteur: Integer;
begin
  // Remplissage
  compteur := 1;
  for x := 1 to 2 do
    for y := 1 to 2 do
      for z := 1 to 2 do
      begin
        cube[x, y, z] := compteur;
        compteur := compteur + 1;
      end;

  // Affichage par "couche"
  for x := 1 to 2 do
  begin
    WriteLn('Couche ', x, ' :');
    for y := 1 to 2 do
    begin
      for z := 1 to 2 do
        Write(cube[x, y, z]:3);
      WriteLn;
    end;
    WriteLn;
  end;
end.
