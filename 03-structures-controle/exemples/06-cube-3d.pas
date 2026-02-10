{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Boucles imbriquees a 3 niveaux - generation de coordonnees 3D
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program Cube3D;  
var  
  x, y, z: Integer;
begin
  WriteLn('Génération d''un cube 3D (coordonnées) :');
  WriteLn;

  for x := 1 to 3 do
  begin
    WriteLn('Couche X = ', x);

    for y := 1 to 3 do
    begin
      Write('  Ligne Y = ', y, ' : ');

      for z := 1 to 3 do
        Write('(', x, ',', y, ',', z, ') ');

      WriteLn;
    end;
    WriteLn;
  end;
end.
