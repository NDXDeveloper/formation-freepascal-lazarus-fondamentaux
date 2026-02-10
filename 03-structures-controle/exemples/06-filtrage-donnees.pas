{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : CONTINUE avec imbrication - filtrage des multiples de 5
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program FiltrageDonnees;  
var  
  i, j, valeur: Integer;
begin
  WriteLn('Tableau avec filtrage :');
  WriteLn;

  for i := 1 to 5 do
  begin
    Write('Ligne ', i, ' : ');

    for j := 1 to 10 do
    begin
      valeur := i * j;

      // Ignorer les multiples de 5
      if (valeur mod 5) = 0 then
        continue;

      Write(valeur:4);
    end;

    WriteLn;
  end;
end.
