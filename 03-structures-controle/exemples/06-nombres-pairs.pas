{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : IF dans boucle FOR - identification des nombres pairs et impairs
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program NombresPairs;
var
  i: Integer;
begin
  WriteLn('Nombres de 1 à 20 :');

  for i := 1 to 20 do
  begin
    Write(i:3);

    if (i mod 2) = 0 then
      Write(' (pair)')
    else
      Write(' (impair)');

    WriteLn;
  end;
end.
