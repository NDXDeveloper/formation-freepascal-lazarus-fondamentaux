{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Table de multiplication par 7
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program TableMultiplication;
var
  i: Integer;
begin
  WriteLn('Table de multiplication par 7 :');
  WriteLn;
  for i := 1 to 10 do
    WriteLn('7 x ', i, ' = ', 7 * i);
end.
