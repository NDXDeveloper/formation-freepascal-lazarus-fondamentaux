{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Break dans des boucles imbriquees (sort de la boucle interne)
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program BreakImbrique;
var
  i, j: Integer;
begin
  WriteLn('Boucles imbriquées avec break :');

  for i := 1 to 3 do
  begin
    WriteLn('Ligne ', i, ' :');

    for j := 1 to 5 do
    begin
      if j = 4 then
        break;  // Sort seulement de la boucle J
      Write(j, ' ');
    end;

    WriteLn;  // Ceci s'exécute car on sort juste de la boucle J
  end;
end.
