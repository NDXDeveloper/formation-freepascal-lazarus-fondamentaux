{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Continue dans des boucles imbriquees (saute dans la boucle interne)
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program ContinueImbrique;  
var  
  i, j: Integer;
begin
  WriteLn('Boucles imbriquées avec continue :');

  for i := 1 to 3 do
  begin
    WriteLn('Ligne ', i, ' :');

    for j := 1 to 5 do
    begin
      if j = 3 then
        continue;  // Saute le 3 dans la boucle J
      Write(j, ' ');
    end;

    WriteLn;
  end;
end.
