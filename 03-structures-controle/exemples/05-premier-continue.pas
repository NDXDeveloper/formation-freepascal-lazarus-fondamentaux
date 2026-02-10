{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Premier exemple de continue avec une boucle for
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program PremierContinue;  
var  
  i: Integer;
begin
  WriteLn('Nombres de 1 à 10, sauf les multiples de 3 :');

  for i := 1 to 10 do
  begin
    if (i mod 3) = 0 then
      continue;  // Saute les multiples de 3

    WriteLn(i);
  end;
end.
