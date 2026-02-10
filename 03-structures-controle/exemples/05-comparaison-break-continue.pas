{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Comparaison du comportement de break et continue
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program ComparaisonBreakContinue;  
var  
  i: Integer;
begin
  WriteLn('=== Avec BREAK ===');
  for i := 1 to 10 do
  begin
    if i = 5 then
      break;  // Arrête tout
    WriteLn(i);
  end;
  WriteLn('Boucle terminée');
  WriteLn;

  WriteLn('=== Avec CONTINUE ===');
  for i := 1 to 10 do
  begin
    if i = 5 then
      continue;  // Saute juste le 5
    WriteLn(i);
  end;
  WriteLn('Boucle terminée');
end.
