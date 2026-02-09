{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Premier exemple de break avec une boucle for
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program PremierBreak;
var
  i: Integer;
begin
  WriteLn('Comptage de 1 à 10, mais arrêt à 5 :');

  for i := 1 to 10 do
  begin
    if i = 5 then
    begin
      WriteLn('Arrêt à ', i);
      break;  // Sort de la boucle immédiatement
    end;
    WriteLn(i);
  end;

  WriteLn('Après la boucle');
end.
