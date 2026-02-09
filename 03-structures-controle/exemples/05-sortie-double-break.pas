{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Sortie de plusieurs boucles imbriquees avec un drapeau
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program SortieDoubleBreak;
var
  i, j: Integer;
  // En Pascal, Break ne sort que de la boucle la plus proche.
  // Un drapeau (flag) booléen permet de propager la sortie aux boucles externes.
  trouve: Boolean;
begin
  trouve := False;

  for i := 1 to 5 do
  begin
    for j := 1 to 5 do
    begin
      WriteLn('i=', i, ', j=', j);

      if (i = 3) and (j = 3) then
      begin
        trouve := True;
        break;  // Sort de la boucle J
      end;
    end;

    if trouve then
      break;  // Sort de la boucle I
  end;

  WriteLn('Fin des boucles');
end.
