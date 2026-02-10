{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Boucles while imbriquees, 3 lignes de nombres 1 2 3 4
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program WhileImbriques;  
var  
  i, j: Integer;
begin
  i := 1;
  while i <= 3 do
  begin
    WriteLn('Ligne ', i, ' :');
    j := 1;
    while j <= 4 do
    begin
      Write(j, ' ');
      j := j + 1;
    end;
    WriteLn;
    i := i + 1;
  end;
end.
