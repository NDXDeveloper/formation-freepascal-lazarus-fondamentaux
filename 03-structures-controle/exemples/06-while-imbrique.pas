{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : WHILE dans WHILE - affichage de groupes de nombres
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program WhileImbrique;  
var  
  i, j: Integer;
begin
  i := 1;

  while i <= 3 do
  begin
    WriteLn('Groupe ', i, ' :');

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
