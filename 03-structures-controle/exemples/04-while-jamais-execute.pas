{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Boucle while qui ne s'execute jamais car condition fausse
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program WhileJamaisExecute;  
var  
  i: Integer;
begin
  i := 10;
  while i < 5 do
  begin
    WriteLn('Ce message ne s''affichera jamais !');
    i := i + 1;
  end;
  WriteLn('i vaut toujours ', i);
end.
