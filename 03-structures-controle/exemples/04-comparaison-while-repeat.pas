{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Comparaison while (jamais execute) vs repeat (execute une fois)
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program ComparaisonWhileRepeat;  
var  
  i: Integer;
begin
  WriteLn('=== Avec WHILE ===');
  i := 10;
  while i < 5 do
  begin
    WriteLn('WHILE : i = ', i);
    i := i + 1;
  end;
  WriteLn('WHILE : Aucune exécution car condition fausse au départ');
  WriteLn;
  WriteLn('=== Avec REPEAT ===');
  i := 10;
  repeat
    WriteLn('REPEAT : i = ', i);
    i := i + 1;
  until i >= 5;
  WriteLn('REPEAT : Exécuté une fois malgré la condition vraie');
end.
