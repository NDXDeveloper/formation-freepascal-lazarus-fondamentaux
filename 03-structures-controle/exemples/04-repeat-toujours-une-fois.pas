{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Repeat s'execute au moins une fois meme si la condition est vraie
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program RepeatToujoursUneFois;  
var  
  i: Integer;
begin
  i := 10;
  { repeat execute le corps AVANT de tester la condition :
    le message s'affiche au moins une fois, meme si i > 5 des le depart }
  repeat
    WriteLn('Ce message s''affiche quand même !');
    WriteLn('i = ', i);
  until i > 5;
  WriteLn('Fin de la boucle');
end.
