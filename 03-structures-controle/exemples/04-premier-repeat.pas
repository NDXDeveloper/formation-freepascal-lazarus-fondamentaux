{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Premier exemple de boucle repeat-until, compteur de 1 a 5
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program PremierRepeat;  
var  
  compteur: Integer;
begin
  compteur := 1;
  repeat
    WriteLn('Compteur = ', compteur);
    compteur := compteur + 1;
  until compteur > 5;  { until = "arrete quand c'est vrai" (logique inversee par rapport a while) }
  WriteLn('Fin de la boucle');
end.
