{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Premier exemple de boucle while, compteur de 1 a 5
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program PremierWhile;  
var  
  compteur: Integer;
begin
  compteur := 1;
  { while verifie la condition AVANT chaque iteration :
    si elle est fausse des le depart, le corps n'est jamais execute }
  while compteur <= 5 do
  begin
    WriteLn('Compteur = ', compteur);
    compteur := compteur + 1;
  end;
  WriteLn('Fin de la boucle');
end.
