{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Comparaison entre to (croissant) et downto (decroissant)
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program ComparaisonToDownto;  
var  
  i: Integer;
begin
  WriteLn('Avec TO (croissant) :');
  for i := 1 to 5 do
    Write(i, ' ');
  WriteLn;
  WriteLn;
  WriteLn('Avec DOWNTO (décroissant) :');
  for i := 5 downto 1 do
    Write(i, ' ');
  WriteLn;
end.
