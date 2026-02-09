{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Bug - boucle infinie (oubli d'incrementer i)
  Fichier source : 09-debogage-pas-a-pas.md
  NOTE : Boucle infinie intentionnelle - ne peut pas etre teste tel quel.
  ============================================================================ }
program BugBoucleInfinie;
var
  i: Integer;
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn(i);
    // BUG : oubli d'incrémenter i
  end;
end.
