{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Boucle for avec une expression comme borne superieure
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program BoucleExpression;
var
  i, n: Integer;
begin
  Write('Entrez un nombre : ');
  ReadLn(n);
  for i := 1 to (n * 2) do
    WriteLn(i);
end.
