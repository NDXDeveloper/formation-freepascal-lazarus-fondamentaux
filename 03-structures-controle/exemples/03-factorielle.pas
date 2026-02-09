{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Calcul de la factorielle d'un nombre saisi par l'utilisateur
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program FactorielleFor;
var
  n, i: Integer;
  resultat: Int64;
begin
  Write('Entrez un nombre : ');
  ReadLn(n);
  resultat := 1;
  for i := 1 to n do
    resultat := resultat * i;
  WriteLn('La factorielle de ', n, ' est : ', resultat);
end.
