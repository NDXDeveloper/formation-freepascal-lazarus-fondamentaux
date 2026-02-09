{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Debogage d'une boucle de recherche dans un tableau
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program DebogageBoucle;
const
  TAILLE = 5;
var
  tableau: array[1..TAILLE] of Integer;
  i, recherche: Integer;
  trouve: Boolean;
begin
  // Initialisation
  tableau[1] := 10;
  tableau[2] := 20;
  tableau[3] := 30;
  tableau[4] := 40;
  tableau[5] := 50;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  trouve := False;
  i := 1;

  // Point d'arrêt ici
  while (i <= TAILLE) and (not trouve) do
  begin
    if tableau[i] = recherche then
      trouve := True
    else
      i := i + 1;
  end;

  if trouve then
    WriteLn('Trouvé à l''indice ', i)
  else
    WriteLn('Non trouvé');
end.
