{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Matrice dynamique 2D avec array of array of
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program MatriceDynamique;  
type  
  TMatrice = array of array of Integer;

var
  matrice: TMatrice;
  lignes, colonnes, i, j: Integer;
begin
  lignes := 3;
  colonnes := 4;

  // Allocation
  SetLength(matrice, lignes, colonnes);  // 3 lignes × 4 colonnes

  // Initialisation
  for i := 0 to lignes-1 do
    for j := 0 to colonnes-1 do
      matrice[i][j] := i * 10 + j;

  // Affichage
  for i := 0 to lignes-1 do
  begin
    for j := 0 to colonnes-1 do
      Write(matrice[i][j]:4);
    WriteLn;
  end;

  // Libération automatique en fin de scope
end.
