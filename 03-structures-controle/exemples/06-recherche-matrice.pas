{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : BREAK dans boucles imbriquees - recherche d'un element dans une matrice
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program RechercheMatrice;
const
  LIGNES = 5;
  COLONNES = 5;
var
  matrice: array[1..LIGNES, 1..COLONNES] of Integer;
  i, j, recherche: Integer;
  trouve: Boolean;
begin
  // Remplissage
  Randomize;
  for i := 1 to LIGNES do
    for j := 1 to COLONNES do
      matrice[i, j] := Random(50) + 1;

  // Affichage
  WriteLn('Matrice :');
  for i := 1 to LIGNES do
  begin
    for j := 1 to COLONNES do
      Write(matrice[i, j]:4);
    WriteLn;
  end;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  // Recherche avec break
  trouve := False;
  for i := 1 to LIGNES do
  begin
    for j := 1 to COLONNES do
    begin
      if matrice[i, j] = recherche then
      begin
        WriteLn('Trouvé à la position [', i, ',', j, ']');
        trouve := True;
        break;  // Sort de la boucle interne
      end;
    end;

    if trouve then
      break;  // Sort de la boucle externe
  end;

  if not trouve then
    WriteLn('Non trouvé');
end.
