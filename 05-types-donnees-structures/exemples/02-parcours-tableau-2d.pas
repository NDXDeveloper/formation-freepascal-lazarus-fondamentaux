{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Parcours d'un tableau 2D avec boucles imbriquees
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program ParcoursTableau2D;
var
  grille: array[1..3, 1..4] of Integer;
  ligne, colonne: Integer;
begin
  // Remplissage du tableau
  for ligne := 1 to 3 do
  begin
    for colonne := 1 to 4 do
    begin
      grille[ligne, colonne] := ligne * 10 + colonne;
    end;
  end;

  // Affichage du tableau
  WriteLn('Contenu de la grille :');
  for ligne := 1 to 3 do
  begin
    for colonne := 1 to 4 do
    begin
      Write(grille[ligne, colonne]:4);  // :4 pour aligner
    end;
    WriteLn;  // Retour à la ligne après chaque ligne
  end;
end.
