{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Jeu du Morpion - affichage d'une grille 3x3
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program Morpion;  
var  
  grille: array[1..3, 1..3] of Char;
  ligne, colonne: Integer;
begin
  // Initialisation de la grille
  for ligne := 1 to 3 do
    for colonne := 1 to 3 do
      grille[ligne, colonne] := '.';

  // Placement de quelques symboles
  grille[1, 1] := 'X';
  grille[1, 2] := 'O';
  grille[2, 2] := 'X';
  grille[3, 3] := 'O';

  // Affichage de la grille
  WriteLn('Grille de jeu :');
  WriteLn('  1 2 3');
  for ligne := 1 to 3 do
  begin
    Write(ligne, ' ');
    for colonne := 1 to 3 do
      Write(grille[ligne, colonne], ' ');
    WriteLn;
  end;
end.
