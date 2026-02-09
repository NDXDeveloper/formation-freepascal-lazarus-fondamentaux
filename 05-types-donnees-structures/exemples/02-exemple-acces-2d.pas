{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Acces aux elements d'un tableau 2D (affectation, lecture, modification)
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program ExempleAcces2D;
var
  grille: array[1..3, 1..4] of Integer;
begin
  // Affectation
  grille[1, 1] := 10;    // Ligne 1, Colonne 1
  grille[1, 2] := 20;    // Ligne 1, Colonne 2
  grille[2, 3] := 30;    // Ligne 2, Colonne 3

  // Lecture
  WriteLn('Valeur en [1,1] : ', grille[1, 1]);
  WriteLn('Valeur en [2,3] : ', grille[2, 3]);

  // Modification
  grille[1, 1] := grille[1, 1] * 2;
  WriteLn('Nouvelle valeur en [1,1] : ', grille[1, 1]);
end.
