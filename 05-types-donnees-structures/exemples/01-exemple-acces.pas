{ ============================================================================
  Section 5.1 : Tableaux statiques unidimensionnels
  Description : Acces aux elements d'un tableau (affectation, lecture, modification)
  Fichier source : 01-tableaux-statiques-unidimensionnels.md
  ============================================================================ }
program ExempleAcces;
var
  notes: array[1..5] of Integer;
begin
  // Affectation de valeurs
  notes[1] := 15;
  notes[2] := 12;
  notes[3] := 18;
  notes[4] := 10;
  notes[5] := 16;

  // Lecture de valeurs
  WriteLn('La première note est : ', notes[1]);
  WriteLn('La troisième note est : ', notes[3]);

  // Modification d'une valeur
  notes[2] := notes[2] + 3;  // On augmente la deuxième note de 3
  WriteLn('La nouvelle deuxième note : ', notes[2]);
end.
