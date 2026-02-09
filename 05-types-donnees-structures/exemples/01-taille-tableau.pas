{ ============================================================================
  Section 5.1 : Tableaux statiques unidimensionnels
  Description : Utilisation de Low() et High() pour connaitre les limites d'un tableau
  Fichier source : 01-tableaux-statiques-unidimensionnels.md
  ============================================================================ }
program TailleTableau;
var
  notes: array[1..5] of Integer;
  i: Integer;
begin
  WriteLn('Premier indice : ', Low(notes));   // Affiche 1
  WriteLn('Dernier indice : ', High(notes));  // Affiche 5
  WriteLn('Nombre d''éléments : ', High(notes) - Low(notes) + 1);

  // Parcours avec Low et High (plus flexible)
  for i := Low(notes) to High(notes) do
  begin
    notes[i] := i * 10;
  end;
end.
