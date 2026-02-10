{ ============================================================================
  Section 5.1 : Tableaux statiques unidimensionnels
  Description : Parcourir un tableau avec une boucle (saisie et affichage)
  Fichier source : 01-tableaux-statiques-unidimensionnels.md
  ============================================================================ }
program ParcoursTableau;  
var  
  notes: array[1..5] of Integer;
  i: Integer;
begin
  // Saisie des notes
  for i := 1 to 5 do
  begin
    Write('Entrez la note ', i, ' : ');
    ReadLn(notes[i]);
  end;

  // Affichage des notes
  WriteLn;
  WriteLn('Vos notes :');
  for i := 1 to 5 do
  begin
    WriteLn('Note ', i, ' : ', notes[i]);
  end;
end.
