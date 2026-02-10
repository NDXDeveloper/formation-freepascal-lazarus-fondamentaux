{ ============================================================================
  Section 5.1 : Tableaux statiques unidimensionnels
  Description : Calcul de la moyenne des notes d'un tableau
  Fichier source : 01-tableaux-statiques-unidimensionnels.md
  ============================================================================ }
program CalculMoyenne;  
var  
  notes: array[1..5] of Integer;
  i, somme: Integer;
  moyenne: Real;
begin
  // Saisie des notes
  WriteLn('Entrez 5 notes :');
  for i := 1 to 5 do
  begin
    Write('Note ', i, ' : ');
    ReadLn(notes[i]);
  end;

  // Calcul de la somme
  somme := 0;
  for i := 1 to 5 do
  begin
    somme := somme + notes[i];
  end;

  // Calcul et affichage de la moyenne
  moyenne := somme / 5;
  WriteLn('La moyenne est : ', moyenne:0:2);
end.
