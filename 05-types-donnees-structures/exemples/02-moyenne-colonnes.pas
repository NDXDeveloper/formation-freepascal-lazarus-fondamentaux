{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Calcul de la moyenne par colonne (matiere) d'un tableau 2D
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program MoyenneColonnes;  
var  
  notes: array[1..5, 1..3] of Real;  // 5 élèves, 3 matières
  eleve, matiere: Integer;
  somme, moyenne: Real;
begin
  // Supposons que les notes sont déjà saisies...

  // Calcul de la moyenne de chaque matière
  WriteLn('Moyennes par matière :');
  for matiere := 1 to 3 do
  begin
    somme := 0;
    for eleve := 1 to 5 do
      somme := somme + notes[eleve, matiere];
    moyenne := somme / 5;
    WriteLn('Matière ', matiere, ' : ', moyenne:0:2);
  end;
end.
