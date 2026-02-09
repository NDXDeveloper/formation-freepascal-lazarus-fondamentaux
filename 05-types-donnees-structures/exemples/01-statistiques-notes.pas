{ ============================================================================
  Section 5.1 : Tableaux statiques unidimensionnels
  Description : Exemple complet de gestion de statistiques sur des notes
  Fichier source : 01-tableaux-statiques-unidimensionnels.md
  ============================================================================ }
program StatistiquesNotes;
var
  notes: array[1..10] of Integer;
  i, somme, nbSuperieur: Integer;
  moyenne: Real;
begin
  // Saisie
  WriteLn('=== Saisie de 10 notes ===');
  for i := 1 to 10 do
  begin
    Write('Note ', i, ' : ');
    ReadLn(notes[i]);
  end;

  // Calcul de la moyenne
  somme := 0;
  for i := 1 to 10 do
    somme := somme + notes[i];
  moyenne := somme / 10;

  // Comptage des notes supérieures à la moyenne
  nbSuperieur := 0;
  for i := 1 to 10 do
  begin
    if notes[i] > moyenne then
      nbSuperieur := nbSuperieur + 1;
  end;

  // Affichage des résultats
  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Moyenne : ', moyenne:0:2);
  WriteLn('Notes supérieures à la moyenne : ', nbSuperieur);
end.
