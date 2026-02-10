{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Tableau de notes 2D (4 eleves, 3 matieres) avec saisie et affichage
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program TableauNotes;  
var  
  notes: array[1..4, 1..3] of Real;  // 4 élèves, 3 matières
  eleve, matiere: Integer;
  nomsMatieres: array[1..3] of String;
begin
  // Noms des matières
  nomsMatieres[1] := 'Maths';
  nomsMatieres[2] := 'Français';
  nomsMatieres[3] := 'Histoire';

  // Saisie des notes
  WriteLn('=== Saisie des notes ===');
  for eleve := 1 to 4 do
  begin
    WriteLn;
    WriteLn('Élève ', eleve, ' :');
    for matiere := 1 to 3 do
    begin
      Write('  ', nomsMatieres[matiere], ' : ');
      ReadLn(notes[eleve, matiere]);
    end;
  end;

  // Affichage sous forme de tableau
  WriteLn;
  WriteLn('=== Tableau récapitulatif ===');
  Write('Élève    ');
  for matiere := 1 to 3 do
    Write(nomsMatieres[matiere]:10);
  WriteLn;
  WriteLn('----------------------------------------');

  for eleve := 1 to 4 do
  begin
    Write('  ', eleve, '      ');
    for matiere := 1 to 3 do
      Write(notes[eleve, matiere]:10:1);
    WriteLn;
  end;
end.
