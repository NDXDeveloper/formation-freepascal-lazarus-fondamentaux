{ ============================================================================
  Section 6.5 : Pointeurs et Enregistrements
  Description : Enregistrement dynamique avec tableau dynamique de notes
  Fichier source : 05-pointeurs-enregistrements.md
  ============================================================================ }
program EtudiantNotes;
type
  PEtudiant = ^TEtudiant;
  TEtudiant = record
    nom: String;
    notes: array of Real;    // Tableau dynamique
  end;

var
  etudiant: PEtudiant;
  i: Integer;
begin
  New(etudiant);
  etudiant^.nom := 'Pierre';

  // Allouer le tableau de notes
  SetLength(etudiant^.notes, 5);

  // Remplir les notes
  for i := 0 to 4 do
    etudiant^.notes[i] := 10 + i * 2;

  // Afficher
  WriteLn('Notes de ', etudiant^.nom, ' :');
  for i := 0 to High(etudiant^.notes) do
    WriteLn('  Note ', i+1, ' : ', etudiant^.notes[i]:0:1);

  // Libération
  SetLength(etudiant^.notes, 0);  // Libérer le tableau
  Dispose(etudiant);
  etudiant := nil;
end.
