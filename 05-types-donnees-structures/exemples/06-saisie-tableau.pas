{ ============================================================================
  Section 5.6 : Tableaux d'enregistrements
  Description : Saisie et affichage d'un tableau d'enregistrements
  Fichier source : 06-tableaux-enregistrements.md
  ============================================================================ }
program SaisieTableau;
type
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..3] of TEleve;
  i: Integer;
begin
  WriteLn('Saisie de 3 élèves');
  WriteLn('==================');

  for i := 1 to 3 do
  begin
    WriteLn('Élève ', i, ' :');
    Write('  Nom : ');
    ReadLn(eleves[i].nom);
    Write('  Prénom : ');
    ReadLn(eleves[i].prenom);
    Write('  Note : ');
    ReadLn(eleves[i].note);
    WriteLn;
  end;

  // Affichage récapitulatif
  WriteLn('Récapitulatif :');
  for i := 1 to 3 do
    WriteLn(eleves[i].prenom, ' ', eleves[i].nom, ' : ', eleves[i].note:0:1);
end.
