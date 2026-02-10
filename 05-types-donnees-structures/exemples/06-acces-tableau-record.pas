{ ============================================================================
  Section 5.6 : Tableaux d'enregistrements
  Description : Acces aux champs d'un tableau d'enregistrements
  Fichier source : 06-tableaux-enregistrements.md
  ============================================================================ }
program AccesTableauRecord;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;

var
  eleves: array[1..3] of TEleve;
begin
  // Affectation
  eleves[1].nom := 'Dupont';
  eleves[1].prenom := 'Marie';
  eleves[1].note := 15.5;

  eleves[2].nom := 'Martin';
  eleves[2].prenom := 'Jean';
  eleves[2].note := 12.0;

  eleves[3].nom := 'Durand';
  eleves[3].prenom := 'Sophie';
  eleves[3].note := 18.0;

  // Lecture
  WriteLn('Premier élève : ', eleves[1].prenom, ' ', eleves[1].nom);
  WriteLn('Sa note : ', eleves[1].note:0:1);
end.
