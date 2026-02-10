{ ============================================================================
  Section 5.6 : Tableaux d'enregistrements
  Description : Parcours et affichage d'un tableau d'enregistrements
  Fichier source : 06-tableaux-enregistrements.md
  ============================================================================ }
program ParcoursTableau;  
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
  // Initialisation (simplifié pour l'exemple)
  eleves[1].nom := 'Dupont';
  eleves[1].prenom := 'Marie';
  eleves[1].note := 15.5;

  eleves[2].nom := 'Martin';
  eleves[2].prenom := 'Jean';
  eleves[2].note := 12.0;

  eleves[3].nom := 'Durand';
  eleves[3].prenom := 'Sophie';
  eleves[3].note := 18.0;

  // Parcours et affichage
  WriteLn('Liste des élèves :');
  WriteLn('==================');
  for i := 1 to 3 do
  begin
    WriteLn(i, '. ', eleves[i].prenom, ' ', eleves[i].nom,
            ' - Note : ', eleves[i].note:0:1);
  end;
end.
