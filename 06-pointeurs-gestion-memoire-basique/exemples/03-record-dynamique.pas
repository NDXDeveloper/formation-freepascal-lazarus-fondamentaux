{ ============================================================================
  Section 6.3 : Allocation Dynamique (New, Dispose)
  Description : Allocation dynamique d'un enregistrement (record)
  Fichier source : 03-allocation-dynamique-new-dispose.md
  ============================================================================ }
program RecordDynamique;  
type  
  TPerson = record
    nom: String;
    age: Integer;
  end;
  PPerson = ^TPerson;

var
  personne: PPerson;
begin
  // Allocation
  New(personne);

  // Initialisation
  personne^.nom := 'Jean Martin';
  personne^.age := 35;

  // Utilisation
  WriteLn('Nom : ', personne^.nom);
  WriteLn('Age : ', personne^.age);

  // Libération
  Dispose(personne);
  personne := nil;
end.
