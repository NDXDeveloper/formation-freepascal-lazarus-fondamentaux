{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Modifier un enregistrement par reference (var) dans une procedure
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program ModifierRecord;  
type  
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

procedure Vieillir(var p: TPersonne; annees: Integer);  
begin  
  p.age := p.age + annees;
end;

var
  personne: TPersonne;
begin
  personne.nom := 'Durand';
  personne.prenom := 'Paul';
  personne.age := 20;

  WriteLn('Age initial : ', personne.age);
  Vieillir(personne, 5);
  WriteLn('Age après 5 ans : ', personne.age);
end.
