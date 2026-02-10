{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Passer un enregistrement a une procedure
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program RecordProcedure;  
type  
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

procedure AfficherPersonne(p: TPersonne);  
begin  
  WriteLn('--- Fiche ---');
  WriteLn('Nom : ', p.nom);
  WriteLn('Prénom : ', p.prenom);
  WriteLn('Age : ', p.age, ' ans');
  WriteLn('-------------');
end;

var
  personne: TPersonne;
begin
  personne.nom := 'Martin';
  personne.prenom := 'Sophie';
  personne.age := 30;

  AfficherPersonne(personne);
end.
