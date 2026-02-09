{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Acces aux champs d'un enregistrement (affectation, lecture, modification)
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program ExempleRecord;
type
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

var
  personne: TPersonne;
begin
  // Affectation des valeurs
  personne.nom := 'Dupont';
  personne.prenom := 'Jean';
  personne.age := 25;

  // Lecture des valeurs
  WriteLn('Nom : ', personne.nom);
  WriteLn('Prénom : ', personne.prenom);
  WriteLn('Age : ', personne.age, ' ans');

  // Modification d'un champ
  personne.age := personne.age + 1;
  WriteLn('Nouvel âge : ', personne.age, ' ans');
end.
