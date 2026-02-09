{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Saisie et affichage d'un enregistrement TPersonne
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program SaisiePersonne;
type
  TPersonne = record
    nom: String;
    prenom: String;
    age: Integer;
  end;

var
  personne: TPersonne;
begin
  // Saisie des informations
  WriteLn('=== Saisie des informations ===');
  Write('Nom : ');
  ReadLn(personne.nom);
  Write('Prénom : ');
  ReadLn(personne.prenom);
  Write('Age : ');
  ReadLn(personne.age);

  // Affichage récapitulatif
  WriteLn;
  WriteLn('=== Récapitulatif ===');
  WriteLn('Nom complet : ', personne.prenom, ' ', personne.nom);
  WriteLn('Age : ', personne.age, ' ans');
end.
