{ ============================================================================
  Section 5.5 : Enregistrements imbriques
  Description : Entreprise avec employe et adresse imbriques
  Fichier source : 05-enregistrements-imbriques.md
  ============================================================================ }
program ExempleEntreprise;  
type  
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TEmploye = record
    nom: String;
    prenom: String;
    dateEmbauche: TDate;
    salaire: Real;
  end;

  TEntreprise = record
    nom: String;
    siret: String;
    adresse: TAdresse;
    directeur: TEmploye;
  end;

function AncienneteAnnees(emp: TEmploye; dateActuelle: TDate): Integer;  
begin  
  AncienneteAnnees := dateActuelle.annee - emp.dateEmbauche.annee;
end;

procedure AfficherEntreprise(ent: TEntreprise);  
begin  
  WriteLn('===== ENTREPRISE =====');
  WriteLn('Nom : ', ent.nom);
  WriteLn('SIRET : ', ent.siret);
  WriteLn('Adresse : ', ent.adresse.rue);
  WriteLn('          ', ent.adresse.codePostal, ' ', ent.adresse.ville);
  WriteLn;
  WriteLn('Directeur : ', ent.directeur.prenom, ' ', ent.directeur.nom);
  WriteLn('Embauché le : ', ent.directeur.dateEmbauche.jour, '/',
          ent.directeur.dateEmbauche.mois, '/',
          ent.directeur.dateEmbauche.annee);
  WriteLn('Salaire : ', ent.directeur.salaire:0:2, ' €');
  WriteLn('======================');
end;

var
  entreprise: TEntreprise;
begin
  entreprise.nom := 'TechCorp';
  entreprise.siret := '12345678901234';

  entreprise.adresse.rue := '15 rue de l''Innovation';
  entreprise.adresse.ville := 'Toulouse';
  entreprise.adresse.codePostal := '31000';

  entreprise.directeur.nom := 'Leblanc';
  entreprise.directeur.prenom := 'Pierre';
  entreprise.directeur.salaire := 5000.00;
  entreprise.directeur.dateEmbauche.jour := 1;
  entreprise.directeur.dateEmbauche.mois := 1;
  entreprise.directeur.dateEmbauche.annee := 2015;

  AfficherEntreprise(entreprise);
end.
