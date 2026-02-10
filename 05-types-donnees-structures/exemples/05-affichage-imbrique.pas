{ ============================================================================
  Section 5.5 : Enregistrements imbriques
  Description : Affichage d'un enregistrement imbrique avec procedures dediees
  Fichier source : 05-enregistrements-imbriques.md
  ============================================================================ }
program AffichageImbrique;  
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

  TPersonne = record
    nom: String;
    prenom: String;
    adresse: TAdresse;
    dateNaissance: TDate;
  end;

procedure AfficherDate(d: TDate);  
begin  
  WriteLn(d.jour, '/', d.mois, '/', d.annee);
end;

procedure AfficherAdresse(a: TAdresse);  
begin  
  WriteLn(a.rue);
  WriteLn(a.codePostal, ' ', a.ville);
end;

procedure AfficherPersonne(p: TPersonne);  
begin  
  WriteLn('=== Fiche Personne ===');
  WriteLn('Nom : ', p.prenom, ' ', p.nom);
  Write('Né(e) le : ');
  AfficherDate(p.dateNaissance);
  WriteLn('Adresse :');
  AfficherAdresse(p.adresse);
  WriteLn('======================');
end;

var
  personne: TPersonne;
begin
  // Initialisation
  personne.nom := 'Durand';
  personne.prenom := 'Sophie';
  personne.adresse.rue := '12 boulevard Haussmann';
  personne.adresse.ville := 'Marseille';
  personne.adresse.codePostal := '13001';
  personne.dateNaissance.jour := 20;
  personne.dateNaissance.mois := 7;
  personne.dateNaissance.annee := 1990;

  AfficherPersonne(personne);
end.
