{ ============================================================================
  Section 5.5 : Enregistrements imbriques
  Description : Acces aux champs d'un enregistrement imbrique (personne avec adresse)
  Fichier source : 05-enregistrements-imbriques.md
  ============================================================================ }
program AccesImbriques;
type
  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
  end;

  TPersonne = record
    nom: String;
    prenom: String;
    adresse: TAdresse;
  end;

var
  personne: TPersonne;
begin
  // Accès aux champs simples
  personne.nom := 'Dupont';
  personne.prenom := 'Marie';

  // Accès aux champs imbriqués
  personne.adresse.rue := '10 rue de la Paix';
  personne.adresse.ville := 'Paris';
  personne.adresse.codePostal := '75001';

  // Affichage
  WriteLn(personne.prenom, ' ', personne.nom);
  WriteLn(personne.adresse.rue);
  WriteLn(personne.adresse.codePostal, ' ', personne.adresse.ville);
end.
