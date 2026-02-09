{ ============================================================================
  Section 5.5 : Enregistrements imbriques
  Description : Carnet d'adresses complet avec telephones et adresse imbriques
  Fichier source : 05-enregistrements-imbriques.md
  ============================================================================ }
program CarnetComplet;
type
  TTelephone = record
    fixe: String;
    mobile: String;
  end;

  TAdresse = record
    rue: String;
    ville: String;
    codePostal: String;
    pays: String;
  end;

  TContact = record
    nom: String;
    prenom: String;
    email: String;
    telephones: TTelephone;
    adresse: TAdresse;
  end;

procedure SaisirContact(var c: TContact);
begin
  WriteLn('=== Nouveau contact ===');
  Write('Nom : ');
  ReadLn(c.nom);
  Write('Prénom : ');
  ReadLn(c.prenom);
  Write('Email : ');
  ReadLn(c.email);

  WriteLn('--- Téléphones ---');
  Write('Fixe : ');
  ReadLn(c.telephones.fixe);
  Write('Mobile : ');
  ReadLn(c.telephones.mobile);

  WriteLn('--- Adresse ---');
  Write('Rue : ');
  ReadLn(c.adresse.rue);
  Write('Code postal : ');
  ReadLn(c.adresse.codePostal);
  Write('Ville : ');
  ReadLn(c.adresse.ville);
  Write('Pays : ');
  ReadLn(c.adresse.pays);
end;

procedure AfficherContact(c: TContact);
begin
  WriteLn('========================');
  WriteLn('Contact : ', c.prenom, ' ', c.nom);
  WriteLn('Email : ', c.email);
  WriteLn('Téléphones :');
  WriteLn('  Fixe : ', c.telephones.fixe);
  WriteLn('  Mobile : ', c.telephones.mobile);
  WriteLn('Adresse :');
  WriteLn('  ', c.adresse.rue);
  WriteLn('  ', c.adresse.codePostal, ' ', c.adresse.ville);
  WriteLn('  ', c.adresse.pays);
  WriteLn('========================');
end;

var
  contact: TContact;
begin
  SaisirContact(contact);
  WriteLn;
  AfficherContact(contact);
end.
