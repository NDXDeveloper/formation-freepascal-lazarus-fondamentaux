{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Carnet d'adresses simple avec saisie et affichage d'un contact
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program CarnetAdresses;  
type  
  TContact = record
    nom: String;
    prenom: String;
    telephone: String;
    email: String;
  end;

procedure SaisirContact(var c: TContact);  
begin  
  WriteLn('=== Nouveau contact ===');
  Write('Nom : ');
  ReadLn(c.nom);
  Write('Prénom : ');
  ReadLn(c.prenom);
  Write('Téléphone : ');
  ReadLn(c.telephone);
  Write('Email : ');
  ReadLn(c.email);
end;

procedure AfficherContact(c: TContact);  
begin  
  WriteLn('--- Contact ---');
  WriteLn('Nom complet : ', c.prenom, ' ', c.nom);
  WriteLn('Téléphone : ', c.telephone);
  WriteLn('Email : ', c.email);
  WriteLn('---------------');
end;

var
  contact: TContact;
begin
  SaisirContact(contact);
  WriteLn;
  AfficherContact(contact);
end.
