{ ============================================================================
  Section 6.5 : Pointeurs et Enregistrements
  Description : Carnet d'adresses avec liste chainee de contacts
  Fichier source : 05-pointeurs-enregistrements.md
  ============================================================================ }
program CarnetAdresses;  
type  
  PContact = ^TContact;
  TContact = record
    nom: String;
    telephone: String;
    email: String;
    suivant: PContact;
  end;

procedure AjouterContact(var carnet: PContact; n, tel, mail: String);  
var  
  nouveau: PContact;
begin
  New(nouveau);
  nouveau^.nom := n;
  nouveau^.telephone := tel;
  nouveau^.email := mail;
  nouveau^.suivant := carnet;
  carnet := nouveau;
end;

procedure AfficherCarnet(carnet: PContact);  
var  
  courant: PContact;
begin
  courant := carnet;
  WriteLn('=== Carnet d''adresses ===');
  while courant <> nil do
  begin
    WriteLn('Nom : ', courant^.nom);
    WriteLn('Tel : ', courant^.telephone);
    WriteLn('Email : ', courant^.email);
    WriteLn('---');
    courant := courant^.suivant;
  end;
end;

procedure LibererCarnet(var carnet: PContact);  
var  
  courant, suivant: PContact;
begin
  courant := carnet;
  while courant <> nil do
  begin
    suivant := courant^.suivant;
    Dispose(courant);
    courant := suivant;
  end;
  carnet := nil;
end;

var
  contacts: PContact;
begin
  contacts := nil;

  AjouterContact(contacts, 'Alice', '0601020304', 'alice@mail.com');
  AjouterContact(contacts, 'Bob', '0605060708', 'bob@mail.com');

  AfficherCarnet(contacts);

  LibererCarnet(contacts);
end.
