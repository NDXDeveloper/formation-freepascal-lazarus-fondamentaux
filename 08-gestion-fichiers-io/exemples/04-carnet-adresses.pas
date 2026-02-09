{ ============================================================================
  Section 8.4 : Fichiers types
  Description : Carnet d'adresses complet avec fichier type
  Fichier source : 04-fichiers-types.md
  Note : Programme interactif (ReadLn)
  ============================================================================ }
program CarnetAdresses;

type
  TContact = record
    ID: Integer;
    Nom: string[30];
    Prenom: string[30];
    Telephone: string[15];
    Email: string[50];
  end;

var
  Fichier: File of TContact;
  Contact: TContact;
  Choix: Integer;

procedure InitialiserFichier;
begin
  Assign(Fichier, 'contacts.dat');
  {$I-}
  Reset(Fichier);
  {$I+}
  if IOResult <> 0 then
    Rewrite(Fichier);
end;

function ProchainID: Integer;
var
  C: TContact;
  MaxID: Integer;
begin
  MaxID := 0;
  Seek(Fichier, 0);

  while not EOF(Fichier) do
  begin
    Read(Fichier, C);
    if C.ID > MaxID then
      MaxID := C.ID;
  end;

  ProchainID := MaxID + 1;
end;

procedure AjouterContact;
begin
  WriteLn('=== NOUVEAU CONTACT ===');

  Contact.ID := ProchainID;

  Write('Nom : ');
  ReadLn(Contact.Nom);

  Write('Prénom : ');
  ReadLn(Contact.Prenom);

  Write('Téléphone : ');
  ReadLn(Contact.Telephone);

  Write('Email : ');
  ReadLn(Contact.Email);

  Seek(Fichier, FileSize(Fichier));
  Write(Fichier, Contact);

  WriteLn('Contact ajouté avec l''ID : ', Contact.ID);
end;

procedure AfficherTous;
begin
  if FileSize(Fichier) = 0 then
  begin
    WriteLn('Aucun contact dans le carnet.');
    Exit;
  end;

  WriteLn('=== LISTE DES CONTACTS ===');
  WriteLn;

  Seek(Fichier, 0);

  while not EOF(Fichier) do
  begin
    Read(Fichier, Contact);
    WriteLn('ID      : ', Contact.ID);
    WriteLn('Nom     : ', Contact.Nom, ' ', Contact.Prenom);
    WriteLn('Tél     : ', Contact.Telephone);
    WriteLn('Email   : ', Contact.Email);
    WriteLn('----------------------------');
  end;

  WriteLn('Total : ', FileSize(Fichier), ' contact(s)');
end;

procedure RechercherParNom;
var
  NomRecherche: string;
  Trouve: Boolean;
begin
  Write('Nom à rechercher : ');
  ReadLn(NomRecherche);

  Trouve := False;
  Seek(Fichier, 0);

  while not EOF(Fichier) do
  begin
    Read(Fichier, Contact);

    if Pos(NomRecherche, Contact.Nom) > 0 then
    begin
      Trouve := True;
      WriteLn;
      WriteLn('Trouvé : ', Contact.Nom, ' ', Contact.Prenom);
      WriteLn('Tél    : ', Contact.Telephone);
      WriteLn('Email  : ', Contact.Email);
    end;
  end;

  if not Trouve then
    WriteLn('Aucun contact trouvé.');
end;

begin
  InitialiserFichier;

  repeat
    WriteLn;
    WriteLn('=== CARNET D''ADRESSES ===');
    WriteLn('1. Ajouter un contact');
    WriteLn('2. Afficher tous les contacts');
    WriteLn('3. Rechercher par nom');
    WriteLn('0. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1: AjouterContact;
      2: AfficherTous;
      3: RechercherParNom;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;

  Close(Fichier);
end.
