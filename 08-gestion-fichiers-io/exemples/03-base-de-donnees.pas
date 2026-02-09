{ ============================================================================
  Section 8.3 : Fichiers binaires et acces direct
  Description : Mini base de donnees de contacts avec acces direct
  Fichier source : 03-fichiers-binaires-acces-direct.md
  Note : Programme interactif (ReadLn)
  ============================================================================ }
program BaseDeDonnees;

type
  TContact = record
    Nom: string[30];
    Telephone: string[15];
    Email: string[50];
  end;

var
  Fichier: File;
  Contact: TContact;
  Choix, Position: Integer;
  NomFichier: string;

procedure AjouterContact;
var
  NbEcrits: Word;
begin
  WriteLn('--- Ajouter un contact ---');
  Write('Nom : '); ReadLn(Contact.Nom);
  Write('Téléphone : '); ReadLn(Contact.Telephone);
  Write('Email : '); ReadLn(Contact.Email);

  // Aller a la fin du fichier
  Seek(Fichier, FileSize(Fichier));

  // Ecrire le nouveau contact
  BlockWrite(Fichier, Contact, 1, NbEcrits);

  if NbEcrits = 1 then
    WriteLn('Contact ajouté avec succès !')
  else
    WriteLn('Erreur lors de l''ajout.');
end;

procedure AfficherContact(Num: Integer);
var
  NbLus: Word;
begin
  if (Num < 1) or (Num > FileSize(Fichier)) then
  begin
    WriteLn('Numéro invalide !');
    Exit;
  end;

  // Aller directement au contact demande
  Seek(Fichier, Num - 1);
  BlockRead(Fichier, Contact, 1, NbLus);

  if NbLus = 1 then
  begin
    WriteLn('--- Contact #', Num, ' ---');
    WriteLn('Nom : ', Contact.Nom);
    WriteLn('Téléphone : ', Contact.Telephone);
    WriteLn('Email : ', Contact.Email);
  end;
end;

procedure ListerContacts;
var
  i: Integer;
  NbLus: Word;
begin
  WriteLn('--- Liste des contacts ---');
  WriteLn('Total : ', FileSize(Fichier), ' contact(s)');
  WriteLn;

  Seek(Fichier, 0);  // Retour au debut

  for i := 1 to FileSize(Fichier) do
  begin
    BlockRead(Fichier, Contact, 1, NbLus);
    if NbLus = 1 then
      WriteLn(i:3, '. ', Contact.Nom);
  end;
end;

begin
  NomFichier := 'contacts.dat';
  Assign(Fichier, NomFichier);

  // Ouvrir ou creer le fichier
  {$I-}
  Reset(Fichier, SizeOf(TContact));  // Taille de bloc = taille d'un record, permet l'accès par numéro de contact
  {$I+}

  if IOResult <> 0 then
    Rewrite(Fichier, SizeOf(TContact));

  // Menu principal
  repeat
    WriteLn;
    WriteLn('=== GESTIONNAIRE DE CONTACTS ===');
    WriteLn('1. Ajouter un contact');
    WriteLn('2. Afficher un contact');
    WriteLn('3. Lister tous les contacts');
    WriteLn('0. Quitter');
    Write('Votre choix : ');
    ReadLn(Choix);
    WriteLn;

    case Choix of
      1: AjouterContact;
      2: begin
           Write('Numéro du contact : ');
           ReadLn(Position);
           AfficherContact(Position);
         end;
      3: ListerContacts;
      0: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide !');
    end;

  until Choix = 0;

  Close(Fichier);
end.
