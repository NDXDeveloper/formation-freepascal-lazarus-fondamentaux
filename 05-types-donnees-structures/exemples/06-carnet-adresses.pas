{ ============================================================================
  Section 5.6 : Tableaux d'enregistrements
  Description : Carnet d'adresses avec menu (ajouter, lister, rechercher)
  Fichier source : 06-tableaux-enregistrements.md
  ============================================================================ }
program CarnetAdresses;  
type  
  TContact = record
    nom: String;
    prenom: String;
    telephone: String;
    email: String;
  end;
  TCarnet = array[1..50] of TContact;

var
  contacts: TCarnet;
  nbContacts: Integer;
  choix, i: Integer;
  nomRecherche: String;

procedure AfficherMenu;  
begin  
  WriteLn;
  WriteLn('=== CARNET D''ADRESSES ===');
  WriteLn('1. Ajouter un contact');
  WriteLn('2. Lister tous les contacts');
  WriteLn('3. Rechercher un contact');
  WriteLn('4. Quitter');
  Write('Votre choix : ');
end;

procedure AjouterContact;  
begin  
  if nbContacts < 50 then
  begin
    nbContacts := nbContacts + 1;
    WriteLn('Nouveau contact :');
    Write('  Nom : ');
    ReadLn(contacts[nbContacts].nom);
    Write('  Prénom : ');
    ReadLn(contacts[nbContacts].prenom);
    Write('  Téléphone : ');
    ReadLn(contacts[nbContacts].telephone);
    Write('  Email : ');
    ReadLn(contacts[nbContacts].email);
    WriteLn('Contact ajouté !');
  end
  else
    WriteLn('Carnet plein !');
end;

procedure ListerContacts;  
var  
  i: Integer;
begin
  if nbContacts = 0 then
    WriteLn('Aucun contact dans le carnet')
  else
  begin
    WriteLn('=== Liste des contacts ===');
    for i := 1 to nbContacts do
    begin
      WriteLn(i, '. ', contacts[i].prenom, ' ', contacts[i].nom);
      WriteLn('   Tel : ', contacts[i].telephone);
      WriteLn('   Email : ', contacts[i].email);
      WriteLn('---');
    end;
  end;
end;

procedure RechercherContact;  
var  
  i: Integer;
  trouve: Boolean;
begin
  Write('Nom à rechercher : ');
  ReadLn(nomRecherche);

  trouve := False;
  for i := 1 to nbContacts do
  begin
    if contacts[i].nom = nomRecherche then
    begin
      WriteLn('Contact trouvé :');
      WriteLn('  ', contacts[i].prenom, ' ', contacts[i].nom);
      WriteLn('  Tel : ', contacts[i].telephone);
      WriteLn('  Email : ', contacts[i].email);
      trouve := True;
    end;
  end;

  if not trouve then
    WriteLn('Contact non trouvé');
end;

begin
  nbContacts := 0;

  repeat
    AfficherMenu;
    ReadLn(choix);

    case choix of
      1: AjouterContact;
      2: ListerContacts;
      3: RechercherContact;
      4: WriteLn('Au revoir !');
    else
      WriteLn('Choix invalide');
    end;
  until choix = 4;
end.
