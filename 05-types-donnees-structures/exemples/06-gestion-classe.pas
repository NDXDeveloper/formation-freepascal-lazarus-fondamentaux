{ ============================================================================
  Section 5.6 : Tableaux d'enregistrements
  Description : Gestion complete d'une classe (saisie, affichage, moyenne)
  Fichier source : 06-tableaux-enregistrements.md
  ============================================================================ }
program GestionClasse;  
type  
  TEleve = record
    nom: String;
    prenom: String;
    note: Real;
  end;
  TClasse = array[1..30] of TEleve;

var
  eleves: TClasse;
  nbEleves: Integer;
  i: Integer;
  somme, moyenne: Real;

begin
  // Saisie du nombre d'élèves
  Write('Nombre d''élèves dans la classe : ');
  ReadLn(nbEleves);

  // Saisie des élèves
  for i := 1 to nbEleves do
  begin
    WriteLn;
    WriteLn('Élève ', i, ' :');
    Write('  Nom : ');
    ReadLn(eleves[i].nom);
    Write('  Prénom : ');
    ReadLn(eleves[i].prenom);
    Write('  Note : ');
    ReadLn(eleves[i].note);
  end;

  // Affichage de la liste
  WriteLn;
  WriteLn('=== LISTE DE LA CLASSE ===');
  for i := 1 to nbEleves do
    WriteLn(i:2, '. ', eleves[i].prenom, ' ', eleves[i].nom,
            ' : ', eleves[i].note:0:1, '/20');

  // Calcul de la moyenne
  somme := 0;
  for i := 1 to nbEleves do
    somme := somme + eleves[i].note;
  moyenne := somme / nbEleves;

  WriteLn;
  WriteLn('Moyenne de la classe : ', moyenne:0:2, '/20');
end.
