{ ============================================================================
  Section 4.11 : Organisation modulaire du code
  Description : Exemple de mauvais code monolithique (anti-pattern)
  Fichier source : 11-organisation-modulaire-code.md
  ============================================================================ }
program Mauvais;

var
  nom, prenom, email: String;
  age: Integer;
  prix1, prix2, prix3, total, moyenne: Real;
  choix, i: Integer;
begin
  // Tout est mélangé dans le programme principal
  WriteLn('=== MENU ===');
  WriteLn('1. Inscription');
  WriteLn('2. Calculer moyenne');
  WriteLn('3. Quitter');
  Write('Choix : ');
  ReadLn(choix);

  if choix = 1 then
  begin
    Write('Nom : ');
    ReadLn(nom);
    Write('Prénom : ');
    ReadLn(prenom);
    Write('Email : ');
    ReadLn(email);
    Write('Age : ');
    ReadLn(age);
    if age < 18 then
      WriteLn('Mineur')
    else
      WriteLn('Majeur');
    WriteLn('Inscription de ', prenom, ' ', nom, ' (', email, ')');
  end
  else if choix = 2 then
  begin
    Write('Prix 1 : ');
    ReadLn(prix1);
    Write('Prix 2 : ');
    ReadLn(prix2);
    Write('Prix 3 : ');
    ReadLn(prix3);
    total := prix1 + prix2 + prix3;
    moyenne := total / 3;
    WriteLn('Moyenne : ', moyenne:0:2);
  end;
end.
