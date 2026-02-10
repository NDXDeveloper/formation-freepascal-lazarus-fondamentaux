{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Formulaire d'inscription complet avec validation de chaque champ
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program FormulaireInscription;  
uses  
  SysUtils;
var
  nom, prenom, email, telephone: String;
  age: Integer;
  i, posArobase: Integer;
  valide: Boolean;
begin
  WriteLn('═══════════════════════════════');
  WriteLn('   FORMULAIRE D''INSCRIPTION');
  WriteLn('═══════════════════════════════');
  WriteLn;

  // Nom
  repeat
    Write('Nom : ');
    ReadLn(nom);
    nom := Trim(nom);

    if Length(nom) < 2 then
      WriteLn('❌ Le nom doit contenir au moins 2 caractères');
  until Length(nom) >= 2;

  // Prénom
  repeat
    Write('Prénom : ');
    ReadLn(prenom);
    prenom := Trim(prenom);

    if Length(prenom) < 2 then
      WriteLn('❌ Le prénom doit contenir au moins 2 caractères');
  until Length(prenom) >= 2;

  // Âge
  repeat
    Write('Âge : ');
    ReadLn(age);

    if (age < 18) or (age > 100) then
      WriteLn('❌ Vous devez avoir entre 18 et 100 ans');
  until (age >= 18) and (age <= 100);

  // Email
  repeat
    Write('Email : ');
    ReadLn(email);
    email := Trim(email);
    valide := True;

    posArobase := Pos('@', email);
    if (posArobase = 0) or (posArobase = 1) or (posArobase = Length(email)) then
    begin
      WriteLn('❌ Format d''email invalide');
      valide := False;
    end;
  until valide;

  // Téléphone
  repeat
    Write('Téléphone (10 chiffres) : ');
    ReadLn(telephone);
    valide := True;

    if Length(telephone) <> 10 then
    begin
      WriteLn('❌ Le numéro doit contenir 10 chiffres');
      valide := False;
    end
    else
    begin
      for i := 1 to 10 do
      begin
        if not (telephone[i] in ['0'..'9']) then
        begin
          WriteLn('❌ Le numéro ne doit contenir que des chiffres');
          valide := False;
          break;
        end;
      end;
    end;
  until valide;

  // Récapitulatif
  WriteLn;
  WriteLn('═══════════════════════════════');
  WriteLn('   RÉCAPITULATIF');
  WriteLn('═══════════════════════════════');
  WriteLn('Nom : ', nom);
  WriteLn('Prénom : ', prenom);
  WriteLn('Âge : ', age, ' ans');
  WriteLn('Email : ', email);
  WriteLn('Téléphone : ', telephone);
  WriteLn('═══════════════════════════════');
  WriteLn('✓ Inscription validée !');
end.
