{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de date complete avec annee bissextile
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationDate;
var
  jour, mois, annee: Integer;
  joursMax: Integer;
  valide: Boolean;
begin
  WriteLn('Entrez une date :');

  // Validation du jour
  repeat
    Write('Jour (1-31) : ');
    ReadLn(jour);
  until (jour >= 1) and (jour <= 31);

  // Validation du mois
  repeat
    Write('Mois (1-12) : ');
    ReadLn(mois);
  until (mois >= 1) and (mois <= 12);

  // Validation de l'année
  repeat
    Write('Année (1900-2100) : ');
    ReadLn(annee);
  until (annee >= 1900) and (annee <= 2100);

  // Validation de cohérence jour/mois
  valide := True;

  case mois of
    1, 3, 5, 7, 8, 10, 12: joursMax := 31;
    4, 6, 9, 11: joursMax := 30;
    2:  // Février
      begin
        // Année bissextile (simplifiée)
        if (annee mod 4 = 0) and ((annee mod 100 <> 0) or (annee mod 400 = 0)) then
          joursMax := 29
        else
          joursMax := 28;
      end;
  else
    joursMax := 31;
  end;

  if jour > joursMax then
  begin
    WriteLn('❌ ERREUR : Ce mois n''a que ', joursMax, ' jours');
    valide := False;
  end;

  if valide then
  begin
    WriteLn('✓ Date valide : ', jour, '/', mois, '/', annee);
    if (mois = 2) and (joursMax = 29) then
      WriteLn('  (', annee, ' est une année bissextile)');
  end;
end.
