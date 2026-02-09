{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation combinee - code a 6 caracteres avec chiffres et lettres
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationCombinee;
var
  code: String;
  i: Integer;
  nbChiffres, nbLettres: Integer;
  valide: Boolean;
const
  LONGUEUR_CODE = 6;
begin
  WriteLn('Créez un code de ', LONGUEUR_CODE, ' caractères');
  WriteLn('(doit contenir au moins 1 chiffre et 1 lettre)');
  WriteLn;

  repeat
    Write('Code : ');
    ReadLn(code);

    valide := True;
    nbChiffres := 0;
    nbLettres := 0;

    // Vérifier la longueur
    if Length(code) <> LONGUEUR_CODE then
    begin
      WriteLn('❌ Le code doit faire exactement ', LONGUEUR_CODE, ' caractères');
      valide := False;
    end
    else
    begin
      // Compter chiffres et lettres
      for i := 1 to Length(code) do
      begin
        if code[i] in ['0'..'9'] then
          nbChiffres := nbChiffres + 1
        else if code[i] in ['A'..'Z', 'a'..'z'] then
          nbLettres := nbLettres + 1;
      end;

      if nbChiffres = 0 then
      begin
        WriteLn('❌ Le code doit contenir au moins 1 chiffre');
        valide := False;
      end;

      if nbLettres = 0 then
      begin
        WriteLn('❌ Le code doit contenir au moins 1 lettre');
        valide := False;
      end;

      if (nbChiffres + nbLettres) <> LONGUEUR_CODE then
      begin
        WriteLn('❌ Le code ne doit contenir que des chiffres et des lettres');
        valide := False;
      end;
    end;
  until valide;

  WriteLn('✓ Code valide : ', code);
end.
