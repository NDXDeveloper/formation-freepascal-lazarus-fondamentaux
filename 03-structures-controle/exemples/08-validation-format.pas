{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de format - verifier qu'un code postal a 5 chiffres
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationFormat;
var
  codePostal: String;
  i: Integer;
  valide: Boolean;
begin
  WriteLn('Entrez un code postal français (5 chiffres) :');

  repeat
    Write('Code postal : ');
    ReadLn(codePostal);

    valide := True;

    // Vérifier la longueur
    if Length(codePostal) <> 5 then
      valide := False
    else
    begin
      // Vérifier que tous sont des chiffres
      for i := 1 to 5 do
      begin
        if not (codePostal[i] in ['0'..'9']) then
        begin
          valide := False;
          break;
        end;
      end;
    end;

    if not valide then
      WriteLn('❌ Le code postal doit contenir exactement 5 chiffres');
  until valide;

  WriteLn('✓ Code postal accepté : ', codePostal);
end.
