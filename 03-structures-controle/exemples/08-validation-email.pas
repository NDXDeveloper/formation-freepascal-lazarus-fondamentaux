{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de format email - verifications basiques arobase et point
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationEmail;  
var  
  email: String;
  posArobase, posPoint: Integer;
  valide: Boolean;
begin
  repeat
    Write('Email : ');
    ReadLn(email);

    valide := True;
    posArobase := Pos('@', email);

    // Vérifications basiques
    if Length(email) < 5 then
    begin
      WriteLn('❌ Email trop court');
      valide := False;
    end
    else if posArobase = 0 then
    begin
      WriteLn('❌ L''email doit contenir @');
      valide := False;
    end
    else if posArobase = 1 then
    begin
      WriteLn('❌ L''email ne peut pas commencer par @');
      valide := False;
    end
    else
    begin
      // Vérifier qu'il y a un point après le @
      // Copy(chaine, debut, longueur) extrait une sous-chaîne
      posPoint := Pos('.', Copy(email, posArobase, Length(email)));
      if posPoint = 0 then
      begin
        WriteLn('❌ L''email doit avoir un point après @');
        valide := False;
      end;
    end;
  until valide;

  WriteLn('✓ Email accepté : ', email);
end.
