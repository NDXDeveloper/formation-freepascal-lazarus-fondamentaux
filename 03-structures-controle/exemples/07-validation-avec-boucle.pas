{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Validation de l'age avec boucle while jusqu'a saisie valide
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program ValidationAvecBoucle;  
var  
  age: Integer;
  valide: Boolean;
begin
  valide := False;

  WriteLn('Veuillez entrer votre âge (0-150)');

  while not valide do
  begin
    Write('Âge : ');
    ReadLn(age);

    if (age < 0) or (age > 150) then
      WriteLn('❌ Âge invalide. Réessayez.')
    else
    begin
      WriteLn('✓ Âge accepté');
      valide := True;
    end;
  end;

  WriteLn('Vous avez ', age, ' ans.');
end.
