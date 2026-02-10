{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Exemple avec validation - programme robuste avec boucle repeat
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program AvecValidation;  
var  
  age: Integer;
begin
  repeat
    Write('Âge (0-150) : ');
    ReadLn(age);
    if (age < 0) or (age > 150) then
      WriteLn('⚠️  Âge invalide. Réessayez.');
  until (age >= 0) and (age <= 150);

  WriteLn('✓ Dans 10 ans, vous aurez ', age + 10, ' ans');
end.
