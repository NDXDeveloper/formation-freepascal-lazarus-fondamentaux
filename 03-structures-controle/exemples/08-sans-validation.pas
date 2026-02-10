{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Exemple sans validation - programme fragile aux entrees invalides
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program SansValidation;  
var  
  age: Integer;
begin
  Write('Âge : ');
  ReadLn(age);
  WriteLn('Dans 10 ans, vous aurez ', age + 10, ' ans');
end.
