{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation avec repeat-until - technique simple pour un pourcentage
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationRepeatPourcentage;
var
  pourcentage: Integer;
begin
  repeat
    Write('Pourcentage (0-100) : ');
    ReadLn(pourcentage);

    if (pourcentage < 0) or (pourcentage > 100) then
      WriteLn('❌ Doit être entre 0 et 100');
  until (pourcentage >= 0) and (pourcentage <= 100);

  WriteLn('✓ Pourcentage : ', pourcentage, '%');
end.
