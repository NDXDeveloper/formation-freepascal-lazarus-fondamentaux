{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation d'un nombre decimal positif avec plafond
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationDecimalPositif;
var
  prix: Real;
begin
  repeat
    Write('Prix en euros : ');
    ReadLn(prix);

    if prix < 0 then
      WriteLn('❌ Le prix ne peut pas être négatif')
    else if prix > 1000000 then
      WriteLn('❌ Prix trop élevé (max 1 000 000 €)');
  until (prix >= 0) and (prix <= 1000000);

  WriteLn('✓ Prix : ', prix:0:2, ' €');
end.
