{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation d'un entier dans un intervalle avec message detaille
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationEntierIntervalle;  
var  
  jour: Integer;
begin
  repeat
    Write('Jour du mois (1-31) : ');
    ReadLn(jour);

    if (jour < 1) or (jour > 31) then
    begin
      WriteLn('❌ Jour invalide');
      WriteLn('   Les jours vont de 1 à 31');
    end;
  until (jour >= 1) and (jour <= 31);

  WriteLn('✓ Jour : ', jour);
end.
