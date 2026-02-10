{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Menu avec validation - choix numerique entre 1 et 4
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program MenuValidation;  
var  
  choix: Integer;
begin
  WriteLn('═══ MENU PRINCIPAL ═══');
  WriteLn('1. Nouveau');
  WriteLn('2. Ouvrir');
  WriteLn('3. Enregistrer');
  WriteLn('4. Quitter');
  WriteLn;

  repeat
    Write('Votre choix (1-4) : ');
    ReadLn(choix);

    if (choix < 1) or (choix > 4) then
      WriteLn('❌ Veuillez choisir entre 1 et 4');
  until (choix >= 1) and (choix <= 4);

  WriteLn('✓ Option ', choix, ' sélectionnée');
end.
