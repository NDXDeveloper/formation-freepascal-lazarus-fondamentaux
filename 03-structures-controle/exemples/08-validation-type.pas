{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de type - verifier que la donnee est du bon type
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationType;  
var  
  choix: Char;
begin
  WriteLn('Choisissez une option (A/B/C) :');

  repeat
    Write('Choix : ');
    ReadLn(choix);
    choix := UpCase(choix);  // Conversion en majuscule

    if not (choix in ['A', 'B', 'C']) then
      WriteLn('❌ Veuillez entrer A, B ou C');
  until choix in ['A', 'B', 'C'];

  WriteLn('✓ Option ', choix, ' sélectionnée');
end.
