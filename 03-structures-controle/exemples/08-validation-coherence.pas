{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de coherence - verifier la logique entre plusieurs donnees
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationCoherence;  
var  
  dateNaissance, dateEmbauche: Integer;
begin
  WriteLn('Saisie d''informations employé :');

  repeat
    Write('Année de naissance : ');
    ReadLn(dateNaissance);
    if (dateNaissance < 1900) or (dateNaissance > 2024) then
      WriteLn('❌ Année invalide');
  until (dateNaissance >= 1900) and (dateNaissance <= 2024);

  repeat
    Write('Année d''embauche : ');
    ReadLn(dateEmbauche);

    if dateEmbauche < dateNaissance then
      WriteLn('❌ L''embauche ne peut pas être avant la naissance !')
    else if (dateEmbauche - dateNaissance) < 16 then
      WriteLn('❌ L''employé doit avoir au moins 16 ans à l''embauche')
    else if dateEmbauche > 2024 then
      WriteLn('❌ Date dans le futur impossible');
  until (dateEmbauche >= dateNaissance + 16) and (dateEmbauche <= 2024);

  WriteLn('✓ Données validées');
end.
