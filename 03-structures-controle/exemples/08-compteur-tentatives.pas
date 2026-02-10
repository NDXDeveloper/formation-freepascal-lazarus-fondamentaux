{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Compteur de tentatives - code secret avec nombre limite d'essais
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program CompteurTentatives;  
var  
  code: Integer;
  tentative: Integer;
const
  CODE_SECRET = 1234;
  MAX_TENTATIVES = 3;
begin
  WriteLn('Entrez le code secret :');

  tentative := 0;
  repeat
    tentative := tentative + 1;
    Write('Tentative ', tentative, '/', MAX_TENTATIVES, ' : ');
    ReadLn(code);

    if code <> CODE_SECRET then
    begin
      WriteLn('❌ Code incorrect');
      if tentative < MAX_TENTATIVES then
        WriteLn('   Il vous reste ', MAX_TENTATIVES - tentative, ' tentative(s)');
    end;
  until (code = CODE_SECRET) or (tentative >= MAX_TENTATIVES);

  if code = CODE_SECRET then
    WriteLn('✓ Accès autorisé')
  else
    WriteLn('✗ Accès refusé - Trop de tentatives');
end.
