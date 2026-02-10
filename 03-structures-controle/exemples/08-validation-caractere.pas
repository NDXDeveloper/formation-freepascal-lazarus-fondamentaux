{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation d'un seul caractere - reponse O/N
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationCaractere;  
var  
  reponse: Char;
begin
  repeat
    Write('Continuer ? (O/N) : ');
    ReadLn(reponse);
    reponse := UpCase(reponse);

    if not (reponse in ['O', 'N']) then
      WriteLn('❌ Répondez par O (Oui) ou N (Non)');
  until reponse in ['O', 'N'];

  if reponse = 'O' then
    WriteLn('✓ Continuation...')
  else
    WriteLn('✓ Arrêt');
end.
