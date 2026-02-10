{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Gestion de compte bancaire avec depot, retrait et decouvert
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program CompteBancaire;  
var  
  solde, montant: Real;
  operation: Char;
  continuer: Boolean;
const
  SOLDE_MIN = 0.0;
  DECOUVERT_MAX = 500.0;
begin
  solde := 1000.0;
  continuer := True;

  WriteLn('═══════════════════════════════');
  WriteLn('   GESTION COMPTE BANCAIRE');
  WriteLn('═══════════════════════════════');
  WriteLn;

  while continuer do
  begin
    WriteLn('Solde actuel : ', solde:0:2, ' €');

    // Avertissements
    if solde < 0 then
      WriteLn('⚠️  Compte à découvert')
    else if solde < 100 then
      WriteLn('⚠️  Solde faible');

    WriteLn;
    WriteLn('D - Déposer');
    WriteLn('R - Retirer');
    WriteLn('Q - Quitter');
    Write('Opération : ');
    ReadLn(operation);
    WriteLn;

    case UpCase(operation) of  // UpCase convertit un Char en majuscule (un seul caractère)
      'D':
        begin
          repeat
            Write('Montant à déposer : ');
            ReadLn(montant);

            if montant <= 0 then
              WriteLn('❌ Le montant doit être positif')
            else
            begin
              solde := solde + montant;
              WriteLn('✓ Dépôt effectué');
              WriteLn('  Nouveau solde : ', solde:0:2, ' €');
            end;
          until montant > 0;
        end;

      'R':
        begin
          repeat
            Write('Montant à retirer : ');
            ReadLn(montant);

            if montant <= 0 then
              WriteLn('❌ Le montant doit être positif')
            else if (solde - montant) < -DECOUVERT_MAX then
            begin
              WriteLn('❌ Opération refusée');
              WriteLn('   Découvert maximum : ', DECOUVERT_MAX:0:2, ' €');
              WriteLn('   Solde disponible : ', (solde + DECOUVERT_MAX):0:2, ' €');
            end;
          until (montant > 0) and ((solde - montant) >= -DECOUVERT_MAX);

          solde := solde - montant;
          WriteLn('✓ Retrait effectué');
          WriteLn('  Nouveau solde : ', solde:0:2, ' €');

          if solde < 0 then
            WriteLn('  ⚠️  Vous êtes maintenant à découvert');
        end;

      'Q':
        begin
          WriteLn('═══════════════════════════════');
          WriteLn('Solde final : ', solde:0:2, ' €');
          WriteLn('Merci de votre visite !');
          WriteLn('═══════════════════════════════');
          continuer := False;
        end;

    else
      WriteLn('❌ Opération invalide');
    end;

    WriteLn;
  end;
end.
