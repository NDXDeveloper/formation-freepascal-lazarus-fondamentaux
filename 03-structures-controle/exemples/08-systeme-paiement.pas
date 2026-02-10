{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Systeme de paiement avec validation du montant et du rendu
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program SystemePaiement;  
var  
  montant, montantPaye, rendu: Real;
  valide: Boolean;
begin
  WriteLn('═══════════════════════════');
  WriteLn('   SYSTÈME DE PAIEMENT');
  WriteLn('═══════════════════════════');
  WriteLn;

  // Montant de l'achat
  repeat
    Write('Montant de l''achat : ');
    ReadLn(montant);

    if montant <= 0 then
      WriteLn('❌ Le montant doit être positif')
    else if montant > 10000 then
      WriteLn('❌ Montant trop élevé (max 10 000 €)');
  until (montant > 0) and (montant <= 10000);

  WriteLn;
  WriteLn('Montant à payer : ', montant:0:2, ' €');
  WriteLn;

  // Montant payé
  repeat
    Write('Montant payé : ');
    ReadLn(montantPaye);
    valide := True;

    if montantPaye < 0 then
    begin
      WriteLn('❌ Le montant ne peut pas être négatif');
      valide := False;
    end
    else if montantPaye < montant then
    begin
      WriteLn('❌ Montant insuffisant');
      WriteLn('   Il manque ', (montant - montantPaye):0:2, ' €');
      valide := False;
    end;
  until valide;

  // Calcul du rendu
  rendu := montantPaye - montant;

  WriteLn;
  WriteLn('═══════════════════════════');
  WriteLn('Montant payé : ', montantPaye:0:2, ' €');
  if rendu > 0 then
    WriteLn('Rendu : ', rendu:0:2, ' €')
  else
    WriteLn('Paiement exact');
  WriteLn('✓ Transaction terminée');
  WriteLn('═══════════════════════════');
end.
