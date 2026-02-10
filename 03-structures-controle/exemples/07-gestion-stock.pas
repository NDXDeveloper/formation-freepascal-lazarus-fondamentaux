{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Gestion de stock avec ajout, retrait et alertes
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program GestionStock;  
const  
  STOCK_INITIAL = 100;
  STOCK_MIN = 10;
var
  stock, quantite: Integer;
  operation: Char;
  continuer: Boolean;
begin
  stock := STOCK_INITIAL;
  continuer := True;

  WriteLn('═══ GESTION DE STOCK ═══');
  WriteLn('Stock initial : ', stock, ' unités');
  WriteLn;

  while continuer do
  begin
    WriteLn('Stock actuel : ', stock, ' unités');

    // Alerte stock faible
    if stock <= STOCK_MIN then
    begin
      WriteLn('⚠️  ALERTE : Stock faible !');
      WriteLn('   Recommandation : Réapprovisionner');
    end;

    WriteLn;
    WriteLn('A - Ajouter au stock');
    WriteLn('R - Retirer du stock');
    WriteLn('Q - Quitter');
    Write('Choix : ');
    ReadLn(operation);
    WriteLn;

    case UpCase(operation) of  // UpCase convertit un Char en majuscule (un seul caractère)
      'A':
        begin
          repeat
            Write('Quantité à ajouter : ');
            ReadLn(quantite);

            if quantite <= 0 then
              WriteLn('❌ La quantité doit être positive')
            else
            begin
              stock := stock + quantite;
              WriteLn('✓ ', quantite, ' unités ajoutées');
            end;
          until quantite > 0;
        end;

      'R':
        begin
          repeat
            Write('Quantité à retirer : ');
            ReadLn(quantite);

            if quantite <= 0 then
              WriteLn('❌ La quantité doit être positive')
            else if quantite > stock then
            begin
              WriteLn('❌ Stock insuffisant !');
              WriteLn('   Stock disponible : ', stock, ' unités');
            end;
          until (quantite > 0) and (quantite <= stock);

          stock := stock - quantite;
          WriteLn('✓ ', quantite, ' unités retirées');
        end;

      'Q':
        begin
          WriteLn('Stock final : ', stock, ' unités');
          continuer := False;
        end;

    else
      WriteLn('❌ Choix invalide');
    end;

    WriteLn;
  end;
end.
