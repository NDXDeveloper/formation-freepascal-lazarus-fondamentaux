{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Distributeur automatique avec PIN, depot et retrait
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program GestionCompteClient;  
var  
  solde, montant: Real;
  choix: Integer;
  continuer: Boolean;
  tentatives: Integer;
  codePin, codeCorrect: String;
begin
  continuer := True;
  codeCorrect := '1234';
  solde := 1000.00;
  WriteLn('================================');
  WriteLn('   DISTRIBUTEUR AUTOMATIQUE   ');
  WriteLn('================================');
  WriteLn;
  tentatives := 0;
  repeat
    tentatives := tentatives + 1;
    Write('Entrez votre code PIN : ');
    ReadLn(codePin);
    if codePin <> codeCorrect then
    begin
      WriteLn('Code incorrect !');
      if tentatives >= 3 then
      begin
        WriteLn('Carte bloquée. Contactez votre banque.');
        continuer := False;
      end;
    end;
  until (codePin = codeCorrect) or (tentatives >= 3);
  while continuer and (codePin = codeCorrect) do
  begin
    WriteLn;
    WriteLn('================================');
    WriteLn('Solde actuel : ', solde:0:2, ' euros');
    WriteLn('================================');
    WriteLn('1. Consulter le solde');
    WriteLn('2. Retirer de l''argent');
    WriteLn('3. Déposer de l''argent');
    WriteLn('4. Quitter');
    WriteLn;
    Write('Votre choix : ');
    ReadLn(choix);
    WriteLn;
    case choix of
      1:
        begin
          WriteLn('--- CONSULTATION ---');
          WriteLn('Votre solde est de : ', solde:0:2, ' euros');
        end;
      2:
        begin
          WriteLn('--- RETRAIT ---');
          repeat
            Write('Montant à retirer : ');
            ReadLn(montant);
            if montant <= 0 then
              WriteLn('Le montant doit être positif !')
            else if montant > solde then
              WriteLn('Solde insuffisant ! (Solde : ', solde:0:2, ' euros)');
          until (montant > 0) and (montant <= solde);
          solde := solde - montant;
          WriteLn('Retrait effectué avec succès !');
          WriteLn('Nouveau solde : ', solde:0:2, ' euros');
        end;
      3:
        begin
          WriteLn('--- DÉPÔT ---');
          repeat
            Write('Montant à déposer : ');
            ReadLn(montant);
            if montant <= 0 then
              WriteLn('Le montant doit être positif !')
            else
            begin
              solde := solde + montant;
              WriteLn('Dépôt effectué avec succès !');
              WriteLn('Nouveau solde : ', solde:0:2, ' euros');
            end;
          until montant > 0;
        end;
      4:
        begin
          WriteLn('Merci d''avoir utilisé nos services.');
          WriteLn('Au revoir !');
          continuer := False;
        end;
    else
      WriteLn('Choix invalide. Veuillez réessayer.');
    end;
  end;
  WriteLn;
  WriteLn('================================');
end.
