{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Traitement de commandes avec filtrage des annulees via continue
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program TraitementCommandes;  
type  
  TCommande = record
    numero: Integer;
    montant: Real;
    statut: String;
  end;
var
  commandes: array[1..5] of TCommande;
  i: Integer;
  totalValide: Real;
begin
  // Initialisation des commandes
  commandes[1].numero := 101; commandes[1].montant := 150.50; commandes[1].statut := 'valide';
  commandes[2].numero := 102; commandes[2].montant := 75.00; commandes[2].statut := 'annulee';
  commandes[3].numero := 103; commandes[3].montant := 200.00; commandes[3].statut := 'valide';
  commandes[4].numero := 104; commandes[4].montant := 50.00; commandes[4].statut := 'annulee';
  commandes[5].numero := 105; commandes[5].montant := 300.00; commandes[5].statut := 'valide';

  totalValide := 0;

  WriteLn('=== TRAITEMENT DES COMMANDES ===');
  WriteLn;

  for i := 1 to 5 do
  begin
    Write('Commande #', commandes[i].numero, ' - ');

    // Ignorer les commandes annulées
    if commandes[i].statut = 'annulee' then
    begin
      WriteLn('ANNULÉE (ignorée)');
      continue;  // Passe à la commande suivante
    end;

    // Traiter les commandes valides
    WriteLn('Montant : ', commandes[i].montant:0:2, ' € - TRAITÉE');
    totalValide := totalValide + commandes[i].montant;
  end;

  WriteLn;
  WriteLn('Total des commandes valides : ', totalValide:0:2, ' €');
end.
