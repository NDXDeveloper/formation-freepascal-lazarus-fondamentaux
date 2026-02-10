{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Type enumere dans un enregistrement (commande avec etat)
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program EnumDansRecord;  
type  
  TEtatCommande = (EnAttente, EnPreparation, Expediee, Livree, Annulee);

  TCommande = record
    numero: String;
    client: String;
    montant: Real;
    etat: TEtatCommande;  // Type énuméré dans l'enregistrement
  end;

var
  commande: TCommande;
begin
  commande.numero := 'CMD-001';
  commande.client := 'Dupont';
  commande.montant := 150.00;
  commande.etat := EnPreparation;

  WriteLn('Commande ', commande.numero);
  WriteLn('Client : ', commande.client);
  WriteLn('Montant : ', commande.montant:0:2, ' €');

  case commande.etat of
    EnAttente:      WriteLn('État : En attente');
    EnPreparation:  WriteLn('État : En préparation');
    Expediee:       WriteLn('État : Expédiée');
    Livree:         WriteLn('État : Livrée');
    Annulee:        WriteLn('État : Annulée');
  end;
end.
