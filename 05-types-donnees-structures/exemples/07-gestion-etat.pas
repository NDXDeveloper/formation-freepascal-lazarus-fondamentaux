{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Actions selon l'etat d'une commande avec type enumere
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program GestionEtat;  
type  
  TEtatCommande = (EnAttente, EnPreparation, Expediee, Livree, Annulee);

var
  etat: TEtatCommande;

procedure TraiterCommande(e: TEtatCommande);  
begin  
  case e of
    EnAttente:
      WriteLn('Commande en attente de validation');
    EnPreparation:
      WriteLn('Préparation en cours...');
    Expediee:
      WriteLn('Commande expédiée, livraison prévue sous 48h');
    Livree:
      WriteLn('Commande livrée avec succès');
    Annulee:
      WriteLn('Commande annulée');
  end;
end;

begin
  etat := EnPreparation;
  TraiterCommande(etat);

  etat := Expediee;
  TraiterCommande(etat);
end.
