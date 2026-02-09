{ ============================================================================
  Section 5.10 : Definition de types personnalises (Type)
  Description : Systeme de commandes complet avec types personnalises combines
  Fichier source : 10-definition-types-personnalises.md
  ============================================================================ }
program SystemeCommandes;
type
  // Énumérés
  TCategorieProduit = (Electronique, Vetement, Alimentaire, Livre);
  TEtatCommande = (EnAttente, Validee, EnPreparation, Expediee, Livree);
  TModePaiement = (CarteBancaire, Virement, Especes, Cheque);

  // Intervalles
  TQuantite = 1..999;
  TRemise = 0..100;  // Pourcentage

  // Structures de base
  TPrix = record
    montant: Real;
    devise: String;
  end;

  TProduit = record
    reference: String;
    designation: String;
    categorie: TCategorieProduit;
    prixUnitaire: TPrix;
    stock: Integer;
  end;

  TLigneCommande = record
    produit: TProduit;
    quantite: TQuantite;
    remise: TRemise;
  end;

  TAdresse = record
    numero: String;
    rue: String;
    codePostal: String;
    ville: String;
    pays: String;
  end;

  TClient = record
    nom: String;
    prenom: String;
    email: String;
    telephone: String;
    adresse: TAdresse;
  end;

  TCommande = record
    numero: String;
    client: TClient;
    lignes: array[1..20] of TLigneCommande;
    nbLignes: Integer;
    etat: TEtatCommande;
    modePaiement: TModePaiement;
    dateCommande: String;
  end;

function CalculerTotalLigne(ligne: TLigneCommande): Real;
var
  total, montantRemise: Real;
begin
  total := ligne.produit.prixUnitaire.montant * ligne.quantite;
  montantRemise := total * ligne.remise / 100;
  CalculerTotalLigne := total - montantRemise;
end;

function CalculerTotalCommande(commande: TCommande): Real;
var
  i: Integer;
  total: Real;
begin
  total := 0;
  for i := 1 to commande.nbLignes do
    total := total + CalculerTotalLigne(commande.lignes[i]);
  CalculerTotalCommande := total;
end;

procedure AfficherCommande(commande: TCommande);
var
  i: Integer;
begin
  WriteLn('=== COMMANDE N° ', commande.numero, ' ===');
  WriteLn('Client : ', commande.client.prenom, ' ', commande.client.nom);
  WriteLn('Email : ', commande.client.email);
  WriteLn;
  WriteLn('Articles :');

  for i := 1 to commande.nbLignes do
  begin
    with commande.lignes[i] do
    begin
      WriteLn('  ', produit.designation);
      WriteLn('    Quantité : ', quantite);
      WriteLn('    Prix unitaire : ', produit.prixUnitaire.montant:0:2, ' ',
              produit.prixUnitaire.devise);
      if remise > 0 then
        WriteLn('    Remise : ', remise, '%');
      WriteLn('    Total : ', CalculerTotalLigne(commande.lignes[i]):0:2, ' ',
              produit.prixUnitaire.devise);
    end;
    WriteLn;
  end;

  WriteLn('TOTAL COMMANDE : ', CalculerTotalCommande(commande):0:2, ' ',
          commande.lignes[1].produit.prixUnitaire.devise);

  Write('État : ');
  case commande.etat of
    EnAttente: WriteLn('En attente');
    Validee: WriteLn('Validée');
    EnPreparation: WriteLn('En préparation');
    Expediee: WriteLn('Expédiée');
    Livree: WriteLn('Livrée');
  end;
end;

var
  commande: TCommande;
begin
  // Initialisation d'une commande exemple
  commande.numero := 'CMD-2025-001';
  commande.dateCommande := '12/10/2025';
  commande.etat := EnPreparation;
  commande.modePaiement := CarteBancaire;

  // Client
  with commande.client do
  begin
    nom := 'Dupont';
    prenom := 'Marie';
    email := 'marie.dupont@example.com';
    telephone := '0123456789';
    adresse.rue := '10 rue de la Paix';
    adresse.codePostal := '75001';
    adresse.ville := 'Paris';
    adresse.pays := 'France';
  end;

  // Ligne 1
  commande.nbLignes := 1;
  with commande.lignes[1] do
  begin
    produit.reference := 'PROD-001';
    produit.designation := 'Ordinateur portable';
    produit.categorie := Electronique;
    produit.prixUnitaire.montant := 899.99;
    produit.prixUnitaire.devise := 'EUR';
    quantite := 1;
    remise := 10;
  end;

  AfficherCommande(commande);
end.
