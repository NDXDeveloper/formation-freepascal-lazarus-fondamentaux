{ ============================================================================
  Section 5.5 : Enregistrements imbriques
  Description : Commande avec client et article imbriques
  Fichier source : 05-enregistrements-imbriques.md
  ============================================================================ }
program GestionCommande;
type
  TDate = record
    jour: Integer;
    mois: Integer;
    annee: Integer;
  end;

  TClient = record
    nom: String;
    email: String;
  end;

  TArticle = record
    reference: String;
    designation: String;
    prixUnitaire: Real;
    quantite: Integer;
  end;

  TCommande = record
    numero: String;
    dateCommande: TDate;
    client: TClient;
    article: TArticle;
  end;

function MontantTotal(cmd: TCommande): Real;
begin
  MontantTotal := cmd.article.prixUnitaire * cmd.article.quantite;
end;

procedure AfficherCommande(cmd: TCommande);
begin
  WriteLn('===== COMMANDE N° ', cmd.numero, ' =====');
  WriteLn('Date : ', cmd.dateCommande.jour, '/',
          cmd.dateCommande.mois, '/', cmd.dateCommande.annee);
  WriteLn('Client : ', cmd.client.nom);
  WriteLn('Email : ', cmd.client.email);
  WriteLn;
  WriteLn('Article : ', cmd.article.designation);
  WriteLn('Référence : ', cmd.article.reference);
  WriteLn('Prix unitaire : ', cmd.article.prixUnitaire:0:2, ' €');
  WriteLn('Quantité : ', cmd.article.quantite);
  WriteLn;
  WriteLn('TOTAL : ', MontantTotal(cmd):0:2, ' €');
  WriteLn('=====================================');
end;

var
  commande: TCommande;
begin
  // Initialisation de la commande
  commande.numero := 'CMD-2025-001';

  with commande.dateCommande do
  begin
    jour := 15;
    mois := 3;
    annee := 2025;
  end;

  with commande.client do
  begin
    nom := 'Entreprise ABC';
    email := 'contact@abc.fr';
  end;

  with commande.article do
  begin
    reference := 'REF-123';
    designation := 'Clavier mécanique';
    prixUnitaire := 89.99;
    quantite := 5;
  end;

  AfficherCommande(commande);
end.
