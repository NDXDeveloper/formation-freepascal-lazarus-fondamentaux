{ ============================================================================
  Section 5.4 : Enregistrements (Records)
  Description : Gestion de stock d'articles avec ajout et retrait
  Fichier source : 04-enregistrements-records.md
  ============================================================================ }
program GestionStock;
type
  TArticle = record
    code: String;
    designation: String;
    prixUnitaire: Real;
    quantiteStock: Integer;
  end;

procedure AjouterStock(var art: TArticle; quantite: Integer);
begin
  art.quantiteStock := art.quantiteStock + quantite;
  WriteLn('Stock ajouté. Nouveau stock : ', art.quantiteStock);
end;

function RetirerStock(var art: TArticle; quantite: Integer): Boolean;
begin
  if art.quantiteStock >= quantite then
  begin
    art.quantiteStock := art.quantiteStock - quantite;
    RetirerStock := True;
  end
  else
  begin
    WriteLn('Stock insuffisant !');
    RetirerStock := False;
  end;
end;

function ValeurStock(art: TArticle): Real;
begin
  ValeurStock := art.prixUnitaire * art.quantiteStock;
end;

var
  article: TArticle;
begin
  // Initialisation
  article.code := 'ART001';
  article.designation := 'Stylo bleu';
  article.prixUnitaire := 1.50;
  article.quantiteStock := 100;

  WriteLn('Article : ', article.designation);
  WriteLn('Prix unitaire : ', article.prixUnitaire:0:2, ' €');
  WriteLn('Stock initial : ', article.quantiteStock);
  WriteLn('Valeur du stock : ', ValeurStock(article):0:2, ' €');
  WriteLn;

  // Opérations
  AjouterStock(article, 50);

  if RetirerStock(article, 30) then
    WriteLn('Vente effectuée');

  WriteLn('Stock final : ', article.quantiteStock);
  WriteLn('Valeur finale : ', ValeurStock(article):0:2, ' €');
end.
