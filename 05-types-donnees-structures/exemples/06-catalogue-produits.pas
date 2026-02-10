{ ============================================================================
  Section 5.6 : Tableaux d'enregistrements
  Description : Catalogue de produits avec affichage, recherche et valeur du stock
  Fichier source : 06-tableaux-enregistrements.md
  ============================================================================ }
program CatalogueProduits;  
type  
  TProduit = record
    code: String;
    designation: String;
    prix: Real;
    stock: Integer;
  end;
  TCatalogue = array[1..100] of TProduit;

var
  produits: TCatalogue;
  nbProduits: Integer;
  i, indice: Integer;
  codeRecherche: String;
  trouve: Boolean;
  valeurTotale: Real;

begin
  // Pour l'exemple, initialisons quelques produits
  nbProduits := 3;

  produits[1].code := 'P001';
  produits[1].designation := 'Clavier mécanique';
  produits[1].prix := 89.99;
  produits[1].stock := 15;

  produits[2].code := 'P002';
  produits[2].designation := 'Souris optique';
  produits[2].prix := 25.50;
  produits[2].stock := 42;

  produits[3].code := 'P003';
  produits[3].designation := 'Écran 24 pouces';
  produits[3].prix := 199.00;
  produits[3].stock := 8;

  // Affichage du catalogue
  WriteLn('=== CATALOGUE ===');
  for i := 1 to nbProduits do
  begin
    WriteLn('Code : ', produits[i].code);
    WriteLn('Produit : ', produits[i].designation);
    WriteLn('Prix : ', produits[i].prix:0:2, ' €');
    WriteLn('Stock : ', produits[i].stock, ' unités');
    WriteLn('-----------------');
  end;

  // Recherche d'un produit
  Write('Code produit à rechercher : ');
  ReadLn(codeRecherche);

  trouve := False;
  for i := 1 to nbProduits do
  begin
    if produits[i].code = codeRecherche then
    begin
      WriteLn('Produit trouvé :');
      WriteLn('  ', produits[i].designation);
      WriteLn('  Prix : ', produits[i].prix:0:2, ' €');
      WriteLn('  Stock : ', produits[i].stock, ' unités');
      trouve := True;
      Break;
    end;
  end;

  if not trouve then
    WriteLn('Produit non trouvé');

  // Calcul de la valeur totale du stock
  valeurTotale := 0;
  for i := 1 to nbProduits do
    valeurTotale := valeurTotale + (produits[i].prix * produits[i].stock);

  WriteLn;
  WriteLn('Valeur totale du stock : ', valeurTotale:0:2, ' €');
end.
