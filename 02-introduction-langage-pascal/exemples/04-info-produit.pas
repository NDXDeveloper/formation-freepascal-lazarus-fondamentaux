{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Fiche produit utilisant tous les types primitifs
                (string, real, integer, boolean, char) avec calcul TTC
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program InfoProduit;
const
  TauxTVA = 20.0;  // const = valeur fixe, non modifiable
var
  nomProduit: string;
  prixHT: real;
  quantite: integer;
  enStock: boolean;
  categorie: char;
  prixTTC: real;
  montantTotal: real;
begin
  nomProduit := 'Clavier sans fil';
  prixHT := 35.50;
  quantite := 5;
  enStock := true;
  categorie := 'I';  // I pour Informatique

  prixTTC := prixHT * (1 + TauxTVA / 100);
  montantTotal := prixTTC * quantite;

  writeln('=== FICHE PRODUIT ===');
  writeln('Produit : ', nomProduit);
  writeln('Catégorie : ', categorie);
  writeln('Prix HT : ', prixHT:0:2, ' €');
  writeln('Prix TTC : ', prixTTC:0:2, ' €');
  writeln('Quantité : ', quantite);
  writeln('En stock : ', enStock);
  writeln('Montant total : ', montantTotal:0:2, ' €');
end.
