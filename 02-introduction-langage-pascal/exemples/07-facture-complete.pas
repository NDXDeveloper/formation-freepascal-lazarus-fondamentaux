{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Facture complete avec articles, quantites, prix unitaires,
                sous-total HT, TVA et total TTC
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program FactureComplete;
const
  TauxTVA = 20.0;
  LargeurNom = 25;
  LargeurQte = 8;
  LargeurPrix = 12;
var
  article1, article2, article3: string;
  prix1, prix2, prix3: real;
  qte1, qte2, qte3: integer;
  total1, total2, total3: real;
  sousTotal, montantTVA, totalTTC: real;
begin
  // Données
  article1 := 'Clavier mécanique';
  prix1 := 89.99;
  qte1 := 2;

  article2 := 'Souris ergonomique';
  prix2 := 45.50;
  qte2 := 1;

  article3 := 'Tapis de souris';
  prix3 := 12.99;
  qte3 := 3;

  // Calculs
  total1 := prix1 * qte1;
  total2 := prix2 * qte2;
  total3 := prix3 * qte3;

  sousTotal := total1 + total2 + total3;
  montantTVA := sousTotal * TauxTVA / 100;
  totalTTC := sousTotal + montantTVA;

  // Affichage
  WriteLn;
  WriteLn('==============================================');
  WriteLn('              FACTURE N° 2024-001');
  WriteLn('==============================================');
  WriteLn;
  // :-LargeurNom = largeur negative via constante, donc alignement a gauche
  WriteLn('Article':-LargeurNom, 'Qté':LargeurQte, 'P.U.':LargeurPrix, 'Total':LargeurPrix);
  WriteLn('------------------------':LargeurNom, '-------':LargeurQte,
          '-----------':LargeurPrix, '-----------':LargeurPrix);

  WriteLn(article1:-LargeurNom, qte1:LargeurQte, prix1:LargeurPrix:2, total1:LargeurPrix:2);
  WriteLn(article2:-LargeurNom, qte2:LargeurQte, prix2:LargeurPrix:2, total2:LargeurPrix:2);
  WriteLn(article3:-LargeurNom, qte3:LargeurQte, prix3:LargeurPrix:2, total3:LargeurPrix:2);

  WriteLn;
  WriteLn('Sous-total HT':LargeurNom + LargeurQte + LargeurPrix, sousTotal:LargeurPrix:2);
  WriteLn('TVA (20%)':LargeurNom + LargeurQte + LargeurPrix, montantTVA:LargeurPrix:2);
  WriteLn('============':LargeurNom + LargeurQte + LargeurPrix, '===========':LargeurPrix);
  WriteLn('TOTAL TTC':LargeurNom + LargeurQte + LargeurPrix, totalTTC:LargeurPrix:2, ' €');
  WriteLn;
  WriteLn('==============================================');
end.
