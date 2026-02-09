{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Affichage direct du resultat de calculs dans WriteLn
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program AfficherCalculs;
var
  a, b: integer;
begin
  a := 10;
  b := 5;

  WriteLn('Somme : ', a + b);
  WriteLn('Différence : ', a - b);
  WriteLn('Produit : ', a * b);
  WriteLn('Quotient : ', a / b);    // / entre entiers donne un real (notation scientifique)
end.
