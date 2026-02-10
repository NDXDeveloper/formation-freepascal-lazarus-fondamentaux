{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Formatage de prix avec calcul HT, TVA et TTC
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program FormatPrix;  
var  
  prixHT, tva, prixTTC: real;
begin
  prixHT := 100.0;
  tva := prixHT * 0.20;
  prixTTC := prixHT + tva;

  WriteLn('Prix HT    : ', prixHT:8:2, ' €');
  WriteLn('TVA (20%)  : ', tva:8:2, ' €');
  WriteLn('Prix TTC   : ', prixTTC:8:2, ' €');
end.
