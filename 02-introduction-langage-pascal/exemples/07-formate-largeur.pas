{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Controle de la largeur d'affichage (alignement a droite)
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program FormateLargeur;
var
  prix: real;
begin
  prix := 19.99;

  WriteLn('|', prix:0:2, '|');      // |19.99|
  WriteLn('|', prix:8:2, '|');      // |   19.99|
  WriteLn('|', prix:10:2, '|');     // |     19.99|
  WriteLn('|', prix:15:2, '|');     // |          19.99|
end.
