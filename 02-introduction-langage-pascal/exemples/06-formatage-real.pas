{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Formatage de nombres reels avec controle des decimales
                et de la largeur d'affichage (syntaxe :largeur:decimales)
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program FormatageReal;  
var  
  prix: real;
begin
  prix := 19.99;

  WriteLn(prix);          // Notation scientifique
  WriteLn(prix:0:2);      // 19.99 (2 décimales)
  WriteLn(prix:10:2);     //      19.99 (largeur 10, aligné à droite)
  WriteLn(prix:0:0);      // 20 (arrondi, pas de décimales)
  WriteLn(prix:0:5);      // 19.99000 (5 décimales)
end.
