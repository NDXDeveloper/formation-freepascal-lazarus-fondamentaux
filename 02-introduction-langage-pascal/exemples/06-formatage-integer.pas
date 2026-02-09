{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Formatage de nombres entiers avec controle de la largeur
                d'affichage (syntaxe :largeur)
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program FormatageInteger;
var
  nombre: integer;
begin
  nombre := 42;

  WriteLn(nombre);        // 42
  WriteLn(nombre:5);      //    42 (largeur 5, aligné à droite)
  WriteLn(nombre:10);     //         42 (largeur 10)
end.
