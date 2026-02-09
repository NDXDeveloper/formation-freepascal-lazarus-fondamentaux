{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Formatage de nombres entiers avec largeur variable
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program FormateEntiers;
var
  nombre: integer;
begin
  nombre := 42;

  WriteLn('|', nombre, '|');        // |42|
  WriteLn('|', nombre:5, '|');      // |   42|
  WriteLn('|', nombre:10, '|');     // |        42|
  WriteLn('|', nombre:2, '|');      // |42| (largeur minimum respectée)
  WriteLn('|', nombre:1, '|');      // |42| (la largeur s'adapte)
end.
