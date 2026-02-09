{ ============================================================================
  Section 2.5 : Operateurs arithmetiques et logiques
  Description : Evaluation d'expressions arithmetiques complexes avec
                priorite des operateurs
  Fichier source : 05-operateurs-arithmetiques-logiques.md
  ============================================================================ }
program ExpressionsComplexes;
var
  a, b, c, resultat: integer;
begin
  a := 10;
  b := 5;
  c := 2;

  // Expression complexe
  resultat := (a + b) * c - (a div b);
  // Étapes :
  // 1. (10 + 5) = 15
  // 2. (10 div 5) = 2
  // 3. 15 * 2 = 30
  // 4. 30 - 2 = 28

  writeln('Résultat : ', resultat);  // 28
end.
