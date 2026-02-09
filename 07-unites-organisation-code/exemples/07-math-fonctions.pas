{ ============================================================================
  Section 7.7 : Unites standard du RTL
  Description : Demonstration des fonctions mathematiques (System et Math)
  Fichier source : 07-unites-standard-rtl.md
  ============================================================================ }
program MathFonctions;

uses
  Math;

var
  x: Real;

begin
  // Fonctions integrees (System) - pas besoin de "uses Math"
  x := Sqrt(16);
  WriteLn('Sqrt(16) = ', x:0:1);           // 4.0
  x := Sqr(5);
  WriteLn('Sqr(5) = ', x:0:1);             // 25.0
  x := Abs(-5.7);
  WriteLn('Abs(-5.7) = ', x:0:1);          // 5.7

  // Fonctions de l'unite Math
  x := Power(2, 3);
  WriteLn('Power(2,3) = ', x:0:1);         // 8.0

  // Fonctions d'arrondi (Math)
  x := Ceil(3.2);
  WriteLn('Ceil(3.2) = ', x:0:1);          // 4.0
  x := Floor(3.8);
  WriteLn('Floor(3.8) = ', x:0:1);         // 3.0

  // Maximum et minimum (Math)
  x := Max(10, 20);
  WriteLn('Max(10,20) = ', x:0:1);         // 20.0
  x := Min(10, 20);
  WriteLn('Min(10,20) = ', x:0:1);         // 10.0

  // Logarithme base 10 (Math)
  x := Log10(100);
  WriteLn('Log10(100) = ', x:0:1);         // 2.0
end.
