{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Controle du nombre de decimales affichees (syntaxe :0:N)
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program FormateDecimales;
var
  pi: real;
begin
  pi := 3.14159265358979;

  WriteLn('Sans formatage    : ', pi);
  WriteLn('0 décimale        : ', pi:0:0);    // 3
  WriteLn('1 décimale        : ', pi:0:1);    // 3.1
  WriteLn('2 décimales       : ', pi:0:2);    // 3.14
  WriteLn('4 décimales       : ', pi:0:4);    // 3.1416 (arrondi)
  WriteLn('10 décimales      : ', pi:0:10);   // 3.1415926536
end.
