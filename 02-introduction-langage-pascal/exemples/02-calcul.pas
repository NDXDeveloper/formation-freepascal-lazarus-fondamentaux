{ ============================================================================
  Section 2.2 : Structure d'un programme Pascal
  Description : Calcul de la surface d'un cercle avec une constante PI
  Fichier source : 02-structure-programme-pascal.md
  ============================================================================ }
program Calcul;
const
  PI = 3.14159;
var
  rayon: real;
  surface: real;
begin
  rayon := 5.0;
  surface := PI * rayon * rayon;
  // :0:2 formate le nombre reel : 0 = largeur minimale, 2 = decimales
  writeln('Surface du cercle : ', surface:0:2);
end.
