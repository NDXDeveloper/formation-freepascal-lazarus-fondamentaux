{ ============================================================================
  Section 2.3 : Variables et constantes
  Description : Calcul de la circonference et de la surface d'un cercle
  Fichier source : 03-variables-constantes.md
  ============================================================================ }
program CalculSurface;

const
  PI = 3.14159;

var
  rayon: real;
  circonference: real;
  surface: real;

begin
  // Initialisation
  rayon := 7.5;

  // Calculs
  circonference := 2 * PI * rayon;
  surface := PI * rayon * rayon;

  // Affichage
  writeln('=== Cercle ===');
  writeln('Rayon : ', rayon:0:2, ' cm');
  writeln('Circonférence : ', circonference:0:2, ' cm');
  writeln('Surface : ', surface:0:2, ' cm²');
end.
