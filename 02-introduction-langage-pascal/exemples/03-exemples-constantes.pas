{ ============================================================================
  Section 2.3 : Variables et constantes
  Description : Declaration de constantes et calcul de surface d'un cercle
  Fichier source : 03-variables-constantes.md
  ============================================================================ }
program ExemplesConstantes;

// Les constantes utilisent = (pas :=) et le type est deduit automatiquement
const
  PI = 3.14159;            // -> real
  TauxTVA = 20;            // -> integer
  NomEntreprise = 'Ma Société';  // -> string
  JoursParSemaine = 7;
  EstDebugMode = false;    // -> boolean

var
  rayon: real;
  surface: real;
begin
  rayon := 5.0;
  surface := PI * rayon * rayon;
  writeln('Surface du cercle : ', surface:0:2);
end.
