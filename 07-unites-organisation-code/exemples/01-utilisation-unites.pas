{ ============================================================================
  Section 7.1 : Concept d'unite en Pascal
  Description : Utilisation des unites standard SysUtils et Math
  Fichier source : 01-concept-unite-pascal.md
  ============================================================================ }
program Exemple;

uses
  SysUtils,  // Unité pour les fonctions système
  Math;      // Unité pour les fonctions mathématiques

var
  resultat: Real;

begin
  resultat := Power(2, 10);  // Power vient de l'unité Math
  WriteLn(resultat:0:0);     // Affiche 1024
end.
