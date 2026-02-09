{ ============================================================================
  Section 7.3 : Clauses Uses et dependances
  Description : Demonstration de l'ordre des unites dans uses (conflit de noms)
  Fichier source : 03-clauses-uses-dependances.md
  ============================================================================ }
program TestOrdre;

uses
  UniteA, UniteB;  // UniteB est en dernier

begin
  Afficher;           // Affichera "Version B" (dernière unité)
  UniteA.Afficher;    // Affichera "Version A" (appel explicite)
end.
