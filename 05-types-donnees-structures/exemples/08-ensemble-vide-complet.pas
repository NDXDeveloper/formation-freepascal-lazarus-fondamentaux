{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Ensemble vide et ensemble complet
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program EnsembleVideComplet;  
type  
  TChiffres = set of 0..9;

var
  vide: TChiffres;
  complet: TChiffres;
begin
  // Ensemble vide
  vide := [];
  if vide = [] then
    WriteLn('L''ensemble est vide');

  // Ensemble complet
  complet := [0..9];  // Tous les chiffres de 0 à 9
  if 5 in complet then
    WriteLn('5 est dans l''ensemble complet');
end.
