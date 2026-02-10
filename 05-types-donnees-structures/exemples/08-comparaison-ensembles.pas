{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Comparaison d'ensembles (egalite, inclusion)
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program ComparaisonEnsembles;  
type  
  TChiffres = set of 0..9;

var
  ensemble1, ensemble2: TChiffres;
begin
  ensemble1 := [1, 2, 3];
  ensemble2 := [1, 2, 3];

  // Égalité
  if ensemble1 = ensemble2 then
    WriteLn('Les ensembles sont identiques');

  // Différence
  ensemble2 := [1, 2, 4];
  if ensemble1 <> ensemble2 then
    WriteLn('Les ensembles sont différents');

  // Inclusion (sous-ensemble)
  ensemble1 := [1, 2];
  ensemble2 := [1, 2, 3, 4];
  if ensemble1 <= ensemble2 then
    WriteLn('[1,2] est inclus dans [1,2,3,4]');

  // Inclusion stricte (pas d'opérateur < pour les sets en FreePascal)
  if (ensemble1 <= ensemble2) and (ensemble1 <> ensemble2) then
    WriteLn('[1,2] est strictement inclus dans [1,2,3,4]');
end.
