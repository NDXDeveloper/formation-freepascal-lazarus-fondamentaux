{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Operation d'union (+) sur les ensembles
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program Union;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  semaine1, semaine2, total: TJours;
begin
  semaine1 := [Lundi, Mardi];
  semaine2 := [Mercredi, Jeudi];

  // Union : tous les éléments des deux ensembles
  total := semaine1 + semaine2;
  // total = [Lundi, Mardi, Mercredi, Jeudi]

  if Mercredi in total then
    WriteLn('Mercredi est dans l''union');
end.
