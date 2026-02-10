{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Operation d'intersection (*) sur les ensembles
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program Intersection;  
type  
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  jours1, jours2, communs: TJours;
begin
  jours1 := [Lundi, Mardi, Mercredi];
  jours2 := [Mardi, Mercredi, Jeudi];

  // Intersection : éléments présents dans les DEUX ensembles
  communs := jours1 * jours2;
  // communs = [Mardi, Mercredi]

  if Mardi in communs then
    WriteLn('Mardi est dans les deux ensembles');

  if not (Lundi in communs) then
    WriteLn('Lundi n''est pas dans les deux ensembles');
end.
