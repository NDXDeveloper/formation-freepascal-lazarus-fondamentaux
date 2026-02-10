{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Operation de difference (-) sur les ensembles
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program Difference;  
type  
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  tousLesJours, weekend, joursOuvres: TJours;
begin
  tousLesJours := [Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche];
  weekend := [Samedi, Dimanche];

  // Différence : éléments du premier SAUF ceux du second
  joursOuvres := tousLesJours - weekend;
  // joursOuvres = [Lundi, Mardi, Mercredi, Jeudi, Vendredi]

  if Lundi in joursOuvres then
    WriteLn('Lundi est un jour ouvré');

  if not (Samedi in joursOuvres) then
    WriteLn('Samedi n''est pas un jour ouvré');
end.
