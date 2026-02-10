{ ============================================================================
  Section 5.9 : Types intervalle
  Description : Tableau indexe par un type intervalle (temperatures par jour)
  Fichier source : 09-types-intervalle.md
  ============================================================================ }
program TableauIntervalle;  
type  
  TJourMois = 1..31;
  TMois = 1..12;

var
  temperaturesJour: array[TJourMois] of Real;
  preciptationsMois: array[TMois] of Real;
  jour: TJourMois;
  mois: TMois;
begin
  // Saisie des températures du mois
  for jour := 1 to 31 do
  begin
    Write('Température du jour ', jour, ' : ');
    ReadLn(temperaturesJour[jour]);
  end;

  // Les indices sont automatiquement restreints
  // temperaturesJour[35] := 20;  // Erreur à la compilation
end.
