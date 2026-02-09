{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Test d'appartenance avec l'operateur IN
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program TestAppartenance;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  joursOuvres: TJours;
begin
  joursOuvres := [Lundi, Mardi, Mercredi, Jeudi, Vendredi];

  // Test d'appartenance
  if Lundi in joursOuvres then
    WriteLn('Lundi est un jour ouvré');

  if not (Samedi in joursOuvres) then
    WriteLn('Samedi n''est pas un jour ouvré');

  // Équivalent à plusieurs OR
  // Au lieu de : if (jour = Lundi) or (jour = Mardi) or (jour = Mercredi)...
  if Mercredi in joursOuvres then
    WriteLn('Mercredi est un jour ouvré');
end.
