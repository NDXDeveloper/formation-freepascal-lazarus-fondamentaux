{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Suppression d'element avec Exclude et operateur -
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program RetirerElement;
type
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  jours: TJours;
begin
  jours := [Lundi, Mardi, Mercredi];
  WriteLn('Départ : Lundi, Mardi et Mercredi');

  // Retirer Mardi
  Exclude(jours, Mardi);

  if not (Mardi in jours) then
    WriteLn('Mardi a été retiré');

  // Alternative avec -
  jours := jours - [Mercredi];
end.
