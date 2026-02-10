{ ============================================================================
  Section 5.8 : Types ensemble (Set)
  Description : Ajout d'element avec Include et operateur +
  Fichier source : 08-types-ensemble-set.md
  ============================================================================ }
program AjouterElement;  
type  
  TJour = (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
  TJours = set of TJour;

var
  jours: TJours;
begin
  jours := [Lundi, Mardi];
  WriteLn('Départ : Lundi et Mardi');

  // Ajouter Mercredi
  Include(jours, Mercredi);

  if Mercredi in jours then
    WriteLn('Mercredi a été ajouté');

  // Alternative avec +
  jours := jours + [Jeudi];
end.
