{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Acces securise a un tableau avec verification des indices
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program AccesTableauSecurise;
const
  TAILLE = 10;
var
  tableau: array[1..TAILLE] of Integer;
  i, indice: Integer;
begin
  // Remplissage
  for i := 1 to TAILLE do
    tableau[i] := i * 10;

  WriteLn('Tableau de ', TAILLE, ' éléments');
  Write('Entrez l''indice à afficher (1-', TAILLE, ') : ');
  ReadLn(indice);

  if (indice < 1) or (indice > TAILLE) then
  begin
    WriteLn('ERREUR : Indice hors limites !');
    WriteLn('Les indices valides sont de 1 à ', TAILLE);
  end
  else
    WriteLn('Valeur à l''indice ', indice, ' : ', tableau[indice]);
end.
