{ ============================================================================
  Section 5.7 : Types enumeres
  Description : Comptage par categorie avec tableau indexe par type enumere
  Fichier source : 07-types-enumeres.md
  ============================================================================ }
program ComptageCategorie;
type
  TCouleur = (Rouge, Vert, Bleu, Jaune);

var
  compteurs: array[TCouleur] of Integer;
  couleur: TCouleur;
begin
  // Initialisation
  for couleur := Rouge to Jaune do
    compteurs[couleur] := 0;

  // Simulation de comptage
  compteurs[Rouge] := 5;
  compteurs[Vert] := 3;
  compteurs[Bleu] := 7;
  compteurs[Jaune] := 2;

  // Affichage
  WriteLn('Statistiques :');
  WriteLn('Rouge : ', compteurs[Rouge]);
  WriteLn('Vert : ', compteurs[Vert]);
  WriteLn('Bleu : ', compteurs[Bleu]);
  WriteLn('Jaune : ', compteurs[Jaune]);
end.
