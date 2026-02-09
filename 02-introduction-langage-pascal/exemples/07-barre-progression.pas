{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Barre de progression textuelle avec caracteres = et espaces
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program BarreProgression;
var
  pourcentage: integer;
  i: integer;
begin
  pourcentage := 65;

  Write('Progression : [');

  // Afficher les caractères remplis
  for i := 1 to pourcentage div 5 do
    Write('=');

  // Afficher les caractères vides
  for i := 1 to 20 - (pourcentage div 5) do
    Write(' ');

  WriteLn('] ', pourcentage, '%');
end.
