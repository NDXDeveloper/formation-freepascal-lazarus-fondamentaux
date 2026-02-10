{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Recherche dans un tableau avec valeur sentinelle
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program RechercheAvecSentinelle;  
const  
  TAILLE = 10;
  NON_TROUVE = -1;  // Valeur sentinelle
var
  tableau: array[1..TAILLE] of Integer;
  i, recherche, position: Integer;
begin
  // Remplissage
  for i := 1 to TAILLE do
    tableau[i] := i * 5;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  // Recherche
  position := NON_TROUVE;
  for i := 1 to TAILLE do
  begin
    if tableau[i] = recherche then
    begin
      position := i;
      break;
    end;
  end;

  // Vérification du résultat
  if position = NON_TROUVE then
    WriteLn('✗ Nombre non trouvé')
  else
    WriteLn('✓ Nombre trouvé à la position ', position);
end.
