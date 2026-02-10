{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Recherche optimisee dans un tableau trie avec arret anticipe
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program RechercheOptimisee;  
const  
  TAILLE = 100;
var
  tableau: array[1..TAILLE] of Integer;
  i, recherche, position: Integer;
begin
  // Initialisation
  for i := 1 to TAILLE do
    tableau[i] := i * 2;

  Write('Nombre à rechercher : ');
  ReadLn(recherche);

  position := -1;

  for i := 1 to TAILLE do
  begin
    // Optimisation : si le tableau est trié et qu'on dépasse
    // la valeur recherchée, inutile de continuer
    if tableau[i] > recherche then
    begin
      WriteLn('Valeur dépassée, arrêt de la recherche');
      break;
    end;

    if tableau[i] = recherche then
    begin
      position := i;
      break;
    end;
  end;

  if position <> -1 then
    WriteLn('Trouvé à la position ', position)
  else
    WriteLn('Non trouvé');
end.
