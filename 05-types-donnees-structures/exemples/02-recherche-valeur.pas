{ ============================================================================
  Section 5.2 : Tableaux multidimensionnels
  Description : Recherche d'une valeur dans un tableau 2D
  Fichier source : 02-tableaux-multidimensionnels.md
  ============================================================================ }
program RechercheValeur;  
var  
  grille: array[1..4, 1..5] of Integer;
  ligne, colonne, valeur: Integer;
  trouve: Boolean;
begin
  // Remplissage du tableau (exemple)
  for ligne := 1 to 4 do
    for colonne := 1 to 5 do
      grille[ligne, colonne] := ligne * colonne;

  // Recherche d'une valeur
  Write('Valeur à rechercher : ');
  ReadLn(valeur);

  trouve := False;
  for ligne := 1 to 4 do
  begin
    for colonne := 1 to 5 do
    begin
      if grille[ligne, colonne] = valeur then
      begin
        WriteLn('Trouvé en position [', ligne, ',', colonne, ']');
        trouve := True;
      end;
    end;
  end;

  if not trouve then
    WriteLn('Valeur non trouvée');
end.
