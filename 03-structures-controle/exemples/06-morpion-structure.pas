{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Jeu du morpion - structure complete avec grille, saisie et validation
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program MorpionStructure;  
type  
  TGrille = array[1..3, 1..3] of Char;
var
  grille: TGrille;
  ligne, colonne: Integer;
  joueur: Char;
  continuer: Boolean;
  coups: Integer;
begin
  // Initialisation
  for ligne := 1 to 3 do
    for colonne := 1 to 3 do
      grille[ligne, colonne] := ' ';

  joueur := 'X';
  continuer := True;
  coups := 0;

  WriteLn('=== JEU DU MORPION ===');

  while continuer and (coups < 9) do
  begin
    // Affichage de la grille
    WriteLn;
    for ligne := 1 to 3 do
    begin
      for colonne := 1 to 3 do
      begin
        Write(' ', grille[ligne, colonne], ' ');
        if colonne < 3 then
          Write('|');
      end;
      WriteLn;

      if ligne < 3 then
        WriteLn('-----------');
    end;

    WriteLn;
    WriteLn('Joueur ', joueur);

    // Saisie avec validation
    // repeat..until False = boucle infinie volontaire, on en sort
    // uniquement par break. L'idiome simule une boucle avec sortie
    // au milieu du corps (ni au debut comme while, ni a la fin)
    repeat
      Write('Ligne (1-3) : ');
      ReadLn(ligne);
      Write('Colonne (1-3) : ');
      ReadLn(colonne);

      if (ligne < 1) or (ligne > 3) or (colonne < 1) or (colonne > 3) then
        WriteLn('Position invalide !')
      else if grille[ligne, colonne] <> ' ' then
        WriteLn('Case déjà occupée !')
      else
        break;  // Position valide
    until False;

    // Placer le symbole
    grille[ligne, colonne] := joueur;
    coups := coups + 1;

    // Vérification victoire (simplifié)
    // Dans un vrai jeu, il faudrait vérifier lignes, colonnes, diagonales

    // Changement de joueur
    if joueur = 'X' then
      joueur := 'O'
    else
      joueur := 'X';
  end;

  // Affichage final
  WriteLn;
  WriteLn('=== GRILLE FINALE ===');
  for ligne := 1 to 3 do
  begin
    for colonne := 1 to 3 do
    begin
      Write(' ', grille[ligne, colonne], ' ');
      if colonne < 3 then
        Write('|');
    end;
    WriteLn;

    if ligne < 3 then
      WriteLn('-----------');
  end;
end.
