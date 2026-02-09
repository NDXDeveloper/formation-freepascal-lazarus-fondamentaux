{ ============================================================================
  Section 4.4 : Parametres par reference (var)
  Description : Systeme de coordonnees avec parametres var
  Fichier source : 04-parametres-par-reference-var.md
  ============================================================================ }
program GestionCoordonnees;

// Initialiser des coordonnées
procedure InitialiserPosition(var x, y: Integer);
begin
  x := 0;
  y := 0;
end;

// Déplacer vers la droite
procedure DeplacerDroite(var x: Integer; distance: Integer);
begin
  x := x + distance;
end;

// Déplacer vers le haut
procedure DeplacerHaut(var y: Integer; distance: Integer);
begin
  y := y + distance;
end;

// Afficher la position
procedure AfficherPosition(x, y: Integer);
begin
  WriteLn('Position actuelle : (', x, ', ', y, ')');
end;

var
  posX, posY: Integer;
begin
  InitialiserPosition(posX, posY);
  AfficherPosition(posX, posY);  // (0, 0)

  DeplacerDroite(posX, 5);
  AfficherPosition(posX, posY);  // (5, 0)

  DeplacerHaut(posY, 3);
  AfficherPosition(posX, posY);  // (5, 3)

  DeplacerDroite(posX, -2);
  AfficherPosition(posX, posY);  // (3, 3)
end.
