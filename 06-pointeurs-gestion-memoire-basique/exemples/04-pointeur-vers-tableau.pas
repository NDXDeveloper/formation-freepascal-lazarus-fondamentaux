{ ============================================================================
  Section 6.4 : Pointeurs et Tableaux
  Description : Pointeur vers un tableau entier - acces et modification
  Fichier source : 04-pointeurs-tableaux.md
  ============================================================================ }
program PointeurVersTableau;  
type  
  TTableau5 = array[1..5] of Integer;
  PTableau5 = ^TTableau5;
var
  nombres: TTableau5;
  pTableau: PTableau5;
  i: Integer;
begin
  // Initialisation du tableau
  for i := 1 to 5 do
    nombres[i] := i * 10;

  // Le pointeur pointe vers le tableau
  pTableau := @nombres;

  // Accès via le pointeur
  WriteLn('Premier élément : ', pTableau^[1]);
  WriteLn('Dernier élément : ', pTableau^[5]);

  // Modification via le pointeur
  pTableau^[3] := 999;
  WriteLn('Element 3 modifié : ', nombres[3]);  // Affiche 999
end.
