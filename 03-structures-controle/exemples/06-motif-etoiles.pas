{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Boucles FOR imbriquees - dessin d'un triangle d'etoiles
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program MotifEtoiles;  
var  
  ligne, espace, etoile: Integer;
  hauteur: Integer;
begin
  Write('Hauteur du triangle : ');
  ReadLn(hauteur);
  WriteLn;

  for ligne := 1 to hauteur do
  begin
    // Espaces avant les étoiles
    for espace := 1 to (hauteur - ligne) do
      Write(' ');

    // Étoiles
    for etoile := 1 to (2 * ligne - 1) do
      Write('*');

    WriteLn;
  end;
end.
