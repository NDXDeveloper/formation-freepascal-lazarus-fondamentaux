{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Dessin d'un sapin avec hauteur saisie par l'utilisateur
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program GenerateurMotifs;  
var  
  ligne, espace, etoile: Integer;
  hauteur: Integer;
begin
  Write('Hauteur du sapin : ');
  ReadLn(hauteur);
  WriteLn;
  for ligne := 1 to hauteur do
  begin
    for espace := 1 to (hauteur - ligne) do
      Write(' ');
    for etoile := 1 to (2 * ligne - 1) do
      Write('*');
    WriteLn;
  end;
  for ligne := 1 to 2 do
  begin
    for espace := 1 to (hauteur - 1) do
      Write(' ');
    Write('|');
    WriteLn;
  end;
end.
