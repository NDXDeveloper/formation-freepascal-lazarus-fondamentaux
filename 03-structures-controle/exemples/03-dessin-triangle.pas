{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Dessin d'un triangle d'etoiles avec boucles imbriquees
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program DessinTriangle;
var
  ligne, colonne: Integer;
begin
  WriteLn('Triangle d''étoiles :');
  WriteLn;
  for ligne := 1 to 5 do
  begin
    for colonne := 1 to ligne do
      Write('*');
    WriteLn;
  end;
end.
