{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : Boucles FOR imbriquees - table de multiplication 2D (1 a 10)
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program TableMultiplication2D;
var
  ligne, colonne: Integer;
begin
  WriteLn('TABLE DE MULTIPLICATION (1 à 10)');
  WriteLn;

  // En-tête
  Write('   ');
  for colonne := 1 to 10 do
    Write(colonne:4);
  WriteLn;
  WriteLn('   ', StringOfChar('-', 40));

  // Contenu
  for ligne := 1 to 10 do
  begin
    Write(ligne:2, ' |');

    for colonne := 1 to 10 do
      Write((ligne * colonne):4);

    WriteLn;
  end;
end.
