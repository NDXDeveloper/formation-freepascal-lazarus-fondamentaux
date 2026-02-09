{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Alignement de chaines a droite avec :largeur positive
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program AlignementDroite;
var
  nom: string;
begin
  nom := 'Alice';

  WriteLn('|', nom, '|');          // |Alice|
  WriteLn('|', nom:10, '|');       // |     Alice|
  WriteLn('|', nom:15, '|');       // |          Alice|
  WriteLn('|', nom:20, '|');       // |               Alice|
end.
