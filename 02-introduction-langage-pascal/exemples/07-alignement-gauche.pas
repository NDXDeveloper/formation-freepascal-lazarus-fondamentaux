{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Alignement de chaines a gauche avec :largeur negative
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program AlignementGauche;
var
  nom: string;
begin
  nom := 'Alice';

  WriteLn('|', nom:-10, '|');      // |Alice     |
  WriteLn('|', nom:-15, '|');      // |Alice          |
  WriteLn('|', nom:-20, '|');      // |Alice               |
end.
