{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Menu avec numerotation alignee grace a une boucle for
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program MenuNumerate;
var
  i: integer;
begin
  WriteLn;
  WriteLn('=== LISTE DES OPTIONS ===');
  WriteLn;

  for i := 1 to 10 do
    WriteLn(i:3, '. Option numéro ', i);

  WriteLn;
  WriteLn('=========================');
end.
