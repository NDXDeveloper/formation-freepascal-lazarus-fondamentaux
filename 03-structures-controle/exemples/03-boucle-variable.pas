{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Boucle for avec bornes saisies par l'utilisateur
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program BoucleVariable;
var
  i, debut, fin: Integer;
begin
  Write('Nombre de départ : ');
  ReadLn(debut);
  Write('Nombre de fin : ');
  ReadLn(fin);
  WriteLn;
  WriteLn('Comptage de ', debut, ' à ', fin, ' :');
  for i := debut to fin do
    WriteLn(i);
end.
