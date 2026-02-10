{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Programme avec bug - division par 2 au lieu de 3 pour la
                moyenne de 3 notes
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program CalculMoyenneBug;  
var  
  note1, note2, note3: Integer;
  moyenne: Real;
begin
  WriteLn('Calcul de moyenne de 3 notes');

  Write('Note 1 : ');
  ReadLn(note1);
  Write('Note 2 : ');
  ReadLn(note2);
  Write('Note 3 : ');
  ReadLn(note3);

  // BUG : Division par 2 au lieu de 3 !
  moyenne := (note1 + note2 + note3) / 2;

  WriteLn('Moyenne : ', moyenne:0:2);
end.
