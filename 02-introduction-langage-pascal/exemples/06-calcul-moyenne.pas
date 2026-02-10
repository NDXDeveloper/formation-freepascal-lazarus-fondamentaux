{ ============================================================================
  Section 2.6 : Entrees/sorties console
  Description : Calcul de moyenne de 3 notes saisies avec appreciation
                automatique (Tres bien, Bien, Assez bien, etc.)
  Fichier source : 06-entrees-sorties-console.md
  ============================================================================ }
program CalculMoyenneIO;  
var  
  note1, note2, note3: real;
  moyenne: real;
  appreciation: string;
begin
  WriteLn('=== CALCUL DE MOYENNE ===');
  WriteLn;

  Write('Note 1 : ');
  ReadLn(note1);

  Write('Note 2 : ');
  ReadLn(note2);

  Write('Note 3 : ');
  ReadLn(note3);

  moyenne := (note1 + note2 + note3) / 3;

  WriteLn;
  WriteLn('--- RÉSULTAT ---');
  WriteLn('Note 1   : ', note1:5:2);
  WriteLn('Note 2   : ', note2:5:2);
  WriteLn('Note 3   : ', note3:5:2);
  WriteLn('----------------');
  WriteLn('Moyenne  : ', moyenne:5:2);

  if moyenne >= 16 then
    appreciation := 'Très bien'
  else if moyenne >= 14 then
    appreciation := 'Bien'
  else if moyenne >= 12 then
    appreciation := 'Assez bien'
  else if moyenne >= 10 then
    appreciation := 'Passable'
  else
    appreciation := 'Insuffisant';

  WriteLn('Appréciation : ', appreciation);

  WriteLn;
  Write('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
