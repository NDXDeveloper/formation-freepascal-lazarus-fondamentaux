{ ============================================================================
  Section 2.7 : Formatage de sortie
  Description : Affichage de notes avec appreciation et etoiles
  Fichier source : 07-formatage-sortie.md
  ============================================================================ }
program NotesFormatees;  
var  
  note: real;
  appreciation: string;
  etoiles: string;
begin
  note := 15.5;

  if note >= 18 then
  begin
    appreciation := 'Excellent';
    etoiles := '*****';
  end
  else if note >= 16 then
  begin
    appreciation := 'Très bien';
    etoiles := '****';
  end
  else if note >= 14 then
  begin
    appreciation := 'Bien';
    etoiles := '***';
  end
  else if note >= 12 then
  begin
    appreciation := 'Assez bien';
    etoiles := '**';
  end
  else if note >= 10 then
  begin
    appreciation := 'Passable';
    etoiles := '*';
  end
  else
  begin
    appreciation := 'Insuffisant';
    etoiles := '';
  end;

  WriteLn('Note : ', note:4:1, '/20');
  WriteLn('Appréciation : ', appreciation:-15, ' ', etoiles);
end.
