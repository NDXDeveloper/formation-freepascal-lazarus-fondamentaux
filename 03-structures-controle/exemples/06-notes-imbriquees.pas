{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : IF-ELSE IF imbrique - determination de mention selon la note
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program NotesImbriquees;
var
  note: Integer;
  mention: String;
begin
  Write('Entrez la note (0-20) : ');
  ReadLn(note);

  if (note >= 0) and (note <= 20) then
  begin
    // Note valide, on détermine la mention
    // "else if" n'est pas un mot-cle unique en Pascal : c'est un else
    // dont la branche contient un nouveau if (pas besoin de begin..end
    // car chaque else contient une seule instruction : le if suivant)
    if note >= 16 then
      mention := 'Très bien'
    else if note >= 14 then
      mention := 'Bien'
    else if note >= 12 then
      mention := 'Assez bien'
    else if note >= 10 then
      mention := 'Passable'
    else
      mention := 'Insuffisant';

    WriteLn('Note : ', note, '/20');
    WriteLn('Mention : ', mention);
  end
  else
    WriteLn('Note invalide !');
end.
