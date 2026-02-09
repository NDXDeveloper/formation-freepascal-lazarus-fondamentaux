{ ============================================================================
  Section 3.6 : Imbrication de structures
  Description : CASE avec IF imbriques - gestion de notes avec mentions
  Fichier source : 06-imbrication-structures.md
  ============================================================================ }
program GestionNotes;
var
  note: Integer;
  mention: String;
begin
  Write('Note (0-20) : ');
  ReadLn(note);

  case note of
    0..9:
      begin
        mention := 'Insuffisant';
        if note < 5 then
          WriteLn('Note très faible - Soutien nécessaire')
        else
          WriteLn('Note faible - Travail à intensifier');
      end;

    10..11:
      begin
        mention := 'Passable';
        WriteLn('Note juste suffisante');
      end;

    12..13:
      begin
        mention := 'Assez bien';
        WriteLn('Bon travail');
      end;

    14..15:
      begin
        mention := 'Bien';
        if note = 15 then
          WriteLn('Très bon travail !')
        else
          WriteLn('Bon travail');
      end;

    16..20:
      begin
        mention := 'Très bien';
        if note >= 18 then
          WriteLn('Excellent ! Félicitations !')
        else
          WriteLn('Très bon résultat');
      end;

  else
    // Le else d'un case..of n'a pas besoin de begin..end :
    // toutes les instructions entre else et end font partie du bloc
    mention := 'Invalide';
    WriteLn('Note hors limites');
  end;

  if (note >= 0) and (note <= 20) then
    WriteLn('Mention : ', mention);
end.
