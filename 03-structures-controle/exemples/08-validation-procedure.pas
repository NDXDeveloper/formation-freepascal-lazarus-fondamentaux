{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation avec procedure - encapsuler la logique de validation
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationProcedure;

procedure LireEntierDansIntervalle(message: String; min, max: Integer; var resultat: Integer);
begin
  repeat
    Write(message, ' (', min, '-', max, ') : ');
    ReadLn(resultat);

    if (resultat < min) or (resultat > max) then
      WriteLn('❌ Valeur hors limites');
  until (resultat >= min) and (resultat <= max);
end;

var
  age, note: Integer;
begin
  LireEntierDansIntervalle('Âge', 0, 150, age);
  WriteLn('✓ Âge : ', age);

  LireEntierDansIntervalle('Note', 0, 20, note);
  WriteLn('✓ Note : ', note);
end.
