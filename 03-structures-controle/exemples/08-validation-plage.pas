{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de plage - verifier qu'une valeur est dans un intervalle
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationPlage;
var
  note: Integer;
begin
  WriteLn('Entrez une note (0-20) :');

  repeat
    Write('Note : ');
    ReadLn(note);

    if (note < 0) or (note > 20) then
      WriteLn('❌ La note doit être entre 0 et 20');
  until (note >= 0) and (note <= 20);

  WriteLn('✓ Note enregistrée : ', note, '/20');
end.
