{ ============================================================================
  Section 3.8 : Validation des entrees utilisateur
  Description : Validation de precision - note avec maximum 1 decimale
  Fichier source : 08-validation-entrees-utilisateur.md
  ============================================================================ }
program ValidationPrecision;
var
  note: Real;
  valide: Boolean;
begin
  repeat
    Write('Note (0.0 à 20.0, max 1 décimale) : ');
    ReadLn(note);

    valide := True;

    if (note < 0) or (note > 20) then
    begin
      WriteLn('❌ Note hors limites');
      valide := False;
    end
    // Trunc supprime les décimales sans arrondir ; cette astuce détecte plus d'1 décimale
    else if (note * 10) <> Trunc(note * 10) then
    begin
      WriteLn('❌ Maximum 1 décimale (ex: 15.5)');
      valide := False;
    end;
  until valide;

  WriteLn('✓ Note : ', note:0:1, '/20');
end.
