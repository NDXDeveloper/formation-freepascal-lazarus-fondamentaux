{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Validation avec repeat-until pour un nombre positif
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program ValidationRepeatNombre;
var
  nombre: Integer;
begin
  WriteLn('Entrez un nombre positif :');

  repeat
    Write('Nombre : ');
    ReadLn(nombre);

    if nombre <= 0 then
      WriteLn('❌ Le nombre doit être positif !')
    else
      WriteLn('✓ Nombre accepté : ', nombre);
  until nombre > 0;
end.
