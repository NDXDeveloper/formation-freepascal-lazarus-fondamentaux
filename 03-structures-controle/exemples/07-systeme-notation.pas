{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Systeme de notation avec validation et attribution de mention
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program SystemeNotation;  
var  
  note: Real;
  mention: String;
  valide: Boolean;
begin
  WriteLn('═══ SYSTÈME DE NOTATION ═══');
  WriteLn;

  valide := False;

  repeat
    Write('Entrez la note (0-20) : ');
    ReadLn(note);

    // Validation
    if note < 0 then
      WriteLn('❌ La note ne peut pas être négative')
    else if note > 20 then
      WriteLn('❌ La note ne peut pas dépasser 20')
    else
      valide := True;
  until valide;

  // Détermination de la mention
  if note < 10 then
    mention := 'Insuffisant'
  else if note < 12 then
    mention := 'Passable'
  else if note < 14 then
    mention := 'Assez bien'
  else if note < 16 then
    mention := 'Bien'
  else
    mention := 'Très bien';

  WriteLn;
  WriteLn('═══════════════════════════');
  WriteLn('Note : ', note:0:1, '/20');
  WriteLn('Mention : ', mention);
  WriteLn('═══════════════════════════');
end.
