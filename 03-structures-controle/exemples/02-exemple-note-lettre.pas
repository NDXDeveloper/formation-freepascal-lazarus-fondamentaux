{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Conversion d'une note numerique en mention avec case-of
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program ExempleNoteLettre;  
var  
  note: Integer;
begin
  Write('Entrez votre note (0-20) : ');
  ReadLn(note);
  { Virgule = plusieurs valeurs pour un meme cas (pas besoin de repeter l'instruction) }
  case note of
    18, 19, 20: WriteLn('Mention Excellent');
    16, 17: WriteLn('Mention Tres Bien');
    14, 15: WriteLn('Mention Bien');
    12, 13: WriteLn('Mention Assez Bien');
    10, 11: WriteLn('Passable');
  end;
end.
