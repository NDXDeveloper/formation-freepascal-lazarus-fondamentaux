{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Conversion de notes lettres (A-F) en equivalents francais
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program ConversionNotes;  
var  
  noteLettre: Char;
begin
  Write('Entrez votre note (A, B, C, D, F) : ');
  ReadLn(noteLettre);
  Write('Equivalent francais : ');
  case noteLettre of
    'A', 'a': WriteLn('Excellent (16-20)');
    'B', 'b': WriteLn('Bien (14-15)');
    'C', 'c': WriteLn('Assez Bien (12-13)');
    'D', 'd': WriteLn('Passable (10-11)');
    'F', 'f': WriteLn('Insuffisant (0-9)');
  else
    WriteLn('Note invalide !');
  end;
end.
