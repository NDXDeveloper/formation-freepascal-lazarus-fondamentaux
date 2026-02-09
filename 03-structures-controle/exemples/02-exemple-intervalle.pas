{ ============================================================================
  Section 3.2 : Instructions de choix multiple (case-of)
  Description : Utilisation d'intervalles dans case-of pour categoriser un age
  Fichier source : 02-instructions-choix-multiple-case-of.md
  ============================================================================ }

program ExempleIntervalle;
var
  age: Integer;
begin
  Write('Entrez votre age : ');
  ReadLn(age);
  { La syntaxe 0..2 signifie "de 0 a 2 inclus" (intervalle) }
  case age of
    0..2: WriteLn('Bebe');
    3..5: WriteLn('Petite enfance');
    6..11: WriteLn('Enfance');
    12..17: WriteLn('Adolescence');
    18..64: WriteLn('Adulte');
    65..120: WriteLn('Senior');
  end;
end.
