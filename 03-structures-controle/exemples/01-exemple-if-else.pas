{ ============================================================================
  Section 3.1 : Instructions conditionnelles (if-then-else)
  Description : Exemple d'instruction if-then-else
  Fichier source : 01-instructions-conditionnelles-if-then-else.md
  ============================================================================ }
program ExempleIfElse;
var
  age: Integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
  if age >= 18 then
    WriteLn('Vous êtes majeur.')  { Pas de point-virgule ici : en Pascal, un ; avant else provoque une erreur }
  else
    WriteLn('Vous êtes mineur.');
end.
