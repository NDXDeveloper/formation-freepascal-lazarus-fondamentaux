{ ============================================================================
  Section 3.1 : Instructions conditionnelles (if-then-else)
  Description : Exemple simple d'instruction if-then
  Fichier source : 01-instructions-conditionnelles-if-then-else.md
  ============================================================================ }
program ExempleIfSimple;
var
  age: Integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
  if age >= 18 then
    WriteLn('Vous êtes majeur.');
  WriteLn('Programme terminé.');
end.
