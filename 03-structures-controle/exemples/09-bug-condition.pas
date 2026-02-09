{ ============================================================================
  Section 3.9 : Debogage pas a pas
  Description : Bug - condition inversee (majeur/mineur intervertis)
  Fichier source : 09-debogage-pas-a-pas.md
  ============================================================================ }
program BugCondition;
var
  age: Integer;
begin
  Write('Âge : ');
  ReadLn(age);

  // BUG : condition inversée
  if age < 18 then
    WriteLn('Vous êtes majeur')
  else
    WriteLn('Vous êtes mineur');
end.
