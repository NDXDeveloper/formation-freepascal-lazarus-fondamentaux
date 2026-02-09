{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Validation d'un age entre 0 et 120 avec boucle while
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program ValidationAgeWhile;
var
  age: Integer;
begin
  age := -1;
  while (age < 0) or (age > 120) do
  begin
    Write('Entrez votre âge (0-120) : ');
    ReadLn(age);
    if (age < 0) or (age > 120) then
      WriteLn('Âge invalide ! Réessayez.');
  end;
  WriteLn('Âge accepté : ', age, ' ans');
end.
