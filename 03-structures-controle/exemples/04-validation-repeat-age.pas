{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Validation d'un age entre 0 et 120 avec boucle repeat-until
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program ValidationRepeatAge;  
var  
  age: Integer;
begin
  { Pas besoin d'initialiser age a une valeur invalide :
    repeat execute le corps au moins une fois, donc ReadLn s'execute d'abord }
  repeat
    Write('Entrez votre âge (0-120) : ');
    ReadLn(age);
    if (age < 0) or (age > 120) then
      WriteLn('Âge invalide ! Réessayez.');
  until (age >= 0) and (age <= 120);
  { Comparer avec la version while : while utilise (age < 0) or (age > 120)
    pour continuer, repeat utilise (age >= 0) and (age <= 120) pour s'arreter }
  WriteLn('Âge accepté : ', age, ' ans');
end.
