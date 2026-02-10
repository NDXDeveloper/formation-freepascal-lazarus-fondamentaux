{ ============================================================================
  Section 3.7 : Gestion des erreurs simples
  Description : Validation simple de l'age avec condition if-else
  Fichier source : 07-gestion-erreurs-simples.md
  ============================================================================ }
program ValidationAgeSimple;  
var  
  age: Integer;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);

  if (age < 0) or (age > 150) then
  begin
    WriteLn('ERREUR : Âge invalide !');
    WriteLn('L''âge doit être entre 0 et 150.');
  end
  else
    WriteLn('Âge accepté : ', age, ' ans');
end.
