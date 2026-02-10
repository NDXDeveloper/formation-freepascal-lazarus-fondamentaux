{ ============================================================================
  Section 3.1 : Instructions conditionnelles (if-then-else)
  Description : Exemple d'instructions if imbriquées avec blocs begin..end
  Fichier source : 01-instructions-conditionnelles-if-then-else.md
  ============================================================================ }
program ExempleImbrique;  
var  
  age: Integer;
  permis: Boolean;
  reponse: String;
begin
  Write('Entrez votre âge : ');
  ReadLn(age);
  if age >= 18 then
  begin
    WriteLn('Vous êtes majeur.');
    Write('Avez-vous le permis de conduire ? (true/false) : ');
    ReadLn(reponse);
    permis := (reponse = 'true');
    if permis then
      WriteLn('Vous pouvez conduire.')
    else
      WriteLn('Vous devez passer le permis.');
  end  { Pas de ; après end quand un else suit : même règle que pour une instruction simple }
  else
    WriteLn('Vous êtes trop jeune pour conduire.');
end.
