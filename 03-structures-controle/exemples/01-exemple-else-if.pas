{ ============================================================================
  Section 3.1 : Instructions conditionnelles (if-then-else)
  Description : Exemple de chaîne else-if pour classification de notes
  Fichier source : 01-instructions-conditionnelles-if-then-else.md
  ============================================================================ }
program ExempleElseIf;  
var  
  note: Integer;
begin
  Write('Entrez votre note (0-20) : ');
  ReadLn(note);
  if note >= 16 then
    WriteLn('Excellent !')
  else if note >= 14 then
    WriteLn('Très bien')
  else if note >= 12 then
    WriteLn('Bien')
  else if note >= 10 then
    WriteLn('Assez bien')
  else
    WriteLn('Insuffisant');
end.
