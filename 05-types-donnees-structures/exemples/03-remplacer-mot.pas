{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Remplacer toutes les occurrences d'un mot dans une chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program RemplacerMot;  
uses SysUtils;  
var  
  texte, ancien, nouveau: String;
  position: Integer;
begin
  texte := 'J''aime Python. Python est génial.';
  ancien := 'Python';
  nouveau := 'Pascal';

  WriteLn('Texte original : ', texte);

  // Remplacer toutes les occurrences
  repeat
    position := Pos(ancien, texte);
    if position > 0 then
    begin
      Delete(texte, position, Length(ancien));
      Insert(nouveau, texte, position);
    end;
  until position = 0;

  WriteLn('Texte modifié : ', texte);
end.
