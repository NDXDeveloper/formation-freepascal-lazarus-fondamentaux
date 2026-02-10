{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : UpperCase() et LowerCase() pour changer la casse
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ExempleCasse;  
uses SysUtils;  
var  
  texte: String;
begin
  texte := 'Pascal';

  WriteLn('Original : ', texte);
  WriteLn('Majuscules : ', UpperCase(texte));  // PASCAL
  WriteLn('Minuscules : ', LowerCase(texte));  // pascal
end.
