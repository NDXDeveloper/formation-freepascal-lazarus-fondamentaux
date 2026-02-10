{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Comparaison insensible a la casse avec CompareText()
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ComparaisonInsensible;  
uses SysUtils;  
var  
  mot1, mot2: String;
begin
  mot1 := 'Pascal';
  mot2 := 'PASCAL';

  // Comparaison normale (sensible)
  if mot1 = mot2 then
    WriteLn('Identiques')
  else
    WriteLn('Différents');  // Différents

  // Comparaison insensible à la casse
  if CompareText(mot1, mot2) = 0 then  // 0 signifie « égal » (insensible à la casse)
    WriteLn('Identiques (insensible)');  // Identiques (insensible)
end.
