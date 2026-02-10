{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Trim() pour supprimer les espaces en debut et fin de chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ExempleTrim;  
uses SysUtils;  
var  
  texte: String;
begin
  texte := '   Bonjour   ';

  WriteLn('[', texte, ']');        // [   Bonjour   ]
  WriteLn('[', Trim(texte), ']');  // [Bonjour]
end.
