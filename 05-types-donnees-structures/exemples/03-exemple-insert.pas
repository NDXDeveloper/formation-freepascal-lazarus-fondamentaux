{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Procedure Insert() pour inserer du texte dans une chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ExempleInsert;  
var  
  texte: String;
begin
  texte := 'Bonjour monde';
  WriteLn('Avant : ', texte);

  // Insérer "le " à la position 9
  Insert('le ', texte, 9);
  WriteLn('Après : ', texte);  // Bonjour le monde
end.
