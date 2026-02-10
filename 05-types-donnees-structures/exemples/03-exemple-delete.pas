{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Procedure Delete() pour supprimer des caracteres
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ExempleDelete;  
var  
  texte: String;
begin
  texte := 'Bonjour tout le monde';
  WriteLn('Avant : ', texte);

  // Supprimer "tout le " (8 caractères à partir de la position 9)
  Delete(texte, 9, 8);
  WriteLn('Après : ', texte);  // Bonjour monde
end.
