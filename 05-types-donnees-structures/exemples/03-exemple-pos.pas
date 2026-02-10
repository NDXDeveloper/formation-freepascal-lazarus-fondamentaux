{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Fonction Pos() pour chercher dans une chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ExemplePos;  
var  
  texte: String;
  position: Integer;
begin
  texte := 'Pascal est un langage génial';

  // Chercher "langage"
  position := Pos('langage', texte);
  if position > 0 then
    WriteLn('Trouvé à la position : ', position)  // 15
  else
    WriteLn('Non trouvé');

  // Chercher quelque chose qui n'existe pas
  position := Pos('Python', texte);
  if position = 0 then
    WriteLn('Python non trouvé dans le texte');
end.
