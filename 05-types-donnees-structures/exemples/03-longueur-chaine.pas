{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Fonction Length() pour connaitre la longueur d'une chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program LongueurChaine;  
var  
  texte: String;
  taille: Integer;
begin
  texte := 'Bonjour';
  taille := Length(texte);
  WriteLn('Le mot "', texte, '" contient ', taille, ' caractères');
  // Affiche : Le mot "Bonjour" contient 7 caractères

  // Chaîne vide
  texte := '';
  WriteLn('Longueur de la chaîne vide : ', Length(texte));  // 0
end.
