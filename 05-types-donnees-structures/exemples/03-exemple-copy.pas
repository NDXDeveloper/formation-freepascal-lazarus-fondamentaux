{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Fonction Copy() pour extraire une portion de chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ExempleCopy;  
var  
  texte, extrait: String;
begin
  texte := 'Bonjour tout le monde';

  // Extraire à partir de la position 1, 7 caractères
  extrait := Copy(texte, 1, 7);
  WriteLn(extrait);  // Bonjour

  // Extraire "monde"
  extrait := Copy(texte, 17, 5);
  WriteLn(extrait);  // monde

  // Si on demande trop de caractères, ça s'arrête à la fin
  extrait := Copy(texte, 1, 100);
  WriteLn(extrait);  // Bonjour tout le monde
end.
