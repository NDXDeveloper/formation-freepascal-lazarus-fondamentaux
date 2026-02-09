{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Creation de chaines sur plusieurs lignes
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ChainesMultiligne;
var
  message: String;
begin
  // Méthode 1 : Concaténation
  message := 'Première ligne' + #13#10 +  // #N = caractère ASCII N ; #13#10 = saut de ligne
             'Deuxième ligne' + #13#10 +
             'Troisième ligne';
  WriteLn(message);

  // Méthode 2 : Utiliser la constante système
  message := 'Ligne 1' + LineEnding + 'Ligne 2';  // LineEnding : constante FPC multi-plateforme
  WriteLn(message);
end.
