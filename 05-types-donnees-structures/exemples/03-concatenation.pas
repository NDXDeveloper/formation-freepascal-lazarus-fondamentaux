{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Concatenation de chaines avec l'operateur +
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program Concatenation;
var
  prenom, nom, phrase: String;
begin
  prenom := 'Marie';
  nom := 'Martin';

  // Assemblage de chaînes
  phrase := 'Bonjour ' + prenom + ' ' + nom + ' !';
  WriteLn(phrase);  // Bonjour Marie Martin !

  // On peut aussi mélanger avec des nombres
  phrase := prenom + ' a ' + '25' + ' ans';
  WriteLn(phrase);  // Marie a 25 ans
end.
