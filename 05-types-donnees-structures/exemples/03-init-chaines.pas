{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Initialisation de chaines et concatenation
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program InitChaines;  
var  
  nom, prenom, complet: String;
begin
  nom := 'Dupont';
  prenom := 'Jean';
  complet := prenom + ' ' + nom;  // Concaténation

  WriteLn(complet);  // Affiche : Jean Dupont
end.
