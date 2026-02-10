{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Modification d'un caractere dans une chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program ModificationCaractere;  
var  
  mot: String;
begin
  mot := 'Pascal';
  WriteLn('Avant : ', mot);

  mot[1] := 'B';  // On change le P en B
  WriteLn('Après : ', mot);  // Bascal
end.
