{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Acces aux caracteres individuels d'une chaine
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program AccesCaractere;
var
  mot: String;
  i: Integer;
begin
  mot := 'Pascal';

  // Accès au premier caractère
  WriteLn('Premier caractère : ', mot[1]);  // P

  // Accès au dernier caractère
  WriteLn('Dernier caractère : ', mot[Length(mot)]);  // l

  // Parcours de tous les caractères
  for i := 1 to Length(mot) do
    WriteLn('Caractère ', i, ' : ', mot[i]);
end.
