{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Inverser une chaine de caracteres
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program InverserChaine;
var
  texte, inverse: String;
  i: Integer;
begin
  Write('Entrez un mot : ');
  ReadLn(texte);

  inverse := '';
  for i := Length(texte) downto 1 do
    inverse := inverse + texte[i];

  WriteLn('Inversé : ', inverse);
end.
