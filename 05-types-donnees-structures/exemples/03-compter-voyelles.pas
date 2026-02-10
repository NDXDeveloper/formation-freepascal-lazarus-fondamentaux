{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Compter les voyelles dans une phrase
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program CompterVoyelles;  
var  
  texte: String;
  i, compte: Integer;
  c: Char;
begin
  Write('Entrez une phrase : ');
  ReadLn(texte);

  compte := 0;
  for i := 1 to Length(texte) do
  begin
    c := UpCase(texte[i]);  // Convertir en majuscule
    if (c = 'A') or (c = 'E') or (c = 'I') or (c = 'O') or
       (c = 'U') or (c = 'Y') then
      compte := compte + 1;
  end;

  WriteLn('Nombre de voyelles : ', compte);
end.
