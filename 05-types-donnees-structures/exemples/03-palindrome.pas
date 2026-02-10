{ ============================================================================
  Section 5.3 : Chaines de caracteres (String, ShortString)
  Description : Verifier si un mot est un palindrome
  Fichier source : 03-chaines-caracteres-string.md
  ============================================================================ }
program Palindrome;  
var  
  texte: String;
  i, j: Integer;
  estPalindrome: Boolean;
begin
  Write('Entrez un mot : ');
  ReadLn(texte);

  estPalindrome := True;
  j := Length(texte);

  for i := 1 to Length(texte) div 2 do
  begin
    if texte[i] <> texte[j] then
    begin
      estPalindrome := False;
      Break;
    end;
    j := j - 1;
  end;

  if estPalindrome then
    WriteLn(texte, ' est un palindrome')
  else
    WriteLn(texte, ' n''est pas un palindrome');
end.
