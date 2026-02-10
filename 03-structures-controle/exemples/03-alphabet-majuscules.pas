{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Affichage de l'alphabet en majuscules avec un for sur Char
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program AlphabetMajuscules;  
var  
  lettre: Char;
begin
  WriteLn('L''alphabet en majuscules :');
  { for fonctionne avec tout type ordinal : Char parcourt l'ordre ASCII }
  for lettre := 'A' to 'Z' do
    Write(lettre, ' ');
  WriteLn;
end.
