{ ============================================================================
  Section 3.3 : Boucles compteur (for-do)
  Description : Affichage des voyelles en minuscules avec un for sur Char
  Fichier source : 03-boucles-compteur-for-do.md
  ============================================================================ }

program Voyelles;
var
  lettre: Char;
begin
  WriteLn('Voyelles en minuscules :');
  for lettre := 'a' to 'e' do
    WriteLn(lettre);
end.
