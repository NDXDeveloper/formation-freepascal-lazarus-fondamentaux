{ ============================================================================
  Section 3.5 : Instructions break et continue
  Description : Utilisation de continue avec une boucle while
  Fichier source : 05-instructions-break-continue.md
  ============================================================================ }
program ContinueAvecWhile;  
var  
  i: Integer;
begin
  WriteLn('Nombres pairs de 1 à 20 :');

  i := 0;
  while i < 20 do
  begin
    // Important : incrémenter AVANT le Continue, sinon boucle infinie
    // (dans un for, l'incrémentation est automatique, pas dans un while)
    i := i + 1;

    if (i mod 2) = 1 then  // Si impair
      continue;  // Passe au suivant

    WriteLn(i);
  end;
end.
