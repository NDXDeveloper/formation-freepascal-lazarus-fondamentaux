{ ============================================================================
  Section 3.4 : Boucles conditionnelles (while-do, repeat-until)
  Description : Melange repeat et for, avec choix de continuer O/N
  Fichier source : 04-boucles-conditionnelles-while-repeat.md
  ============================================================================ }

program MelangeBoucles;
var
  continuer: Char;
  i: Integer;
begin
  repeat
    WriteLn('Affichage des nombres de 1 à 5 :');
    for i := 1 to 5 do
      Write(i, ' ');
    WriteLn;
    WriteLn;
    Write('Continuer ? (O/N) : ');
    ReadLn(continuer);
    WriteLn;
  until (continuer = 'N') or (continuer = 'n');
  WriteLn('Au revoir !');
end.
