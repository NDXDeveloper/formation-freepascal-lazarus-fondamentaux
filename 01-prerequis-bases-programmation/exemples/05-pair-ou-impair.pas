{ ============================================================================
  Section 1.5 : Algorithmes et pseudo-code
  Description : Verifie si un nombre entier saisi est pair ou impair.
                Illustre la structure conditionnelle if/else
  Fichier source : 05-algorithmes-pseudo-code.md
  ============================================================================ }
program PairOuImpair;
var
  nombre: Integer;
begin
  WriteLn('Entrez un nombre entier : ');
  ReadLn(nombre);

  if (nombre mod 2) = 0 then
    WriteLn(nombre, ' est pair')
  else
    WriteLn(nombre, ' est impair');
end.
