{ ============================================================================
  Section 1.5 : Algorithmes et pseudo-code
  Description : Calcule la factorielle d'un nombre avec une fonction
                recursive. Illustre la recursivite (une fonction qui
                s'appelle elle-meme)
  Fichier source : 05-algorithmes-pseudo-code.md
  ============================================================================ }
program Factorielle;

function Fact(n: Integer): Integer;  
begin  
  if n <= 1 then
    Fact := 1
  else
    Fact := n * Fact(n - 1);
end;

var
  nombre, resultat: Integer;

begin
  WriteLn('Entrez un nombre : ');
  ReadLn(nombre);

  resultat := Fact(nombre);

  WriteLn('Factorielle de ', nombre, ' = ', resultat);
end.
