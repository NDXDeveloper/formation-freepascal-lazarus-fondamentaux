{ ============================================================================
  Section 2.4 : Types de donnees primitifs
  Description : Declaration et utilisation de variables de type char
                (caractere unique)
  Fichier source : 04-types-donnees-primitifs.md
  ============================================================================ }
program ExempleChar;  
var  
  initiale: char;
  note: char;
  symbole: char;
begin
  initiale := 'M';
  note := 'A';
  symbole := '@';

  writeln('Initiale : ', initiale);
  writeln('Note : ', note);
  writeln('Symbole : ', symbole);
end.
